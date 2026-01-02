# down: int
# dist: int (Distance to go.)
# yardline_100: int (Numeric distance in the number of yards from the opponent's endzone for the posteam.)
# half: Literal[1, 2]
# half_seconds_remaining: int
# score: int (score_differential)

# How to calculate Target:
# `result` (home_score - away_score) but flip for posteam (possesion team) and defteam (defending team).
# By flipping for the correct team, you then turn it into a binary by determining if it's positive (win)
# or negative (loss.)

import pickle
from pathlib import Path

import numpy as np
import polars as pl


def load_data(sample_frac: float = 0.3) -> pl.DataFrame:
    """Load and sample play-by-play data."""
    data_dir = Path(__file__).parent.parent / "data"

    dfs = []
    for parquet_file in data_dir.glob("play_by_play_*.parquet"):
        df = pl.read_parquet(parquet_file)
        dfs.append(df)

    combined = pl.concat(dfs)

    # Select relevant columns and filter valid rows
    df = combined.select(
        [
            "down",
            "ydstogo",
            "yardline_100",
            "game_half",
            "half_seconds_remaining",
            "score_differential",
            "result",
            "posteam_type",  # "home" or "away"
        ]
    ).filter(
        pl.col("down").is_not_null()
        & pl.col("ydstogo").is_not_null()
        & pl.col("yardline_100").is_not_null()
        & pl.col("half_seconds_remaining").is_not_null()
        & pl.col("score_differential").is_not_null()
        & pl.col("result").is_not_null()
        & pl.col("posteam_type").is_not_null()
        & pl.col("game_half").is_in(["Half1", "Half2"])
    )

    # Sample for speed
    return df.sample(fraction=sample_frac, seed=42)


def create_target(df: pl.DataFrame) -> pl.DataFrame:
    """Create binary win target adjusted for possession team."""
    return df.with_columns(
        pl.when(pl.col("posteam_type") == "home")
        .then(pl.col("result"))
        .otherwise(-pl.col("result"))
        .alias("posteam_result")
    ).with_columns((pl.col("posteam_result") > 0).cast(pl.Int8).alias("win"))


def build_features(
    down: np.ndarray,
    dist: np.ndarray,
    yardline_100: np.ndarray,
    half: np.ndarray,
    half_seconds_remaining: np.ndarray,
    score: np.ndarray,
) -> np.ndarray:
    """
    Build feature matrix with interaction terms.

    Base features (normalized):
    - down, dist, yardline_100, half, half_seconds_remaining, score

    Interaction terms for non-linearity:
    - score * time_remaining (score matters more late)
    - yardline * down (field position context by down)
    - dist * down (distance context by down)
    - score^2 (non-linear score effect)
    - time^2 (non-linear time effect)
    """
    n = len(down)

    # Normalize features for stability
    down_norm = down / 4.0
    dist_norm = dist / 30.0  # Most distances under 30
    yard_norm = yardline_100 / 100.0
    half_norm = half - 1.0  # 0 or 1
    time_norm = half_seconds_remaining / 1800.0
    score_norm = score / 28.0  # ~4 TDs as scale

    # Build feature matrix with interactions
    features = np.column_stack(
        [
            np.ones(n),  # Intercept
            # Base features
            down_norm,
            dist_norm,
            yard_norm,
            half_norm,
            time_norm,
            score_norm,
            # Interaction terms
            score_norm * time_norm,  # Score matters more late game
            score_norm * half_norm,  # Score matters more in 2nd half
            yard_norm * down_norm,  # Field position by down
            dist_norm * down_norm,  # Distance by down
            # Polynomial terms
            score_norm**2,  # Non-linear score
            time_norm**2,  # Non-linear time
            yard_norm**2,  # Non-linear field position
        ]
    )

    return features


def sigmoid(z: np.ndarray) -> np.ndarray:
    """Numerically stable sigmoid."""
    return np.where(z >= 0, 1 / (1 + np.exp(-z)), np.exp(z) / (1 + np.exp(z)))


def log_loss(y: np.ndarray, p: np.ndarray, eps: float = 1e-15) -> float:
    """Binary cross-entropy loss."""
    p = np.clip(p, eps, 1 - eps)
    return -np.mean(y * np.log(p) + (1 - y) * np.log(1 - p))


def fit_logistic(
    X: np.ndarray,
    y: np.ndarray,
    lr: float = 0.5,
    n_iter: int = 1000,
    tol: float = 1e-6,
) -> np.ndarray:
    """Fit logistic regression via gradient descent."""
    n_samples, n_features = X.shape
    weights = np.zeros(n_features)

    prev_loss = float("inf")
    for i in range(n_iter):
        z = X @ weights
        p = sigmoid(z)

        # Gradient
        grad = X.T @ (p - y) / n_samples
        weights -= lr * grad

        # Check convergence
        if i % 100 == 0:
            loss = log_loss(y, p)
            if abs(prev_loss - loss) < tol:
                print(f"Converged at iteration {i}")
                break
            prev_loss = loss

    return weights


def main():
    print("Loading data...")
    df = load_data(sample_frac=0.3)
    print(f"Loaded {len(df):,} plays")

    # Create target
    df = create_target(df)

    # Convert half to numeric
    df = df.with_columns(
        pl.when(pl.col("game_half") == "Half1").then(1).otherwise(2).alias("half")
    )

    # Extract arrays
    down = df["down"].to_numpy().astype(np.float64)
    dist = df["ydstogo"].to_numpy().astype(np.float64)
    yardline_100 = df["yardline_100"].to_numpy().astype(np.float64)
    half = df["half"].to_numpy().astype(np.float64)
    half_seconds_remaining = df["half_seconds_remaining"].to_numpy().astype(np.float64)
    score = df["score_differential"].to_numpy().astype(np.float64)
    y = df["win"].to_numpy().astype(np.float64)

    # Build features
    print("Building features...")
    X = build_features(down, dist, yardline_100, half, half_seconds_remaining, score)
    print(f"Feature matrix shape: {X.shape}")

    # Train/test split (80/20)
    n = len(y)
    indices = np.random.default_rng(42).permutation(n)
    split = int(0.8 * n)
    train_idx, test_idx = indices[:split], indices[split:]

    X_train, X_test = X[train_idx], X[test_idx]
    y_train, y_test = y[train_idx], y[test_idx]

    # Fit model
    print("Training logistic regression...")
    weights = fit_logistic(X_train, y_train, lr=0.5, n_iter=2000)

    # Predictions
    p_train = sigmoid(X_train @ weights)
    p_test = sigmoid(X_test @ weights)

    # Metrics
    train_loss = log_loss(y_train, p_train)
    test_loss = log_loss(y_test, p_test)

    # RMSE (as requested)
    train_rmse = np.sqrt(np.mean((y_train - p_train) ** 2))
    test_rmse = np.sqrt(np.mean((y_test - p_test) ** 2))

    # Accuracy
    train_acc = np.mean((p_train > 0.5) == y_train)
    test_acc = np.mean((p_test > 0.5) == y_test)

    # Brier score (proper scoring rule for probabilities)
    train_brier = np.mean((y_train - p_train) ** 2)
    test_brier = np.mean((y_test - p_test) ** 2)

    print("\n" + "=" * 50)
    print("METRICS")
    print("=" * 50)
    print(f"{'Metric':<20} {'Train':>12} {'Test':>12}")
    print("-" * 50)
    print(f"{'Log Loss':<20} {train_loss:>12.4f} {test_loss:>12.4f}")
    print(f"{'RMSE':<20} {train_rmse:>12.4f} {test_rmse:>12.4f}")
    print(f"{'Brier Score':<20} {train_brier:>12.4f} {test_brier:>12.4f}")
    print(f"{'Accuracy':<20} {train_acc:>12.2%} {test_acc:>12.2%}")

    # Examples
    print("\n" + "=" * 50)
    print("EXAMPLE PREDICTIONS")
    print("=" * 50)

    # Sample some test cases
    sample_indices = np.random.default_rng(123).choice(
        len(test_idx), size=10, replace=False
    )

    print(
        f"{'Down':<5} {'Dist':<5} {'Yard':<5} {'Half':<5} {'Time':<6} {'Score':<6} {'Pred':<8} {'Actual':<8}"
    )
    print("-" * 60)

    for i in sample_indices:
        idx = test_idx[i]
        print(
            f"{int(down[idx]):<5} "
            f"{int(dist[idx]):<5} "
            f"{int(yardline_100[idx]):<5} "
            f"{int(half[idx]):<5} "
            f"{int(half_seconds_remaining[idx]):<6} "
            f"{int(score[idx]):<6} "
            f"{p_test[np.where(test_idx == idx)[0][0]]:<8.3f} "
            f"{int(y[idx]):<8}"
        )

    # Sanity check examples
    print("\n" + "=" * 50)
    print("SANITY CHECK SCENARIOS")
    print("=" * 50)

    def calc_prob(d: int, di: int, yl: int, h: int, t: int, s: int) -> float:
        feat = build_features(
            np.array([d], dtype=np.float64),
            np.array([di], dtype=np.float64),
            np.array([yl], dtype=np.float64),
            np.array([h], dtype=np.float64),
            np.array([t], dtype=np.float64),
            np.array([s], dtype=np.float64),
        )
        return float(sigmoid(feat @ weights)[0])

    # (description, down, dist, yardline, half, time, score, min_wp, max_wp)
    scenarios = [
        ("1st & 10, own 25, 1st half, 15:00, tied", 1, 10, 75, 1, 900, 0, 0.40, 0.60),
        ("1st & 10, own 25, 2nd half, 2:00, up 14", 1, 10, 75, 2, 120, 14, 0.90, 1.00),
        (
            "1st & 10, own 25, 2nd half, 2:00, down 14",
            1,
            10,
            75,
            2,
            120,
            -14,
            0.00,
            0.10,
        ),
        ("4th & 1, opp 1, 2nd half, 0:30, down 4", 4, 1, 1, 2, 30, -4, 0.20, 0.70),
        ("1st & goal, opp 5, 2nd half, 0:10, down 6", 1, 10, 5, 2, 10, -6, 0.15, 0.60),
    ]

    print(f"{'Scenario':<45} {'Win Prob':>10} {'Expected':>15}")
    print("-" * 75)

    for desc, d, di, yl, h, t, s, min_wp, max_wp in scenarios:
        prob = calc_prob(d, di, yl, h, t, s)
        expected = f"[{min_wp:.0%}-{max_wp:.0%}]"
        print(f"{desc:<45} {prob:>10.1%} {expected:>15}")

    # Sanity check assertions
    print("\n" + "=" * 50)
    print("SANITY CHECK ASSERTIONS")
    print("=" * 50)

    # TODO: Test the default state is roughly 50%

    for desc, d, di, yl, h, t, s, min_wp, max_wp in scenarios:
        prob = calc_prob(d, di, yl, h, t, s)
        assert min_wp <= prob <= max_wp, (
            f"FAIL: {desc} -> {prob:.1%} not in [{min_wp:.0%}-{max_wp:.0%}]"
        )
        print(f"PASS: {desc}")

    # Out of sample sanity checks
    print("\n" + "=" * 50)
    print("OUT OF SAMPLE CHECKS")
    print("=" * 50)

    # All test predictions should be in [0, 1]
    assert np.all((p_test >= 0) & (p_test <= 1)), (
        "FAIL: Test predictions outside [0, 1]"
    )
    print(f"PASS: All {len(p_test):,} test predictions in [0, 1]")

    # No extreme predictions (< 0.001 or > 0.999) for neutral situations
    # Check predictions where score is close to 0 and time > 5 min
    neutral_mask = (np.abs(score[test_idx]) <= 3) & (
        half_seconds_remaining[test_idx] > 300
    )
    neutral_preds = p_test[neutral_mask]
    if len(neutral_preds) > 0:
        assert np.all((neutral_preds >= 0.10) & (neutral_preds <= 0.90)), (
            f"FAIL: Extreme predictions for neutral situations: min={neutral_preds.min():.3f}, max={neutral_preds.max():.3f}"
        )
        print(
            f"PASS: {len(neutral_preds):,} neutral situations have reasonable predictions [{neutral_preds.min():.1%}-{neutral_preds.max():.1%}]"
        )

    # Check that being ahead late increases WP
    ahead_late = calc_prob(1, 10, 50, 2, 120, 7)  # Up 7, 2 min left
    behind_late = calc_prob(1, 10, 50, 2, 120, -7)  # Down 7, 2 min left
    assert ahead_late > behind_late, (
        f"FAIL: Ahead late ({ahead_late:.1%}) should be > behind late ({behind_late:.1%})"
    )
    print(f"PASS: Up 7 late ({ahead_late:.1%}) > Down 7 late ({behind_late:.1%})")

    # Check that score matters more late than early
    ahead_early = calc_prob(1, 10, 50, 1, 1500, 7)  # Up 7, 1st half
    score_diff_late = ahead_late - behind_late
    score_diff_early = ahead_early - calc_prob(1, 10, 50, 1, 1500, -7)
    assert score_diff_late > score_diff_early, (
        f"FAIL: Score should matter more late ({score_diff_late:.1%}) than early ({score_diff_early:.1%})"
    )
    print(
        f"PASS: Score diff impact late ({score_diff_late:.1%}) > early ({score_diff_early:.1%})"
    )

    # Check predictions don't blow up at extreme inputs
    extreme_scenarios = [
        (1, 1, 1, 2, 10, 28),  # Best case: goal line, up 4 TDs, end of game
        (4, 30, 99, 2, 10, -28),  # Worst case: 4th & 30, own 1, down 4 TDs, end of game
    ]
    for d, di, yl, h, t, s in extreme_scenarios:
        prob = calc_prob(d, di, yl, h, t, s)
        assert 0.0 <= prob <= 1.0, (
            f"FAIL: Extreme scenario prediction {prob} outside [0, 1]"
        )
    print("PASS: Extreme scenarios produce valid probabilities")

    print("\nAll assertions passed!")

    # Save model
    model_dir = Path(__file__).parent / "dev"
    model_dir.mkdir(exist_ok=True)
    model_path = model_dir / "wp.pkl"

    model_data = {
        "weights": weights,
        "feature_names": [
            "intercept",
            "down_norm",
            "dist_norm",
            "yard_norm",
            "half_norm",
            "time_norm",
            "score_norm",
            "score_time",
            "score_half",
            "yard_down",
            "dist_down",
            "score_sq",
            "time_sq",
            "yard_sq",
        ],
    }

    with open(model_path, "wb") as f:
        pickle.dump(model_data, f)

    print(f"\nModel saved to: {model_path}")

    # Print weights
    print("\n" + "=" * 50)
    print("MODEL WEIGHTS")
    print("=" * 50)
    for name, w in zip(model_data["feature_names"], weights):
        print(f"{name:<15} {w:>10.4f}")


if __name__ == "__main__":
    main()
