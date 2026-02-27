"""Tests for CSR play index construction and single-tree sampling."""

import numpy as np
import pytest

from nfl_sim.models.outcomes import _build_play_index


@pytest.fixture
def leaves() -> np.ndarray:
    """Small deterministic leaf matrix: 10 plays x 4 trees."""
    return np.array(
        [
            [0, 1, 2, 0],
            [0, 0, 2, 1],
            [1, 1, 0, 0],
            [1, 0, 1, 1],
            [0, 1, 2, 0],
            [2, 2, 2, 0],
            [0, 1, 0, 1],
            [1, 0, 1, 0],
            [2, 2, 1, 1],
            [0, 1, 2, 0],
        ],
        dtype=np.int32,
    )


class TestPlayIndex:  # noqa: D101
    def test_bucket_contents_match_leaves(self, leaves: np.ndarray) -> None:
        """Every play in a CSR bucket actually has that leaf in the original matrix."""
        idx = _build_play_index(leaves, outcomes={})
        n_trees = leaves.shape[1]
        max_leaf = int(leaves.max()) + 1
        for t in range(n_trees):
            for lv in range(max_leaf):
                s, e = idx.starts[t, lv], idx.ends[t, lv]
                plays = idx.groups[t, s:e]
                for p in plays:
                    assert leaves[p, t] == lv

    def test_all_plays_accounted_for(self, leaves: np.ndarray) -> None:
        """Union of all buckets for a tree covers every play exactly once."""
        idx = _build_play_index(leaves, outcomes={})
        n_plays, n_trees = leaves.shape
        max_leaf = int(leaves.max()) + 1
        for t in range(n_trees):
            all_plays = []
            for lv in range(max_leaf):
                s, e = idx.starts[t, lv], idx.ends[t, lv]
                all_plays.extend(idx.groups[t, s:e].tolist())
            assert sorted(all_plays) == list(range(n_plays))

    def test_single_tree_sampling(self, leaves: np.ndarray) -> None:
        """Sampling from a CSR bucket only yields plays with the correct leaf."""
        idx = _build_play_index(leaves, outcomes={})
        rng = np.random.default_rng(42)
        query = np.array([0, 1, 2, 0], dtype=np.int32)
        for t in range(4):
            lv = query[t]
            s, e = idx.starts[t, lv], idx.ends[t, lv]
            for _ in range(100):
                pick = int(idx.groups[t, s + rng.integers(e - s)])
                assert leaves[pick, t] == lv
