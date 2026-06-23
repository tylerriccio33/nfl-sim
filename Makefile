
server: ## Run the Web Server (marimo dashboard)
	@uv run marimo run nfl_sim/web/app.py

clean: ## Cleans all artifacts including models
	@rm -rf .venv
	@rm -rf training/artifacts && mkdir -p training/artifacts
	@rm -rf sim_rs/target

rebuild: clean ## Wipe everything (venv, models, rust) and rebuild from scratch
	@uv sync
	@$(MAKE) build
	@$(MAKE) train

lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check
	@cargo fmt --manifest-path sim_rs/Cargo.toml
	@cargo clippy --manifest-path sim_rs/Cargo.toml --all-targets -- -D warnings

prek: ## Run prek pre-commit hooks on all files
	@uvx prek run --all-files

prek-install: ## Install prek git hook
	@uvx prek install --overwrite

generate-outcome: ## Generate Outcome dataclass from pipeline.toml
	@uv run python scripts/generate_outcome.py

agg-types: ## Generate type stubs for aggregation types
	@uv run python scripts/gen_agg_stubs.py

test: ## Run tests
	@uv run pytest \
		--cov nfl_sim \
		--cov-report term-missing \
		--durations 10

test-model: ## Run model grading tests (parity, efficacy, contract)
	@uv run pytest tests/model/

cov-api: ## Run web app tests with coverage
	@uv run --no-sync pytest tests/web/test_web_app.py \
		--cov nfl_sim \
		--cov-report term-missing

load-dictionaries: ## Download data dictionaries
	@curl -L -o dictionary/pbp.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_pbp.csv
	@curl -L -o dictionary/dc.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_depth_charts.csv

bench-time: ## Run performance benchmarks for time
	@uv run --no-sync bench/time_perf.py

bench-prof: ## Run line profiler on single game
	@uv run python bench/profile_time.py > bench/profile_results.txt
	@echo "Profile results written to bench/profile_results.txt"

bench-perf: ## Run performance of results against real
	@uv run --no-sync bench/accuracy_perf.py

bench-converge: ## Run convergence benchmark
	@uv run --no-sync bench/convergence_perf.py

bench-chunk: ## Sweep sim_games chunk_size to find throughput sweet spot
	@uv run --no-sync bench/chunk_perf.py

infer-plays: ## Inspect model predictions on random plays (marimo notebook)
	@uv run marimo run training/analysis/infer_plays.py

train: train-token train-time export-onnx ## Train all models and export to ONNX

train-token: ## Train the single token classifier
	@uv run training/analysis/token_model.py

train-time: ## Train time-elapsed regressor
	@uv run training/analysis/time.py

export-onnx: ## Export trained models to ONNX format (for Rust inference)
	@uv run training/export_onnx.py

online-features: ## Materialize online features to data/features.parquet
	@uv run python scripts/materialize_features.py

play-pool: ## Materialize the play pool to data/play_pool.parquet
	@uv run python scripts/materialize_play_pool.py

refresh-data: ## Refresh all data files
	@uv run python data/refresh_data.py

build: ## Build the rust library
	# sim_rs is an editable path dependency (see [tool.uv.sources]), so every
	# `uv run` re-syncs it. A bare `maturin develop` install gets silently
	# clobbered by the next sync. Reinstall through uv so the artifact uv hands
	# to the runtime IS the one we just built (maturin backend, release=true).
	@uv sync --reinstall-package sim_rs

.PHONY: help
help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort
