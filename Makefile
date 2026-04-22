
server: ## Run the Web Server (live debugging with latest week data)
	@NFL_SIM_LIVE=1 uv run --no-sync tests/web/test_live_server.py

clean: ## Cleans all artifacts including models
	@rm -rf .venv
	@rm -rf training/artifacts && mkdir -p training/artifacts

lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check
	@cargo fmt --manifest-path sim_rs/Cargo.toml
	@cargo clippy --manifest-path sim_rs/Cargo.toml --all-targets -- -D warnings

prek: ## Run prek pre-commit hooks on all files
	@uvx prek run --all-files

prek-install: ## Install prek git hook
	@uvx prek install

generate-outcome: ## Generate Outcome dataclass from pipeline.toml
	@uv run --no-sync python scripts/generate_outcome.py

agg-types: ## Generate type stubs for aggregation types
	@uv run --no-sync python scripts/gen_agg_stubs.py

test: ## Run tests
	@uv run pytest \
		--cov nfl_sim \
		--cov-report term-missing \
		--durations 10

test-model: ## Run model grading tests (parity, efficacy, contract)
	@uv run pytest tests/model/

cov-api: ## Run web API integration tests with coverage
	@uv run --no-sync pytest tests/web/test_web_integration.py \
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

infer-plays: ## Run XGB predictions on 1k random plays for inspection
	@uv run training/infer_plays.py

train-xgb: ## Train XGB token model
	@uv run training/train_xgb.py

export-onnx: ## Export trained models to ONNX format (for Rust inference)
	@uv run training/export_onnx.py

train-time: ## Train time model
	@uv run training/train_time.py

train-punt: ## Train punt yards model (blocked is sampled at 0.05%)
	@uv run training/train_punt.py

online-features: ## Materialize online features to data/features.parquet
	@uv run python scripts/materialize_features.py

refresh-data: ## Refresh all data files
	@uv run python data/refresh_data.py

.PHONY: help
help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort
