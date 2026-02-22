
server: ## Run the Web Server (live debugging with latest week data)
	@NFL_SIM_LIVE=1 uv run --no-sync tests/web/test_live_server.py

clean: ## Cleans all artifacts including models
	@rm -rf .venv
	@rm -rf training/artifacts && mkdir -p training/artifacts


lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check

generate-outcome: ## Generate Outcome dataclass from pipeline.toml
	@uv run --no-sync python scripts/generate_outcome.py

types: ## Generate type stubs for aggregation types
	@uv run --no-sync python scripts/gen_agg_stubs.py

test: ## Run tests
	@uv run --no-sync pytest \
		--cov nfl_sim \
		--cov-report term-missing \
		--durations 10

test-train: ## Run training-related tests
	@NFL_SIM_TRAIN_TEST=1 uv run pytest -sv tests/analysis/test_model.py tests/analysis/test_model_efficacy.py

parity: ## Run parity tests
	@NFL_SIM_PARITY=1 uv run pytest tests/engine/test_parity.py

cov-api: ## Run web API integration tests with coverage
	@uv run --no-sync pytest tests/web/test_web_integration.py \
		--cov nfl_sim \
		--cov-report term-missing

vulture: ## Detect dead code
	@uvx vulture nfl_sim

infer: ## Run inference and look at results in the browser
	@uv run training/infer.py
	@duckdb -ui training/artifacts/predictions/predictions.csv

load-dictionaries: ## Download data dictionaries
	@curl -L -o dictionary/pbp.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_pbp.csv
	@curl -L -o dictionary/dc.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_depth_charts.csv

time: ## Run performance benchmarks for time
	@uv run --no-sync bench/time_perf.py

prof: ## Run line profiler on single game
	@uv run python bench/profile_time.py > bench/profile_results.txt
	@echo "Profile results written to bench/profile_results.txt"
	
perf: ## Run performance of results against real
	@uv run --no-sync bench/accuracy_perf.py

converge: ## Run convergence benchmark
	@uv run --no-sync bench/convergence_perf.py

train-nn: ## Train neural network model
	@uv run training/train.py

train-all: ## Run all model training scripts
	@uv run training/train_intent.py
	@uv run training/train.py
	@uv run training/train_time.py
	@uv run training/train_punt.py

train-intent: ## Train and compile intent (RF) model
	@uv run training/train_intent.py

train-time: ## Train time model
	@uv run training/train_time.py

train-punt: ## Train punt yards model (blocked is sampled at 0.05%)
	@uv run training/train_punt.py

refresh-data: ## Refresh all data files
	@uv run python data/refresh_data.py

.PHONY: help
help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort
