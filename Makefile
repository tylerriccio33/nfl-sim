
lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check
	@uv run complexipy src --quiet

build: ## Run maturin develop
	@uv run maturin develop --release

run: ## Run the program
	@uv run nfl-sim run-week

test: ## Run tests
	@uv run pytest \
		--cov src \
		--cov-report term-missing

load-dictionaries: ## Download data dictionaries
	@curl -L -o dictionary/pbp.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_pbp.csv
	@curl -L -o dictionary/dc.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_depth_charts.csv

bench-time: ## Run performance benchmarks for time
	@uv run bench/time_perf.py

bench-profile: ## Run line profiler on single game
	@uv run python bench/profile_time.py
	
bench-results: ## Run performance of results against real
	@uv run bench/accuracy_perf.py

train-wp-model: ## Run the model training script for WP model
	@uv run model/wp_model_train.py

refresh-data: ## Refresh all data files
	@uv run python data/refresh_data.py

.PHONY: help
help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-22s\033[0m %s\n", $$1, $$2}' | sort
