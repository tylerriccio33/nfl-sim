
lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check
	@uv run complexipy src --quiet

run: ## Run the program
	@uv run main.py

test: ## Run tests
	@uv run pytest \
		--cov src \
		--cov-report term-missing

load-dictionaries: ## Download data dictionaries
	@curl -L -o dictionary/pbp.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_pbp.csv
	@curl -L -o dictionary/dc.csv https://raw.githubusercontent.com/nflverse/nflreadr/refs/heads/main/data-raw/dictionary_depth_charts.csv

bench-time: ## Run performance benchmarks for time
	@uv run bench/time_perf.py

train-wp-model: ## Run the model training script for WP model
	@uv run model/wp_model_train.py