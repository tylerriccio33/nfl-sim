
lint: ## Run ruff and typer
	@uv run ruff check --fix
	@uv run ruff format
	@uv run ty check

run: ## Run the program
	@uv run main.py

test: ## Run tests
	@uv run pytest \
		--cov src \
		--cov-report term-missing