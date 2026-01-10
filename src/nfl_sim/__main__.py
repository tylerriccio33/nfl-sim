"""Main module."""

import fire

from nfl_sim import run_week as _run_week


class Main:
    """Main class for interacting with NFL simulations."""

    @staticmethod
    def server():  # noqa: D102
        raise NotImplementedError

    @staticmethod
    def tui():  # noqa: D102
        raise NotImplementedError

    @staticmethod
    def run_week():
        """Run this week's games."""
        _run_week()


def main():
    """Runnable for the CLI."""
    fire.Fire(Main)
