"""Main module."""

import fire

from nfl_sim import run_week as _run_week


class Main:  # pragma: no cover
    """Main class for interacting with NFL simulations."""

    @staticmethod
    def server(host: str = "127.0.0.1", port: int = 5000, *, debug: bool = True):
        """Run the web server.

        Args:
            host: Host address to bind to.
            port: Port number to listen on.
            debug: Enable Flask debug mode.

        """
        from nfl_sim.web import create_app

        app = create_app()
        app.run(host=host, port=port, debug=debug)

    @staticmethod
    def tui():  # noqa: D102
        raise NotImplementedError

    @staticmethod
    def run_week():
        """Run this week's games."""
        _run_week()


def main():  # pragma: no cover
    """Runnable for the CLI."""
    fire.Fire(Main)
