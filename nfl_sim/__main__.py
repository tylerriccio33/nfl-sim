"""Main module."""

import fire

from nfl_sim.web import create_app


class Main:  # pragma: no cover
    """Main class for interacting with NFL simulations."""

    @staticmethod
    def server(host: str = "127.0.0.1", port: int = 5000, *, debug: bool = True) -> None:
        """Run the web server.

        Args:
            host: Host address to bind to.
            port: Port number to listen on.
            debug: Enable Flask debug mode.

        """
        app = create_app()
        app.run(host=host, port=port, debug=debug)


def main() -> None:  # pragma: no cover
    """Runnable for the CLI."""
    fire.Fire(Main)
