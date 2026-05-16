"""Main module."""

import subprocess
import sys
from pathlib import Path

import fire

_APP = Path(__file__).parent / "web" / "app.py"


class Main:  # pragma: no cover
    """Main class for interacting with NFL simulations."""

    @staticmethod
    def server(host: str = "127.0.0.1", port: int = 5000) -> None:
        """Run the web server (marimo dashboard).

        Args:
            host: Host address to bind to.
            port: Port number to listen on.

        """
        # Fixed argv (interpreter + bundled app path); host/port are CLI-local.
        subprocess.run(  # noqa: S603
            [sys.executable, "-m", "marimo", "run", str(_APP), "--host", host, "--port", str(port)],
            check=True,
        )


def main() -> None:  # pragma: no cover
    """Runnable for the CLI."""
    fire.Fire(Main)
