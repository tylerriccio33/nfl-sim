"""Flask web application for NFL game simulator."""

from flask import Flask
from jinja2 import StrictUndefined

from nfl_sim.web.routes import bp


def create_app() -> Flask:
    """Application factory for the NFL simulator web interface."""
    app = Flask(__name__, template_folder="templates")
    app.jinja_env.undefined = StrictUndefined
    app.config["SECRET_KEY"] = "nfl-sim-dev-key"

    app.register_blueprint(bp)

    return app
