"""Flask web application for NFL game simulator."""

from flask import Flask


def create_app() -> Flask:
    """Application factory for the NFL simulator web interface."""
    app = Flask(__name__, template_folder="templates")
    app.config["SECRET_KEY"] = "nfl-sim-dev-key"

    from nfl_sim.web.routes import bp

    app.register_blueprint(bp)

    return app
