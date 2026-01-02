class MoveChains(Exception):
    pass


class Flip(Exception):
    pass


class Interception(Flip):
    pass


class TurnoverOnDowns(Flip):
    pass


class PuntRegular(Flip):
    pass


class PuntBlocked(Flip):
    pass


class FieldGoalFail(Flip):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)


class FlipReset(Exception):
    pass


class Touchdown(FlipReset):
    def __init__(self, **kwargs):
        super().__init__(**kwargs)


class PuntEndzone(FlipReset):
    pass


class FieldGoalSuccess(FlipReset):
    pass


class Safety(Exception):
    pass


class ScoreReset(Exception):
    """Score change; reset but not flip."""


class PickSix(ScoreReset):
    pass


class FumbleSix(ScoreReset):
    pass


class GameOver(Exception):
    """Raised when game clock expires."""


class HalfOver(Exception):
    """Raised when half clock expires."""
