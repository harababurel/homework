"""
    Module implements the Color class, which allows
    for prettier output.
"""


class Color:
    PURPLE = '\033[95m'
    CYAN = '\033[96m'
    DARKCYAN = '\033[36m'
    BLUE = '\033[94m'
    GREEN = '\033[92m'
    YELLOW = '\033[93m'
    RED = '\033[91m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m'
    END = '\033[0m'

def bold(s):
    """
        Method takes a string and
        makes it bold.
    """
    return '%s%s%s' % (Color.BOLD, str(s), Color.END)

def error(s):
    """
        Method takes a string and
        makes it bold and red.
    """
    return '%s%s%s' % (Color.RED, bold(s), Color.END)
