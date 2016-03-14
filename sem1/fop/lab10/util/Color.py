"""
    Module implements the Color class, which allows
    for prettier output.
"""
from random import sample, choice


class Color:
    """
        Class implements some special strings
        that correspond to color output + others.
    """
    colors = {
            'PURPLE': '\033[95m',
            'CYAN': '\033[96m',
            'DARKCYAN': '\033[36m',
            'BLUE': '\033[94m',
            'GREEN': '\033[92m',
            'YELLOW': '\033[93m',
            'RED': '\033[91m'
            }
    specials = {
        'BOLD': '\033[1m',
        'UNDERLINE': '\033[4m',
        'END': '\033[0m'
        }

    def getColorList():
        """
            Method returns the list of available color names.
        """
        return [x for x in Color.colors]

    def bold(s):
        """
            Method takes a string and returns its bold equivalent.
        """
        return '%s%s%s' % (Color.specials['BOLD'], str(s), Color.specials['END'])

    def error(s):
        """
            Method takes a string and returns its bold and red equivalent.
        """
        return '%s%s%s' % (Color.colors['RED'], Color.bold(s), Color.specials['END'])

    def strong(s):
        """
            Method takes a string and returns its bold and blue equivalent.
        """
        return '%s%s%s' % (Color.colors['BLUE'], Color.bold(s), Color.specials['END'])

    def colorize(s):
        """
            Method takes a string and paints it randomly.
        """
        chosenColor = choice(Color.getColorList())
        return '%s%s%s' % (Color.colors[chosenColor], str(s), Color.specials['END'])
