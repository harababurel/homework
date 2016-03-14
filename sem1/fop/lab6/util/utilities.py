"""
    Module implements some general-purpose methods
    that may be used anywhere.
"""
import os
import sys

def clear():
    """
        Method clears the screen. Should work on both
        Windows and POSIX-compliant (Linux/OS X) OS's.
    """
    os.system('cls' if os.name == 'nt' else 'clear')


def getFilename():
    """
        Method returns the filename of the script from
        which it is executed.
    """
    return sys.argv[0]


def getAbsoluteLocation(filename):
    """
        Method returns the absolute location of the script
        from which it is executed.
        Useful when wanting to save data.bin in the same
        dir as main.py (instead of saving it wherever the user
        is when launching the application).
    """
    absoluteLocation = os.path.realpath(
            os.path.join(os.getcwd(), os.path.dirname(filename)))
    return absoluteLocation
