"""
    Module implements some general-purpose methods
    that may be used anywhere.
"""
import os

def clear():
    """
        Method clears the screen. Should work on both
        Windows and POSIX-compliant (Linux/OS X) OS's.
    """
    os.system('cls' if os.name == 'nt' else 'clear')

