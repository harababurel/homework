"""
    The brains of the application.
    Module creates an instance of the application
    and runs it.
"""

from ui.FacultyApplication import FacultyApplication
from tests.Test import *
from models import *


def main():
    Test().testEverything()

    app = FacultyApplication()
    app.run()

if __name__ == '__main__':
    main()
