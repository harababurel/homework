"""
    The brains of the application.
    Module creates an instance of the application
    and runs it.
"""

from ui.FacultyApplication import FacultyApplication
from controllers.FacultyController import FacultyController
from repo.FacultyRepository import FacultyRepository
from tests.Test import *
from models import *


def main():
    Test().testEverything()

    while True:
        command = input("memory or file? ")
        if command == 'memory':
            repo = FacultyRepository(False)
            break
        if command == 'file':
            repo = FacultyRepository(True)
            break

    controller = FacultyController(repo)
    app = FacultyApplication(controller)
    app.run()

if __name__ == '__main__':
    main()
