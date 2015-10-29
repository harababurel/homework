"""
    The FacultyApplication class implements the user interface
    which makes use of FacultyController in order to manage the
    students and assignments of a faculty.
"""
from controllers.FacultyController import FacultyController

class FacultyApplication():
    def __init__(self):
        self.controller = FacultyController()

    def run(self):
        while True:
            self.showPrompt()
            self.getInput()

    def showPrompt(self):
        print(">", end='')

    def getInput(self):
        return input()
