"""
    The FacultyApplication class implements the user interface
    which makes use of FacultyController in order to manage the
    students and assignments of a faculty.
"""
from controllers.FacultyController import FacultyController
from settings import SETTINGS

class FacultyApplication():
    def __init__(self):
        self.controller = FacultyController()

    def run(self):
        while True:
            self.showPrompt()
            self.command = self.getInput()

            self.commandArgs = self.command.split()[1:]
            self.command = self.command.split()[0]

            if not self.command in SETTINGS['validCommands']:
                print("%s: command not found. Try 'help'." % self.command)
                continue

            if not len(self.commandArgs) in SETTINGS['neededArgs'][self.command]:
                print("Error: <%s> takes %r arguments (%i provided)." %
                        (
                            self.command,
                            SETTINGS['neededArgs'][self.command],
                            len(self.commandArgs)
                            )
                        )
                continue

            print("Command recognized, but not implemented yet.")

            

    def showPrompt(self):
        print(">", end='')

    def getInput(self):
        return input()
