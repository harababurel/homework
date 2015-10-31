"""
    The FacultyApplication class implements the user interface
    which makes use of FacultyController in order to manage the
    students and assignments of a faculty.
"""
from controllers.FacultyController import FacultyController
from static.settings import SETTINGS
from static.strings import STRINGS
from util.utilities import *

class FacultyApplication():
    def __init__(self):
        self.controller = FacultyController()

    def run(self):
        """
            Method runs the main loop of the application.
            In each iteration, it provides a prompt and waits
            for the user input. It then validates it and sometimes
            does what it's told.
        """
        while True:
            self.showPrompt()
            self.command = self.getInput()

            # NULL COMMAND
            if self.command == '':
                continue

            self.commandArgs = self.command.split()[1:]
            self.command = self.command.split()[0]

            # UNRECOGNIZED COMMAND
            if not self.command in SETTINGS['validCommands']:
                print("%s: command not found. Try 'help'." % self.command)
                continue

            # ARGUMENT COUNT CHECK
            if not len(self.commandArgs) in SETTINGS['neededArgs'][self.command]:
                print("Error: <%s> takes %r arguments (%i provided)." %
                        (
                            self.command,
                            SETTINGS['neededArgs'][self.command],
                            len(self.commandArgs)
                            )
                        )
                continue

            # HELP
            if self.command == 'help':
                print(STRINGS['helpPrompt'])

            # CLEAR
            elif self.command == 'clear':
                clear()

            # EXIT
            elif self.command == 'exit':
                self.controller.exitApplication()


    def showPrompt(self):
        print('%s> ' % getAbsoluteLocation(getFilename()), end='')

    def getInput(self):
        return input()
