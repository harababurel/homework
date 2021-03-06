"""
    The FacultyApplication class implements the user interface
    which makes use of FacultyController in order to manage the
    students and assignments of a faculty.
"""
from controllers.FacultyController import FacultyController
from static.settings import SETTINGS
from static.strings import STRINGS
from util.utilities import *
from util.Color import *
from models.Student import *
from models.Assignment import *
from models.Faculty import *


class FacultyApplication:
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
            # print(self.controller.repository)
            self.command = self.getInput(bold('> '))

            # NULL COMMAND
            if self.command == '':
                continue

            self.commandArgs = self.command.split()[1:]
            self.command = self.command.split()[0]

            # UNRECOGNIZED COMMAND
            if not self.command in SETTINGS['validCommands']:
                print("%s: command not found. Try '%s'." % (self.command, bold('help')))
                continue

            # ARGUMENT COUNT CHECK
            if not len(self.commandArgs) in SETTINGS['neededArgs'][self.command]:
                print("%s: <%s> takes %s arguments (%s provided)." %
                        (
                            error("Error"),
                            bold(self.command),
                            bold(SETTINGS['neededArgs'][self.command]),
                            bold(len(self.commandArgs))
                            )
                        )
                continue

            # HELP
            if self.command == 'help':
                print(STRINGS['helpPrompt'])

            # ADD
            elif self.command == 'add':
                self.showAddSubmenu()

            # LIST
            elif self.command == 'list':
                print(bold('Students:'))
                for x in self.controller.getCurrentStudents():
                    print(x)

                print('\n%s' % bold('Assignments:'))
                for x in self.controller.getCurrentAssignments():
                    print(x)

            # UNDO
            elif self.command == 'undo':
                self.controller.undo()

            # REDO
            elif self.command == 'redo':
                self.controller.redo()

            # CLEAR
            elif self.command == 'clear':
                clear()

            # EXIT
            elif self.command == 'exit':
                self.controller.exitApplication()

    def getInput(self, prompt=None):
        return input(prompt if prompt else '')

    def showAddSubmenu(self):
        while True:
            try:
                self.addType = self.getInput("Student or Assignment? ")
                assert self.addType.lower() in ['student', 'assignment', 's', 'a']

                if self.addType.lower() in ['student', 's']:
                    self.showAddStudentSubmenu()
                else:
                    self.showAddAssignmentSubmenu()
                return

            except AssertionError:
                continue

    def showAddStudentSubmenu(self):
        print("You chose to add a student.")

        while True:
            try:
                self.newStudentID = int(self.getInput("ID: "))
                assert 0 < self.newStudentID
                break
            except:
                continue

        while True:
            try:
                self.newStudentName = self.getInput("Name: ")
                assert self.newStudentName != ''
                break
            except:
                continue

        while True:
            try:
                self.newStudentGroup = int(self.getInput("Group: "))
                assert 0 < self.newStudentGroup
                break
            except:
                continue

        self.controller.addStudent(
                Student(
                    self.newStudentID,
                    self.newStudentName,
                    self.newStudentGroup
                    )
                )

    def showAddAssignmentSubmenu(self):
        print("You chose to add an assignment.")

        while True:
            try:
                self.newAssignmentStudentID = int(self.getInput("ID: "))
                assert self.controller.studentIDExists(self.newAssignmentStudentID)
                break
            except AssertionError:
                print("%s: the assignment must be assigned to an existing user." % error("Error"))
            except:
                continue

        self.newAssignmentDescription = self.getInput("Description: ")
        self.newAssignmentDeadline = self.getInput("Deadline: ")

        while True:
            try:
                self.newAssignmentGrade = float(self.getInput("Grade: "))
                assert 1.0 <= self.newAssignmentGrade and self.newAssignmentGrade <= 10.0
                break
            except AssertionError:
                print("%s: the grade must be between %s and %s." % 
                        (
                            error("Error"),
                            bold("1.0"),
                            bold("10.0")
                            )
                        )
            except:
                continue

        self.controller.addAssignment(
            Assignment(
                self.newAssignmentStudentID,
                self.newAssignmentDescription,
                self.newAssignmentDeadline,
                self.newAssignmentGrade
                )
            )
