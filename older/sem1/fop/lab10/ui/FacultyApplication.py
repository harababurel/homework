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
    def __init__(self, controller):
        self.controller = controller

    def run(self):
        """
            Method runs the main loop of the application.
            In each iteration, it provides a prompt and waits
            for the user input. It then validates it and sometimes
            does what it's told.
        """

        while True:
            # print(self.controller.repository)
            self.command = self.getInput(Color.bold('> '))

            # NULL COMMAND
            if self.command == '':
                continue

            self.commandArgs = self.command.split()[1:]
            self.command = self.command.split()[0]

            # UNRECOGNIZED COMMAND
            if not self.command in SETTINGS['validCommands']:
                print("%s: command not found. Try '%s'." % (self.command, Color.bold('help')))
                continue

            # ARGUMENT COUNT CHECK
            if not len(self.commandArgs) in SETTINGS['neededArgs'][self.command]:
                print("%s: <%s> takes %s arguments (%s provided)." %
                        (
                            Color.error("Error"),
                            Color.bold(self.command),
                            Color.bold(SETTINGS['neededArgs'][self.command]),
                            Color.bold(len(self.commandArgs))
                            )
                        )
                continue

            # HELP
            if self.command == 'help':
                print(STRINGS['helpPrompt'])

            # ADD
            elif self.command == 'add':
                self.showAddSubmenu()

            # REMOVE
            elif self.command == 'remove':
                self.showRemoveSubmenu()

            # LIST
            elif self.command == 'list':
                self.listEverything()

            # UNDO
            elif self.command == 'undo':
                self.controller.undo()

            # REDO
            elif self.command == 'redo':
                self.controller.redo()

            # STATS
            elif self.command == 'stats':
                self.showStatsSubmenu()

            # CLEAR
            elif self.command == 'clear':
                clear()

            # EXIT
            elif self.command == 'exit':
                self.controller.exitApplication()

    def getInput(self, prompt=None):
        """
            Method requests user input, displaying an optional prompt message.
        """
        return input(prompt if prompt else '')

    def listStudents(self):
        """
            Method prints all students in the current faculty.
        """
        print(Color.bold('Students:'))
        for i, x in enumerate(self.controller.getCurrentStudents()):
            print("%i: %r" % (i, x))

    def listAssignments(self):
        """
            Method prints all assignments in the current faculty.
        """
        print('\n%s' % Color.bold('Assignments:'))
        for i, x in enumerate(self.controller.getCurrentAssignments()):
            print("%i: %r" % (i, x))

    def listEverything(self):
        """
            Method prints both students and assignments in the current faculty.
        """
        self.listStudents()
        self.listAssignments()

    def showAddSubmenu(self):
        """
            Method implements the menu shown when using the 'add' command.
            This will split furtherly on two paths, based on the object
            added (student or assignment).
        """
        while True:
            try:
                self.addType = self.getInput("%student or %sssignment? " % (Color.bold('S'), Color.bold('A')))
                assert self.addType.lower() in ['student', 'assignment', 's', 'a']

                if self.addType.lower() in ['student', 's']:
                    self.showAddStudentSubmenu()
                else:
                    self.showAddAssignmentSubmenu()
                return

            except AssertionError:
                continue

    def showRemoveSubmenu(self):
        """
            Method implements the menu shown when using the 'remove' command.
            This will split furtherly on two paths, based on the object
            removed (student or assignment).
        """
        while True:
            try:
                self.removeType = self.getInput("%student or %sssignment? " % (Color.bold('S'), Color.bold('A')))
                assert self.removeType.lower() in ['student', 'assignment', 's', 'a']

                if self.removeType.lower() in ['student', 's']:
                    self.showRemoveStudentSubmenu()
                else:
                    self.showRemoveAssignmentSubmenu()
                return

            except AssertionError:
                continue

    def showAddStudentSubmenu(self):
        """
            Method implements the menu shown when adding a new student.
        """
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

        print(self.controller.addStudent(
                Student(
                    self.newStudentID,
                    self.newStudentName,
                    self.newStudentGroup
                    )
                )
                )

    def showRemoveStudentSubmenu(self):
        """
            Method implements the menu shown when removing an existing student.
        """
        print("You chose to remove a student.")
        self.listStudents()

        if self.controller.getCurrentStudentCount() == 0:
            print("There are no students.")
            return

        while True:
            try:
                self.studentID = int(self.getInput("ID: "))
                assert self.controller.studentIDExists(self.studentID)
                break
            except ValueError:
                print("%s: the student ID should be a positive integer." % Color.error("Error"))
            except AssertionError:
                print("%s: there is no such student in the faculty." % Color.error("Error"))
            except:
                continue

        self.controller.removeStudent(self.studentID)

    def showAddAssignmentSubmenu(self):
        """
            Method implements the menu shown when adding a new assingment.
        """
        print("You chose to add an assignment.")

        while True:
            try:
                self.newAssignmentStudentID = int(self.getInput("ID: "))
                assert self.controller.studentIDExists(self.newAssignmentStudentID)
                break
            except AssertionError:
                print("%s: the assignment must be assigned to an existing student." % Color.error("Error"))
                self.listStudents()
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
                            Color.bold("1.0"),
                            Color.bold("10.0")
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

    def showRemoveAssignmentSubmenu(self):
        """
            Method implements the menu shown when removing an existing assignment.
        """
        print("You chose to remove an assignment.")
        self.listAssignments()

        if self.controller.getCurrentAssignmentCount() == 0:
            print("There are no assignments.")
            return

        while True:
            try:
                self.assignmentID = int(self.getInput("Enter the ID of the assignment: "))
                assert 0 <= self.assignmentID and self.assignmentID < self.controller.getCurrentAssignmentCount()
                break
            except ValueError:
                print("%s: integers please." % Color.error("Error"))
            except AssertionError:
                print("%s: there is no such asssignment in the faculty." % Color.error("Error"))

        self.controller.removeAssignment(self.assignmentID)

    def showStatsSubmenu(self):
        """
            Method implements the menu shown when displaying faculty statistics.
        """
        print("You chose to display statistics.")
        self.listAssignments()

        if self.controller.getCurrentAssignmentCount() == 0:
            print("There are no assignments.")
            return

        while True:
            try:
                self.assignmentID = int(self.getInput("Enter the ID of the assignment: "))
                assert 0 <= self.assignmentID and self.assignmentID < self.controller.getCurrentAssignmentCount()
                break
            except ValueError:
                print("%s: integers please." % Color.error("Error"))
            except AssertionError:
                print("%s: there is no such asssignment in the faculty." % Color.error("Error"))

        print(self.controller.getStatistics(self.assignmentID))
