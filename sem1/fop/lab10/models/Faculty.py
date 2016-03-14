"""
    Module implements the Faculty class.
"""
from models.Student import *
from models.Assignment import *

class Faculty:
    """
        Class that models a generation of students (faculty) as an object.
        Each faculty is described by:
            - students - <list of Student()>
            - assignments - <list of Assignment()>
    """
    def __init__(self, students=None, assignments=None):
        """
            If no parameters are provided, empty lists are used.
        """
        self.students = [] if students is None else students
        self.assignments = [] if assignments is None else assignments

    def __repr__(self):
        message = ''
        for student in self.students:
            message += "%r\n" % student

        for assignment in self.assignments:
            message += "%r\n" % assignment

        return message

    # SET STUFF
    def setStudents(self, students):
        """
            Method sets the students of the current faculty.
        """
        self.students = students

    def setAssignments(self, assignments):
        """
            Method sets the assignments of the current faculty.
        """
        self.assignments = assignments

    def addStudent(self, student):
        """
            Method adds a new student to the current faculty.
        """
        self.students.append(student)

    def addAssignment(self, assignment):
        """
            Method adds a new assignment to the current faculty.
        """
        self.assignments.append(assignment)

    # GET STUFF
    def getStudents(self):
        """
            Method returns the list of students contained in the
            current faculty.
        """
        return self.students

    def getAssignments(self):
        """
            Method returns the list of assignments contained in the
            current faculty.
        """
        return self.assignments

    def removeStudent(self, studentID):
        """
            Method removes a student (identified by their studentID)
            from the list of students. If not found, nothing happens.
        """
        self.students = [x for x in self.students if x.studentID != studentID]

    def removeAssignment(self, assignmentID):
        """
            Method removes an assignment (identified by its position
            in the list of assignments). If something goes wrong
            (position does not exist, or such), nothing happens.
        """
        try:
            del self.assignments[assignmentID]
        except:
            pass
