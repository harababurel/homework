"""
    Module implements the Faculty class.
"""
from models.Student import *
from models.Assignment import *

class Faculty:
    """
        Class that models a generation of students as an object.
        Each generation is identified by:
            - students - <list of Student()>
            - assignments - <list of Assignment()>
    """
    def __init__(self, students=None, assignments=None):
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
        self.students = students

    def setAssignments(self, assignments):
        self.assignments = assignments

    def addStudent(self, student):
        self.students.append(student)

    def addAssignment(self, assignment):
        self.assignments.append(assignment)

    # GET STUFF
    def getStudents(self):
        return self.students

    def getAssignments(self):
        return self.assignments

    def removeStudent(self, studentID):
        self.students = [x for x in self.students if x.studentID != studentID]

    def removeAssignment(self, assignmentID):
        del self.assignments[assignmentID]
