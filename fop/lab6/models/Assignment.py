"""
    Module implements the Assignment class.
"""
from util.Color import *


class Assignment:
    """
        Class that models assignments as objects.
        Each assignment is identified by 4 fields:
            - studentID <int> - aka numar matricol
            - description <string>
            - deadline <string>
            - grade <float>
    """
    def __init__(self, studentID, description=None, deadline=None, grade=None):
        self.studentID = studentID
        self.description = description
        self.deadline = deadline
        self.grade = grade

    def __repr__(self):
        message = "Student with ID=%s has the following assignment:\n" % Color.strong(self.studentID)
        message += "\t%s: %s\n" % (Color.bold("Description"), self.description)
        message += "\t%s: %s\n" % (Color.bold("Deadline"), self.deadline)
        message += "\t%s: %.2f\n" % (Color.bold("Grade"), self.grade)

        return message

    # SET STUFF
    def setStudentID(self, newStudentID):
        self.studentID = newStudentID

    def setDescription(self, newDescription):
        self.description = newDescription

    def setDeadline(self, newDeadline):
        self.deadline = newDeadline

    def setGrade(self, newGrade):
        self.grade = newGrade

    # GET STUFF
    def getStudentID(self):
        return self.studentID

    def getDescription(self):
        return self.description

    def getDeadline(self):
        return self.deadline

    def getGrade(self):
        return self.grade
