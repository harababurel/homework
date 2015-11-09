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
        """
            studentID is the only mandatory field.
            Everything else is optional.
        """
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
        """
            Method sets the studentID of the current assignment.
        """
        self.studentID = newStudentID

    def setDescription(self, newDescription):
        """
            Method sets the description of the current assignment.
        """
        self.description = newDescription

    def setDeadline(self, newDeadline):
        """
            Method sets the deadline of the current assignment.
        """
        self.deadline = newDeadline

    def setGrade(self, newGrade):
        """
            Method sets the grade of the current assignment.
        """
        self.grade = newGrade

    # GET STUFF
    def getStudentID(self):
        """
            Method returns the studentID of the current assignment.
        """
        return self.studentID

    def getDescription(self):
        """
            Method returns the description of the current assignment.
        """
        return self.description

    def getDeadline(self):
        """
            Method returns the deadline of the current assignment.
        """
        return self.deadline

    def getGrade(self):
        """
            Method returns the grade of the current assignment.
        """
        return self.grade
