"""
    Module implements the Student class.
"""
from util.Color import *


class Student:
    """
        Class that models students as objects.
        Each student is identified by 3 fields:
            - studentID <int> - aka numar matricol
            - name <string>
            - group <int>
    """
    def __init__(self, studentID, name=None, group=None):
        """
            studentID is the only mandatory field.
            Everything else is optional.
        """
        self.studentID = studentID
        self.name = name
        self.group = group

    def __repr__(self):
        return "ID=%s: %s from group %s." % (
                Color.strong(self.studentID),
                Color.bold(self.name),
                Color.strong(self.group)
                )

    # SET STUFF
    def setStudentID(self, newID):
        """
            Method sets the studentID of the current student.
        """
        self.studentID = newID

    def setStudentName(self, newName):
        """
            Method sets the name of the current student.
        """
        self.name = newName

    def setStudentGroup(self, newGroup):
        """
            Method sets the group of the current student.
        """
        self.group = newGroup

    # GET STUFF
    def getStudentID(self):
        """
            Method returns the studentID of the current student.
        """
        return self.studentID

    def getStudentName(self):
        """
            Method returns the name of the current student.
        """
        return self.name

    def getStudentGroup(self):
        """
            Method returns the group of the current student.
        """
        return self.group
