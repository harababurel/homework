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
    def __init__(self, studentID=None, name=None, group=None):
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
        self.studentID = newID

    def setStudentName(self, newName):
        self.name = newName

    def setStudentGroup(self, newGroup):
        self.group = newGroup

    # GET STUFF
    def getStudentID(self):
        return self.studentID

    def getStudentName(self):
        return self.name

    def getStudentGroup(self):
        return self.group
