"""
    Module implements the Assignment class.
"""


class Assignment():
    """
        Class that models assignments as objects.
        Each assignment is identified by 4 fields:
            - studentID <int> - aka numar matricol
            - description <string>
            - deadline <datetime.datetime>
            - grade <float>
    """
    def __init__(self, studentID, description, deadline, grade):
        self.studentID = studentID
        self.description = description
        self.deadline = deadline
        self.grade = grade

    def __repr__(self):
        message = "Student #%i has the following assignment:\n"
        message += "\tDescription: %s\n"
        message += "\tDeadline: %r\n"
        message += "\tGrade: %.2f\n"

        message %= (
                self.studentID,
                self.description,
                self.deadline,
                self.grade
                )
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
