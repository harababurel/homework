"""
    Module implements the Faculty class.
"""


class Faculty():
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
        message = "The faculty contains the following students:\n"
        for student in students:
            message += "\t%r\n" % student

        message += "\nAnd assignments:\n"
        for assignment in assignments:
            message += "\t%r\n" % assignment

        return message

    # SET STUFF
    def setGraduationYear(self, newGraduationYear):
        self.graduationYear = newGraduationYear

    def setStudents(self, students):
        self.studends = students

    def setAssignments(self, assignments):
        self.assignments = assignments

    def addStudent(self, student):
        self.students.append(student)

    # GET STUFF
    def getStudents(self):
        return self.students

    def getAssignments(self):
        return self.assignments
