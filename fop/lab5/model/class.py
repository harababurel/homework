"""
    Module implements the Class class.
"""


class Class():
    """
        Class that models student classes as objects.
        Each class is identified by:
            - graduationYear - <int>
            - students - <list of Student()>
            - assignments - <list of Assignment()>
    """
    def __init__(self, graduationYear, students, assignments):
        self.graduationYear = graduationYear
        self.students = students
        self.assignments = assignments

    def __repr__(self):
        message = "The class of %i contains the following students:\n" % self.graduationYear
        for student in students:
            message += "\t%r\n" % student

        message += "\nAnd assignments:\n"
        for assignment in assignments:
            message += "\t%r\n" % assignment

        return message






