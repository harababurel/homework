"""
    Module implements the Faculty class.
"""


class Faculty():
    """
        Class that models a generation of students as an object.
        Each generation is identified by:
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

    # SET STUFF
    def setGraduationYear(self, newGraduationYear):
        self.graduationYear = newGraduationYear

    def setStudents(self, students):
        self.studends = students

    def setAssignments(self, assignments):
        self.assignments = assignments

    def addStudent(self, student):
        if checkStudentExists(self):
            return "Student with id #%i already exists in this class." % student.studentID
        self.students.append(student)

    # GET STUFF
    def getGraduationYear(self):
        return self.graduationYear

    def getStudents(self):
        return self.students

    def getAssignments(self):
        return self.assignments
