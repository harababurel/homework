"""
    Module implements the Test class,
    which provides methods that test
    stuff.

    Most things here repesent some
    work of mantuiala.
"""
from models.Student import *
from models.Assignment import *
from models.Faculty import *
from repo.FacultyRepository import *


class Test:
    def __init__(self):
        pass

    def testStudent(self):
        """
            Method tests the Student class.
        """
        who = Student(666)

        who.setStudentID(1)
        who.setStudentName("Decebal")
        who.setStudentGroup("Dacia")

        assert who.getStudentID() == 1
        assert who.getStudentName() == "Decebal"
        assert who.getStudentGroup() == "Dacia"

    def testAssignment(self):
        """
            Method tests the Assignment class.
        """
        what = Assignment(123)

        what.setDescription("aerobic in aer liber")
        what.setDeadline("martiembrie 201B")
        what.setGrade(5.1231231231231231231231231)

        assert what.getStudentID() == 123
        assert what.getDescription() == "aerobic in aer liber"
        assert what.getDeadline() == "martiembrie 201B"
        assert what.getGrade() == 5.1231231231231231231231231

    def testFaculty(self):
        """
            Method tests the Faculty class.
        """
        fac = Faculty()

        decebal = Student(1, "Decebal", "Dacia")
        burebista = Student(2, "Burebista", "Dacia")
        fac.addStudent(decebal)
        fac.addStudent(burebista)

        fac.addAssignment(Assignment(1, "deshidratat romani", "102i.Hr", "4.0"))

        assert fac.getStudents() == [decebal, burebista]

    def testRepo(self):
        """
            Method tests the Repository class.
        """
        repo = FacultyRepository(False)
        repo.duplicateCurrentState()

        assert len(repo.getStates()) == 2

    def testEverything(self):
        """
            Method tests everything implemented using
            the other methods implemented in this class.
        """
        try:
            self.testStudent()
            self.testAssignment()
            self.testFaculty()
            self.testRepo()
            print("All tests passed :).")
        except:
            print("Some tests failed :(. Terminating execution")
            exit(0)
