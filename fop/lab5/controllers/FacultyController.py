"""
    The FacultyController class implements methods for
    managing the students and assignments of a faculty.
"""
from repo.FacultyRepository import FacultyRepository


class FacultyController:
    def __init__(self):
        self.repository = FacultyRepository()

        print("Restoring previous session.")
        try:
            self.repository.restoreSession()
            print("Previous session restored :).")
        except IOError:
            print("No saved session exists.")
            print("Starting a new session.")
            try:
                self.repository.saveSession()
                print("New session saved :).")
            except IOError:
                print("Could not save new session.", end='')
                print("Your work will be lost once you close the application :(.")

    def getCurrentFaculty(self):
        return self.repository.states[self.repository.now]

    def getCurrentStudents(self):
        return self.getCurrentFaculty().students

    def getCurrentAssignments(self):
        return self.getCurrentFaculty().assignments

    def studentExists(self, student):
        return student.studentID in [x.studentID for x in self.getCurrentStudents()]


    def addStudent(self, who):
        if self.studentExists(who):
            print("Student %i already exists." % who.studentID)
        else:
            self.repository.prepare()
            #self.getCurrentStudents().append(who)
            # ^hopefully this works as expected
            self.repository.states[self.repository.now].addStudent(who)

    def undo(self):
        if self.repository.now == 0:
            print("Already at oldest state.")
        else:
            self.repository.now -= 1

    def redo(self):
        if self.repository.now + 1 == len(self.repository.states):
            print("Already at most recent state.")
        else:
            self.repository.now += 1

    def exitApplication(self):
        """
            Method saves current session and then exits
            with an exit-code of 0.
        """
        try:
            self.repository.saveSession()
            print("Session saved :).")
        except IOError:
            print("Could not save session. Your work will be lost :(.")

        print("Exiting.")
        exit(0)
