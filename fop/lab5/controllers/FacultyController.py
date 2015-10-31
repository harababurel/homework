"""
    The FacultyController class implements methods for
    managing the students and assignments of a faculty.
"""
from repo.FacultyRepository import FacultyRepository

class FacultyController():
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
                print("Could not save new session. Your work will be lost once you close the application :(.")

    def addStudent(self, student):
        # do something
        pass

    def exitApplication(self):
        try:
            self.repository.saveSession()
            print("Session saved :).")
        except IOError:
            print("Could not save session. Your work will be lost :(.")

        print("Exiting.")
        exit(0)
