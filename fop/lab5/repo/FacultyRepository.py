"""
    The FacultyRepository class implements the actual object
    that represents a faculty. It basically stores students
    and assignments, as well as different temporal states.
"""
import pickle

class FacultyRepository():
    def __init__(self):
        self.now = 0
        self.states = [ ([], []) ]
        #                ^   ^
        #         students   assignments

    def restoreSession(self):
        try:
            with open("data.bin", "rb") as f:
                self = pickle.load(f)
        except:
            raise IOError("Could not open the data file for reading.")

    def saveSession(self):
        try:
            with open("data.bin", "wb") as g:
                pickle.dump(self, g)
        except:
            raise IOError("Could not open the data file for writing.")

