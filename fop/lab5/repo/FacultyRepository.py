"""
    The FacultyRepository class implements the actual object
    that represents a faculty. It basically stores students
    and assignments, as well as different temporal states.
"""
import pickle
import copy
from models.Faculty import *


class FacultyRepository:
    """
        Structure of the repository:
            - now <int> - the current position in the list of states
            - states [faculties] - the application states timeline
    """
    def __init__(self):
        try:
            self.restoreSession()
        except:
            self.now = 0
            self.states = [Faculty()]

    def __repr__(self):
        message = "Repository (now = %i):\n" % self.now
        for i, x in enumerate(self.states):
            message += "State %i:\n" % i
            message += "%r\n" % x

        return message

    def restoreSession(self):
        try:
            with open("data.bin", "rb") as f:
                savedRepo = pickle.load(f)
                self.now = savedRepo.now
                self.states = savedRepo.states
        except:
            raise IOError("Could not open the data file for reading.")

    def saveSession(self):
        try:
            with open("data.bin", "wb") as g:
                pickle.dump(self, g)
        except:
            raise IOError("Could not open the data file for writing.")

    def forgetFuture(self):
        """
            Method erases all states that are more recent
            than the current one (indicated by self.now).
        """
        self.states = self.states[:self.now+1]

    def duplicateCurrentState(self):
        """
            Method creates a duplicate of the current state,
            which can be altered in the future.
        """
        self.states.append(copy.deepcopy(self.states[self.now]))
        self.now += 1

    def prepare(self):
        """
            Method prepares the structure for abiding a future
            user alteration. Should be executed before each command.
        """
        self.forgetFuture()
        self.duplicateCurrentState()
