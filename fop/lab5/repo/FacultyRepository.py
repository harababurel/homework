"""
    The FacultyRepository class implements the actual object
    that represents a faculty. It basically stores students
    and assignments, as well as different temporal states.
"""
import pickle
from models.Faculty import *


class FacultyRepository():
    """
        Structure of the repository:
            - now <int> - the current position in the list of states
            - states [int] - the application states timeline
    """
    def __init__(self):
        self.now = 0
        self.states = [Faculty()]

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
        self.states.append(self.states[self.now])
        self.now += 1

    def prepare(self):
        """
            Method prepares the structure for abiding a future
            user alteration. Should be executed before each command.
        """
        self.forgetFuture()
        self.duplicateCurrentState()
