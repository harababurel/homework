"""
    The FacultyRepository class implements the actual object
    that represents a faculty. It basically stores students
    and assignments, as well as different temporal states.
"""
import pickle
import copy
from models.Faculty import *
from util.utilities import *

class FacultyRepository:
    """
        Structure of the repository:
            - now <int> - the current position in the list of states
            - states [list of Faculty()] - the application states timeline
    """
    def __init__(self, persistent):
        """
            The repository tries to restore a saved state
            when initializing. If this fails, then it uses
            the default values for its fields (this can also
            be requested via the persistent argument).
        """
        self.now = 0
        self.states = [Faculty()]
        self.persistent = persistent

        if persistent:
            try:
                self.restoreSession()
            except:
                pass

    def __repr__(self):
        message = "Repository (now = %i):\n" % self.now
        for i, x in enumerate(self.states):
            message += "State %i:\n" % i
            message += "%r\n" % x

        return message

    def getStates(self):
        """
            Method returns the states (list of faculties) of
            the repository.
        """
        return self.states

    def getDataFileLocation(self):
        """
            The data file should always be saved in the
            same directory as main.py, not in the directory
            that the script is run from.
            This method returns the data file's full path.
        """
        motherFile = getFilename()                           # hopefully, this is the location of main.py
        motherFileLocation = getAbsoluteLocation(motherFile) # and this is the full path to the directory

        dataFileLocation = '%s/data.bin' % motherFileLocation
        return dataFileLocation

    def restoreSession(self):
        """
            Method tries to unpickle the object saved in data.bin,
            and use it as the current repo.
            If this fails, an IOError is raised.
        """
        try:
            with open(self.getDataFileLocation(), 'rb') as f:
                savedRepo = pickle.load(f)
                self.now = savedRepo.now
                self.states = savedRepo.states
        except:
            raise IOError("Could not open the data file for reading.")

    def saveSession(self):
        """
            Method tries to pickle the current repository in data.bin,
            for later use.
            If this fails, an IOError is raised.
        """
        try:
            with open(self.getDataFileLocation(), 'wb') as g:
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
            user alteration. Should be executed before each user
            command that alters the repository.
        """
        self.forgetFuture()
        self.duplicateCurrentState()
