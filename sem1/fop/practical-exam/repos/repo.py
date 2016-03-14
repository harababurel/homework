from models.task import Task
import copy

class Repository:
    """
    Class contains the working data for the application.
    Structured as a list of states, where each state is
    a list of tasks.
    """
    def __init__(self, silent=False, empty=False):
        """
        A repository adds tasks from the data.in file, by default.
        This process can be explicitly bypassed by setting the empty flag.
        Additionally, the silent flag cancels all print() functions, when set.

        Attributes:
            self.states:   a list of lists, ordered chronologically; each list represents
                           a list of tasks that define a state of the application.
            self.now:      an integer indicating the current state of the application.
            self.category: the current category (active/done/archived).
            self.current:  an integer indicating the index of the selected task.
        """
        self.states = [[]]
        self.now = 0
        self.category = 'active'
        self.current = 0

        if empty:
            return

        if not silent:
            print("Adding tasks from file.")

        try:
            f = open("data.in", "r")
        except:
            if not silent:
                print("Could not open data.in. Check if file exists.")
            return

        newState = []

        for line in f:
            try:
                status, text = line.split(':')

                if status not in ['active', 'done', 'archived']:
                    if not silent:
                        print("Task has invalid status (%s). Not going to add." % status)
                    continue

                newState.append(Task(text, status))
                if not silent:
                    print("Added a new task from file. :)")
            except:
                if not silent:
                    print("Task has invalid format. Not going to add.")

        self.states.append(newState)
        self.now += 1

        f.close()
        if not silent:
            print("Finished adding tasks from file.")
            print()

    def getCurrentState(self):
        return self.states[self.now]

    def getCategory(self):
        return self.category

    def getTasksInCurrentCategory(self):
        return [x for x in self.getCurrentState() if x.getStatus() == self.category]

    def getCurrentCategorySize(self):
        return len(self.getTasksInCurrentCategory())

    def getCurrentTask(self):
        return self.getTasksInCurrentCategory()[self.current]

    def incrementCurrent(self):
        """
        Method is used for the 'next' command.
        It increments the current task indicator and
        cycles it when it goes out of range.
        """
        self.current += 1

        if self.current == self.getCurrentCategorySize():
            self.current = 0

    def decrementCurrent(self):
        """
        Method is used for the 'prev' command.
        It decrements the current task indicator and
        cycles it when it goes out of range.
        """
        self.current -= 1

        if self.current == -1:
            self.current = self.getCurrentCategorySize() - 1

    def forgetFuture(self):
        """
        Method deletes all application states that are more recent
        than the current one. This 'makes place' for the effects
        of commands that disregard the future (example: undo followed
        by add).
        """
        self.states = self.states[:self.now+1]

    def duplicateCurrentState(self):
        """
        Method creates a deep copy of the current application state.
        The next command operates on the copy, not the original.
        """
        self.states.append(copy.deepcopy(self.states[self.now]))
        self.now += 1

    def prepareFuture(self):
        """
        Method prepares the states of the application for an upcoming command.
        It deletes the states more recent than the current one, and duplicates
        the current state.
        """
        self.forgetFuture()
        self.duplicateCurrentState()

    def saveChanges(self):
        """
        Method writes all tasks to the file data.in.
        """
        print("Saving changes to disk.")
        try:
            g = open("data.in", "w")
        except:
            print("Could not open data.in. Check file permissions.")

        for x in self.getCurrentState():
            try:
                g.write("%s: %s\n" % (x.getStatus(), x.getText()))
            except Exception as e:
                print("Could not save a certain task. Moving on.")
                print("Reason: %s" % e)

        g.close()
        print("Finished saving changes to disk.")

    def deleteTask(self, what):
        """
        Method deletes the task specified by `what`.
        """
        self.states[self.now] = [x for x in self.states[self.now] if x is not what]
        if self.current == self.getCurrentCategorySize():
            self.current = 0

    def getTaskCountFor(self, category):
        """
        Method returns the number of tasks belonging to a certain category.
        It is a more general version of getCurrentCategorySize(), which may
        be removed in a future update.
        """
        return len([x for x in self.getCurrentState() if x.getStatus() == category])
