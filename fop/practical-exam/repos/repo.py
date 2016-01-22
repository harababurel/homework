from models.task import Task

class Repository:
    """
    Class contains the working data for the application.
    Structured as a list of states, where each state is
    a list of tasks.
    """
    def __init__(self):
        self.states = [[]]
        self.now = 0
        self.category = 'active'
        self.current = 0

        print("Adding tasks from file.")

        try:
            f = open("data.in", "r")
        except:
            print("Could not open data.in. Check if file exists.")
            return

        newState = []

        for line in f:
            try:
                status, text = line.split(':')

                if status not in ['active', 'done', 'archived']:
                    print("Task has invalid status (%s). Not going to add." % status)
                    continue

                newState.append(Task(text, status))
                print("Added a new task from file. :)")
            except:
                print("Task has invalid format. Not going to add.")

        self.states.append(newState)
        self.now += 1
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
        self.current += 1

        if self.current == self.getCurrentCategorySize():
            self.current = 0

    def decrementCurrent(self):
        self.current -= 1

        if self.current == -1:
            self.current = self.getCurrentCategorySize() - 1
