from models.task import Task

class Controller:
    def __init__(self, repo):
        """
        Controller objects initialize with a repository
        upon which they apply operations.
        """
        self.repo = repo

    def addTask(self, text):
        """
        Method adds a new task to the repo's current category.
        """
        self.repo.prepareFuture()
        self.repo.getCurrentState().append(Task(text, self.repo.category))

    def delete(self):
        """
        Method deletes the currently selected task.
        """
        self.repo.prepareFuture()
        task = self.repo.getCurrentTask()
        self.repo.deleteTask(task)

    def undo(self):
        """
        Method takes one step back in time (if possible).
        """
        if self.repo.now == 1:
            print("Already at oldest state.\n")
        else:
            self.repo.now -= 1

            if self.repo.current >= self.repo.getCurrentCategorySize():
                self.repo.current = self.repo.getCurrentCategorySize() - 1

    def redo(self):
        """
        Method takes one step forward in time (if possible).
        """
        if self.repo.now == len(self.repo.states) - 1:
            print("Already at most recent state.\n")
        else:
            self.repo.now += 1

            if self.repo.current >= self.repo.getCurrentCategorySize():
                self.repo.current = self.repo.getCurrentCategorySize - 1

