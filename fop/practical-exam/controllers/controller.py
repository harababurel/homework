from models.task import Task

class Controller:
    def __init__(self, repo):
        """
        Controller objects initialize with a repository
        upon which they apply operations.
        """
        self.repo = repo

    def addTask(self, text):
        self.repo.prepareFuture()
        self.repo.getCurrentState().append(Task(text, self.repo.category))

    def delete(self):
        self.repo.prepareFuture()
        task = self.repo.getCurrentTask()
        self.repo.deleteTask(task)

    def undo(self):
        if self.repo.now == 1:
            print("Already at oldest state.\n")
        else:
            self.repo.now -= 1

            if self.repo.current >= self.repo.getCurrentCategorySize():
                self.repo.current = self.repo.getCurrentCategorySize() - 1

    def redo(self):
        if self.repo.now == len(self.repo.states) - 1:
            print("Already at most recent state.\n")
        else:
            self.repo.now += 1

            if self.repo.current >= self.repo.getCurrentCategorySize():
                self.repo.current = self.repo.getCurrentCategorySize - 1

