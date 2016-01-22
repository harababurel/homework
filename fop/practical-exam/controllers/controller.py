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
        
