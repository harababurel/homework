from controllers.controller import Controller
from models.task import Task
from repos.repo import Repository


class Test:
    def __init__(self):
        self.dummyRepo = Repository(silent=True, empty=True)
        self.dummyController = Controller(self.dummyRepo)

    def testEverything(self):
        assert self.dummyRepo.getCategory() == 'active'
        assert self.dummyRepo.getTasksInCurrentCategory() == []
        assert self.dummyRepo.getCurrentCategorySize() == 0

        self.dummyController.addTask("test task #1")
        self.dummyController.addTask("test task #2")
        self.dummyController.addTask("test task #3")

        self.dummyController.undo()
        self.dummyController.redo()

        assert self.dummyRepo.getCurrentCategorySize() == 3
        self.dummyController.undo()
        assert self.dummyRepo.getCurrentCategorySize() == 2
        self.dummyController.undo()
        assert self.dummyRepo.getCurrentCategorySize() == 1

        self.dummyController.redo()
        self.dummyController.redo()
        assert self.dummyRepo.getCurrentCategorySize() == 3

        self.dummyController.delete()
        assert self.dummyRepo.getCurrentCategorySize() == 2

        assert self.dummyRepo.getCurrentTask().getText() == 'test task #2'
        self.dummyRepo.incrementCurrent()
        assert self.dummyRepo.getCurrentTask().getText() == 'test task #3'

        assert self.dummyRepo.getCurrentCategorySize() == 2

        self.dummyRepo.category = 'done'
        assert self.dummyRepo.getCategory() == 'done'
        assert self.dummyRepo.getCurrentCategorySize() == 0

        self.dummyController.addTask("done task #1")
        assert self.dummyRepo.getCurrentCategorySize() == 1
        self.dummyRepo.category = 'done'
        self.dummyRepo.decrementCurrent()
        self.dummyRepo.getCurrentTask().setStatus("active")

        assert self.dummyRepo.getTaskCountFor('active') == 3

