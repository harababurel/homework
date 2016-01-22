#!/usr/bin/python
from repos.repo import Repository
from models.task import Task
from controllers.controller import Controller
from ui.console import Console


def main():
    repo = Repository()
    controller = Controller(repo)
    app = Console(controller)

    app.run()

if __name__=='__main__':
    main()
