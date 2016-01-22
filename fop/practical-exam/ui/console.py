class Console:
    """
    Class defines the text interface of the application.
    """
    def __init__(self, controller):
        """
        Console objects initialize with a controller.
        """
        self.controller = controller

    def run(self):
        while True:
            print("Current category: %s" % self.controller.repo.getCategory())
            print("Tasks in this category:")
            for i, x in enumerate(self.controller.repo.getTasksInCurrentCategory()):
                if i == self.controller.repo.current:
                    print("      * %r" % x)
                else:
                    print("\t%r" % x)
            # print("Current task: %r" % self.controller.repo.getCurrentTask())

            command = input("> ")

            if command == "":
                continue

            elif command == "exit":
                # TODO: save changes first
                exit(0)

            elif command == "next":
                self.controller.repo.incrementCurrent()
            elif command == "prev":
                self.controller.repo.decrementCurrent()
            elif command.split()[0] == "filter":
                try:
                    assert len(command.split()) == 2
                    newCategory = command.split()[1]
                    assert newCategory in ['active', 'done', 'archived']
                    self.controller.repo.category = newCategory
                    self.controller.repo.current = 0
                except:
                    print("Invalid command. Usage:")
                    print("\t filter {active/done/archived}")
                    print()
            else:
                print("Command not recognized. Try something else.")
