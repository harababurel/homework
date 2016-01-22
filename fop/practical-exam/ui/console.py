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
                self.controller.repo.saveChanges()
                print("The application will now close.")
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

            elif command.split()[0] == 'add':
                newText = ' '.join(command.split()[1:])

                try:
                    assert newText != ''
                except:
                    print("The text can't be null.")
                    print()
                    continue

                try:
                    self.controller.addTask(newText)
                    print("Task successfully added.")
                except:
                    print("Something went wrong :(. Could not add task.\n")

            elif command.split()[0] == 'status':
                try:
                    assert len(command.split()) == 2
                    newStatus = command.split()[1]
                    assert newStatus in ['active', 'done', 'archived']
                except:
                    print("Invalid command. Usage:")
                    print("\t status {active/done/archived}")
                    print()
                    continue

                try:
                    self.controller.repo.getCurrentTask().setStatus(newStatus)
                    print("Status successfully changed :).")
                except:
                    print("Something went wrong :(. Could not change status.\n")

            elif command.split()[0] == 'text':
                newText = ' '.join(command.split()[1:])

                try:
                    assert newText != ''
                except:
                    print("The text can't be null.")
                    print()
                    continue

                try:
                    self.controller.repo.getCurrentTask().setText(newText)
                    print("Text sucessfully changed :).")
                except:
                    print("Something went wrong :(. Could not change status.\n")

            elif command == 'delete':
                try:
                    self.controller.delete()
                except:
                    print("Something went wrong :(. Could not delete current task.\n")

            elif command == 'report':
                for category in ['active', 'done', 'archived']:
                    print("Category %s contains %i tasks." % (category, self.controller.repo.getTaskCountFor(category)))
                print()

            else:
                print("Command not recognized. Try something else.")
