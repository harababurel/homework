class Console:
    """
    Class models the UI of the application.
    """

    def __init__(self, controller):
        self.controller = controller

    def run(self):
        """
        The main loop of the UI.
        """

        while True:

            print("Current routes:")
            for route in self.controller.getRoutes():
                print("\t%r" % route)
            print()


            command = input("Enter command: ")

            if command == '':
                continue

            elif command == 'exit':
                exit(0)

            else:
                print("Command not recognized.")
