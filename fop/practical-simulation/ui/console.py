from static.strings import *

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
            print(menu)


            command = input("Enter command: ")

            if command == '':
                continue

            elif command == 'update':
                self.showUpdateMenu()

            elif command == 'increase':
                self.showIncreaseMenu()

            elif command == 'remove':
                self.showRemoveMenu()

            elif command == 'exit':
                exit(0)

            else:
                print("Command not recognized.")

    def showUpdateMenu(self):
        print("You chose to update a route.")

        while True:
            try:
                ID = int(input("Please enter the ID of the route: "))
                assert self.controller.idExists(ID)
                break
            except ValueError:
                print("The ID must be an integer.")
            except AssertionError:
                print("\n### There is no route with this ID. ###\n")
                return

        while True:
            try:
                code = input("Please enter the new code for this route [1-3 chars]: ")
                assert 0 < len(code) and len(code) <= 3
                break
            except AssertionError:
                print("The code must contain at most 3 characters.")

        while True:
            try:
                usage = int(input("Please enter the new usage for this route [0-100]: "))
                assert 0 <= usage and usage <= 100
                break
            except ValueError:
                print("The usage must be an integer.")
            except AssertionError:
                print("The usage must be a percentage between 0 and 100.")

        while True:
            try:
                busCount = int(input("Please enter the new bus count for this route: "))
                assert 0 <= busCount
                break
            except ValueError:
                print("The bus count must e an integer.")
            except AssertionError:
                print("The bus count must be positive.")

        route = self.controller.getRouteByID(ID)

        route.setCode(code)
        route.setUsage(usage)
        route.setBusCount(busCount)

        try:
            self.controller.saveChanges()
        except Exception as e:
            print("Something unexpected: %s" % e)

        print("Route updated successfully :).")


    def showIncreaseMenu(self):
        print("You chose to increase the bus count on all busy routes.")

    def showRemoveMenu(self):
        print("You chose to remove all almost-vacant routes.")
