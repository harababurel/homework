from models.route import Route

class RouteRepository:
    """
    The repository that contains all existing routes.

    Attributes:
        self.__routes: list of Route objects.
    """

    def __init__(self):

        self.__routes = []

        try:
            f = open('routes.in', 'r')

        except:
            raise IOError("Could not open routes.in for reading.")

        for line in f:
            v = line.split(',')

            try:
                newRoute = Route(v[0], v[1], v[2], v[3])
                self.addRoute(newRoute)

            except Exception as e:
                # the current route from the file is not valid
                print(e)
        f.close()

    def getRoutes(self):
        return self.__routes

    def setRoutes(self, routes):
        self.__routes = routes

    def idExists(self, ID):
        return ID in [route.getID() for route in self.getRoutes()]

    def codeExists(self, code):
        return code in [route.getCode() for route in self.getRoutes()]

    def addRoute(self, route):
        try:
            assert not self.idExists(route.getID())
            assert not self.codeExists(route.getCode())
            self.__routes.append(route)
        except AssertionError:
            # either the ID or the code already exist,
            # so there is nothing to be done.
            print("some assertion error here")
            pass

    def saveChanges(self):
        try:
            with open('routes.in', 'w') as g:
                for route in self.__routes:
                    g.write("%i,%s,%i,%i\n" % (route.getID(), route.getCode(), route.getUsage(), route.getBusCount()))
        except:
            raise IOError("Could not open routes.in for writing.")
