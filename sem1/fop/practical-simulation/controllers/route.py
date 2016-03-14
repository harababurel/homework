class RouteController:
    """
    Class provides methods that operate on some RouteRepository.
    """

    def __init__(self, repo):
        self.__repo = repo

    def getRoutes(self):
        return self.__repo.getRoutes()

    def addRoute(self, route):
        self.__repo.addRoute(route)

    def idExists(self, ID):
        """
        Method checks whether or not a route with a specific ID exists.
        """
        return self.__repo.idExists(ID)

    def getRouteByID(self, ID):
        """
        Method returns the route that has a specific ID.
        """
        for x in self.getRoutes():
            if x.getID() == ID:
                return x

    def saveChanges(self):
        """
        Method writes the list of routes to disk.
        """
        self.__repo.saveChanges()

    def increaseBusyRoutes(self):
        """
        Method increments the bus count for each route with >85% usage.
        """
        for route in self.getRoutes():
            if route.getUsage() > 85:
                route.increaseBusCount()

    def removeAlmostVacantRoutes(self):
        """
        Method removes all routes with <20% usage.
        """
        remainingRoutes = [route for route in self.getRoutes() if route.getUsage() >= 20]
        self.__repo.setRoutes(remainingRoutes)

