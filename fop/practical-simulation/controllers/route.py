class RouteController:
    """
    Class provides methods that operate on some RouteRepository.
    """

    def __init__(self, repo):
        self.__repo = repo

    def getRoutes(self):
        return self.__repo.getRoutes()

    def idExists(self, ID):
        return self.__repo.idExists(ID)

    def getRouteByID(self, ID):
        for x in self.getRoutes():
            if x.getID() == ID:
                return x

    def saveChanges(self):
        self.__repo.saveChanges()

    def increaseBusyRoutes(self):
        for route in self.getRoutes():
            if route.getUsage() > 85:
                route.increaseBusCount()

    def removeAlmostVacantRoutes(self):
        remainingRoutes = [route for route in self.getRoutes() if route.getUsage() >= 20]
        self.__repo.setRoutes(remainingRoutes)

