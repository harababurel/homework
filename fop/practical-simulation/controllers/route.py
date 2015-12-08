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
