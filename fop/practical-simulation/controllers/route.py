class RouteController:
    """
    Class provides methods that operate on some RouteRepository.
    """

    def __init__(self, repo):
        self.__repo = repo

    def getRoutes(self):
        return self.__repo.getRoutes()
