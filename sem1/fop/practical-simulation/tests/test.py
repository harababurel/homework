from models.route import Route
from repos.route import RouteRepository
from controllers.route import RouteController

class Test:
    """
    Class implements some method for testing different features
    of the application.
    """

    def testFeatures(self):
        repo = RouteRepository()
        repo.setRoutes([])  # clear any routes that were read from the disk

        controller = RouteController(repo)

        firstRoute  = Route(1,  '24b', 99, 14)
        secondRoute = Route(4,  '25',  75, 22)
        thirdRoute  = Route(16, '6',   10, 100)

        controller.addRoute(firstRoute)
        controller.addRoute(secondRoute)
        controller.addRoute(thirdRoute)

        controller.removeAlmostVacantRoutes()  # thirdRoute should be removed
        assert controller.getRoutes() == [firstRoute, secondRoute]

        controller.increaseBusyRoutes()
        assert controller.getRouteByID(1).getBusCount() == 15 # one extra bus
        assert controller.getRouteByID(4).getBusCount() == 22 # no extra bus


    def testEverything(self):
        self.testFeatures()
