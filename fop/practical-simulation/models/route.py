class Route:
    """
    Class models a bus route as a real world object.

    A bus route is defined by:

        self.__ID       (int): the unique ID of the route.
        self.__code     (str): the unique code of the route (<= 3 characters).
        self.__usage    (int): the percentage that indicates the route's usage.
        self.__busCount (int): the number of buses that run on the route.
    """

    def __init__(self, ID, code, usage, busCount):

        try:
            ID = int(ID)
            assert 0 < len(code) and len(code) <= 3
            usage = int(usage)
            assert 0 <= usage and usage <= 100
            busCount = int(busCount)
        except:
            raise Exception("Could not create Route. Check the parameters.")

        self.__ID = ID
        self.__code = code
        self.__usage = usage
        self.__busCount = busCount


    def __repr__(self):
        return("ID: %i, code: %s, usage: %i, busCount: %i" % (self.getID(), self.getCode(), self.getUsage(), self.getBusCount()))

    def getID(self):
        return self.__ID

    def setID(self, newID):
        self.__ID = newID

    def getCode(self):
        return self.__code

    def setCode(self, newCode):
        self.__code = newCode

    def getUsage(self):
        return self.__usage

    def setUsage(self, newUsage):
        self.__usage = newUsage

    def getBusCount(self):
        return self.__busCount

    def setBusCount(self, newBusCount):
        self.__busCount = newBusCount
