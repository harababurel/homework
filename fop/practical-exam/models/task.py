class Task:
    def __init__(self, text, status):
        """
        Uniqueness of id is guaranteed by the hash function.
        """
        self.__id = hash("%s%s" % (text, status))
        self.__text = text[1:-1]
        self.__status = status


    def getStatus(self):
        return self.__status

    def __repr__(self):
        return self.__text
