import time

class Task:
    def __init__(self, text, status):
        """
        Uniqueness of id is guaranteed by applying the hash() function
        on the current time, expressed in fractional seconds.
        """
        self.__id = hash(time.monotonic())
        self.__text = text
        self.__status = status

        if self.__text[0] == ' ':
            self.__text = self.__text[1:]
        if self.__text[-1] == '\n':
            self.__text = self.__text[:-1]

    def getStatus(self):
        return self.__status

    def setStatus(self, status):
        if status in ['active', 'done', 'archived']:
            self.__status = status

    def getText(self):
        return self.__text

    def setText(self, text):
        self.__text = text

    def __repr__(self):
        return self.__text
