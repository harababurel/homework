from character import *


class Scanner:
    def __init__(self, filename):
        try:
            self.file = open(filename, 'r')
        except FileNotFoundError as e:
            print(e)
            exit(1)

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_value, traceback):
        self.file.close()

    def get(self):
        sourceIndex = 0
        for lineIndex, line in enumerate(self.file):
            for colIndex, c in enumerate(line + '\n'):
                sourceIndex += 1
                yield Character(c, lineIndex + 1, colIndex + 1, sourceIndex,
                                '')
