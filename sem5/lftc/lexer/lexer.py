from scanner import Scanner
from keywords import *


class Lexer:
    def __init__(self, filename):
        self.scanner = Scanner(filename)

    def get(self):
        """
        Construct and return the next token.
        """
        current_char = self.scanner.get()

        while current_char in WHITESPACE_CHARS:
            token = Token(cha
