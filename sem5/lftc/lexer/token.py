from scanner import Scanner


class LexerError(Exception):
    pass


class Lexer:
    def __init__(self, startChar):
        self.cargo = startChar.cargo

        # The token stores information about its location in the sourceText
        self.sourceText = startChar.sourceText
        self.lineIndex = startChar.lineIndex
        self.colIndex = startChar.colIndex

        self.type = None

    def show(self, *, showLineNumbers=False, align=True):
        if align:
            tokenTypeLen = 12
            separator = " "
        else:
            tokenTypeLen = 0
            separator = ""

        if showLineNumbers:
            ret = "%s%s  " % (str(self.lineIndex).rjust(6),
                              str(self.colIndex).rjust(4))
        else:
            ret = ""

        if self.type == self.cargo:
            ret += "%s:%s%s" % ("Symbol".ljust(tokenTypeLen, '.'), separator,
                                self.type)
        elif self.type == "Whitespace":
            ret += "%s:%s%s" % ("Whitespace".ljust(tokenTypeLen, '.'),
                                separator, repr(self.cargo))
        else:
            ret += "%s:%s%s" % (self.type.ljust(tokenTypeLen, '.'), separator,
                                self.cargo)

        return s

    guts = property(show)
