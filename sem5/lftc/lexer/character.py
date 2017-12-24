class Character:
    def __init__(self, c, lineIndex, colIndex, sourceIndex, sourceText):
        self.cargo = c
        self.lineIndex = lineIndex
        self.colIndex = colIndex
        self.sourceIndex = sourceIndex
        self.sourceText = sourceText

    def __str__(self):
        cargo = self.cargo

        to_human = {
            ' ': '\tspace',
            '\t': '\ttab',
            '\n': '\tnewline',
            '\0': '\tendmark',
        }

        if cargo in to_human.keys():
            cargo = to_human[cargo]

        return "%s%s %s" % (str(self.lineIndex).rjust(6),
                            str(self.colIndex).rjust(4), cargo)
