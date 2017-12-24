from string import ascii_letters, digits

keywords = """
if
then
else
while
std::cout
return
""".split()

singleCharacterKeywords = """
=
( )
/ * + -
! &
. ;
""".split()

doubleCharacterKeywords = """
==
<=
>=
!=
||
&&
""".split()

IDENTIFIER_STARTCHARS = ascii_letters
IDENTIFIER_CHARS = ascii_letters + digits + '_'

NUMBER_STARTCHARS = digits
NUMBER_CHARS = digits + '.'

STRING_STARTCHARS = "'" + '"'
WHITESPACE_CHARS = " \t\n"

# TokenTypes
STRING = "String"
IDENTIFIER = "Identifier"
NUMBER = "Number"
WHITESPACE = "Whitespace"
COMMENT = "Comment"
EOF = "Eof"
