# from scanner import Scanner
from lexer import Lexer
import os


def main():
    workspace = os.path.dirname(__file__)
    filepath = os.path.join(workspace, "examples/ex1.cc")

    # with Scanner(filepath) as scanner:
    #     print("acquired scanner")

    #     for c in scanner.get():
    #         print(c)

    lexer = Lexer(filepath)


if __name__ == '__main__':
    main()
