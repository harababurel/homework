py_binary(
    name = "main",
    srcs = ["main.py"],
    data = ["examples/ex1.cc"],
    deps = [
        ":character",
        ":lexer",
        ":scanner",
        ":token",
    ],
)

py_library(
    name = "keywords",
    srcs = ["keywords.py"],
)

py_library(
    name = "character",
    srcs = ["character.py"],
)

py_library(
    name = "scanner",
    srcs = ["scanner.py"],
    deps = [":character"],
)

py_library(
    name = "token",
    srcs = ["token.py"],
    deps = [":character"],
)

py_library(
    name = "lexer",
    srcs = ["lexer.py"],
    deps = [
        ":character",
        ":keywords",
        ":scanner",
    ],
)
