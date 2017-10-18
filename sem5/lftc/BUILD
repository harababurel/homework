cc_binary(
    name = "main",
    srcs = ["main.cc"],
    data = [
        "ex1.cc",
        "ex2.cc",
        "symbols.csv",
    ],
    deps = [
        ":lexer",
        "//util:status",
    ],
)

cc_library(
    name = "lexer",
    srcs = ["lexer.cc"],
    hdrs = ["lexer.h"],
    deps = ["//util:status"],
)
