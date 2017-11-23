cc_binary(
    name = "main",
    srcs = ["main.cc"],
    copts = [
        "-std=c++14",
    ],
    data = [
        "data/bad_gcd.cc",
        "data/constant.json",
        "data/gcd.cc",
        "data/identifier.json",
        "data/perimeter.cc",
        "reserved_words.csv",
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
    copts = [
        "-std=c++14",
    ],
    deps = [
        "//dfa",
        "//util:status",
    ],
)
