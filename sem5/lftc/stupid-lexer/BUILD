cc_binary(
    name = "main",
    srcs = ["main.cc"],
    data = [
        "data/bad_gcd.cc",
        "data/gcd.cc",
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
    deps = ["//util:status"],
)
