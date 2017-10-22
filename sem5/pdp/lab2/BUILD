cc_library(
    name = "matrix",
    hdrs = [
        "matrix.hpp",
    ],
)

cc_library(
    name = "matrix_generator",
    hdrs = [
        "matrix_generator.hpp",
    ],
    deps = [":matrix"],
)

cc_binary(
    name = "main",
    srcs = ["main.cc"],
    copts = [
        "-std=c++14",
    ],
    linkopts = [
        "-lpthread",
    ],
    deps = [
        ":matrix",
        ":matrix_generator",
    ],
)
