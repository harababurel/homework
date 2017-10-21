cc_library(
    name = "node",
    hdrs = [
        "composite_node.h",
        "literal_node.h",
        "node.h",
    ],
)

cc_library(
    name = "tree",
    srcs = [
        "tree.cc",
    ],
    hdrs = [
        "tree.h",
    ],
    copts = [
        "-std=c++14",
    ],
    deps = [":node"],
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
    deps = [":tree"],
)
