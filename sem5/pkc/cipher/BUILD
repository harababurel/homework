cc_library(
    name = "cipher",
    hdrs = ["cipher.h"],
    visibility = ["//visibility:public"],
    deps = [
        "//util:status",
    ],
)

cc_library(
    name = "caesar",
    srcs = ["caesar.cc"],
    hdrs = ["caesar.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":cipher",
    ],
)

cc_library(
    name = "hill",
    srcs = ["hill.cc"],
    hdrs = ["hill.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":cipher",
        "@eigen//:eigen",
    ],
)
