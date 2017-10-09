cc_library(
    name = "cipher",
    hdrs = [
        "cipher.h",
        "icipher.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//util:status",
        "@boost//:detail",
        "@boost//:static_assert",
        "@boost//:type_index",
        "@boost//:type_traits",
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
