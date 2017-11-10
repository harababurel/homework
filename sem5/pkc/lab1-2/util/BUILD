cc_library(
    name = "status",
    hdrs = ["status.h"],
    visibility = ["//visibility:public"],
)

cc_library(
    name = "gcd",
    srcs = ["gcd.cc"],
    hdrs = ["gcd.h"],
    visibility = ["//visibility:public"],
    deps = [
        # "//external:givaro",
        "//external:ntl",
        ":factorizer",
    ],
)

cc_library(
    name = "factorizer",
    srcs = ["factorizer.cc"],
    hdrs = [
        "factorizer.h",
        "ntl_hash.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        ":factorization",
        ":status",
        "//external:ntl",
    ],
)

cc_library(
    name = "factorization",
    srcs = ["factorization.cc"],
    hdrs = [
        "factorization.h",
        "ntl_hash.h",
    ],
    copts = ["-std=c++14"],
    visibility = ["//visibility:public"],
    deps = [
        ":status",
        "//external:ntl",
    ],
)
