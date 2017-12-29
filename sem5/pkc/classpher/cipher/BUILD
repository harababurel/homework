cc_library(
    name = "icipher",
    hdrs = [
        "icipher.h",
    ],
    deps = [
        "//external:ntl",
    ],
)

cc_library(
    name = "classical_cipher",
    hdrs = [
        "classical_cipher.h",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "icipher",
        "//util:status",
        "@boost//:detail",
        "@boost//:static_assert",
        "@boost//:type_index",
        "@boost//:type_traits",
    ],
)

cc_library(
    name = "public_key_cipher",
    hdrs = [
        "public_key_cipher.h",
    ],
    copts = ["-std=c++17"],
    visibility = ["//visibility:public"],
    deps = [
        "icipher",
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
        ":classical_cipher",
    ],
)

cc_library(
    name = "hill",
    srcs = ["hill.cc"],
    hdrs = ["hill.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":classical_cipher",
        "@eigen//:eigen",
    ],
)

cc_library(
    name = "vigenere",
    srcs = ["vigenere.cc"],
    hdrs = ["vigenere.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":classical_cipher",
    ],
)

cc_library(
    name = "affine",
    srcs = ["affine.cc"],
    hdrs = ["affine.h"],
    copts = ["-std=c++17"],
    visibility = ["//visibility:public"],
    deps = [
        ":classical_cipher",
        "//external:ntl",
    ],
)

cc_library(
    name = "substitution",
    srcs = ["substitution.cc"],
    hdrs = ["substitution.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":classical_cipher",
    ],
)

cc_library(
    name = "permutation",
    srcs = ["permutation.cc"],
    hdrs = ["permutation.h"],
    visibility = ["//visibility:public"],
    deps = [
        ":classical_cipher",
    ],
)

cc_library(
    name = "rsa",
    srcs = ["rsa.cc"],
    hdrs = ["rsa.h"],
    copts = ["-std=c++17"],
    visibility = ["//visibility:public"],
    deps = [
        ":public_key_cipher",
    ],
)
