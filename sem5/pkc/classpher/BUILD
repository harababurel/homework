load("//tools/build_rules:qt.bzl", "qt_cc_library")

cc_binary(
    name = "main",
    srcs = [
        "main.cc",
    ],
    copts = ["-std=c++17"],
    deps = [
        "//cipher:rsa",
        "//external:ntl",
        "//ui:cipher_widget",
        "//util:gcd",
    ],
)
