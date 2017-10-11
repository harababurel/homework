load("//tools/build_rules:qt.bzl", "qt_cc_library")

qt_cc_library(
    name = "cipher_widget",
    src = "cipher_widget.cc",
    copts = [
        "-fpic",
        "--std=c++14",
    ],
    hdr = "cipher_widget.h",
    ui = "cipher_widget.ui",
    ui_deps = [
        "@qt//:qt_widgets",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//cipher",
        "//cipher:affine",
        "//cipher:caesar",
        "//cipher:hill",
        "//cipher:substitution",
        "//cipher:vigenere",
        "@qt//:qt_core",
        "@qt//:qt_gui",
        "@qt//:qt_widgets",
    ],
)
