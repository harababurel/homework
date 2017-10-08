load("//tools/build_rules:qt.bzl", "qt_cc_library")

qt_cc_library(
    name = "caesar_cipher_widget",
    src = "caesar_cipher_widget.cc",
    copts = [
        "-fpic",
        "--std=c++14",
    ],
    hdr = "caesar_cipher_widget.h",
    ui = "caesar_cipher_widget.ui",
    ui_deps = [
        "@qt//:qt_widgets",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "//cipher:caesar",
        "@qt//:qt_core",
        "@qt//:qt_gui",
        "@qt//:qt_widgets",
    ],
)
