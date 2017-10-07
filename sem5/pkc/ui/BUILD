load("//tools/build_rules:qt.bzl", "qt_cc_library")

qt_cc_library(
    name = "main_window",
    src = "main_window.cc",
    copts = [
        "-fpic",
    ],
    hdr = "main_window.h",
    ui = "main_window.ui",
    ui_deps = [
        "@qt//:qt_widgets",
    ],
    visibility = ["//visibility:public"],
    deps = [
        "@qt//:qt_core",
        "@qt//:qt_gui",
        "@qt//:qt_widgets",
    ],
)

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
