load("//tools/build_rules:qt.bzl", "qt_cc_library")

cc_binary(
    name = "main",
    srcs = [
        "main.cc",
    ],
    deps = [
        "//cipher:caesar",
        "//cipher:hill",
        "//ui:cipher_widget",
        "@qt//:qt_gui",
        "@qt//:qt_widgets",
    ],
)
