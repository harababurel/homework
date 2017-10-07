load("//tools/build_rules:qt.bzl", "qt_cc_library")

cc_binary(
    name = "main",
    srcs = [
        "main.cc",
    ],
    deps = [
        "//cipher:caesar",
        "//cipher:hill",
        "//ui:caesar_cipher_widget",
        "//ui:main_window",
        "@qt//:qt_gui",
        "@qt//:qt_widgets",
    ],
)
