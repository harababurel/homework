py_binary(
    name = "main",
    srcs = ["main.py"],
    data = ["data/1.json"],
    deps = [
        ":dfa",
        ":dfa_drawer",
    ],
)

py_library(
    name = "dfa",
    srcs = [
        "dfa.py",
        "state.py",
    ],
)

py_library(
    name = "dfa_drawer",
    srcs = [
        "dfa_drawer.py",
    ],
    deps = [":dfa"],
)
