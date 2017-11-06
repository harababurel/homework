py_binary(
    name = "main",
    srcs = ["main.py"],
    data = ["data/1.json"],
    deps = [":dfa"],
)

py_library(
    name = "dfa",
    srcs = [
        "dfa.py",
        "state.py",
    ],
)
