py_binary(
    name = "main",
    srcs = ["main.py"],
    data = glob(["data/*.json"]),
    deps = [
        ":dfa",
    ],
)

py_library(
    name = "dfa",
    srcs = [
        "dfa.py",
        "state.py",
    ],
)
