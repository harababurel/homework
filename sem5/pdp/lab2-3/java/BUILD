java_binary(
    name = "main",
    srcs = ["Main.java"],
    main_class = "pdp.Main",
    deps = [
        ":matrix",
        ":matrix_multiplier",
    ],
)

java_library(
    name = "matrix",
    srcs = ["Matrix.java"],
)

java_library(
    name = "matrix_multiplier",
    srcs = ["MatrixMultiplier.java"],
    deps = [":matrix"],
)
