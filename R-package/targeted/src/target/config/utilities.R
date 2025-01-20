load_packages <- function(x, repos="https://cloud.r-project.org/", ...) {
    for (p in x) {
        res <- try(suppressPackageStartupMessages(library(p, character.only=TRUE)), silent=FALSE)
        if (inherits(res, "try-error")) {
            install.packages(p, repos=repos, ...)
            suppressPackageStartupMessages(library(p, character.only=TRUE))
        }
    }
}
