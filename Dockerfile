FROM rocker/r-devel-ubsan-clang

RUN R -e \
	'.libPaths("/usr/lib/R/library"); install.packages(c("devtools", "optimx", "futile.logger", "RcppArmadillo", "lava", "knitr", "testthat", "rmarkdown"))'

WORKDIR /project

CMD RD CMD check --library=/usr/lib/R/library --as-cran targeted
