# Dockerfile for testing the `targeted` package against the
# pre-release (development) version of R.
#
# The image is based on rocker/r-devel, which ships the current R-devel
# build (i.e. the upcoming R release) as the `RD` / `Rscript` binaries.
#
# Build:
#   docker build -t targeted-rdevel .
#
# Run the full CRAN check (equivalent to `make check-cran`). The package
# source is mounted at run time rather than copied into the image, so code
# changes need no rebuild:
#   docker run --rm -it -v "$(pwd)":/pkg targeted-rdevel
#
# Open a shell instead:
#   docker run --rm -it -v "$(pwd)":/pkg targeted-rdevel bash
#
# NOTE: src/target is a git submodule (with its own submodules). Because the
# source is mounted from the host, initialise them on the host first, or the
# C++ backend will not compile:
#   git submodule update --init --recursive

FROM rocker/r-devel:latest

# rocker/r-devel installs the development R as `RD` / `Rdevel` /
# `Rscriptdevel` in /usr/local/bin, while the plain `R` / `Rscript` in
# /usr/bin remain the *released* R. Because /usr/local/bin precedes /usr/bin
# on PATH, symlinking `R`/`Rscript` there makes the development build the
# default for every step below (dependency installs AND `make check-cran`).
RUN ln -sf /usr/local/bin/Rdevel /usr/local/bin/R \
    && ln -sf /usr/local/bin/Rscriptdevel /usr/local/bin/Rscript \
    && R --version | head -1

ENV R_KEEP_PKG_SOURCE=yes \
    DEBIAN_FRONTEND=noninteractive \
    _R_CHECK_FORCE_SUGGESTS_=true \
    MAKEFLAGS=-j2 \
    PKG_SYSREQS=true \
    TZ=UTC

# ---------------------------------------------------------------------------
# System tooling (NOT R-package library dependencies).
#
# Only tools that are not tied to a single R package's SystemRequirements are
# installed here: the compile toolchain, cmake/pkg-config for the src/target
# C++ submodule, and qpdf/ghostscript/pandoc used by the build & check steps.
#
# The per-package -dev libraries (libcurl, libssl, libxml2, poppler,
# harfbuzz, fribidi, glpk, gsl, ...) are deliberately NOT listed: pak reads
# each dependency's SystemRequirements and installs the matching apt packages
# automatically (see PKG_SYSREQS=true above).
# ---------------------------------------------------------------------------
RUN apt-get update && apt-get install -y --no-install-recommends \
        build-essential \
        gfortran \
        git \
        make \
        cmake \
        pkg-config \
        pandoc \
        qpdf \
        ghostscript \
        curl \
        wget \
        ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# ---------------------------------------------------------------------------
# Corporate TLS-inspection root CA(s).
#
# If you are behind a proxy that re-signs HTTPS traffic (e.g. Zscaler), the
# container will not trust CRAN / GitHub and package installs fail with
# "unable to get local issuer certificate". Drop any required root CA(s) into
# the `certs/` directory (PEM, one cert per file) and they are added to the
# system trust store here. The directory may be empty.
COPY certs/ /usr/local/share/ca-certificates/extra/
RUN for f in /usr/local/share/ca-certificates/extra/*; do \
        [ -e "$f" ] || continue; \
        mv "$f" "${f%.*}.crt" 2>/dev/null || true; \
    done; \
    update-ca-certificates \
    && curl -sSf -o /dev/null https://cloud.r-project.org/src/contrib/PACKAGES \
    && echo "CRAN TLS OK"

# ---------------------------------------------------------------------------
# Quarto (SystemRequirements: Quarto command line tools) for vignettes
# ---------------------------------------------------------------------------
ARG QUARTO_VERSION=1.9.38
RUN ARCH="$(dpkg --print-architecture)" \
    && curl -L -o /tmp/quarto.deb \
        "https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-${ARCH}.deb" \
    && dpkg -i /tmp/quarto.deb \
    && rm -f /tmp/quarto.deb \
    && quarto --version

# ---------------------------------------------------------------------------
# pak + R tooling needed by the check-cran make target.
#
# pak is installed from source (its version-matched Linux binaries are not
# published for R-devel). `pak::pkg_install()` also installs the system
# requirements (apt packages) of the requested packages automatically.
# ---------------------------------------------------------------------------
RUN Rscript -e 'options(warn = 2); install.packages("pak", repos = "https://cloud.r-project.org"); pak::pkg_install(c("rcmdcheck", "pkgbuild", "tinytest"))'

WORKDIR /pkg

# Only DESCRIPTION is needed at build time so that pak can resolve the
# dependency list. The package source itself is NOT baked into the image:
# mount it at run time instead (see below). Installed dependencies live in R's
# site-library, so they persist regardless of the mount.
COPY DESCRIPTION /pkg/DESCRIPTION

# Install all package dependencies (Depends/Imports/Suggests/LinkingTo).
# `dependencies = TRUE` pulls Suggests as well, which check-cran needs
# because _R_CHECK_FORCE_SUGGESTS_ defaults to true. pak resolves the tree,
# installs the required system (apt) libraries via SystemRequirements, and
# parallelizes downloads/builds automatically; `MAKEFLAGS=-jN` (set above)
# additionally parallelizes the compile steps within each package.
RUN Rscript -e 'pak::local_install_deps(".", dependencies = TRUE)'

# Mount the package source at /pkg when running the container, e.g.:
#   docker run --rm -it -v "$(pwd)":/pkg targeted-rdevel
# The default command runs the full CRAN check exactly as `make check-cran`.
CMD ["make", "check-cran"]
