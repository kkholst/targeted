# zzz.R

.onLoad <- function(libname, pkgname) {
 dSigma_dtheta <<- memoise::memoise(dSigma_dtheta)
}
