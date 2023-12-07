dormand_prince <- function(f, y0, t1, t0=0, h0=0.1,
                           control=list(hmax=h0, hmin=1e-16,
                                        fac=0.9,
                                        fac_max=5, fac_min=1/5,
                                        atol=1e-6, rtol=1e-6)) {
  control0 <-list(hmax=h0, hmin=1e-16,
                  fac=0.9,
                  fac_max=5, fac_min=1/5,
                  atol=1e-6, rtol=1e-6)
  control0[names(control)] <- control
  control <- control0

  # Adaptive step size
  cs <- c(0, 1/5, 3/10, 4/5, 8/9, 1, 1)
  as <- matrix(0, 7,7)-
  as[,1] <- c(0,1/5, 3/40, 44/45, 19327/6561, 9017/3168, 35/384)
  as[,2] <- c(0,0, 9/40, -56/15, -25360/2187, -355/33, 0)
  as[,3] <- c(0,0,0, 32/9, 64448/6561, 46732/5247, 500/1113)
  as[,4] <- c(0,0,0,0, -212/729, 49/176, 125/192)
  as[,5] <- c(0,0,0,0,0, -5103/18656, -2187/6784)
  as[,6] <- c(0,0,0,0,0,0, 11/84)
  bs <-  rbind(as[7,],
               c(5179/57600, 0, 7571/16695, 393/640, -92097/339200, 187/2100, 1/40))
  q <- nrow(as)-1

  intstep <- function(t, h, y) {
    k <- matrix(NA, nrow=q+1, ncol=length(y0))
    for (i in seq(nrow(k))) {
      yval <- y
      for (j in seq_along(i-1))
        yval <- yval + as[i,j]*k[j,]
      k[i,] <- h*f(t+cs[i]*h, yval)
    }
    ## k1 <- h*f(t+cs[1]*h, y)
    ## k2 <- h*f(t+cs[2]*h, y + as[2,1]*k1)
    ## k3 <- h*f(t+cs[3]*h, y + as[3,1]*k1 + as[3,2]*k2)
    ## k4 <- h*f(t+cs[4]*h, y + as[4,1]*k1 + as[4,2]*k2 + as[4,3]*k3)
    ## k5 <- h*f(t+cs[5]*h, y + as[5,1]*k1 + as[5,2]*k2 + as[5,3]*k3 + as[5,4]*k4)
    ## k6 <- h*f(t+cs[6]*h, y + as[6,1]*k1 + as[6,2]*k2 + as[6,3]*k3 + as[6,4]*k4 + as[6,5]*k5)
    ## y1 <- y + as[7,1]*k1 + as[7,2]*k2 + as[7,3]*k3 + as[7,4]*k4 + as[7,5]*k5 + as[7,6]*k6
    ## k7 <- h*f(t+cs[7]*h, y1)
    ## Estimate y(x+h) as weighted average of the q increments
    print(k)
    if (identical(as[,q+1], bs[1,])) {
      y1 <- yval
    } else {
      y1 <- y
      for (j in seq_len(q+1))
        y1 <- y1 + bs[1,j]*k[j,]
    }
    ## Estimate y(x+h) as weighted average of the q+1 increments
    y2 <- y
    for (j in seq_len(q+1))
        y2 <- y2 + bs[2,j]*k[j,]
    ## ## Estimate y(x+h) as weighted average of the 7 increments
    #y1 <- y + as[7,1]*k1 + as[7,2]*k2 + as[7,3]*k3 + as[7,4]*k4 + as[7,5]*k5 + as[7,6]*k6
    ## k7 <- h*f(t+cs[7]*h, )
    ## ## Estimate y(x+h) as weighted average of the 7 increments
    #y2 <- y + bs[2,1]*k1 + bs[2,2]*k2 + bs[2,3]*k3 +
    #  bs[2,4]*k4 + bs[2,5]*k5 + bs[2,6]*k6 + bs[2,7]*k7
    return(list(y1, y2))
  }

  step <- function(t, h, y, fac_max) {
    accept <- FALSE;
    teval <- t+h
    while (!accept) {
      teval <- t+h
      step <- intstep(t, h, y)
      y1 <- step[[1]]
      y2 <- step[[2]]
      cat("y =\n")
      print(y1)
      print(y2)
      d <- abs(y2-y1)
      sc <- control$atol + control$rtol*pmax(abs(y1), abs(y2))
      err <- mean((d/sc)^2)^.5 ## err ~ C*h^(q+1)
      if (err<=1) { # accepts
        if (err<1e-30) {
          fac <- fac_max
        }
        accept <- TRUE
      }

      fac <- min(fac_max, max(control$fac_min, control$fac*(1/err)^(1/(q+1))))
      h <- min(control$hmax, h*fac)
      if (accept) {
        fac_max <- control$fac_max
      } else {
        fac_max <- 1
      }
    }
    return(structure(c(teval, y2), h=h, fac_max=fac_max))
  }

  t <- t0
  h <- max(h0,1e-30)
  y <- y0
  r0 <- c(t, y)
  fac <- control$fac_max
  while (t<t1) {
    r1 <- step(t, h, y, fac)
    r0 <- rbind(r0, r1)
    t <- r1[1]
    h <- min(t1-t, attr(r1, "h"))
    fac <- attr(r1, "fac_max")
    y <- r1[-1]
  }
  return(r0)
}
