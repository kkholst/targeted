code <-
'
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(targeted)]]
#include <RcppArmadillo.h>
#include <targeted.h>
using namespace arma;
// [[Rcpp::export]]
 arma::rowvec dy_1(const arma::rowvec &inp,
        		   const arma::rowvec &y,
                   const arma::rowvec &p) {
  arma::rowvec result(y.n_elem, arma::fill::zeros);
  result = p(0) + p(1)*y;
  return result;
}
// [[Rcpp::export]]
odeptr_t make_dy() {
  return odeptr_t(new target::odefunc(dy_1));
}
'
res <- Rcpp::sourceCpp(code=code)
f <- make_dy()

tt <- seq(0, 2, length.out=100)
y <- targeted::solve_ode(f, cbind(tt), cbind(0), rbind(c(1, 1)))

head(cbind(tt,y))


library(targeted)

f <- specify_ode('result = p(0) + p(1)*y;')

tt <- seq(0, 2, length.out=100)
y <- solve_ode(f, tt, 0, c(1, 1))
head(cbind(tt,y))


ode <-
  'dy(0) = p(0)*(y(1)-y(0));
   dy(1) = y(0)*(p(1)-y(2));
   dy(2) = y(0)*y(1)-p(2)*y(2);'

f <- specify_ode(ode)

tt <- seq(0, 100, length.out=2e4)
y <- solve_ode(f, tt, c(x=1,y=1,z=1), c(sigma=10,rho=28,beta=8/3))
colnames(y) <- c("x","y","z")

scatterplot3d::scatterplot3d(y, cex.symbols=0.1, type='b', color=viridisLite::viridis(nrow(y)))


y
head(cbind(tt,y))




// arma::vec dy(const arma::vec &input,
//              const arma::vec &x,

//              const arma::vec &par) {
//   return par(0) + par(1)*x;
// }
// // [[Rcpp::export]]
// odeptr_t make_dy() {
//   return odeptr_t(new target::odefunc(dy));
// }



code <- '
#define ARMA_R
  return par(0) + par(1)*x;
}
'

res <- Rcpp::sourceCpp(code=code)

ode(0:10)


code <- '
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(targeted)]]
#define ARMA_R
#include <RcppArmadillo.h>
#include <targeted.h>
#include <target/odesolver.hpp>
arma::vec dl(const arma::vec &input,
             const arma::vec &x,
             const arma::vec &par) { // (sigma,)
  return par(0) + par(1)*x;
}
// [[Rcpp::export(name="ode")]]
arma::mat odetest(arma::vec &t) {
   arma::vec y = targeted::ode(t);
return(y);
}
'

res <- Rcpp::sourceCpp(code=code)
