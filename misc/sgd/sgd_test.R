pwd <- file.path(getwd(), 'Documents', 'PhD', 'workspace', 'target', 'misc')
setwd(pwd)
# getwd() == pwd

library("sgd")


# Dimensions
N <- 1e5  # number of data points
d <- 1e1  # number of features

# Generate data.
X <- matrix(rnorm(N*d), ncol=d)
theta <- rep(1.0, d+1)
eps <- rnorm(N)
y <- cbind(1, X) %*% theta + eps
dat <- data.frame(y=y, x=X)

sgd.theta <- sgd(y ~ ., data=dat, model="lm")


glmodel <- glm(y ~ ., data=dat, family = gaussian(link='identity'))

mean((as.vector(sgd.theta$coefficients) - as.vector(glmodel$coefficients))^2)

test_data <- as.data.frame(cbind(y,X))
colnames(test_data)[1] <- "y"
colnames(test_data)[2:(d+1)] <- plyr::laply(colnames(test_data)[2:(d+1)], .fun = function(x) return(paste0("x", as.numeric(gsub("[A-z]", "", x))-1)))

write.csv(test_data,
          file = "~/Documents/Phd/workspace/target/misc/glm_test.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")

read.csv(file = "~/Documents/Phd/workspace/target/misc/glm_test.csv",
         fileEncoding = "UTF-8")
