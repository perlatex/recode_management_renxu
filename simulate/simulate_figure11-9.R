library(tidyverse)
library(MASS)
library(mvtnorm)
library(Matrix)
library(semTools)
library(semPlot)


######################################################################
# setup
N       <- 500
a       <- 0.508
cprime  <- 0.37
b       <- 0.70
######################################################################





# 这里做了最一般的假定:  即误差项之间，以及潜变量之间的协方差为0
######################################################################
X <- rnorm(N, mean = 2, sd = 3)
M <- a * X + rnorm(N, sd = 3)
Y <- b * M + cprime * X + rnorm(N, sd = 3)
######################################################################





######################################################################
# loading matrix
Lambda <- bdiag(
  c(0.78, 0.75, 0.76, 0.82), 
  c(0.78, 0.82, 0.81, 0.85),
  c(0.81, 0.83, 0.85, 0.86)
  )
######################################################################





######################################################################
Theta <- diag(0.3, nrow = 12)
epsilon <- mvrnorm(N, mu = rep(0, ncol(Theta)), Sigma = Theta)
dat <- Matrix::tcrossprod(cbind(X, Y, M), Lambda) + epsilon

dat <- dat %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  setNames(
    c(paste0("X", 1:4), 
      paste0("M", 1:4), 
      paste0("Y", 1:4))
  ) %>% 
  as_tibble()
dat
######################################################################






######################################################################
library(lavaan)

model <- '
  X  =~ X1 + X2 + X3 + X4
  M  =~ M1 + M2 + M3 + M4
  Y  =~ Y1 + Y2 + Y3 + Y4 
  
  M ~ a * X
  Y ~ b * M + cprime * X

  
'


fit <- sem(model, data = dat, meanstructure = TRUE)

summary(fit)
######################################################################







FIT <-
  semPlotModel_lavaanModel(model, auto.var = TRUE, auto.fix.first = TRUE)

p <- semPaths(
  FIT,
  what = "paths",
  whatLabels = "par",
  nodeLabels = c(
    expression(paste(x[1])),
    expression(paste(x[2])),
    expression(paste(x[3])),
    expression(paste(x[4])),
    expression(paste(x[5])),
    expression(paste(x[6])),
    expression(paste(x[7])),
    expression(paste(x[8])),
    expression(paste(y[1])),
    expression(paste(y[2])),
    expression(paste(y[3])),
    expression(paste(y[4])),
    expression(paste(eta[1])),
    expression(paste(eta[2])),
    expression(paste(xi))
  ),
  exoCov = TRUE,
  label.cex = 2,
  edge.label.cex = 1.5,
  edgeLabels = c(
    expression(paste(1)),
    expression(paste(lambda[2])),
    expression(paste(lambda[3])),
    expression(paste(lambda[4])),
    expression(paste(1)),
    expression(paste(lambda[6])),
    expression(paste(lambda[7])),
    expression(paste(lambda[8])),
    expression(paste(1)),
    expression(paste(lambda[10])),
    expression(paste(lambda[11])),
    expression(paste(lambda[12])),
    expression(paste(psi[12])),   
    expression(paste(gamma[1])),
    expression(paste(gamma[2])),
    expression(paste(epsilon[1])),
    expression(paste(epsilon[2])),
    expression(paste(epsilon[3])),
    expression(paste(epsilon[4])),
    expression(paste(epsilon[5])),
    expression(paste(epsilon[6])),
    expression(paste(epsilon[7])),
    expression(paste(epsilon[8])),
    expression(paste(delta[1])),
    expression(paste(delta[2])),
    expression(paste(delta[3])),
    expression(paste(delta[4])),
    expression(paste(psi[1])),
    expression(paste(psi[2])),
    expression(paste(psi[3]))
  )
)




library(qgraph)
qgraph(p, filetype = "png", height = 8, width = 12)


