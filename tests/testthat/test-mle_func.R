# source("./R/lm_mle_pinv.R")
source("./R/lm_mle_BFGS.R")
test_that("return TRUE if MLE via pseudo-inverse is close to truth", {
  set.seed(1234)
  ##Linear Regression
  #Generate the independent variable and the error
  x1=rnorm(100,50,9)
  x2=rnorm(100,200,64)
  error=rnorm(100,0,0.1)
  #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
  outcome=as.matrix(150-(4*x1)+(2.5*x2)+error)
  design=as.matrix(cbind(rep(1, 100), cbind(x1, x2)))
  truth=as.matrix(c(150, -4, 2.5))
  expect_true(are_all_close(lm_mle_pinv(design, outcome), truth, abs_tol = 0.1, rel_tol = 0.1))
})


test_that("return TRUE if analytical and numerical gradient match", {
  set.seed(1234)
  x1=rnorm(100,50,9)
  x2=rnorm(100,200,64)
  design=as.matrix(cbind(rep(1, 100), cbind(x1, x2)))
  outcome=as.matrix(rnorm(100))
  beta = rnorm(3)
  noise_var = 1

  ana_grad <- loglik_gradient(design, outcome, beta, noise_var)
  num_grad <- approx_grad(function(beta) log_likelihood(design, outcome, beta, noise_var), beta)
  expect_true(are_all_close(ana_grad, num_grad))
})


test_that("return TRUE if MLE via BFSG is close to truth", {
  set.seed(1234)
  ##Linear Regression
  #Generate the independent variable and the error
  x1=rnorm(100,50,9)
  x2=rnorm(100,200,64)
  error=rnorm(100,0,0.1)
  #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
  outcome=as.matrix(150-(4*x1)+(2.5*x2)+error)
  design=as.matrix(cbind(rep(1, 100), cbind(x1, x2)))
  truth=as.matrix(c(150, -4, 2.5))
  expect_true(are_all_close(lm_mle_BFGS(design, outcome, noise_var = 0.1), truth, abs_tol = 0.1, rel_tol = 0.1))
})


test_that("return TRUE if MLE via pseudo-inverse and BFGS match", {
  set.seed(1234)
  ##Linear Regression
  #Generate the independent variable and the error
  x1=rnorm(100,50,9)
  x2=rnorm(100,200,64)
  error=rnorm(100,0,0.1)
  #Generate the dependent variable (b0=150, b1=-4, b2=2.5)
  outcome=as.matrix(150-(4*x1)+(2.5*x2)+error)
  design=as.matrix(cbind(rep(1, 100), cbind(x1, x2)))
  pinv_result <- hiper_glm(design, outcome, model = 'linear', method = "pinv")
  BFGS_result <- hiper_glm(design, outcome, model = 'linear', method = "BFGS")
  expect_true(are_all_close(coef(pinv_result), coef(BFGS_result), abs_tol = 1e-3, rel_tol = 1e-3))
})


