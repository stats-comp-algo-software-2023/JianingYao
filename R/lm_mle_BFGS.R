# calculate log-likelihood under a linear model
log_likelihood <- function (design, outcome, beta, noise_var = 1) {
  n <- length(outcome)
  est <- design %*% beta
  # loglik = -(n/2)*log(2*pi*noise_var) - (1/(2*noise_var))*sum((y - est)^2)
  loglik = - (1/(2*noise_var))*sum((y - est)^2)
  return(loglik)
}

# calculate gradient under a linear model
loglik_gradient <- function (design, outcome, beta, noise_var = 1) {
  est <- design %*% beta
  grad <- t(design) %*% (outcome - est) / noise_var
  return(grad)
}


# approximate the gradient of a given function via finite difference
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  numerical_grad <- rep(0, length(x))
  for (i in 1:length(x)){
    delta <- rep(0, length(x))
    delta[i] <- dx
    numerical_grad[i] <- (func(x+delta)-func(x-delta))/(2*dx)
  }
  return(numerical_grad)
}

# implement BFGS to fund MLE
lm_mle_BFGS <- function (design, outcome, noise_var = 1) {
  np = ncol(design)
  result <- optim(par = rep(0, np), fn = log_likelihood, gr = loglik_gradient, design = design, outcome = outcome, noise_var = noise_var, method = "BFGS")
  beta_est <- result$par
  return(beta_est)
}


