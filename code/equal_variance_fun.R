equal_variance_fun <- function(fn, par, lower, upper, control){
  fn2 <- function(par) fn(par[c(1, 1)])
  
  opt <- optim(par[[1]], fn2, lower = lower, upper = upper, control = control,
               method = "L-BFGS-B")
  list(
    par = rep(opt$par, 2), fval = opt$value, 
    conv = opt$convergence, message = opt$message
  )
}