extract_loglik <- function(input) 
  
{
  mod_fits <- NULL
  
  mod_fits$logl <- colMeans(extract_log_lik(input, parameter_name = "log_lik", merge_chains = TRUE))
  mod_fits$logl_med <- apply(extract_log_lik(input, parameter_name = "log_lik", merge_chains = TRUE), 2, FUN=median)
  mod_fits$loo <- loo(input, pars="log_lik")

  return(mod_fits)
}