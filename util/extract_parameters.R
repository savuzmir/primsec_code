extract_parameters <- function(input, modType)
  # INPUT - stan object
  # modType - string (hyperbolic / exponential / cs)
{
  
  extract_parameters <- NULL
  
  if (strcmp(modType, 'hyperbolic')) {
  tmp1 = data.frame(summary(input, pars = c("k"), probs = c(0.05, 0.95))$summary)
  tmp2 = data.frame(summary(input, pars = c("beta"), probs = c(0.05, 0.95))$summary)  
  
  extract_parameters$k = tmp1
  extract_parameters$beta = tmp2
  }
  else if (strcmp(modType, 'exponential')) {
    tmp1 = data.frame(summary(input, pars = c("r"), probs = c(0.05, 0.95))$summary)
    tmp2 = data.frame(summary(input, pars = c("beta"), probs = c(0.05, 0.95))$summary)      

    extract_parameters$r = tmp1
    extract_parameters$beta = tmp2
    
  }
  else
    
  {
  tmp1 = data.frame(summary(input, pars = c("r"), probs = c(0.05, 0.95))$summary)
  tmp2 = data.frame(summary(input, pars = c("s"), probs = c(0.05, 0.95))$summary)
  tmp3 = data.frame(summary(input, pars = c("beta"), probs = c(0.05, 0.95))$summary) 
  
  extract_parameters$r = tmp1
  extract_parameters$s = tmp2
  extract_parameters$beta = tmp3
  }
    
  
  return(extract_parameters)
}

