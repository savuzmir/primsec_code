run_stan_model = function(model, inp, chains, warmup, iter, cores, refresh)
{
  
  out <- stan(
    file = model,  # Stan program
    data = inp,    # named list of data
    init = "random", # random initalisation of param
    chains = chains,             # number of Markov chains
    warmup = warmup,          # number of warmup iterations per chain
    iter = iter,            # total number of iterations per chain
    cores = cores,              # number of cores (could use one per chain)
    refresh = refresh             # no progress shown
  )
  
  return(out)
}