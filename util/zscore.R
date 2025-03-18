zscore = function(inp){
  
  out = (inp - mean(inp)) / sd(inp)
  
  
  return(out)
  
}