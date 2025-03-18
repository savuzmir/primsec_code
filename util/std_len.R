std_len = function(data, mean, leng) {
  dev <- 0
  tmp <- (data - mean)^2 
  dev <- dev + tmp
  std_len <- sqrt(dev)/(leng - 1)
}