eff_sizes = function(df) {
  print("This is the Omega² from Xu 2003:")
  omega_sq_val <- 1-var(residuals(df))/(var(model.response(model.frame(df))))
  print(omega_sq_val)

  print("This is the Jarret Byrnes R²")
  
  tmp <- lm(model.response(model.frame(df)) ~ fitted(df))
  r2_val  <- summary(tmp)$r.squared
  
  print(r2_val)
}