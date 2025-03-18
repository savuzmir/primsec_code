run_diagnostics = function(df) {
  
  library(numDeriv)
  if (class(df) != "lm") {
    print("This is the Variance information:")
    print(VarCorr(df))
  }
  
  
  print("This is the data fit information (AIC, BIC, loglikelihood, deviance)")
  tmp <- c(AIC(df), BIC(df), logLik(df), deviance(df))
  print(tmp)
  
  par(mfrow=c(2,2))
  tmp <- cooks.distance(df)
  
  id <- tmp <= mean(tmp)*4 # find a principled reason here
  plot(tmp, xlab = "Values", ylab = "Cooks Distance", main = "Potential Outliers")
  abline(h = 4*mean(tmp, na.rm=T), lty = 2, col="red")
  text(x=1:length(tmp) + 1, y= tmp, labels=ifelse(tmp>4*mean(tmp, na.rm=T),names(tmp),""), col="blue") 
  hist((resid(df) - mean(resid(df))) / sd(resid(df)), freq = FALSE, xlab = "Residual Values", main = "Hist. of Residuals"); curve(dnorm, add = TRUE)
  
  qqnorm(residuals(df)); qqline(residuals(df))
  plot(fitted(df), residuals(df), xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted Values");
  abline(h = 0, lty = 3, col = "gray"); abline(v = 0, lty = 3, col = "gray"); lines(smooth.spline(fitted(df), residuals(df)), col="red")
  
  plot(model.response(model.frame(df)) ~ fitted(df), xlab = "Actual", ylab = "Fitted", main = "Correlation between fitted and actual data")
  
  print(corr.test(model.response(model.frame(df)), fitted(df)))
  
  plot(df,type=c("p","smooth"), xlab = "Fitted values", ylab = "Standardized residuals")
  par(ask=TRUE)
  plot(df, sqrt(abs(resid(.)))~fitted(.), xlab = "Fitted values", ylab = "sqrt(residuals)")
  
  
  tt <- getME(df,"theta")
  ll <- getME(df,"lower")
  # The definition of singularity is that some of the constrained parameters of the random effects theta parameters 
  # are on the boundary (equal to zero, or very very close to zero, say <10−6):
  print(min(tt[ll==0]))
  print('Minimum of theta parameter, should not be on the boundary i.e. <10-6:')
  
  
  derivs1 <- df@optinfo$derivs
  sc_grad1 <- with(derivs1,solve(Hessian,gradient))
  
  # Max absolute and scaled gradient
  print("Maximum absolute and scaled gradient:")
  print(max(abs(sc_grad1)))
  print("-------------------")
  # One general problem is that large scaled gradients are often associated 
  # with small absolute gradients: we might decide that we’re more interested in testing the 
  # (parallel) minimum of these two quantities:
  print("Comparing in parallel the minimum of scaled and absolute gradients:")
  print(max(pmin(abs(sc_grad1),abs(derivs1$gradient))))
  print("If this is smaller or bigger compared to the first quantity, as long as it is above the tolerance limit (usually 0.001) it should be okay")   
  # Even if this is smaller compared to the first quantity, 
  # if its above the tolerance limit (usually 0.001) it should be okay
  
  # we can recompute this with the NumDeriv package:
  dd <- update(df, devFunOnly=TRUE)
  pars <- unlist(getME(df, c("theta","fixef")))
  grad2 <- grad(dd,pars)
  hess2 <- hessian(dd,pars)
  sc_grad2 <- solve(hess2,grad2)
  print("This quantity should be similar to the ones above")
  print(max(pmin(abs(sc_grad2),abs(grad2))))
  
}