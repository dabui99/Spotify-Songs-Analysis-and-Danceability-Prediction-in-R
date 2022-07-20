nemolm2 <- function(Y, Xk, ridge=0){
  #ridge = lambda >= 0 (defaulting to 0 results in OLS)
  n <- length(Y)
  v1s <- rep(1, n)
  X <- cbind(v1s,Xk)
  p <- dim(X)[2]-1
  
  # Ridge Regression If-Statement
  if(ridge != 0){
    lambda = ridge
    S <- svd(t(X)%*%X + lambda^2*diag(p+1))
  }
  else{
    S <- svd(t(X)%*%X)
  }
  
  # Singular Value Decomposition
  U <- S$u
  D <- diag(S$d)
  V <- S$v
  
  # Condition Number for XtX
  kappa <- max(S$d)/min(S$d)
  
  betahat <- V%*%solve(D)%*%t(U)%*%t(X)%*%Y
  Yhat <- X%*%betahat
  H <- X%*%V%*%solve(D)%*%t(U)%*%t(X)
  lv <- diag(H)
  
  res <- Y - Yhat
  
  SSE <- sum(res^2)
  MSE <- SSE/(n-p-1)
  SST <- sd(Y)^2*(n-1)
  MST <- SST/(n-1)
  SSM <- SST - SSE
  MSM <- SSM/p
  
  sres <- res/(sqrt(MSE)*sqrt(1-lv))
  SEbetahat <- sqrt(MSE)*sqrt(diag(V%*%solve(D)%*%t(U)))
  
  Fstat <- MSM/MSE
  pval <- pf(Fstat, p, n-p-1, lower.tail = F)
  
  r2 <- 1-SSE/SST
  r2adj <- 1- MSE/MST
  
  results <- list("predicted" = Yhat,
                  "residual" = res,
                  "sres" = sres,
                  "condition" = kappa,
                  "leverage"= lv,
                  "sse" = SSE,
                  "mse" = MSE,
                  "ssm" = SSM,
                  "msm" = MSM,
                  "pval" = pval,
                  "betahat" = betahat,
                  "SEbetahat" = SEbetahat,
                  "r2" = r2,
                  "r2adj" = r2adj
  )
  
  return(results)
}