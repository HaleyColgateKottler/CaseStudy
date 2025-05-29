library(boot)
library(lavaan)
full_df <- read.csv('data_for_CATEs.csv')

covariates <- c("anchor_age",             "raceasian",              "raceblack",             
                "racehispanic.or.latino", "racemissing",            "raceother",             
                "insuranceMedicaid",      "insuranceOther",         "marital_statusDIVORCED",
                "marital_statusSINGLE",   "marital_statusWIDOWED",  "genderM" )
outcome <- 'readmit'
treatment <- 'disposition'
proxies <- c('temperature', 'heartrate', 'resprate', 'o2sat', 'sbp', 'dbp',
             'acuity', 'treatment_time')
k <- 7


proxyest <- function(full_df, indices){
  full_df <- full_df[indices, ]
  z.vars <- c(proxies, treatment)
  h.vars <- paste0("h", 1:k)
  zerovars <- c()
  if (k > 1){
    # Add covariances to be zero
    for (i in 1:(k-1)) {
      for (j in (i+1):k) {
        zerovars <- c(zerovars, paste(h.vars[i], "~~ 0 *", h.vars[j], "\n"))
      }
    }
  }
  model <- paste(paste(paste0("h", 1:k), collapse = " + "), "=~",
                 paste(z.vars, collapse = " + "), "\n",
                 paste(paste0("h", 1:k), collapse = " + "), "~",
                 paste(covariates, collapse = " + "),
                 "\n",
                 paste0("h", 1:k, sep = "", collapse = " ~ 0*1\n"),
                 "~ 0*1 \n",
                 paste(paste0(proxies, " ~~ 1*", proxies, " \n"), collapse = ""), 
                 paste(paste0(treatment, " ~~ 1*", treatment, " \n"), collapse = ""),
                 paste(zerovars, collapse = "")
  )
  fit <- sem(model, data = full_df, rotation = "varimax")
  p = length(proxies)
  m = length(covariates)
  params <- inspect(fit, what = "est")
  lambda.est <- params$lambda[1:(p+1), 1:k]
  psi.est <- params$theta[1:(p+1), 1:(p+1)]
  theta.est <- params$psi[(k+1):(m+k), (k+1):(m+k)]
  gamma.est <- params$beta[1:k, (k+1):(m+k)]
  
  
  psi.inv <- solve(psi.est)
  covU_ZX <- solve(t(lambda.est) %*% psi.inv %*% lambda.est + diag(k))
  predU_Xcomp <- apply(full_df[,covariates], 1,
                       function(row){gamma.est %*% row})
  predU_Zcomp <- apply(full_df[, z.vars], 1,
                       function(row){t(lambda.est) %*% psi.inv %*% row})
  
  predU_ZXA <- covU_ZX %*% (predU_Zcomp + predU_Xcomp)
  expanded_df <- cbind(full_df, t(predU_ZXA))
  
  model2 <- paste(outcome, "~ 1",
                  " + ", treatment,
                  paste(paste0(paste("+", treatment, "*"), paste0(" h", 1:k)),
                        collapse = " ")
  )
  reg <- lm(model2, expanded_df)
  betas <- reg$coefficients
  gamma <- c(betas[[treatment]], betas[paste0(treatment, ":h", 1:k)])
  
  
  lambda.est <- params$lambda[1:(p), 1:k]
  psi.est <- params$theta[1:(p), 1:(p)]
  theta.est <- params$psi[(k+1):(m+k), (k+1):(m+k)]
  gamma.est <- params$beta[1:k, (k+1):(m+k)]
  
  
  psi.inv <- solve(psi.est)
  covU_ZX <- solve(t(lambda.est) %*% psi.inv %*% lambda.est + diag(k))
  predU_Xcomp <- apply(full_df[,covariates], 1,
                       function(row){gamma.est %*% row})
  predU_Zcomp <- apply(full_df[, z.vars[1:p]], 1,
                       function(row){t(lambda.est) %*% psi.inv %*% row})
  
  predU_ZX <- covU_ZX %*% (predU_Zcomp + predU_Xcomp)
  
  CATE.ests <- (max(full_df$disposition) - min(full_df$disposition)) *
    (gamma[1] + gamma[2:(1+k)]%*%predU_ZX)
  ATE.est <- mean(CATE.ests)
}


set.seed(127)
boot.out <- boot(data = full_df, statistic = proxyest, R = 5000)
cis <- boot.ci(boot.out, type = c("norm", "perc", "basic"))
cis$t0
proxyest(df, 1:nrow(df))
