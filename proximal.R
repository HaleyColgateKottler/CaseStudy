library(boot)
df <- read.csv('data_for_CATEs.csv')

df$disposition <- ifelse(df$disposition > 0, 1, 0)

covariates <- c("anchor_age",             "raceasian",              "raceblack",             
                "racehispanic.or.latino", "racemissing",            "raceother",             
                "insuranceMedicaid",      "insuranceOther",         "marital_statusDIVORCED",
                "marital_statusSINGLE",   "marital_statusWIDOWED",  "genderM" )
outcome <- c('readmit')
treatment <- c('disposition')
proxies <- c('temperature', 'heartrate', 'resprate', 'o2sat', 'sbp', 'dbp',
             'acuity', 'treatment_time')

select_n <- function(varset, n){
  df = data.frame(matrix(0, nrow = 0, ncol = n))
  print(c(n, varset))
  if (n > 1) {
    for (k in 1:(length(varset) - n + 1)){
      n1fd <- select_n(varset[(k+1):length(varset)], n-1)
      print(n1fd)
      for (i in 1:nrow(n1fd)){
        df[nrow(df) + 1, ] <- c(varset[k], n1fd[i,])
      }
    }
    return(df)
  } else {
    return(data.frame(varset))
  }
}
all_subsets <- select_n(proxies[1:length(proxies)], 4)

proxest <- function(df, nco.names, nce.names){
  wformulas <- paste(nco.names,
                     " ~ disposition + ", paste0(nce.names, sep = "", collapse = " + "),
                     " + ", paste0("disposition * ", nce.names, sep = "", collapse = " + "),
                     " + ", paste(covariates, collapse = " + "),
                     sep = "")
  m.hW <- lapply(wformulas, lm, data = df)
  
  wav <- sapply(m.hW, predict, data = df[,c("disposition", "readmit", nce.names, covariates)])
  
  prior.names <- colnames(df)
  df <- cbind(df, wav)
  colnames(df) <- c(prior.names, paste("wav", 1:length(nco.names), sep = ""))
  
  # estimate tau_a
  m1formula <- paste("readmit ~ disposition + ",
                     paste0("wav", 1:length(nco.names), collapse = " + "),
                     " + ",
                     paste0("disposition * ", paste("wav", 1:length(nco.names), sep = ""),
                            collapse = " + "),
                     sep = "")
  m1 <- lm(m1formula, df)
  alpha <- m1$coefficients[c("(Intercept)", paste("wav", 1:length(nco.names), sep = ""))]
  gamma <- m1$coefficients[c("disposition", paste("disposition:wav", 1:length(nco.names), sep = ""))]
  
  # calc CATE
  mWVformulas <- paste(nco.names,
                       " ~ ", paste0(nce.names, collapse = " + "), " + ",
                       paste0(covariates, collapse = " + "))
  m.WVs <- lapply(mWVformulas, lm, data = df)
  WVs <- lapply(m.WVs, predict, data = df)
  CATE <- gamma[1] + gamma[2:(1 + length(nco.names))] %*% t(sapply(WVs, unlist))
  ATE <- mean(CATE)
  ATE
}

ate.ests <- c()
for (rown in 1:70){
  nce.names <- unlist(all_subsets[rown,])
  nco.names <- proxies[!(proxies %in% nce.names)]
  ate.ests[rown] <- proxest(df, nco.names, nce.names)
}
hist(ate.ests)
summary(ate.ests)

proxest <- function(df, indices){
  df <- df[indices,]
  nco.names <- c('marital_statusDIVORCED', 'marital_statusSINGLE', 'marital_statusWIDOWED',
                 'genderF', 'raceasian', 'raceblack', 'racehispanic.or.latino', 
                 'raceother', 'treatment_time')
  nce.names <- c('anchor_age', 'temperature', 'heartrate', 'resprate', 'o2sat',
                 'sbp', 'dbp', 'acuity', 'insuranceMedicaid', 'insuranceMedicare')
  
  wformulas <- paste(nco.names,
                     " ~ disposition + ", paste0(nce.names, sep = "", collapse = " + "),
                     " + ", paste0("disposition * ", nce.names, sep = "", collapse = " + "),
                     sep = "")
  m.hW <- lapply(wformulas, lm, data = df)
  
  wav <- sapply(m.hW, predict, data = df[,c("disposition", "readmit", nce.names)])
  
  prior.names <- colnames(df)
  df <- cbind(df, wav)
  colnames(df) <- c(prior.names, paste("wav", 1:length(nco.names), sep = ""))
  
  # estimate tau_a
  m1formula <- paste("readmit ~ disposition + ",
                     paste0("wav", 1:length(nco.names), collapse = " + "),
                     " + ",
                     paste0("disposition * ", paste("wav", 1:length(nco.names), sep = ""),
                            collapse = " + "),
                     sep = "")
  m1 <- lm(m1formula, df)
  alpha <- m1$coefficients[c("(Intercept)", paste("wav", 1:length(nco.names), sep = ""))]
  gamma <- m1$coefficients[c("disposition", paste("disposition:wav", 1:length(nco.names), sep = ""))]
  
  # calc CATE
  mWVformulas <- paste(nco.names,
                       " ~ ", paste0(nce.names, collapse = " + "))
  m.WVs <- lapply(mWVformulas, lm, data = df)
  WVs <- lapply(m.WVs, predict, data = df)
  CATE <- gamma[1] + gamma[2:(1 + length(nco.names))] %*% t(sapply(WVs, unlist))
  ATE <- mean(CATE)
  ATE
}

set.seed(127)
boot.out <- boot(data = df, statistic = proxest, R = 5000)
proximalcis <- boot.ci(boot.out, type = c("norm", "perc", "basic"))
proximalcis
proxest(df, 1:nrow(df))
