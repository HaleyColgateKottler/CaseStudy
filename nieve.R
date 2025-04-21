library(boot)
df <- read.csv('data_for_CATEs.csv')

nieveest <- function(df, indices){
  df <- df[indices, ]
  outcome <- "readmit"
  treatment <- "disposition"
  
  A1 <- which(df$disposition > 0)
  A0 <- which(df$disposition < 0)
  
  Y1 <- mean(unlist(df$readmit[A1]))
  Y0 <- mean(unlist(df$readmit[A0]))
  
  Y1 - Y0
}

set.seed(127)
boot.out <- boot(data = df, statistic = nieveest, R = 5000)
nievecis <- boot.ci(boot.out, type = c("norm", "perc", "basic"))
nievecis$t0
