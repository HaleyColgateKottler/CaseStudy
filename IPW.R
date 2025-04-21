library(boot)

df <- read.csv('data_for_CATEs.csv')

df$readmit <- ifelse(df$readmit > 0, 1, 0)
df$disposition <- ifelse(df$disposition > 0, 1, 0)

ipwest <- function(df, incides){
  df <- df[incides,]
  withoutReadmit <- df[, colnames(df) != "readmit"]
  
  m1 <- glm(formula = disposition ~ ., family = binomial(link = "logit"),
            data = withoutReadmit)
  x <- predict(m1, newdata = withoutReadmit[, colnames(withoutReadmit != "disposition")],
               type = "response")
  
  EY1 <- mean(unlist(df$disposition * df$readmit / x))
  EY0 <- mean(unlist((1 - df$disposition) * df$readmit / (1 - x)))
  
  IPW.ATE <- EY1 - EY0
  IPW.ATE
}

set.seed(127)
boot.out <- boot(data = df, statistic = ipwest, R = 5000)
IPWcis <- boot.ci(boot.out, type = c("norm", "perc", "basic"))
IPWcis$t0
