library(boot)
df <- read.csv('data_for_CATEs.csv')

df$disposition <- ifelse(df$disposition > 0, 1, 0)

linearest <- function(df, incides){
  df <- df[incides,]
  
  m1 <- lm(readmit ~ ., df)
  m1$coefficients["disposition"]
}
set.seed(127)
boot.out <- boot(data = df, statistic = linearest, R = 5000)
linearcis <- boot.ci(boot.out, type = c("norm", "perc", "basic"))
linearcis$t0
