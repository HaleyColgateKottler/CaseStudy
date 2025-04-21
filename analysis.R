source("latentMethod.R")
new_NullSEM <- function(x = double(), convergence = FALSE) {
  stopifnot(is.double(x))
  structure(x,
            class = "NullSEM",
            converged = convergence
  )
}

inspect.NullSEM <- function(x, what) {
  attr(x, what)
}
library(caret)
library(lavaan)

df <- read.csv('full_data.csv')

covariates <- c('gender', 'anchor_age', 'race', 'insurance', 'marital_status')
outcome <- c('readmit')
treatment <- c('disposition')
proxies <- c('temperature', 'heartrate', 'resprate', 'o2sat', 'sbp', 'dbp',
             'acuity', 'treatment_time')

df$race <- ifelse(df$race == "american indian or alaska native", "other", df$race)
dummy <- dummyVars(" ~ race + insurance + marital_status + gender", data=df)
new_df <- data.frame(predict(dummy, newdata=df))
covariates <- c('anchor_age',
                colnames(new_df))
full_df <- cbind(df[,c('anchor_age', outcome, treatment, proxies)], new_df)
full_df$disposition <- ifelse(df$disposition == "ADMITTED", 1, 0)

library(reshape2)
melted <- melt(full_df[, c('anchor_age', 'temperature',
                           'heartrate', 'resprate',
                           'o2sat', 'sbp', 'dbp',
                           'treatment_time')])
ggplot(melted, aes(x = value)) + geom_histogram() +
  facet_wrap(.~variable, scales = 'free')

library(car)
library(corrplot)

to.scale <- c(proxies, treatment, covariates)
scale.means <- colMeans(full_df[,to.scale])
full_df[,to.scale] <- data.frame(t(apply(full_df[,to.scale], 1, function(row){row - scale.means})))
scale.vars <- sqrt(apply(full_df[,to.scale], 2, var))
full_df[,to.scale] <- data.frame(t(apply(full_df[,to.scale], 1, function(row){row/scale.vars})))


apply(df[, c('race', 'insurance', 'marital_status', 'gender')], 2,
      function(col){tail(names(sort(table(col))), 1)})

# drop most common level of each categorical variable
full_df <- full_df[, !(colnames(full_df) %in% c("genderF", "insuranceMedicare",
                                                "racewhite",
                                                "marital_statusMARRIED"))]
covariates <- covariates[!(covariates %in% 
                             c("genderF", "insuranceMedicare",
                               "racewhite",
                               "marital_statusMARRIED"))]

corm <- cor(full_df[,covariates])
corrplot(corm, number.digits = 1, method = 'color')
eigen(corm)$values
# covariates <- covariates[!(covariates  %in% c('insuranceMedicare',
# 'racewhite'))]
m1 <- lm(paste('readmit ~', paste0(covariates, collapse = " + ")), full_df)
vifout <- vif(m1)
sort(vifout)

# remove highest vif until < 2
full_df <- full_df[, !(colnames(full_df) %in% c('marital_statusmissing',
                                                'insurancemissing'))]
covariates <- covariates[!(covariates %in% 
                             c("marital_statusmissing",
                               "insurancemissing"))]


write.csv(full_df, "data_for_CATEs.csv", row.names = FALSE)


summary(full_df)

# factor analysis with covariates
kvals <- 1:8
z.vars <- c(proxies, 'disposition')

fits <- c()
BICs <- c()
AICs <- c()
k.keeps <- c()
for (k in kvals) {
  print(k)
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

  fit <- tryCatch(
    {
      sem(model, data = full_df, rotation = "varimax")
    },
    error = function(e) {
      print(e)
      new_NullSEM(0)
    }
  )

  if (inspect(fit, "converged")) {
    fits <- c(fits, fit)
    BICs <- c(BICs, BIC(fit))
    AICs <- c(AICs, AIC(fit))
    k.keeps <- c(k.keeps, k)
  }
}

plot(BICs)
plot(AICs)
fit <- fits[[which(AICs == min(AICs))]]
pars <- parameterEstimates(fit)
params = inspect(fit, what = 'est')
k = k.keeps[which(AICs == min(AICs))]
summary(fit)

lavInspect(fit)

print(k)
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
z.vars <- c(proxies, 'disposition')
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
inspect(fit, "converged")
# summary(fit)

library(semPlot)
library(ggplot2)
png(file = "model_diagram.png", width = 12, height = 12, units = "in",
    res = 500)
semPaths(fit, what = "cons", whatLabels = "cons",
             intercepts = FALSE, layout = "tree",
             nCharNodes = 0, curve = 1.5,
             sizeMan = 7)
dev.off()



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
full_df$predU_ZX <- predU_ZXA
expanded_df <- cbind(full_df, t(predU_ZXA))
hist(predU_ZXA)

model2 <- paste(outcome, "~ 1",
                " + disposition",
                "+ disposition * h1",
                "+ disposition * h2",
                "+ disposition * h3",
                "+ disposition * h4",
                "+ disposition * h5",
                "+ disposition * h6",
                "+ disposition * h7"
                )
reg <- lm(model2, expanded_df)
betas <- reg$coefficients
gamma <- c(betas[["disposition"]], betas[["disposition:h1"]],
           betas[["disposition:h2"]], betas[["disposition:h3"]],
           betas[["disposition:h4"]], betas[["disposition:h5"]],
           betas[["disposition:h6"]], betas[["disposition:h7"]])


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
hist(CATE.ests)
ATE.est <- mean(CATE.ests)
