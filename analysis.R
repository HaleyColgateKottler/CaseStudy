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

dummy <- dummyVars(" ~ race + insurance + marital_status + gender", data=df)
new_df <- data.frame(predict(dummy, newdata=df))
covariates <- c('anchor_age',
                colnames(new_df))
full_df <- cbind(df[,c('anchor_age', outcome, treatment, proxies)], new_df)
full_df$disposition <- ifelse(df$disposition == "ADMITTED", 1, 0)

library(car)
library(corrplot)

full_df <- full_df[full_df$insuranceMissing != 1, ]
full_df <- full_df[full_df$raceunknown != 1, ]
full_df <- full_df[full_df$marital_statusMissing != 1, ]

to.scale <- c(proxies, treatment, covariates)
scale.means <- colMeans(full_df[,to.scale])
full_df[,to.scale] <- data.frame(t(apply(full_df[,to.scale], 1, function(row){row - scale.means})))
scale.vars <- sqrt(apply(full_df[,to.scale], 2, var))
full_df[,to.scale] <- data.frame(t(apply(full_df[,to.scale], 1, function(row){row/scale.vars})))

full_df <- full_df[, colnames(full_df) != "genderM"]
full_df$raceother <- full_df$raceother + full_df$raceamerican.indian.or.alaska.native
full_df <- full_df[, colnames(full_df) != "raceamerican.indian.or.alaska.native"]
covariates <- covariates[!(covariates %in% 
                             c("genderM", "insuranceMissing", "raceunknown",
                               "marital_statusMissing", "racewhite",
                               "insuranceOther", "marital_statusMARRIED",
                               "raceamerican.indian.or.alaska.native"))]

corm <- cor(full_df[,covariates])
corrplot(corm, number.digits = 1, method = 'color')
eigen(corm)$values
# covariates <- covariates[!(covariates  %in% c('insuranceMedicare',
# 'racewhite'))]
m1 <- lm(paste('readmit ~', paste0(covariates, collapse = " + ")), full_df)
vifout <- vif(m1)
vifout

write.csv(full_df, "data_for_CATEs.csv", row.names = FALSE)


summary(full_df)

# factor analysis with covariates
# kvals <- 1:7
# z.vars <- c(proxies, 'disposition')
# 
# fits <- c()
# AICs <- c()
# k.keeps <- c()
# for (k in kvals) {
#   print(k)
#   h.vars <- paste0("efa('efa1')*h", 1:k)
#   
#   model <- paste(paste(h.vars, collapse = " + "), "=~",
#                  paste(z.vars, collapse = " + "), "\n",
#                  paste(paste0("h", 1:k), collapse = " + "), "~",
#                  paste(covariates, collapse = " + "),
#                  "\n",
#                  paste0("h", 1:k, sep = "", collapse = " ~ 0*1\n"),
#                  "~ 0*1")
#   
#   fit <- tryCatch(
#     {
#       sem(model, data = full_df, rotation = "varimax")
#     },
#     error = function(e) {
#       print(e)
#       new_NullSEM(0)
#     }
#   )
#   
#   if (inspect(fit, "converged")) {
#     fits <- c(fits, fit)
#     AICs <- c(AICs, AIC(fit))
#     k.keeps <- c(k.keeps, k)
#   }
# }
# 
# fit <- fits[[which(AICs == min(AICs))]]
# pars <- parameterEstimates(fit)
# params = inspect(fit, what = 'est')
# k = k.keeps[which(AICs == min(AICs))]
# lavInspect(fit)

k = 1
print(k)
h.vars <- paste0("efa('efa1')*h", 1:k)
z.vars <- c(proxies, 'disposition')
model <- paste(paste(h.vars, collapse = " + "), "=~",
               paste(z.vars, collapse = " + "), "\n",
               paste(paste0("h", 1:k), collapse = " + "), "~",
               paste(covariates, collapse = " + "),
               "\n",
               paste0("h", 1:k, sep = "", collapse = " ~ 0*1\n"),
               "~ 0*1")

fit <- sem(model, data = full_df, rotation = "varimax")
inspect(fit, "converged")


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
lambda.est <- params$lambda[1:(p+1), k]
psi.est <- params$theta[1:(p+1), 1:(p+1)]
theta.est <- params$psi[(k+1):(m+k), (k+1):(m+k)]
gamma.est <- params$beta[1:k, (k+1):(m+k)]


psi.inv <- solve(psi.est)
covU_ZX <- solve(t(lambda.est) %*% psi.inv %*% lambda.est + diag(k))
predU_Xcomp <- apply(full_df[,covariates], 1,
                            function(row){gamma.est %*% row})
predU_Zcomp <- apply(full_df[, z.vars], 1,
                     function(row){t(lambda.est) %*% psi.inv %*% row})
  
predU_ZX <- covU_ZX[1,1] * (predU_Zcomp + predU_Xcomp)
full_df$predU_ZX <- predU_ZX
hist(predU_ZX)

model2 <- paste(outcome, "~ 1 + ",
                paste0(covariates, " * predU_ZX", collapse = " + "),
                "+ disposition * predU_ZX"
                )
reg <- lm(model2, full_df)
betas <- reg$coefficients
gamma <- c(betas[["disposition"]], betas[["predU_ZX:disposition"]])

CATE.ests <- (max(full_df$disposition) - min(full_df$disposition)) *
  (gamma[1] + gamma[2]*predU_ZX)
hist(CATE.ests)
ATE.est <- mean(CATE.ests)
