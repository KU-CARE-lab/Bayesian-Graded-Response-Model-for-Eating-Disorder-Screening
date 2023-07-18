
require(rstan)

source('functions.R')

# use "results"

x <- read.csv("Cleaned_KUsurvey_di.csv")


data <- list()
data$I <- dim(x)[1]
data$N <- 5    # SCOFF
data$Y <- x[,grep("scoff",names(x))[1:5]]
data$K <- 2
data$Z <- x$ED_dx
data$FM <- ifelse(x$gender_s21==2,1,0)
data$M <- ifelse(x$gender_s21==1,1,0)
data$other <- ifelse(x$gender_s21>2,1,0)

inv_logit <- function(x){
  1/(1+exp(-x))
}

load("scoff_prob_all.Rdata")

x$Bayesian_GRM_risk <- round(prob*10000)

x$p <- prob

x <- subset(x,gender_s21==1)

MSE1 <- mean((x$ED_dx-ifelse(x$scoff_total>1,1,0))^2)
MSE1
MSE2 <- mean((x$ED_dx-x$p)^2)
MSE2

# ROC

predictors <- c('Bayesian_GRM_risk')

prc <- setNames(lapply(predictors, function(z){
  ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
}), predictors)

thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))

print(list(prc,thresholds))

# PR


predictors <- c('Bayesian_GRM_risk')

prc <- setNames(lapply(predictors, function(z){
  ROCcurve(x$ED_dx, x[, z],prc=T, cutoff = '>')
}), predictors)

thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=T, cutoff = '>'))

print(list(prc,thresholds))


