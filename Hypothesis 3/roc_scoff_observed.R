
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

load("scoff_boot_thetas_p.Rdata")

MSE1 <- rep(0,30)
MSE2 <- rep(0,30)

for(k in 1:30){
  x$p <- boot_outcome[[k]]$p1
  x1 <- subset(x,gender_s21==2)
  MSE1[k] <- mean((x1$ED_dx-ifelse(x1$scoff_total>1,1,0))^2)
  MSE2[k] <- mean((x1$ED_dx-x1$p)^2)
}

MSE1
mean(MSE2)
min(MSE2)
max(MSE2)

load("scoff_boot_thetas_p.Rdata")
load("result_scoff_bootstrap_180.Rdata")

MSE1 <- rep(0,30)
MSE2 <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  x$p <- boot_outcome[[k]]$p1[x$X]
  x1 <- subset(x,gender_s21==1)
  MSE1[k] <- mean((x1$ED_dx-ifelse(x1$scoff_total>1,1,0))^2)
  MSE2[k] <- mean((x1$ED_dx-x1$p)^2)
}

print(c(mean(MSE2), sd(MSE2),min(MSE2),max(MSE2)))
print(c(mean(MSE1), sd(MSE1),min(MSE1),max(MSE1)))



# ROC AUC - sub

load("result_scoff_bootstrap_180.Rdata")

par(mfrow=c(1,3))

AUC <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  p <- boot_outcome[[k]]$p1[x$X]
  x$Bayesian_GRM_risk <- round(p*10000)
  predictors <- c('Bayesian_GRM_risk')
  
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC[k] <- prc$Bayesian_GRM_risk$AUC
}



AUC0 <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  predictors <- c('scoff_total')
  
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC0[k] <- prc$scoff_total$AUC
}

boxplot(AUC0,AUC,names=c("Total score","Bayesian GRM"),
        xlab="Method",ylab="ROC AUC",main="SCOFF ROC - Full sample")

points(1,0.808,col="blue",pch=18,cex=3)
points(2,0.821,col="red",pch=10,cex=3)



# ROC AUC - female

load("result_scoff_bootstrap_180.Rdata")

AUC <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  p <- boot_outcome[[k]]$p1[x$X]
  x$Bayesian_GRM_risk <- round(p*10000)
  x <- subset(x,gender_s21==2)
  predictors <- c('Bayesian_GRM_risk')
  
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC[k] <- prc$Bayesian_GRM_risk$AUC
}


AUC0 <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  x <- subset(x,gender_s21==2)
  predictors <- c('scoff_total')
 
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC0[k] <- prc$scoff_total$AUC
}

boxplot(AUC0,AUC,names=c("Total score","Bayesian GRM"),
        xlab="Method",ylab="ROC AUC",main="SCOFF ROC - Women")

points(1,0.809,col="blue",pch=18,cex=3)
points(2,0.819,col="red",pch=10,cex=3)



# ROC AUC - male

load("result_scoff_bootstrap_180.Rdata")

AUC <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  p <- boot_outcome[[k]]$p1[x$X]
  x$Bayesian_GRM_risk <- round(p*10000)
  x <- subset(x,gender_s21==1)
  predictors <- c('Bayesian_GRM_risk')
  
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC[k] <- prc$Bayesian_GRM_risk$AUC
}


AUC0 <- rep(0,30)

for(k in 1:30){
  x <- scoff_boot[[k]]$x
  x <- subset(x,gender_s21==1)
  predictors <- c('scoff_total')
  
  prc <- setNames(lapply(predictors, function(z){
    ROCcurve(x$ED_dx, x[, z],prc=F, cutoff = '>')
  }), predictors)
  
  thresholds <- sapply(predictors, function(z) thresh(x[, z], x$ED_dx,prc=F, cutoff = '>'))
  
  AUC0[k] <- prc$scoff_total$AUC
}

boxplot(AUC0,AUC,names=c("Total score","Bayesian GRM"),
        xlab="Method",ylab="ROC AUC",main="SCOFF ROC - Men")

points(1,0.747,col="blue",pch=18,cex=3)
points(2,0.754,col="red",pch=10,cex=3)



