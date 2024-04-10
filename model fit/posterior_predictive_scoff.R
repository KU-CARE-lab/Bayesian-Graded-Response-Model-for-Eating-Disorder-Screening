
### 1) DATA CLEANING
# Import data and remove "s21" from column names

x <- read.csv("Cleaned_KUsurvey_di.csv")

x <- subset(x,!is.na(ED_dx))

#x <- subset(x,Year==2022)

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

data <- list()
data$I <- dim(x)[1]
data$N <- 5    # SCOFF
data$Y <- x[,grep("scoff",names(x))[1:5]]
data$K <- 2
data$ED_dx <- x$ED_dx
data$Z <- x$ED_dx
data$FM <- ifelse(x$gender_s21==2,1,0)
data$M <- ifelse(x$gender_s21==1,1,0)
data$other <- ifelse(x$gender_s21>2,1,0)
data$scoff <- x$scoff_total

# NA treatment

for(i in 1:dim(data$Y)[2]){
  data$Y[,i] <- ifelse(is.na(data$Y[,i]),999,data$Y[,i]+1)
}



require(rstan)

load("result_scoff.Rdata")

post <- extract(fit)

###############
# we don't estimate thetas here

inv_logit <- function(x){
  1/(1+exp(-x))
}

ordered_logistic_random <- function(K,eta,c){
  rnumber <- runif(1,0,1)
  if(rnumber <= inv_logit(eta-c[K-1])){
    res <- K
  }
  if(K>2){
    for(i in 2:(K-1)){
      if(rnumber <= inv_logit(eta-c[K-i]) & rnumber > inv_logit(eta-c[K-i+1])){
        res <- K-i+1
      }
    }    
  }
  
  if(rnumber > inv_logit(eta-c[1])){
    res <- 1
  }
  return(res)
}



survey_response <- function(data,i,post,j,n){
  result <- list(K=data$K,
                 theta=post$theta[j,i],
                 alpha=post$alpha[j,n],
                 c=post$c[j,n,])
  response <- ordered_logistic_random(result$K,result$alpha*result$theta,result$c)
  return(response)
}


# we actually need to generate two things: score of base, and diagnosis.

L <- length(post$lp__)


response_est <- array(dim=c(L,data$I,data$N))

for(i in 1:data$I){
  for(j in 1:L){
    for(n in 1:data$N){
      response_est[j,i,n] <- survey_response(data,i,post,j,n)      
    }
  }
  print(i)
}

response_total_est <- apply(response_est-1,c(1,2),sum)

result <- list(response_total_est=response_total_est,
               response_est=response_est)

save(result,file="post_pre_scoff.Rdata")



#########################################
#####  posterior predictive p-value

load("post_pre_scoff.Rdata")

x <- read.csv("Cleaned_KUsurvey_di.csv")

x <- subset(x,!is.na(ED_dx))

#x <- subset(x,Year==2022)

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

data <- list()
data$I <- dim(x)[1]
data$N <- 5    # SCOFF
data$Y <- x[,grep("scoff",names(x))[1:5]]
data$K <- 2
data$ED_dx <- x$ED_dx
data$Z <- x$ED_dx
data$FM <- ifelse(x$gender_s21==2,1,0)
data$M <- ifelse(x$gender_s21==1,1,0)
data$other <- ifelse(x$gender_s21>2,1,0)
data$scoff <- x$scoff_total

# NA treatment

for(i in 1:dim(data$Y)[2]){
  data$Y[,i] <- ifelse(is.na(data$Y[,i]),999,data$Y[,i]+1)
}


# each score


p_score <- rep(0,6)
pp <- rep(0,dim(result$response_total_est)[1])

for(i in 1:6){
  for(j in 1:dim(result$response_total_est)[1]){
    pp[j] <- mean(ifelse(result$response_total_est[j,]==i-1,1,0))
  }
  pp.t <- mean(ifelse(x$scoff_total==i-1,1,0))
  p_score[i] <- mean(ifelse(pp>pp.t,1,0))
}

# plot of scores

par(mfrow=c(2,3))

# males

response_total_est <- result$response_total_est

true_freq <- rep(0,5)
est_freq <- array(dim=c(1000,6))
g <- ifelse(!is.na(data$scoff) & data$M==1,1,0)*(1:data$I)
g <- g[g>0]

for(k in 0:5){
  true_freq[k+1] <- sum(ifelse(data$scoff[g]==k,1,0))/length(g)
  for(j in 1:1000){
    est_freq[j,k+1] <- sum(ifelse(response_total_est[j,g]==k,1,0))/length(g)
  }
}


boxplot(est_freq,main="SCOFF: Cisgender men",xlab="Aggregated score",ylab="Proportion in population",
        at=0:5,names=0:5)
lines(0:5,true_freq,type="o",col='red')

# females

response_total_est <- result$response_total_est

true_freq <- rep(0,5)
est_freq <- array(dim=c(1000,6))
g <- ifelse(!is.na(data$scoff) & data$FM==1,1,0)*(1:data$I)
g <- g[g>0]

for(k in 0:5){
  true_freq[k+1] <- sum(ifelse(data$scoff[g]==k,1,0))/length(g)
  for(j in 1:1000){
    est_freq[j,k+1] <- sum(ifelse(response_total_est[j,g]==k,1,0))/length(g)
  }
}


boxplot(est_freq,main="SCOFF: Cisgender women",xlab="Aggregated score",ylab="Proportion in population",
        at=0:5,names=0:5)
lines(0:5,true_freq,type="o",col='red')

# other

response_total_est <- result$response_total_est

true_freq <- rep(0,5)
est_freq <- array(dim=c(1000,6))
g <- ifelse(!is.na(data$scoff) & data$other==1,1,0)*(1:data$I)
g <- g[g>0]

for(k in 0:5){
  true_freq[k+1] <- sum(ifelse(data$scoff[g]==k,1,0))/length(g)
  for(j in 1:1000){
    est_freq[j,k+1] <- sum(ifelse(response_total_est[j,g]==k,1,0))/length(g)
  }
}

boxplot(est_freq,main="SCOFF: Non-cisgender",xlab="Aggregated score",ylab="Proportion in population",
        at=0:5,names=0:5)
lines(0:5,true_freq,type="o",col='red')






