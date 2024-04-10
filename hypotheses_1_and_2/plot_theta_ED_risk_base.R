
### 1) DATA CLEANING
# Import data and remove "s21" from column names

x <- read.csv("Cleaned_KUsurvey_di.csv")

x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

data <- list()
data$I <- dim(x)[1]
data$N <- 10    # SCOFF
data$Y <- x[,grep("base",names(x))[1:10]]
data$K <- 5
data$Z <- x$ED_dx
data$FM <- ifelse(x$gender_s21==2,1,0)
data$M <- ifelse(x$gender_s21==1,1,0)
data$other <- ifelse(x$gender_s21>2,1,0)

# NA treatment

for(i in 1:dim(data$Y)[2]){
  data$Y[,i] <- ifelse(is.na(data$Y[,i]),999,data$Y[,i]+1)
}


require(rstan)

load("result_base.Rdata")

post <- extract(fit)

###############
# Let me try to estimate the thetas


Estimate_result_one_participant <- "
data {
    int N;     // questions
    int Y[1,N];  // score
    int<lower=2> K; // number of possible responses in each question
    int FM;
    int M;
    int other;
    
    // estimated parameters of the model
    vector<lower=0>[N] alpha; // questions
    ordered[K-1] c[N]; // thresholds    
    real b_o;
    real b_o_1;
    real b_f;
    real b_m;
    real b_f_1;
    real b_m_1;
}
parameters {
    real theta;
}
model {
    theta ~ normal(0.,1.); 
    for(n in 1:N){
          if(Y[1,n]<999){
               Y[1,n] ~ ordered_logistic(alpha[n]*theta,
                        c[n]);             
          }
    }
}
generated quantities {
   real q;
   q = inv_logit(b_o*other + b_o_1*theta*other
                                   + b_f*FM + b_m*M
                                   + b_f_1*FM*theta + b_m_1*M*theta);
}
"


# calculate for everybody

make_data_mean <- function(data,i,post){
  result <- list(N=data$N,
                 K=data$K,
                 Y=data$Y[i,],
                 FM=data$FM[i],
                 M=data$M[i],
                 other=data$other[i],
                 alpha=apply(post$alpha,2,mean),
                 c=apply(post$c,c(2,3),mean),
                 b_o=mean(post$b_o),
                 b_o_1=mean(post$b_o_1),
                 b_f=mean(post$b_f),
                 b_m=mean(post$b_m),
                 b_f_1=mean(post$b_f_1),
                 b_m_1=mean(post$b_m_1))
  return(result)
}


set.seed(117)

result <- list()

for(i in 1:25){
  dat <- make_data_mean(data,i,post)
  
  fit <- stan(model_code=Estimate_result_one_participant,
              data=dat,chains=1,iter=1500,warmup=500)  
  result[[i]] <- extract(fit)
}


thetas <- array(dim=c(1000,25))
q <- array(dim=c(1000,25))

for(i in 1:25){
  thetas[,i] <- result[[i]]$theta
  q[,i] <- result[[i]]$q
}

boxplot(thetas,outline=FALSE,col=ifelse(x$ED_dx[1:25]==1,"red","white"),
        xlab="Participants",ylab="Theta",main="BASE thetas")
boxplot(q,outline=FALSE,col=ifelse(x$ED_dx[1:25]==1,"red","white"),
        xlab="Participants",ylab="ED risk estimates",main="BASE ED predictions")

