
### 1) DATA CLEANING
# Import data and remove "s21" from column names

x <- read.csv("Cleaned_KUsurvey_di.csv")



x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

data <- list()
data$I <- dim(x)[1]
data$N <- 5   # SCOFF
data$Y <- x[,grep("scoff",names(x))[1:5]]
data$K <- 2
data$Z <- x$ED_dx
data$FM <- ifelse(x$gender_s21==2,1,0)
data$M <- ifelse(x$gender_s21==1,1,0)
data$other <- ifelse(x$gender_s21>2,1,0)

# NA treatment

for(i in 1:dim(data$Y)[2]){
  data$Y[,i] <- ifelse(is.na(data$Y[,i]),999,data$Y[,i]+1)
}


require(rstan)

load("result_scoff_no_DIF.Rdata")

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
   q = (b_o*other + b_o_1*theta*other
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

prob <- rep(0,data$I)
thetas <- rep(0,data$I)

for(i in 1:data$I){
  dat <- make_data_mean(data,i,post)
  
  fit <- stan(model_code=Estimate_result_one_participant,
              data=dat,chains=1,iter=1500,warmup=500)  
  result <- extract(fit)
  prob[i] <- mean(1/(1+exp(-result$q)))
  thetas[i] <- mean(result$theta)
  print(i)
}

save(prob,file="scoff_prob_no_DIF.Rdata")
save(thetas,file="scoff_theta_no_DIF.Rdata")

