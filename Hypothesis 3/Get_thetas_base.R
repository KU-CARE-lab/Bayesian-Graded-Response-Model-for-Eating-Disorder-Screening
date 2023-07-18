

require(rstan)

load("result_base_bootstrap_180.Rdata")

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
    theta ~ normal(0.,0.5); 
    for(n in 1:N){
          if(Y[1,n]<999){
               Y[1,n] ~ ordered_logistic(alpha[n]*theta,
                        c[n]);             
          }
    }
}
generated quantities {
   real p;
   p = (b_o*other + b_o_1*theta*other
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


boot_outcome <- list()

for(k in 1:30){
  
  x <- read.csv("Cleaned_KUsurvey_di.csv")
  
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
  
  thetas <- rep(0,data$I)
  p1 <- rep(0,data$I)
  p2 <- rep(0,data$I)
  
  for(i in 1:data$I){
    dat <- make_data_mean(data,i,base_boot[[k]])
    fit <- stan(model_code=Estimate_result_one_participant,
                data=dat,chains=1,iter=1500,warmup=500) 
    
    res <- extract(fit)
    thetas[i] <- mean(res$theta)
    p1[i] <- mean(1/(1+exp(-res$p)))
    p2[i] <- 1/(1+exp(-mean(res$p)))
  }
  boot_outcome[[k]] <- list(thetas=thetas,
                            p1=p1,
                            p2=p2)
}

save(boot_outcome,file="base_boot_thetas_p.Rdata")








