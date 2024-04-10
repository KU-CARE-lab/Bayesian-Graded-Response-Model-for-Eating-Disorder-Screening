
# This file creates simulated data

require(rstan)

Graded_test <- "
data {
    int I;     // participants
    int N;     // questions
    int Y[I,N];  // score
    int<lower=2> K; // number of possible responses in each question
    int FM[I];
    int M[I];
    int Z[I];
    int other[I];
}

parameters {

    vector<lower=0>[N] alpha; // questions
    ordered[K-1] c[N]; // thresholds
    real c_0;
    real<lower=0> sd_c_0;
    
    vector[I] theta; // people
    
    real b_o;
    real b_o_1;
    real b_f;
    real b_m;
    real b_f_1;
    real b_m_1;
}

model {

    c_0 ~ normal(0.,1.);
    sd_c_0 ~ gamma(1.,1.); 
    
    b_o ~ normal(0.,1.);
    b_o_1 ~ normal(0.,1.);    

    b_f ~ normal(0.,1.);
    b_m ~ normal(0.,1.); 
    
    b_f_1 ~ normal(0.,1.);
    b_m_1 ~ normal(0.,1.); 
    
    for(i in 1:I){
        theta[i] ~ normal(0.,1.);
    }
    for(n in 1:N){
        alpha[n] ~ gamma(1.,1.);
        for(k in 1:(K-1)){
            c[n,k] ~ normal(c_0,sd_c_0);
        }
    }
    for(i in 1:I){
        for(n in 1:N){
            if(Y[i,n]<999){
               Y[i,n] ~ ordered_logistic(alpha[n]*theta[i],
                        c[n]);             
            }
        }
        Z[i] ~ bernoulli_logit(b_o*other[i] + b_o_1*theta[i]*other[i]
                                   + b_f*FM[i] + b_m*M[i]
                                   + b_f_1*FM[i]*theta[i] + b_m_1*M[i]*theta[i]);
    }
}
"

################################
# functions

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

graded_response_simulation <- function(b_o,
                           b_o_1,
                           b_f,
                           b_f_1,
                           b_m,
                           b_m_1,
                           I_male,
                           I_female,
                           I_other,
                           N,
                           K,
                           alpha,
                           c,
                           theta){
  
  I <- I_male + I_female + I_other
  Y <- array(dim=c(I,N))    # responses
  
  Z <- rep(0,I)
  
  # generate responses based on the ordered logistic distribution
  
  other <- c(rep(1,I_other),rep(0,I_male),rep(0,I_female))
  M <- c(rep(0,I_other),rep(1,I_male),rep(0,I_female))
  FM <- c(rep(0,I_other),rep(0,I_male),rep(1,I_female))
  
  for(i in 1:I){
    for(n in 1:N){
      eta <- alpha[n]*theta[i]
      Y[i,n] <- ordered_logistic_random(K,eta,c[n,])
    }
    eta <- (b_o*other[i] + b_o_1*theta[i]*other[i] 
            + b_f*FM[i] + b_m*M[i]
            + b_f_1*FM[i]*theta[i] + b_m_1*M[i]*theta[i])
    Z[i] <- rbinom(1,1,inv_logit(eta))
  }
  
  data <- list(I=I,
               N=N,
               K=K,
               FM=FM,
               M=M,
               Z=Z,
               theta.t=theta,
               alpha.t=alpha,
               c.t=c,
               b_o.t=b_o,
               b_o_1.t=b_o_1,
               b_f.t=b_f,
               b_m.t=b_m,
               b_f_1.t=b_f_1,
               b_m_1.t=b_m_1,
               Y=Y)
  fit <- stan(model_code=Graded_test,data=data,chains=1,iter=3500,warmup=500)
  
  post <- extract(fit)
  
  result <- list(I=I,
                 N=N,
                 K=K,
                 FM=FM,
                 M=M,
                 theta.t=theta,
                 alpha.t=alpha,
                 c.t=c,
                 b_o.t=b_o,
                 b_o_1.t=b_o_1,
                 b_f.t=b_f,
                 b_m.t=b_m,
                 b_f_1.t=b_f_1,
                 b_m_1.t=b_m_1,
                 theta=post$theta,
                 alpha=post$alpha,
                 c=post$c,
                 b_o=post$b_o,
                 b_o_1=post$b_o_1,
                 b_f=post$b_f,
                 b_m=post$b_m,
                 b_f_1=post$b_f_1,
                 b_m_1=post$b_m_1,
                 Y=Y,
                 Z=Z)
  
  return(result)
}


###################################

###################################

# parameters
set.seed(117)

load("result_base.Rdata")

post <- extract(fit)

I_male <- 60
I_female <- 90
I_other <- 30

I <- 180

numbers <- sample(1:5000,50)

result <- list()

for(k in 1:50){
  i <- numbers[k]
  result[[k]] <- graded_response_simulation(post$b_o[i],
                                       post$b_o_1[i],
                                       post$b_f[i],
                                       post$b_f_1[i],
                                       post$b_m[i],
                                       post$b_m_1[i],
                                       I_male,
                                       I_female,
                                       I_other,
                                       N=10,
                                       K=5,
                                       alpha=post$alpha[i,],
                                       c=array(post$c[i,,],dim=c(10,4)),
                                       theta=sample(post$theta[i,],I))
  print(k)
  
}


save(result,file="base_simulation_results.Rdata")
