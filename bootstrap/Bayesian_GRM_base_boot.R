
### 1) DATA CLEANING
# Import data and remove "s21" from column names



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

    c_0 ~ normal(0.,5.);
    sd_c_0 ~ cauchy(0.,5.); 
    
    b_o ~ normal(0.,1.);
    b_o_1 ~ normal(0.,1.);    

    b_f ~ normal(0.,1.);
    b_m ~ normal(0.,1.); 
    
    b_f_1 ~ normal(0.,1.);
    b_m_1 ~ normal(0.,1.); 

    for(i in 1:I){
        theta[i] ~ normal(0.,0.5);
    }
    for(n in 1:N){
        alpha[n] ~ cauchy(0.,5.);
        for(k in 1:(K-1)){
            c[n,k] ~ normal(c_0,sd_c_0);
        }
    }
    for(i in 1:I){
        for(n in 1:N){
            if(Y[i,n]<999){
               Y[i,n] ~ ordered_logistic((alpha[n])*theta[i],
                        c[n]);             
            }
        }
        Z[i] ~ bernoulli_logit(b_o*other[i] + b_o_1*theta[i]*other[i]
                                + b_f*FM[i] + b_m*M[i]
                                   + b_f_1*FM[i]*theta[i] + b_m_1*M[i]*theta[i]
                                     );
    }
}
"

set.seed(117)

seeds <- sample(1:10000,30,replace=F)

base_boot <- list()

for(k in 1:3){
  
  set.seed(seeds[k])
  
  x <- read.csv("Cleaned_KUsurvey_di.csv")
  
  x1 <- subset(x,gender_s21==1)
  
  keep1 <- sample(x1$id,90,replace=T)
  
  x2 <- subset(x,gender_s21==2)
  
  keep2 <- sample(x2$id,90,replace=T)
  
  x <- x[match(c(keep1,keep2),x$id),]
  
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
  
  fit <- stan(model_code=Graded_test,data=data,chains=1,iter=2500,warmup=500)
  
  post <- extract(fit)
  
  base_boot[[k]] <- list(x=x,
                          alpha=post$alpha,
                          c=post$c,
                          b_o=post$b_o,
                          b_o_1=post$b_o_1,
                          b_f=post$b_f,
                          b_f_1=post$b_f_1,
                          b_m=post$b_m,
                          b_m_1=post$b_m_1)
  
  print(k)
  
}



save(base_boot,file="result_base_bootstrap_180_only_m_f.Rdata")
