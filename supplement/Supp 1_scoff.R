
# scoff

require(rstan)

load("result_scoff_10.Rdata")

post <- extract(fit,permuted=F)

sd_lower <- rep(0,dim(post)[3])
sd_upper <- rep(0,dim(post)[3])

for(i in 1:dim(post)[3]){
  param <- post[,,i]
  lower <- apply(param,2,function(x){quantile(x,0.05)})
  upper <- apply(param,2,function(x){quantile(x,0.95)})
  sd_lower[i] <- sd(lower)
  sd_upper[i] <- sd(upper)
}



# base

require(rstan)

load("result_base_10.Rdata")

post <- extract(fit,permuted=F)
post1 <- extract(fit)

sd_lower <- rep(0,dim(post)[3])
sd_upper <- rep(0,dim(post)[3])

for(i in 1:dim(post)[3]){
  param <- post[,,i]
  lower <- apply(param,2,function(x){quantile(x,0.05)})
  upper <- apply(param,2,function(x){quantile(x,0.95)})
  sd_lower[i] <- sd(lower)
  sd_upper[i] <- sd(upper)
  if(sd_upper[i]>0.05){
    print(i)
    print(sd_upper[i])
    }
}


