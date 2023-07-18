

require(rstan)

par(mfrow=c(1,2))


load("result_scoff.Rdata")

post <- extract(fit)

# item and threshold parameters

boxplot(post$alpha,outline=F,
        main="SCOFF discrimination",xlab="SCOFF questions",ylab="Posterior estimates")



load("result_base.Rdata")

post <- extract(fit)

# item and threshold parameters

boxplot(post$alpha,outline=F,
        main="BASE discrimination",xlab="BASE questions",ylab="Posterior estimates")

