

require(rstan)

load("result_scoff.Rdata")

post <- extract(fit)


############### Estimate the chances of having positive diagnosis

theta <- seq(-0.6,1,0.05)

theta0 <- c(-0.50,0.90)

est <- data.frame(array(dim=c(3*length(theta),5)))
names(est) <- c("Estimate","Gender","Theta","Upper","Lower")

inv_logit <- function(x){
  1/(1+exp(-x))
}

est[,3] <- c(theta,theta,theta)

for(i in 1:length(theta)){
  est[i,2] <- "Non-cisgender"
  est[i,1] <- mean(inv_logit(post$b_o +
                               post$b_o_1*theta[i]))
  est[i,4] <- quantile(inv_logit(post$b_o +
                                   post$b_o_1*theta[i]),probs=0.975)
  est[i,5] <- quantile(inv_logit(post$b_o +
                                   post$b_o_1*theta[i]),probs=0.025)
  est[i+length(theta),2] <- "Cisgender men"
  est[i+length(theta),1] <- mean(inv_logit(post$b_m +
                                             post$b_m_1*theta[i]))
  est[i+length(theta),4] <- quantile(inv_logit(post$b_m +
                                                 post$b_m_1*theta[i]),probs=0.975)
  est[i+length(theta),5] <- quantile(inv_logit(post$b_m +
                                                 post$b_m_1*theta[i]),probs=0.025)
  est[i+2*length(theta),2] <- "Cisgender women" 
  est[i+2*length(theta),1] <- mean(inv_logit(post$b_f +
                                               post$b_f_1*theta[i]))
  est[i+2*length(theta),4] <- quantile(inv_logit(post$b_f +
                                                   post$b_f_1*theta[i]),probs=0.975)
  est[i+2*length(theta),5] <- quantile(inv_logit(post$b_f +
                                                   post$b_f_1*theta[i]),probs=0.025)
}

require(ggplot2)

g1 <- ggplot(data=est,
       aes(x=Theta, y=Estimate, colour=Gender)) +
  geom_line()+
  labs(title = "SCOFF: Estimated probabilities of positive ED diagnosis") +
  labs(x="Severity of disordered eating (theta)",y="ED probability") +
  geom_ribbon(aes(x=Theta,ymin = Lower, ymax = Upper, fill=Gender),alpha=0.4, colour = NA) +
  geom_vline(xintercept= theta0[1],col="orange") +
  geom_vline(xintercept= theta0[2],col="orange") + ylim(0,1)






require(rstan)

load("result_base.Rdata")

post <- extract(fit)


############### Estimate the chances of having positive diagnosis

theta <- seq(-1.5,2,0.05)

theta0 <- c(-1.09,1.73)

est <- data.frame(array(dim=c(3*length(theta),5)))
names(est) <- c("Estimate","Gender","Theta","Upper","Lower")

inv_logit <- function(x){
  1/(1+exp(-x))
}

est[,3] <- c(theta,theta,theta)

for(i in 1:length(theta)){
  est[i,2] <- "Non-cisgender"
  est[i,1] <- mean(inv_logit(post$b_o +
                               post$b_o_1*theta[i]))
  est[i,4] <- quantile(inv_logit(post$b_o +
                                   post$b_o_1*theta[i]),probs=0.975)
  est[i,5] <- quantile(inv_logit(post$b_o +
                                   post$b_o_1*theta[i]),probs=0.025)
  est[i+length(theta),2] <- "Cisgender men"
  est[i+length(theta),1] <- mean(inv_logit(post$b_m +
                                             post$b_m_1*theta[i]))
  est[i+length(theta),4] <- quantile(inv_logit(post$b_m +
                                                 post$b_m_1*theta[i]),probs=0.975)
  est[i+length(theta),5] <- quantile(inv_logit(post$b_m +
                                                 post$b_m_1*theta[i]),probs=0.025)
  est[i+2*length(theta),2] <- "Cisgender women" 
  est[i+2*length(theta),1] <- mean(inv_logit(post$b_f +
                                               post$b_f_1*theta[i]))
  est[i+2*length(theta),4] <- quantile(inv_logit(post$b_f +
                                                   post$b_f_1*theta[i]),probs=0.975)
  est[i+2*length(theta),5] <- quantile(inv_logit(post$b_f +
                                                   post$b_f_1*theta[i]),probs=0.025)
}

require(ggplot2)

g2 <- ggplot(data=est,
       aes(x=Theta, y=Estimate, colour=Gender)) +
  geom_line()+
  labs(title = "BASE: Estimated probabilities of positive ED diagnosis") +
  labs(x="Severity of disordered eating (theta)",y="ED probability") +
  geom_ribbon(aes(x=Theta,ymin = Lower, ymax = Upper, fill=Gender),alpha=0.4, colour = NA) +
  geom_vline(xintercept= theta0[1],col="orange") +
  geom_vline(xintercept= theta0[2],col="orange") + ylim(0,1)


require(ggpubr)

ggarrange(g1,g2,nrow=1,ncol=2)
