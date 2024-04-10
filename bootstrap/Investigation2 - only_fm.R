
# scoff

par(mfrow=c(1,2))

x <- read.csv("Cleaned_KUsurvey.csv")

x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)


load("scoff_prob_no_DIF.Rdata")

x$prob <- prob

x <- subset(x,gender_s21<=2)

load("scoff_boot_thetas_p_only_f_m.Rdata")

cor.num <- rep(0,30)


for(i in 1:30){
  x$boot_prob <- boot_outcome[[i]]$p1
  cor.num[i] <- cor(x$boot_prob,x$prob)
  sub <- subset(x,gender_s21==2)
  cor.num[i] <- cor(sub$boot_prob,sub$prob)
}

mean(cor.num)
min(cor.num)
max(cor.num)


for(i in 1:1){
  x$boot_prob <- boot_outcome[[i]]$p1
  cor.num[i] <- cor(x$boot_prob,x$prob)
  sub <- x
#  sub <- subset(x,gender_s21==2)
  plot(sub$prob,sub$boot_prob,
       xlim=c(0,1),ylim=c(0,1),
       xlab="Full sample training",
       ylab="Bootstrapped sample training",
       main="SCOFF: ED risks",
       col=ifelse(x$gender_s21==1,"red","darkgreen"),
       pch=ifelse(x$gender_s21==1,1,2))
  abline(a=0,b=1,col="blue")
}



# scoff

x <- read.csv("Cleaned_KUsurvey.csv")

x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)


load("base_prob_no_DIF.Rdata")

x$prob <- prob

x <- subset(x,gender_s21<=2)

load("base_boot_thetas_p_only_f_m.Rdata")

cor.num <- rep(0,30)


for(i in 1:30){
  x$boot_prob <- boot_outcome[[i]]$p1
  cor.num[i] <- cor(x$boot_prob,x$prob)
  sub <- subset(x,gender_s21==2)
  cor.num[i] <- cor(sub$boot_prob,sub$prob)
}

mean(cor.num)
min(cor.num)
max(cor.num)


for(i in 1:1){
  x$boot_prob <- boot_outcome[[i]]$p1
  cor.num[i] <- cor(x$boot_prob,x$prob)
#  sub <- subset(x,gender_s21==2)
  sub <- x
  plot(sub$prob,sub$boot_prob,
       xlim=c(0,1),ylim=c(0,1),
       xlab="Full sample training",
       ylab="Bootstrapped sample training",
       main="BASE: ED risks",
       col=ifelse(x$gender_s21==1,"red","darkgreen"),
       pch=ifelse(x$gender_s21==1,1,2))
  abline(a=0,b=1,col="blue")
}


