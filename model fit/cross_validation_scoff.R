

x <- read.csv("Cleaned_KUsurvey_di.csv")

x <- subset(x,id>3444)

x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)


# scoff

load("scoff_prob_no_DIF_cross_validation_2.Rdata")

prob1 <- prob

load("scoff_prob_no_DIF_cross_validation.Rdata")

prob2 <- prob

col <- ifelse(x$gender_s21==1,"red",ifelse(x$gender_s21==2,"dark green","blue"))
type <- ifelse(x$gender_s21==1,1,ifelse(x$gender_s21==2,2,3))

par(mfrow=c(1,2))

plot(prob1,prob2,col=col,pch=type,xlim=c(0,1),ylim=c(0,1),
     xlab="ED probability - estimated from 2022 data",
     ylab="ED probability - estimated from 2021 data",
     main="SCOFF: ED probabilities of 2022 students")

abline(a=0,b=1)


# base

load("base_prob_no_DIF_cross_validation_2.Rdata")

prob1 <- prob

load("base_prob_no_DIF_cross_validation.Rdata")

prob2 <- prob

col <- ifelse(x$gender_s21==1,"red",ifelse(x$gender_s21==2,"dark green","blue"))
type <- ifelse(x$gender_s21==1,1,ifelse(x$gender_s21==2,2,3))

plot(prob1,prob2,col=col,pch=type,xlim=c(0,1),ylim=c(0,1),
     xlab="ED probability - estimated from 2022 data",
     ylab="ED probability - estimated from 2021 data",
     main="BASE: ED probabilities of 2022 students")

abline(a=0,b=1)





