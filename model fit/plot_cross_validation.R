

par(mfrow=c(1,2))

data <- read.csv("Cleaned_KUsurvey_di.csv")

# scoff

load("scoff_prob_no_DIF_cross_validation.Rdata")

prob1 <- prob

load("scoff_prob_no_DIF_cross_validation_2.Rdata")

prob2 <- prob

plot(prob2,prob1,
     xlab="ED probability - estimated from 2022 data",
     ylab="ED probability - estimated from 2021 data",
     main="SCOFF: ED probability of 2022 students",
     col=ifelse(data$gender_s21==1,"red",
                ifelse(data$gender_s21==2,"darkgreen","blue")),
     pch=ifelse(data$gender_s21==1,1,
                ifelse(data$gender_s21==2,2,3)))
abline(a=0,b=1)


# base

load("base_prob_no_DIF_cross_validation.Rdata")

prob1 <- prob

load("base_prob_no_DIF_cross_validation_2.Rdata")

prob2 <- prob

plot(prob2,prob1,
     xlab="ED probability - estimated from 2022 data",
     ylab="ED probability - estimated from 2021 data",
     main="BASE: ED probability of 2022 students",
     col=ifelse(data$gender_s21==1,"red",
                ifelse(data$gender_s21==2,"darkgreen","blue")),
     pch=ifelse(data$gender_s21==1,1,
                ifelse(data$gender_s21==2,2,3)))
abline(a=0,b=1)

