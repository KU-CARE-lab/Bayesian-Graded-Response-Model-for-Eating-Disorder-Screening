
x <- read.csv("Cleaned_KUsurvey_di.csv")

x$individual <- 1:1397

x <- subset(x,scoff_total==2)

sub1 <- subset(x,scoff_02_s21==1 & scoff_05_s21==1 & gender_s21==2)

sub2 <- subset(x,scoff_03_s21==1 & scoff_04_s21==1 & gender_s21==2)

