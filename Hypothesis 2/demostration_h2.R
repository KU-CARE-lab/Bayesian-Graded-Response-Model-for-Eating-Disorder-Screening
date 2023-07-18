
x <- read.csv("Cleaned_KUsurvey_di.csv")

x$individual <- 1:1397

x <- subset(x,scoff_total==2)

sub1 <- subset(x,scoff_02_s21==1 & scoff_05_s21==1 & gender_s21==2)

dim(sub1)

mean(sub1$ED_dx)

sub2 <- subset(x,scoff_03_s21==1 & scoff_04_s21==1 & gender_s21==2)

dim(sub2)

mean(sub2$ED_dx)

load("/Users/chenyiyang/Desktop/CARE2/GRM_final/hypothesis 1/scoff_theta_all.Rdata")

mean(thetas[sub1$individual])

mean(thetas[sub1$individual])