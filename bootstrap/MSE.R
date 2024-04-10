

require(rstan)

x <- read.csv("Cleaned_KUsurvey_di.csv")

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

x <- subset(x,gender_s21<=2)

load("scoff_boot_thetas_p_only_f_m.Rdata")

# all
# men

mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$scoff_total>1,1,0)
  sub <- subset(x,gender_s21<=2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))

# men

mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$scoff_total>1,1,0)
  sub <- subset(x,gender_s21==1)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))


# women

mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$scoff_total>1,1,0)
  sub <- subset(x,gender_s21==2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))

# Non-cisgender

mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$scoff_total>1,1,0)
  sub <- subset(x,gender_s21>2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))



# BASE full

load("base_boot_thetas_p_only_f_m.Rdata")


mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$base_10item>11,1,0)
  sub <- subset(x,gender_s21<=2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))

# BASE Men


mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$base_10item>11,1,0)
  sub <- subset(x,gender_s21==1)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))


# BASE women


mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$base_10item>11,1,0)
  sub <- subset(x,gender_s21==2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))

# BASE non-cisgender

mse.g <- rep(0,30)
mse.r <- rep(0,30)

for(i in 1:30){
  x$prob <- boot_outcome[[i]]$p1
  x$ed.r <- ifelse(x$base_10item>11,1,0)
  sub <- subset(x,gender_s21>2)
  mse.g[i] <- mean((sub$prob-sub$ED_dx)^2)
  mse.r[i] <- mean((sub$ed.r-sub$ED_dx)^2)
}

a <- c(mean(mse.g),sd(mse.g),min(mse.g),max(mse.g),mean(mse.r))

print(paste0(round(a,digits=3),collapse=" & "))




