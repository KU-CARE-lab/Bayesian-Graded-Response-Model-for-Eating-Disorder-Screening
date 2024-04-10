
### 1) DATA CLEANING
# Import data and remove "s21" from column names

x <- read.csv("Cleaned_KUsurvey_di.csv")

x <- subset(x,!is.na(ED_dx))

#x <- subset(x,Year==2022)

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

load("scoff_prob_no_DIF.Rdata")

x$scoff_prob <- prob

x$scoff_ed <- ifelse(x$scoff_total>1,1,0)

# full

mean((x$scoff_ed - x$ED_dx)^2)

mean((x$scoff_prob - x$ED_dx)^2)

# men

sub <- subset(x,gender_s21==1)

mean((sub$scoff_ed - sub$ED_dx)^2)

mean((sub$scoff_prob - sub$ED_dx)^2)

# women

sub <- subset(x,gender_s21==2)

mean((sub$scoff_ed - sub$ED_dx)^2)

mean((sub$scoff_prob - sub$ED_dx)^2)

# non-cisgender

sub <- subset(x,gender_s21>2)

mean((sub$scoff_ed - sub$ED_dx)^2)

mean((sub$scoff_prob - sub$ED_dx)^2)


load("base_prob_no_DIF.Rdata")

x$base_prob <- prob

x$base_ed <- ifelse(x$base_10item>11,1,0)

# full

mean((x$base_ed - x$ED_dx)^2)

mean((x$base_prob - x$ED_dx)^2)

# men

sub <- subset(x,gender_s21==1)

mean((sub$base_ed - sub$ED_dx)^2)

mean((sub$base_prob - sub$ED_dx)^2)

# women

sub <- subset(x,gender_s21==2)

mean((sub$base_ed - sub$ED_dx)^2)

mean((sub$base_prob - sub$ED_dx)^2)

# non-cisgender

sub <- subset(x,gender_s21>2)

mean((sub$base_ed - sub$ED_dx)^2)

mean((sub$base_prob - sub$ED_dx)^2)




# ranges


sub <- subset(x,scoff_prob<0.3)
dim(sub)
mean(sub$ED_dx)
mean(sub$scoff_ed)
mean(sub$scoff_prob)

sub <- subset(x,scoff_prob>0.3 & scoff_prob<0.7)
dim(sub)
mean(sub$ED_dx)
mean(sub$scoff_ed)
mean(sub$scoff_prob)

sub <- subset(x,scoff_prob>0.7)
dim(sub)
mean(sub$ED_dx)
mean(sub$scoff_ed)
mean(sub$scoff_prob)


sub <- subset(x,base_prob<0.3)
dim(sub)
mean(sub$ED_dx)
mean(sub$base_ed)
mean(sub$base_prob)

sub <- subset(x,base_prob>0.3 & base_prob<0.7)
dim(sub)
mean(sub$ED_dx)
mean(sub$base_ed)
mean(sub$base_prob)

sub <- subset(x,base_prob>0.7)
dim(sub)
mean(sub$ED_dx)
mean(sub$base_ed)
mean(sub$base_prob)


