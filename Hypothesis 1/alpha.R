
### 1) DATA CLEANING
# Import data and remove "s21" from column names

x <- read.csv("Cleaned_KUsurvey_di.csv")

x1 <- x[,grep("scoff",names(x))][1:5]

require(ltm)

cronbach.alpha(x1,na.rm=T)

x2 <- x[,grep("base",names(x))][1:10]

require(ltm)

cronbach.alpha(x2,na.rm=T)