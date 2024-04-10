

###################################################
########## BASE ###################################


x <- read.csv("Cleaned_KUsurvey.csv")

x <- subset(x,!is.na(ED_dx))

x$gender_s21 <- ifelse(is.na(x$gender_s21),8,x$gender_s21)

load("base_prob_no_DIF.Rdata")

x$prob <- prob

# Low ED prob, high score

sub <- subset(x,prob<0.3 & base_10item>11)

dim(sub)

table(sub$gender_s21)

mean(sub$ED_dx)

mean(sub$prob)

a <- apply(sub[,grep("base",names(sub))],2,function(x){mean(x,na.rm=T)})

b <- apply(x[,grep("base",names(sub))],2,function(x){mean(x,na.rm=T)})

sub1 <- subset(x,ED_dx==0)

c <- apply(sub1[,grep("base",names(sub))],2,function(x){mean(x,na.rm=T)})

# 1, 2, 3, 7

require(ggplot2)

x$group <- "All participants"
sub$group <- "High aggregated score, low ED prob"

data <- rbind(x,sub)


#Let's calculate the average value for each condition and each specie with the *aggregate* function
bilan <- aggregate(cbind(data$s21_epsi_base_1,
                         data$s21_epsi_base_2,
                         data$s21_epsi_base_3,
                         data$s21_epsi_base_4,
                         data$s21_epsi_base_5,
                         data$s21_epsi_base_6,
                         data$s21_epsi_base_7,
                         data$s21_epsi_base_8,
                         data$s21_epsi_base_9,
                         data$s21_epsi_base_10) ~ group , data=data , mean)
rownames(bilan) <- bilan[,1]
bilan <- as.matrix(bilan[,-1])

#Plot boundaries
lim <- 1.2*max(bilan)

#A function to add arrows on the chart
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
}

#Then I calculate the standard deviation for each specie and condition :
stdev <- aggregate(cbind(data$s21_epsi_base_1,
                         data$s21_epsi_base_2,
                         data$s21_epsi_base_3,
                         data$s21_epsi_base_4,
                         data$s21_epsi_base_5,
                         data$s21_epsi_base_6,
                         data$s21_epsi_base_7,
                         data$s21_epsi_base_8,
                         data$s21_epsi_base_9,
                         data$s21_epsi_base_10) ~ group, data=data , sd)
rownames(stdev) <- stdev[,1]
stdev <- as.matrix(stdev[,-1]) * 1.96 / 10

#I am ready to add the error bar on the plot using my "error bar" function !
ze_barplot <- barplot(bilan , beside=T , legend.text=T,col=c("blue" , "skyblue") , 
                      ylim=c(0,lim) , ylab="Item score",xlab="BASE questions",
                      main="Average scores on BASE items",names.arg=1:10)
error.bar(ze_barplot,bilan, stdev)


table(sub$eddsdx)



# don't have this condition
# sub <- subset(x,prob>0.7 & base_10item<=11)

