setwd("C:/Users/Rohan/Desktop/data science/great lakes/books, Q&A, reference material/problem sets/project -CAPStone/6. Coronory Heart Risk Study")
datahrt <-read.csv("Coronary_heart_risk_study.csv")
datahrt.backup<-datahrt #creating a backup of data

##############################   EDA   ###############################
######################################################################
######################################################################

#Eyeballing the data
head(datahrt)
summary(datahrt)
str(datahrt)
rpivotTable::rpivotTable(datahrt)

#checking imbalance in data
sum(datahrt$TenYearCHD==1)/nrow(datahrt)
#data is not imbalanced as 15% of data has CHD
sum(datahrt$male)

#Reclassifying nominal variables as factor 
class(datahrt$TenYearCHD)
head(datahrt$education)
datahrt$TenYearCHD=as.factor(datahrt$TenYearCHD)
datahrt$Gender=as.factor(datahrt$male)
datahrt$currentSmoker=as.factor(datahrt$currentSmoker)
datahrt$education=as.factor(datahrt$education)
datahrt$BPMeds=as.factor(datahrt$BPMeds)
datahrt$prevalentHyp=as.factor(datahrt$prevalentHyp)
datahrt$prevalentStroke=as.factor(datahrt$prevalentStroke)
datahrt$diabetes=as.factor(datahrt$diabetes)
datahrt<-datahrt[,-1]

####### filter out NAs
library(tidyverse) 
datahrt.woNA<-datahrt %>% filter(complete.cases(.))

datahrt.factor<-datahrt.woNA[,c(2,3,15,16,5:8)] ### without missing values
datahrt.numeric<-datahrt.woNA[,c(1,4,9:14)]
boxplot(datahrt.numeric, horizontal = T)

#checking Missing values and pattern

library(mice) 
md.pattern(datahrt, rotate.names = TRUE) 

install.packages("DataExplorer") 
library("DataExplorer") 
library(tidyverse) 
plot_str(datahrt,type="d")

library(tidyverse)
library(caret)
library(reshape2)
library(RColorBrewer)




Hmisc::describe(datahrt)# gives missing values and distinct values 
psych::describe(datahrt) 


####################################################################
####################################################################
####################################################################

#Univariate analysis- distribution
hist(datahrt$cigsPerDay, main = "Histogram: No of Cigs/Day",xlab = "data bins No of Cigs/Day", col = c( "Orange","White","Blue", "Green") )
hist(datahrt$age, main = "Histogram: AGE",xlab = "data bins AGE", col = c( "Orange","White","Blue", "Green") )
hist(datahrt$education, main = "Histogram: AGE",xlab = "data bins AGE", col = c( "Orange","White","Blue", "Green") )
hist(datahrt$totChol,xlab = "data bins AGE", col = c( "Orange","White","Blue", "Green") ), main="Histogram: Total Cholesterol")
hist(datahrt$sysBP,xlab = "data bins Systolic BP", col = c( "Orange","White","Blue", "Green") , main="Histogram: Systolic BP")
hist(datahrt$diaBP,xlab = "data bins Diast BP", col = c( "Orange","White","Blue", "Green") , main="Histogram: Diastolic BP")
hist(datahrt$BMI,xlab = "data bins BMI", col = c( "Orange","White","Blue", "Green") , main="Histogram: BMI")
hist(datahrt$heartRate,xlab = "data bins Heart Rate", col = c( "Orange","White","Blue", "Green") , main="Histogram: Heart rate")
hist(datahrt$glucose,xlab = "data bins Glucose", col = c( "Orange","White","Blue", "Green") , main="Histogram: Glucose")


boxplot(datahrt$TenYearCHD~.,data= datahrt, horizontal = TRUE, col = "Dark Blue", main="Ten year CHD relation with Variables")
?complete.cases

boxplot(datahrt$cigsPerDay,datahrt$TenYearCHD[datahrt$TenYearCHD=="2"], horizontal = TRUE)
?boxplot

library(ggplot2)

gg <- ggplot(datahrt.woNA, aes(x=datahrt.woNA$age, y=datahrt.woNA$cigsPerDay, 
                               color=datahrt.woNA$TenYearCHD)) + geom_point() + labs(title="Scatterplot", x="Age", y="Cigs/Day")  # add axis lables and plot title.
print(gg)
gg1 <- ggplot(datahrt.woNA, aes(x=datahrt.woNA$age, y=datahrt.woNA$sysBP, 
                               color=datahrt.woNA$TenYearCHD)) + geom_point() + labs(title="Age Vs SysBP Vs 10 yr CHD Risk", x="Age", y="SysBP")  # add axis lables and plot title.
print(gg1)

gg2 <- ggplot(datahrt.woNA, aes(x=datahrt.woNA$age, y=datahrt.woNA$cigsPerDay, 
                               color=datahrt.woNA$TenYearCHD)) + geom_point() + labs(title="Age Vs DiasBP Risk Vs 10 yr CHD ", x="Age", y="DiasBP")  # add axis lables and plot title.
print(gg2)

gg3 <- ggplot(datahrt.woNA, aes(x=datahrt.woNA$age, y=datahrt.woNA$BMI, 
                                color=datahrt.woNA$TenYearCHD)) + geom_point() + labs(title="Age Vs BMI Risk Vs 10 yr CHD ", x="Age", y="BMI")  # add axis lables and plot title.
print(gg3)

head(datahrt$TenYearCHD)
datahrt$TenYearCHD<- as.factor(datahrt$TenYearCHD)
boxplot(datahrt$cigsPerDay~datahrt$TenYearCHD, horizontal=TRUE, col = "Orange" ,main= "Cigs/Day", ylab="Ten yrCHD")
boxplot(datahrt$totChol~datahrt$TenYearCHD, horizontal=TRUE, col = "White",main= "Total Cholesterol", ylab="Ten yrCHD")
boxplot(datahrt$sysBP~datahrt$TenYearCHD, horizontal=TRUE, col = "Blue",main= "Systolic Blood Pressure", ylab="Ten yrCHD")
boxplot(datahrt$diaBP~datahrt$TenYearCHD, horizontal=TRUE, col = "Dark Green",main= "Diastolic Blood Pressure", ylab="Ten yrCHD")
boxplot(datahrt$BMI ~datahrt$TenYearCHD, horizontal=TRUE, col = "Orange",main= "Body Mass Index", ylab="Ten yrCHD")
boxplot(datahrt$heartRate~datahrt$TenYearCHD, horizontal=TRUE, col = "White",main= "Heart rate", ylab="Ten yrCHD")
boxplot(datahrt$glucose~datahrt$TenYearCHD, horizontal=TRUE, col = "Green",main= "Glucose Levels", ylab="Ten yrCHD")

#Cross tables
table(datahrt$male, datahrt$TenYearCHD)
 head(datahrt.factor)
#chi Sq test
chisq.test(datahrt$TenYearCHD, datahrt$Gender)
chisq.test(datahrt$TenYearCHD, datahrt$age)
chisq.test(datahrt$TenYearCHD, datahrt$education)
chisq.test(datahrt$TenYearCHD, datahrt$currentSmoker)
chisq.test(datahrt$TenYearCHD, datahrt$BPMeds)
chisq.test(datahrt$TenYearCHD, datahrt$prevalentStroke)
chisq.test(datahrt$TenYearCHD, datahrt$prevalentHyp)
chisq.test(datahrt$TenYearCHD, datahrt$diabetes)
chisq.test(datahrt$TenYearCHD, datahrt$age)

ChiSqStat<-NA
for (i in 1: (ncol(datahrt.factor))) {
  statistic<-data.frame(
    "ROW" = colnames(datahrt.factor[3]),
    "Column"= colnames(datahrt.factor[i]),
     "ChiSq"=chisq.test(datahrt.factor[[3]], datahrt.factor[[i]])$statistic,
     "p.value" = chisq.test(datahrt.factor[[3]], datahrt.factor[[i]])$p.value)
  ChiSqStat <- rbind(ChiSqStat, statistic)
}
ChiSqStat <- data.table::data.table(ChiSqStat)
ChiSqStat
str(datahrt.factor)

ChiSqStat2<-NA
for (i in 1: (ncol(datahrt.factor))) {
  statistic<-data.frame(
    "ROW" = colnames(datahrt.factor[8]),
    "Column"= colnames(datahrt.factor[i]),
    "ChiSq"=chisq.test(datahrt.factor[[8]], datahrt.factor[[i]])$statistic,
    "p.value" = chisq.test(datahrt.factor[[8]], datahrt.factor[[i]])$p.value)
  ChiSqStat2 <- rbind(ChiSqStat2, statistic)
}
ChiSqStat2 <- data.table::data.table(ChiSqStat2)
ChiSqStat2


chisq.test(datahrt.woNA$currentSmoker,datahrt.woNA$cigsPerDay)
chisq.test(datahrt.woNA$glucose,datahrt.woNA$diabetes)
chisq.test(datahrt.woNA$prevalentHyp,datahrt.woNA$sysBP)

###########running a Corelation
library(corrplot)
m<-cor(datahrt.numeric)
corrplot(m,method = "number",type = "upper", tweak=1)


#######running multiple plots
par(mfrow=c(1,1))
####AGE Vs others
qqplot(datahrt.woNA$age,datahrt.woNA$cigsPerDay,xlab = "Age", ylab = "Cigs /day", main="Trend of Cigs/Day with increasing age")
qqplot(datahrt.woNA$age,datahrt.woNA$totChol, xlab = "Age", ylab = "Cholesterol")
qqplot(datahrt.woNA$age,datahrt.woNA$sysBP, xlab = "Age", ylab = "Systolic BP")
qqplot(datahrt.woNA$age,datahrt.woNA$diaBP, xlab = "Age", ylab = "Diastolic BP")
qqplot(datahrt.woNA$age,datahrt.woNA$BMI, xlab = "Age", ylab = "Cholesterol")
qqplot(datahrt.woNA$age,datahrt.woNA$heartRate , xlab = "Age", ylab = "Cholesterol")
qqplot(datahrt.woNA$age,datahrt.woNA$glucose , xlab = "Age", ylab = "Cholesterol")
qqplot(datahrt.woNA$age,datahrt.woNA$TenYearCHD, xlab = "Age", ylab = "Cholesterol")

ggpairs(datahrt.woNA[, c("age",
                         "currentSmoker")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "cigsPerDay")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "BPMeds")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "prevalentStroke")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "prevalentHyp")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "diabetes")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "prevalentHyp")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "totChol")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "sysBP")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "diaBP")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "BMI")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "heartRate")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "glucose")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("age",
                         "Gender")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))

####Education Vs others
ggpairs(datahrt.woNA[, c("education",
                         "currentSmoker")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("education",
                         "cigsPerDay")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))

ggpairs(datahrt.woNA[, c("education",
                         "BPMeds")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("education",
                         "prevalentHyp")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("education",
                         "sysBP")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("education",
                         "diaBP")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[, c("education",
                         "prevalentHyp")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))
ggpairs(datahrt.woNA[,c("education",
                        "diabetes")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))
ggpairs(datahrt.woNA[,c("education",
                        "Gender")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))
####Education
plot(datahrt.woNA$education, datahrt.woNA$TenYearCHD)
plot(datahrt.woNA$education, datahrt.woNA$cigsPerDay)

####Cigs/day Vs others
####BPmeds Vs others
ggpairs(datahrt.woNA[,c("BPMeds",
                        "BMI")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))
ggpairs(datahrt.woNA[,c("BPMeds",
                        "Gender")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))

####prevalentHyp Vs others
ggpairs(datahrt.woNA[,c("prevalentHyp",
                        "sysBP")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))
ggpairs(datahrt.woNA[,c("prevalentHyp",
                        "Gender")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))

ggpairs(datahrt.woNA[,c("diabetes",
                        "glucose")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))
ggpairs(datahrt.woNA[,c("totChol",
                        "Gender")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))

ggpairs(datahrt.woNA[,c("sysBP",
                        "Gender")],ggplot2::aes(colour=as.factor((datahrt.woNA$TenYearCHD))))


library(GGally)
ggpairs(datahrt.woNA[, c("currentSmoker",
                   "cigsPerDay")],ggplot2::aes(colour = as.factor(datahrt.woNA$TenYearCHD)))

ggpairs(datahrt.woNA[, c("age","totChol",
                         "sysBP","diaBP",
                         "BMI","heartRate","glucose")],ggplot2::aes(colour = as.factor(LRdata$TenYearCHD)))

############################################
#checking for multicollinearity among numeric variables
library(car)

TenYearCHD<-as.integer(datahrt.woNA$TenYearCHD)
datahrt.numeric2<-cbind(datahrt.numeric, TenYearCHD)
str(datahrt.numeric2)

dummylm<-lm(datahrt.numeric$age~datahrt.numeric$cigsPerDay)
dummylm<- lm(datahrt.numeric2$TenYearCHD ~.,data = datahrt.numeric2)
summary(dummylm)
vif(dummylm,)

str(datahrt.woNA)

##############################   Variable transform   ################
######################################################################
######################################################################

############treating outliers with capping technique####
boxplot(datahrt.woNA, horizontal = TRUE, cex.axis=.5)
####glucose
datahrt.woNA2<-datahrt.woNA#saving backup
datahrt.woNA3<-datahrt.woNA#file for outlier treated datA

datahrt.woNA3$glucose[which(datahrt.woNA3$glucose 
                            >quantile(datahrt.woNA3$glucose, c(0.95)))]= 
  quantile(datahrt.woNA3$glucose, c(0.95)) 

datahrt.woNA3$glucose[which(datahrt.woNA3$glucose 
                            <quantile(datahrt.woNA3$glucose, c(0.05)))]= 
  quantile(datahrt.woNA3$glucose, c(0.05)) 

boxplot(datahrt.woNA3$heartRate, horizontal = TRUE)

#####heartrate
datahrt.woNA3$heartRate[which(datahrt.woNA3$heartRate
                              >quantile(datahrt.woNA3$heartRate, c(0.95)))]=
  quantile(datahrt.woNA3$heartRate, c(0.95))
datahrt.woNA3$heartRate[which(datahrt.woNA3$heartRate
                              <quantile(datahrt.woNA3$heartRate, c(0.05)))]=
  quantile(datahrt.woNA3$heartRate, c(0.05))

#####BMI
datahrt.woNA3$BMI[which(datahrt.woNA3$BMI
                        >quantile(datahrt.woNA3$BMI, c(0.95)))]=
  quantile(datahrt.woNA3$BMI, c(0.95))
datahrt.woNA3$BMI[which(datahrt.woNA3$BMI
                        <quantile(datahrt.woNA3$BMI, c(0.05)))]=
  quantile(datahrt.woNA3$BMI, c(0.05))

#####sysBP
datahrt.woNA3$sysBP [which(datahrt.woNA3$sysBP
                           >quantile(datahrt.woNA3$sysBP, c(0.95)))]=
  quantile(datahrt.woNA3$sysBP, c(0.95))
datahrt.woNA3$sysBP[which(datahrt.woNA3$sysBP
                          <quantile(datahrt.woNA3$sysBP, c(0.05)))]=
  quantile(datahrt.woNA3$sysBP, c(0.05))

#####diaBP
datahrt.woNA3$diaBP[which(datahrt.woNA3$diaBP
                          >quantile(datahrt.woNA3$diaBP, c(0.95)))]=
  quantile(datahrt.woNA3$diaBP, c(0.95))
datahrt.woNA3$diaBP[which(datahrt.woNA3$diaBP
                          <quantile(datahrt.woNA3$diaBP, c(0.05)))]=
  quantile(datahrt.woNA3$diaBP, c(0.05))

#####totChol
datahrt.woNA3$totChol[which(datahrt.woNA3$totChol
                            >quantile(datahrt.woNA3$totChol, c(0.95)))]=
  quantile(datahrt.woNA3$totChol, c(0.95))

datahrt.woNA3$totChol[which(datahrt.woNA3$totChol
                            <quantile(datahrt.woNA3$totChol, c(0.05)))]=
  quantile(datahrt.woNA3$totChol, c(0.05))

#####cigs/day
datahrt.woNA3$cigsPerDay[which(datahrt.woNA3$cigsPerDay
                               >quantile(datahrt.woNA3$cigsPerDay, c(0.95)))]=
  quantile(datahrt.woNA3$cigsPerDay, c(0.95))
boxplot(datahrt.woNA3, horizontal = TRUE, cex.axis=.5)


str(datahrt.factor)
str(datahrt.woNA3)

str(datahrt.scaled.factor)



##########        scaling the data ####################

datahrt.scaled<-datahrt.woNA3
datahrt.scaled.factor<-datahrt.scaled[,c(2,3,15,16,5:8)]
datahrt.scaled.numeric<-datahrt.scaled[,c(1,4,9:14)]

datahrt.scaled$TenYearCHD=as.factor(datahrt.scaled$TenYearCHD)
datahrt.scaled$Gender=as.factor(datahrt.scaled$Gender)
datahrt.scaled$currentSmoker=as.factor(datahrt.scaled$currentSmoker)
datahrt.scaled$education=as.factor(datahrt.scaled$education)
datahrt.scaled$BPMeds=as.factor(datahrt.scaled$BPMeds)
datahrt.scaled$prevalentHyp=as.factor(datahrt.scaled$prevalentHyp)
datahrt.scaled$prevalentStroke=as.factor(datahrt.scaled$prevalentStroke)
datahrt.scaled$diabetes=as.factor(datahrt.scaled$diabetes)
#datahrt.scaled<-datahrt.scaled[,-1]

datahrt.scaled.numeric<- scale(datahrt.scaled.numeric, center=TRUE, scale=TRUE)
datahrt.scaled.numeric<-as.data.frame(datahrt.scaled.numeric)
class(datahrt.scaled.numeric)
boxplot(datahrt.scaled.numeric, horizontal = TRUE, cex.axis=.5, ylab= "Variables")



boxplot(datahrt.scaledfinal, horizontal = TRUE, cex.axis=.5, ylab= "Variables")
####creating new data frame with scaled numeric variables###
str(datahrt.scaledfinal)
str(datahrt.scaled.numeric)
str(datahrt.scaled.factor)
datahrt.scaledfinal<-cbind(datahrt.scaled.numeric,datahrt.scaled.factor)
datahrt.unscaledfinal<- datahrt.woNA
str(datahrt.scaledfinal)
#############################################################################
##############################   sample splits   ###############################
######################################################################
######################################################################

table(datahrt$prevalentStroke)
set.seed=800
library(caTools)
split<- sample.split(datahrt.scaledfinal, SplitRatio = 0.8 )
train.datahrt<- subset(datahrt.scaledfinal, split==TRUE)
test.datahrt<- subset(datahrt.scaledfinal, split==FALSE)

##checking for proportion of seperation
table(datahrt.scaledfinal$TenYearCHD)
table(train.datahrt$TenYearCHD)
table(test.datahrt$TenYearCHD)
557/3101


str(train.datahrt.untreated)





##############################   Modelling data   ####################
######################################################################
######################################################################


#############################   Logistic regression###################


####creating a base model
##attempting multivariate logistic regression
base.model1 <- glm(TenYearCHD~., data = datahrt.factor, family = binomial)
summary(base.model1)

basemodel2<-glm(TenYearCHD~., data = datahrt.numeric3, family = binomial)
summary(basemodel2)

basemodel3<- glm(TenYearCHD~.,data=datahrt.scaledfinal, family = binomial)
 summary(basemodel3)
 

#trying univariate logistic regression
dummy<-glm(TenYearCHD~totChol, data=datahrt.numeric3, family= binomial)
summary(dummy)
 
dummy<-glm(TenYearCHD~BMI, data=datahrt.numeric3, family= binomial)
summary(dummy)

dummy<-glm(TenYearCHD~diaBP, data=datahrt.numeric3, family= binomial)
summary(dummy)

dummy<-glm(TenYearCHD~heartRate, data=datahrt.numeric3, family= binomial)
summary(dummy)

dummy<-glm(TenYearCHD~currentSmoker, data=datahrt.factor, family= binomial)
summary(dummy)
 
dummy<-glm(TenYearCHD~prevalentStroke, data=datahrt.factor, family= binomial)
summary(dummy)

####Dropping variables
train.datahrt1 <- subset(train.datahrt, select = -c( heartRate))
test.datahrt1 <- subset(test.datahrt, select = -c( heartRate))
str(train.datahrt1)

str(datahrt.woNA)
str(datahrt.numeric3)
datahrt.numeric3<- datahrt.numeric2
datahrt.numeric3$TenYearCHD<-ifelse(datahrt.numeric3$TenYearCHD==1,0,1)
datahrt.numeric3$TenYearCHD<- as.factor(datahrt.numeric3$TenYearCHD)

#############building a full logistic regression model
logmodel0<-glm(TenYearCHD~.,data=datahrt.woNA,family = binomial)
summary(logmodel0)

logmodel1<-glm(TenYearCHD~.,data=train.datahrt1,family= binomial)
summary(logmodel1) 

anova(logmodel0,test = "Chisq")
anova(logmodel1, test = "Chisq")

####Dropping few more variables and running unscaled log model
datahrt.unscaledfinal1<-subset(datahrt.unscaledfinal, 
                               select = -c(currentSmoker,prevalentStroke,BMI,heartRate,diaBP,glucose))
logmodel0unscaled<-glm(TenYearCHD~.,data=datahrt.unscaledfinal1,
                       family = binomial)
summary(logmodel0unscaled)
###########################################3
set.seed=800
split2<-sample.split(datahrt.unscaledfinal1$TenYearCHD,
                             SplitRatio = 0.7)
train.datahrt2<-subset(datahrt.unscaledfinal1, split2= TRUE)
test.datahrt2<-subset(datahrt.unscaledfinal1, split2= FALSE)

train.logmodel0unscaled<-glm(TenYearCHD~.,data=train.datahrt2,
                       family = binomial)
summary(train.logmodel0unscaled)


################################################################
##################################################################
#############################################################

########################## Log model option2
datahrt.woNAdummy<-datahrt.woNA
############train test using unscaled uncapped data
datahrt.woNAdummy1<-datahrt.woNAdummy
str(datahrt.woNAdummy1)
datahrt.woNAdummy1<-subset(datahrt.woNAdummy1, 
                            select = -c(BMI,heartRate,diaBP))
datahrt.woNAdummy2<-subset(datahrt.woNAdummy1, 
                           select = -c(education
                                       ,currentSmoker,BPMeds,prevalentStroke
                                       ,diabetes))

split<- sample.split(datahrt.woNAdummy2, SplitRatio = 0.8 )
train.datahrt.untreated<- subset(datahrt.woNAdummy2, split==TRUE)
test.datahrt.untreated<- subset(datahrt.woNAdummy2, split==FALSE)

logmodeluntreated<-glm(TenYearCHD~.,data=train.datahrt.untreated,
                       family = binomial)
summary(logmodeluntreated)

trainpredict<- predict(logmodeluntreated,newdata= train.datahrt.untreated,
                       type="response")
train.datahrt.untreated$GLMclass<- predict(logmodeluntreated,newdata= train.datahrt.untreated,
                                           type="response")
train.datahrt.untreated$GLMclassfn<-ifelse(train.datahrt.untreated$GLMclass>0.5,1,0)
train.datahrt.untreated$GLMclassfn1<-ifelse(train.datahrt.untreated$GLMclass>0.4,1,0)

################################################################
################################################################
################################################################
#################################################################
## attempting score of ROC with Caret package
# transfroming y data for caret in yes no fashion instead of 0 and 1
train.datahrt.untreatednew1<-train.datahrt.untreated
test.datahrt.untreatednew1<-test.datahrt.untreated
str(train.datahrt.untreatednew1$TenYearCHD)
head(train.datahrt.untreatednew1$TenYearCHD)
train.datahrt.untreatednew1$TenYearCHD<- ifelse(train.datahrt.untreatednew1$TenYearCHD==1,"yes", "No")
library(caret)

control <- trainControl(classProbs = T,
                        summaryFunction = twoClassSummary,
                        method="repeatedcv", number=10, 
                        repeats=3)
modellogtrain<- caret::train(TenYearCHD~.,data=train.datahrt.untreatednew1,
                             method="glm", family="binomial",
                             metric='ROC',trControl=control)
modellogtrain

#################################################################
###threhold value checks at 0.5 and 0.4 shows 0.5 is better
table(train.datahrt.untreated$TenYearCHD, train.datahrt.untreated$GLMclass>0.5)
table(train.datahrt.untreated$TenYearCHD, train.datahrt.untreated$GLMclass>0.4)

train.datahrt.untreated$GLMclassfn<- as.factor(train.datahrt.untreated$GLMclassfn)
caret::confusionMatrix(train.datahrt.untreated$TenYearCHD, train.datahrt.untreated$GLMclassfn)

###################
train.datahrt.untreated$GLMclassfn1<- as.factor(train.datahrt.untreated$GLMclassfn1)
caret::confusionMatrix(train.datahrt.untreated$TenYearCHD, train.datahrt.untreated$GLMclassfn1)
###################

testpredict<- predict(logmodeluntreated,newdata= test.datahrt.untreated,
                       type="response")
test.datahrt.untreated$GLMclass<- predict(logmodeluntreated,newdata= test.datahrt.untreated,
                                           type="response")
test.datahrt.untreated$GLMclassfn<-ifelse(test.datahrt.untreated$GLMclass>0.5,1,0)
table(test.datahrt.untreated$TenYearCHD, test.datahrt.untreated$GLMclass>0.5)
test.datahrt.untreated$GLMclassfn<- as.factor(test.datahrt.untreated$GLMclassfn)
caret::confusionMatrix(test.datahrt.untreated$TenYearCHD, test.datahrt.untreated$GLMclassfn)

testpredict<- predict(logmodeluntreated,newdata= test.datahrt.untreated,
                      type="response")
test.datahrt.untreated$GLMclass<- predict(logmodeluntreated,newdata= test.datahrt.untreated,
                                          type="response")
test.datahrt.untreated$GLMclassfn<-ifelse(test.datahrt.untreated$GLMclass>0.4,1,0)
table(test.datahrt.untreated$TenYearCHD, test.datahrt.untreated$GLMclass>0.4)
test.datahrt.untreated$GLMclassfn<- as.factor(test.datahrt.untreated$GLMclassfn)
caret::confusionMatrix(test.datahrt.untreated$TenYearCHD, test.datahrt.untreated$GLMclassfn)
21/(139)
table(test.datahrt.untreated$GLMclassfn)
anova(logmodeluntreated, test = "Chisq")

######         ROCR for Log model

library(ROCR)

ROCRpred=prediction(testpredict, test.datahrt.untreated$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)
perf.log<-performance(ROCRpred,"tpr", "fpr")
plot(perf.log,col="black",lty=2,lwd=2)
plot(perf.log,colorize=TRUE,lwd=3)

?blorr
library(blorr)
blr_step_aic_both(logmodeluntreated,details=FALSE)
blr_rsq_mcfadden(logmodeluntreated)


#coefficients()
exp(coefficients(logmodeluntreated))

k<- blr_gains_table(logmodeluntreated)
blr_ks_chart(k, title="KS CHART", ks_line_color = "black")

#################################################################
predTest1 = predict(train.logmodel0unscaled, newdata= test.datahrt2, type="response")
table(train.datahrt2$TenYearCHD, predTest1>0.5)
summary(predTest1)
predTest1
(3087+30)/(3087+30+14+527)

###### prediction on train and test
predTest = predict(logmodel1, newdata= train.datahrt1, type="response")
table(train.datahrt1$TenYearCHD, predTest>0.5)

 
predTest = predict(logmodel1, newdata= test.datahrt1, type="response")
table(test.datahrt1$TenYearCHD, predTest>0.5) 
logmodel1

######Dropping few more  variables and building new model
train.datahrt2 <- subset(train.datahrt1, select = -c(diaBP,BMI,totChol,education,BPMeds))
test.datahrt2 <- subset(test.datahrt1, select = -c(diaBP,BMI, totChol,education, BPMeds))

logmodel2<-glm(TenYearCHD~.,data=train.datahrt2,family= binomial)
summary(logmodel2) 
predTest = predict(logmodel2, newdata= train.datahrt2, type="response")
table(train.datahrt2$TenYearCHD, predTest>0.5) 


logmodel21<-glm(TenYearCHD~.,data=test.datahrt2,family= binomial)
predTest = predict(logmodel21, newdata= test.datahrt2, type="response")
table(test.datahrt2$TenYearCHD, predTest>0.5)
summary(logmodel21) 

#############################   CART  ###################

str(train.datahrt.untreated)
str(datahrt.woNAdummy)

cart.train<-subset(train.datahrt.untreated, select = -c(GLMclass,GLMclassfn,GLMclassfn1 ))
str(cart.train)
str(cart.test)
cart.test<-subset(test.datahrt.untreated, select = -c(GLMclass,GLMclassfn,GLMclassfn1 ))

################################Sample split with Full variable
library(caTools)
split<- sample.split(datahrt.woNAdummy, SplitRatio = 0.8 )
cart.train<- subset(datahrt.woNAdummy, split==TRUE)
cart.test<- subset(datahrt.woNAdummy, split==FALSE)
str(cart.train)

###########################CART Model with all variables#######

library(rpart)
library(rattle)
library(rpart.plot)
r.ctrl<-rpart.control(minsplit = 100, minbucket = 50,
                    cp=0, xval = 10)
cart.model0<-rpart(formula=TenYearCHD~.,data =cart.train,
                  control = r.ctrl, method = "class")

cart.model0$variable.importance
p<-cart.model0$variable.importance
barplot(p)
cart.model0
summary((cart.model0))
 printcp(cart.model0)
 
 
 
fancyRpartPlot(cart.model0)
rpart.rules(cart.model0, cover = TRUE)
plotcp(cart.model0)

#####prediction
train.cart0<-cart.train ###backup
str(train.cart0)
#train.cart0$TenYearCHD<-as.factor(train.cart$TenYearCHD)


train.cart0$predcart<-predict(cart.model0,
                              train.cart0,
                              type="class")

train.cart0$scorecart<-predict(cart.model0,
                               train.cart0,
                               type="prob")[,"1"]
summary(train.cart0)

library(caret)
table(train.cart0$TenYearCHD,train.cart0$Gender)

caret::confusionMatrix(train.cart0$TenYearCHD,
                       train.cart0$predcart)


###########################CART Model with outlier treated#######




str(datahrt.woNA3)
split<- sample.split(datahrt.woNA3, SplitRatio = 0.8 )
cart.train1<- subset(datahrt.woNA3, split==TRUE)
cart.test1<- subset(datahrt.woNA3, split==FALSE)
str(cart.train1)

r.ctrl<-rpart.control(minsplit = 100, minbucket = 50,
                      cp=0, xval = 10)
cart.model1<-rpart(formula=TenYearCHD~.,data =cart.train1,
                   control = r.ctrl, method = "class")
cart.model1$variable.importance
p1<-cart.model1$variable.importance
barplot(p1)
cart.model1
summary((cart.model1))
printcp(cart.model1)

fancyRpartPlot(cart.model1)
rpart.rules(cart.model1, cover = TRUE)
plotcp(cart.model1)


#####prediction

library(caret)
train.cart1<-cart.train1 ###backup
str(train.cart1)
#train.cart1$TenYearCHD<-as.factor(train.cart$TenYearCHD)

train.cart1$predcart<-predict(cart.model1,
                              train.cart1,
                              type="class")

train.cart1$scorecart<-predict(cart.model1,
                              train.cart1,
                              type="prob")[,"1"]
summary(train.cart1)

table(train.cart1$TenYearCHD,train.cart1$predcart)

caret::confusionMatrix(train.cart1$TenYearCHD,
                       train.cart1$predcart)


################## datahrt.scaledfinal
datahrt.woNA4<-datahrt.scaledfinal

str(datahrt.woNA4)
split<- sample.split(datahrt.woNA4, SplitRatio = 0.8 )
cart.train2<- subset(datahrt.woNA4, split==TRUE)
cart.test2<- subset(datahrt.woNA4, split==FALSE)

str(cart.train2)

r.ctrl<-rpart.control(minsplit = 100, minbucket = 50,
                      cp=0, xval = 10)
cart.model2<-rpart(formula=TenYearCHD~.,data =cart.train2,
                   control = r.ctrl, method = "class")
cart.model2$variable.importance
p2<-cart.model2$variable.importance
barplot(p2, cex.names = .6)
 
cart.model2
summary((cart.model2))
printcp(cart.model2)

fancyRpartPlot(cart.model2)
rpart.rules(cart.model2, cover = TRUE)
plotcp(cart.model2)


#####prediction

library(caret)
train.cart2<-cart.train2 ###backup
str(train.cart2)
#train.cart1$TenYearCHD<-as.factor(train.cart$TenYearCHD)

train.cart2$predcart<-predict(cart.model2,
                              train.cart2,
                              type="class")

train.cart2$scorecart<-predict(cart.model2,
                               train.cart2,
                               type="prob")[,"1"]
summary(train.cart2)

table(train.cart2$TenYearCHD,train.cart2$predcart)

caret::confusionMatrix(train.cart2$TenYearCHD,
                       train.cart2$predcart)

#### scaling did not help improve scores. specificity was 53.9


###################################

datahrt.woNA5<- datahrt.woNAdummy2 #### this is variables chosen by log

str(datahrt.woNA5)
split<- sample.split(datahrt.woNA5, SplitRatio = 0.8 )
cart.train3<- subset(datahrt.woNA5, split==TRUE)
cart.test3<- subset(datahrt.woNA5, split==FALSE)

str(cart.train3)

r.ctrl<-rpart.control(minsplit = 100, minbucket = 50,
                      cp=0, xval = 10)
cart.model3<-rpart(formula=TenYearCHD~.,data =cart.train3,
                   control = r.ctrl, method = "class")
cart.model3$variable.importance
p3<-cart.model3$variable.importance
barplot(p3, cex.names = .6)

cart.model3
summary((cart.model3))
printcp(cart.model3)

fancyRpartPlot(cart.model3)

rpart.rules(cart.model3, cover = TRUE)
plotcp(cart.model3)


#####prediction

library(caret)
train.cart3<-cart.train3 ###backup
str(train.cart3)
#train.cart1$TenYearCHD<-as.factor(train.cart$TenYearCHD)

train.cart3$predcart<-predict(cart.model3,
                              train.cart3,
                              type="class")

train.cart3$scorecart<-predict(cart.model3,
                               train.cart3,
                               type="prob")[,"1"]
summary(train.cart3)

table(train.cart3$TenYearCHD,train.cart3$predcart)

caret::confusionMatrix(train.cart3$TenYearCHD,
                       train.cart3$predcart)







############################################################
############################################################
############################################################
############################################################



####setting a grid search for hypertuning parameters

hypergrid<-expand.grid(minsplit= seq(2,6,1),maxdepth= seq(2,6,1))


head(hypergrid)
r.ctrl<-rpart.control(minsplit = 100, minbucket = 50,
                      cp=0, xval = 10)

models<-list()
for (i in 1:nrow(hypergrid)) 
{
  minsplita<-hypergrid$minsplit[i]
  maxdeptha<-hypergrid$minbucket[i]
  models[i]<- rpart(formula = TenYearCHD~.,
  data =train.cart0,
   control = list(minsplit=minsplita, 
                   minbucket=maxdeptha), 
   method = "class"
  )
}
str(cart.train1)
head(models)
printcp(cart.model0)
cart.model0$parms
models[1]$

newcartmodel<-rpart(formula=TenYearCHD~.,data =cart.train1,
      control = r.ctrl, method = "class", tuneGrid)

grid <- expand.grid(size=c(5,10,20,50), k=c(1,2,3,4,5))
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(TenYearCHD~., data=cart.train1, 
               method="lvq", trControl=control, tuneGrid=grid)
print(model)
# summarize the model
View(models)
??lvq

sapply(models, function(x) fancyRpartPlot(models[1]))
fancyRpartPlot(models[1])
rpart.rules(models[1], cover = TRUE)

print(models)
#printcp(models)
#datamodels<- as.data.frame(models)
#View(datamodels)

models[1]

#######model performance rates#########
library(ROCR)### for ROC scores
library(ineq) ##reqd for gini scores
library(InformationValue)

####ROC for train data

predobjtrain<-prediction(train.cart$predcart,train.cart$TenYearCHD)
perf.train<-performance(prediction.obj = predobjtrain
                        ,"tpr", "fpr")
plot(perf.train)

####ROC for train data

predobjtest<-prediction(test.cart$scorecart,
                         test.cart$TenYearCHD)
perf.test<-performance(prediction.obj = predobjtrain
                        ,"tpr", "fpr")
plot(perf.test)

####AUC
AUCtrain<-performance(predobjtrain,"auc")
as.numeric(AUCtrain@y.values)

AUCtest<-performance(predobjtest,"auc")
as.numeric(AUCtest@y.values)

##KS
max(perf.train@y.values[[1]]-perf.train@x.values[[1]])
max(perf.test@y.values[[1]]-perf.test@x.values[[1]])






################################################################################################
################################################################
#################################     Random forest  #############################

################################Sample split with Full variable
str(datahrt.woNAdummy)
summary(datahrt.woNAdummy$cigsPerDay)
library(caTools)
datahrt.woNA4<-datahrt.woNAdummy #####data for random forest
set.seed=800
split<- sample.split(datahrt.woNA4, SplitRatio = 0.8 )
RF.train0<- subset(datahrt.woNA4, split==TRUE)
RF.test0<- subset(datahrt.woNA4, split==FALSE)
str(RF.train0)

##modifying variables as per full model
datahrt.woNA5<-subset(datahrt.woNA4,select=-c(totChol,currentSmoker, BPMeds,prevalentStroke))
split<- sample.split(datahrt.woNA5, SplitRatio = 0.8 )
RF.train1<- subset(datahrt.woNA5, split==TRUE)
RF.test1<- subset(datahrt.woNA5, split==FALSE)
str(RF.train1)

####trying with outlier corrected data
str(datahrt.woNA6)
        
datahrt.woNA6<-datahrt.woNA3

datahrt.woNA6<-subset(datahrt.woNA6,select=-c(totChol,currentSmoker, BPMeds,prevalentStroke))
split<- sample.split(datahrt.woNA6, SplitRatio = 0.8 )
RF.train6<- subset(datahrt.woNA6, split==TRUE)
RF.test6<- subset(datahrt.woNA6, split==FALSE)
str(RF.train6)





###########################Randomforest Model with all variables#######
library(randomForest)
RF.model0<-randomForest(TenYearCHD~.,RF.train0,
                        ntree=501,
                        mtry=5,
                        nodesize=10, 
                        importance=TRUE)

?randomForest
print(RF.model0)
plot(RF.model0)
importance(RF.model0)
varImpPlot(RF.model0)

#### new model with dropped variables
RF.model1<-randomForest(TenYearCHD~.,RF.train1,
                        ntree=501,
                        mtry=5,
                        nodesize=10, 
                        importance=TRUE)

print(RF.model1)
plot(RF.model1)
importance(RF.model1)
varImpPlot(RF.model1)

tune.RFModel1<-tuneRF(x=(RF.train1[,-c(11)]),
                      y=RF.train1$TenYearCHD,
                      mtryStart = 7,
                      stepFactor = 1.5,
                      ntreeTry = 1001,
                      improve = 0.0001,
                      nodesize=5,
                      plot = TRUE,
                      trace=TRUE,
                      doBest = FALSE,
                      importance=TRUE)
RF.model3<-randomForest(TenYearCHD~.,RF.train1,
                        ntree=501,
                        mtry=5,
                        nodesize=10, 
                        importance=TRUE)



tune.RFModel0<-tuneRF(x=(RF.train0[,-c(15)]),
                      y=RF.train0$TenYearCHD,
                      mtryStart = 7,
                      stepFactor = 1.5,
                      ntreeTry = 1001,
                      improve = 0.0001,
                      nodesize=5,
                      plot = TRUE,
                      trace=TRUE,
                      doBest = FALSE,
                      importance=TRUE)

RF.model2<-randomForest(TenYearCHD~.,RF.train0,
                        ntree=1001,
                        mtry=4,
                        nodesize=10, 
                        importance=TRUE)

print(RF.model2)
plot(RF.model2)
importance(RF.model2)
varImpPlot(RF.model2)

#####model prediction

### run on train data

RF.train0$predclass<-predict(RF.model2,
                              RF.train0,
                              type="class")

RF.train0$predscore<-predict(RF.model2,
                             RF.train0,
                             type="prob")[,"1"]

caret::confusionMatrix(RF.train0$TenYearCHD,RF.train0$predclass)

##### RF model3
RF.train1$predclass<-predict(RF.model3,
                             RF.train1,
                             type="class")

RF.train1$predscore<-predict(RF.model3,
                             RF.train1,
                             type="prob")[,"1"]

caret::confusionMatrix(RF.train1$TenYearCHD,RF.train1$predclass)



#### run on test data
RF.test0$predclass<-predict(RF.model2,
                             RF.test0,
                             type="class")

RF.test0$predscore<-predict(RF.model2,
                             RF.test0,
                             type="prob")[,"1"]

caret::confusionMatrix(RF.test0$TenYearCHD,RF.test0$predclass)

##### RF model3
RF.test1$predclass<-predict(RF.model3,
                            RF.test1,
                            type="class")

RF.test1$predscore<-predict(RF.model3,
                            RF.test1,
                            type="prob")[,"1"]

caret::confusionMatrix(RF.test1$TenYearCHD,RF.test1$predclass)

################ trying with treated outlier data
RF.model4<-randomForest(TenYearCHD~.,RF.train6,
                        ntree=501,
                        mtry=5,
                        nodesize=10, 
                        importance=TRUE)

print(RF.model4)
plot(RF.model4)
importance(RF.model4)
varImpPlot(RF.model4)

tune.RFModel4<-tuneRF(x=(RF.train6[,-c(11)]),
                      y=RF.train6$TenYearCHD,
                      mtryStart = 7,
                      stepFactor = 1.5,
                      ntreeTry = 1001,
                      improve = 0.0001,
                      nodesize=5,
                      plot = TRUE,
                      trace=TRUE,
                      doBest = FALSE,
                      importance=TRUE)
RF.model4<-randomForest(TenYearCHD~.,RF.train6,
                        ntree=501,
                        mtry=2,
                        nodesize=10, 
                        importance=TRUE)
##train data
RF.train6$predclass<-predict(RF.model4,
                             RF.train6,
                             type="class")

RF.train6$predscore<-predict(RF.model4,
                             RF.train6,
                             type="prob")[,"1"]

caret::confusionMatrix(RF.train6$TenYearCHD,RF.train6$predclass)
### running on test data
RF.test6$predclass<-predict(RF.model4,
                             RF.test6,
                             type="class")

RF.test6$predscore<-predict(RF.model4,
                             RF.test6,
                             type="prob")[,"1"]

caret::confusionMatrix(RF.test6$TenYearCHD,RF.test6$predclass)

# random forest shows overfitting in most models with
#max specificity of 89%
##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

#############             KNN

################################################################################################
################################################################
################################################################
################################Sample split with Full variable
library(class)
knn.data<-datahrt.scaled
str(knn.data)

set.seed=800
split<- sample.split(knn.data, SplitRatio = 0.8 )
knn.train0<- subset(knn.data, split==TRUE)
knn.test0<- subset(knn.data, split==FALSE)
str(knn.train0)

library(LiblineaR)

#control <- trainControl(method="repeatedcv", number=10, 
 #                       repeats=3, classProbs = T, 
  #                      summaryFunction = twoClassSummary)
#kNNmodel <- caret::train(TenYearCHD~., data=knn.train0,
#                         method="knn", metric="ROC",
#                         trControl="control")
#summary(kNNmodel)

cl =knn.train0[15]
predknn<- knn(knn.train0[-15],knn.test0[-15],knn.train0[,15],k=3, prob="TRUE")

testtable.knn=table(knn.test0[,15],predknn)

testtable.knn
sum(diag(testtable.knn)/sum(testtable.knn))
(735+13)/(735+13+137+30)
13/43
10/22
16/(47+16)
summary(knn.test0$TenYearCHD)
summary(predknn)
###############Knn=11
predknn1<- knn(knn.train0[-15],knn.test0[-15],knn.train0[,15],k=9, prob="TRUE")

testtable.knn1=table(knn.test0[,15],predknn1)

testtable.knn1
sum(diag(testtable.knn1)/sum(testtable.knn1))
(735+13)/(735+13+137+30)
13/43

##############################################################
##############################################################
##############################################################
##############################################################
##############################################################

#############             NAIVE BAyes

################################################################################################
################################################################
################################################################
################################Sample split with Full variable

datahrtNB <-read.csv("Coronary_heart_risk_study.csv")
str(datahrt.NB)
datahrtNB$TenYearCHD=as.factor(datahrtNB$TenYearCHD)
datahrtNB$Gender=as.factor(datahrtNB$male)
datahrtNB$currentSmoker=as.factor(datahrtNB$currentSmoker)
datahrtNB$education=as.factor(datahrtNB$education)
datahrtNB$BPMeds=as.factor(datahrtNB$BPMeds)
datahrtNB$prevalentHyp=as.factor(datahrtNB$prevalentHyp)
datahrtNB$prevalentStroke=as.factor(datahrtNB$prevalentStroke)
datahrtNB$diabetes=as.factor(datahrtNB$diabetes)
datahrtNB<-datahrtNB[,-1]
####### filter out NAs
library(tidyverse)
datahrt.NB<-datahrtNB %>% filter(complete.cases(.))


### trying binning with cut function


#age
agebin<-c(-Inf,30,50,60, Inf)
agename<-c("young", "midage", "mature","old")
datahrt.NB$age.cat<-cut(datahrt.NB$age, breaks=agebin, 
                        labels=agename)

#cigsperday
summary(datahrt.NB$cigsPerDay)
hist(datahrt.NB$cigsPerDay)
plot(datahrt.NB$cigs.cat, datahrt.NB$TenYearCHD)
cigsbin<-c(-Inf,1,10,20,Inf)
cigsname<-c("Non", "Avg Smoker", "high Smoker","Chain Smoker")
datahrt.NB$cigs.cat<-cut(datahrt.NB$cigsPerDay, breaks=cigsbin, 
                         labels=cigsname)

#totChol
summary(datahrt.NB$totChol)
cholbin<-c(-Inf,200,240, Inf)
cholname<-c("normal", "borderhigh", "High")
datahrt.NB$totChol.cat<-cut(datahrt.NB$totChol, breaks=cholbin, 
                            labels=cholname)
#SysBP
summary(datahrt.NB$sysBP)
sysBPbin<-c(-Inf,120,130,140,Inf)
sysBPname<-c("normal", "elevated","border high","high BP")
datahrt.NB$sysBP.cat<-cut(datahrt.NB$sysBP, breaks=sysBPbin, 
                          labels=sysBPname)
plot(datahrt.NB$sysBP.cat,datahrt.NB$TenYearCHD)
head(datahrt.NB)

#diaBP
summary(datahrt.NB$diaBP)
diaBPbin<-c(-Inf,80,85,90, Inf)
diaBPname<-c("normal", "elevated","border high","high BP")
datahrt.NB$diaBP.cat<-cut(datahrt.NB$diaBP, breaks=diaBPbin, 
                          labels=diaBPname)
plot(datahrt.NB$diaBP.cat,datahrt.NB$TenYearCHD)
str(datahrt.NB)

#BMI
BMIbin<-c(-Inf,18.5,25, Inf)
BMIname<-c("underweight", "healthy","overweight")
datahrt.NB$BMI.cat<-cut(datahrt.NB$BMI, breaks=BMIbin, 
                        labels=BMIname)
#heartRate
summary(datahrt.NB$heartRate)
heartRatebin<-c(-Inf,60,80, Inf)
heartRatename<-c("athletic", "regular", "elevated")
datahrt.NB$heartRate.cat<-cut(datahrt.NB$heartRate, breaks=heartRatebin, 
                              labels=heartRatename)
plot(datahrt.NB$heartRate.cat,datahrt.NB$TenYearCHD)

#glucose
glucosebin<-c(-Inf,120,140,150, Inf)
glucosename<-c("normal", "pre-diabetic", "diabetic","high")
datahrt.NB$glucose.cat<-cut(datahrt.NB$glucose, breaks=glucosebin, 
                            labels=glucosename)
plot(datahrt.NB$glucose.cat,datahrt.NB$TenYearCHD)


####making only categorical data frame for naive bayes
NB.data.cat<-subset(datahrt.NB, select=-c(age,cigsPerDay,totChol,
                                          sysBP,diaBP,BMI,heartRate,glucose))
str(NB.data.cat)
str(datahrt.NB)
                                      


###########################################
split<- sample.split(NB.data.cat, SplitRatio = 0.8 )
NB.train0<- subset(NB.data.cat, split==TRUE)
NB.test0<- subset(NB.data.cat, split==FALSE)
str(NB.train0)

table(NB.data.cat$TenYearCHD)
table(NB.train0$TenYearCHD)
table(NB.test0$TenYearCHD)
557/(557+3101)
432/(432+2312)
125/(125+789)

library(mlbench)
library(e1071)
NB0=naiveBayes(NB.train0[,7]~.,data=NB.train0[,-c(7)])
NB0=naiveBayes(x=NB.train0[,-c(7)], y=as.factor(NB.train0[,7]),data=NB.train0)

predNB0<-predict(NB0,NB.test0, type="class")
str(predNB0)
table.NB0<-table(NB.test0[,7],predNB0)
table.NB0
summary(predNB0)


NB.trainnew<-NB.train0
NB.testnew<-NB.test0
NB.trainnew$predclass<-predict(NB0,
                               NB.trainnew,type ="class")
                               
NB.trainnew$predscore<-predict(NB0,
                               NB.trainnew,type="raw")[,"0"]
head(NB.trainnew$predscore)

caret::confusionMatrix(NB.trainnew$TenYearCHD,NB.trainnew$predclass)

NB.testnew$predclass<-predict(NB0,
                               NB.testnew,type ="class")
caret::confusionMatrix(NB.testnew$TenYearCHD,NB.testnew$predclass)



head(NB.train0[7])

predNB2<-predict(NB1,NB.train0, type="class")
table.NB2<-table(NB.train0[,7],predNB2)
table.NB2

summary(NB1)
NB0$tables
  
NB.test1<-NB.test0
NB.test1$predclass<-predict(NB1,
                            NB.test1,
                            type="class")

NB.test1$predscore<-predict(NB1,
                            NB.test1,
                            type="prob")[,"1"]

caret::confusionMatrix(NB.test1$TenYearCHD,NB.test1$predclass)
?plot
?mf_row
#####################
par(mfrow=c(1,1))
plot(datahrt.NB$age.cat, datahrt.NB$TenYearCHD, xlab="AGE", ylab="TEN yr CHD")
plot(datahrt.NB$cigs.cat, datahrt.NB$TenYearCHD, xlab="CIGs/Day", ylab="TEN yr CHD")
plot(datahrt.NB$totChol.cat, datahrt.NB$TenYearCHD,xlab="Total Cholesterol", ylab="TEN yr CHD")
plot(datahrt.NB$sysBP.cat,datahrt.NB$TenYearCHD,xlab="Systolic BP", ylab="TEN yr CHD")
plot(datahrt.NB$diaBP.cat,datahrt.NB$TenYearCHD,xlab="Diastolic BP", ylab="TEN yr CHD")
plot(datahrt.NB$BMI.cat,datahrt.NB$TenYearCHD,xlab="BMI", ylab="TEN yr CHD")
plot(datahrt.NB$heartRate.cat,datahrt.NB$TenYearCHD,xlab="Heart rate", ylab="TEN yr CHD")
plot(datahrt.NB$glucose.cat,datahrt.NB$TenYearCHD,xlab="Glucose", ylab="TEN yr CHD")


############Rough work
control <- trainControl(method="repeatedcv", classProbs = TRUE,
                        number=10, repeats=3)
Ctrl<- trainControl(method = "repeatedcv", classProbs =T,
                    number = 10,summaryFunction = twoClassSummary,repeats = 1)
modelCART <-caret::train(TenYearCHD ~., data=datahrt.woNA, 
               method="rpart", metric='ROC',
               trControl=control,tuneGrid=grid)
datahrt.woNA$TenYearCHD<-factor(datahrt.woNA$TenYearCHD)

modelLogit<-caret::train(TenYearCHD~.,data=datahrt.woNA,
                         method="regLogistic", metric='Sensitivity',
                         trcontrol=Ctrl)
model


library(caret)
modelCART
str(datahrt.woNA)


### trying binning with rbin
install.packages("rbin")
install.packages("rbinAddin")
library(rbin)
library(rbinaddin)
datatry=datahrt.woNA
str(datatry.woNA)
datatry.numeric<-subset(datatry, select=c(age,cigsPerDay,totChol,sysBP,diaBP,
                                          BMI ,heartRate,glucose, TenYearCHD))
str(datatry.numeric) 
summary(datatry.numeric$sysBP)
sysBPbins<-rbin_manual(datatry.numeric,TenYearCHD,sysBP, c(120,130,150,200,250))
sysBPbins 
str(sysBP   bins)

################################################################################################

#students$Income.cat2 <- cut(students$Income, breaks = 4, labels = c("Level1", "Level2", "Level3","Level4"))
knn.data1<-knn.data
knn.data1$age.cat1<-cut(knn.data1$age, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$cigsPerDay.cat1<-cut(knn.data1$cigsPerDay, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$totChol.cat1<-cut(knn.data1$totChol, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$sysBP.cat1<-cut(knn.data1$sysBP, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$diaBP.cat1<-cut(knn.data1$diaBP, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$BMI.cat1<-cut(knn.data1$BMI, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$glucose.cat1<-cut(knn.data1$glucose, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

knn.data1$heartRate.cat1<-cut(knn.data1$heartRate, breaks=4, labels=c("Level1", "Level2", "Level3","Level4"))

str(knn.data1)

knn.data.cat1<-subset(knn.data1, select=-c(age,cigsPerDay,totChol,sysBP,diaBP,BMI,heartRate,glucose))
str(knn.data.cat1) 

split<- sample.split(knn.data.cat1, SplitRatio = 0.8 )
NB.train1<- subset(knn.data.cat1, split==TRUE)
NB.test1<- subset(knn.data.cat1, split==FALSE)
str(NB.train1)

NB1=naiveBayes(NB.train1[,7]~.,data=NB.train1)
predNB1<-predict(NB1,NB.test1, type="class")
table.NB1<-table(NB.test1[,7],predNB1)
table.NB1


##################################################################

##################################################################

## attempting xgboost
library(xgboost)
xgdata<- datahrt.woNA3
xgdata.numeric<-xgdata[,c(1,4,9:14,15)]
str(xgdata.numeric)

library(caTools)
split<- sample.split(xgdata.numeric, SplitRatio = 0.8 )
xgb.train0<- subset(xgdata.numeric, split==TRUE)
xgb.test0<- subset(xgdata.numeric, split==FALSE)
str(xgb.test0)

######                      XGB requires MATRIX DATA


xgb.train1<-as.matrix(xgb.train0[,-c(9)])
View(xgb.train1.label)
xgb.train1.label<-as.matrix(xgb.train0[,9])
xgb.test1<-as.matrix(xgb.test0[,1:8])
str(xgb.test1)
View(xgb.test1)

xgb.fit<-xgboost(
  data = xgb.train1,
  label = xgb.train1.label,
  eta=0.1,
  max_depth=3,
  min_child_weight=3,
  nrounds = 10000,
  nfold=5,
  objective="binary:logistic",
  verbose = 0,
  early_stopping_rounds = 10
)

xgb.test0$pred.class<-predict(xgb.fit,xgb.test1)
table(xgb.test0$TenYearCHD,xgb.test0$pred.class>0.5 )
View(xgb.test1)

tp_xgb<-vector()
lr<-c(0.001, 0.01,0.1,0.5,0.7,0.9,1)
md<-c(1,3,5,7)
nr<-c(2,50,100,1000,10000)
for (i in nr) {
  xgb.fit<-xgboost(
    data = xgb.train1,
    label = xgb.train1.label,
    eta=1,
    max_depth=5,
    min_child_weight=3,
    nrounds = i,
    nfold=5,
    objective="binary:logistic",
    verbose = 0,
    early_stopping_rounds = 10
  )
  
  xgb.test0$pred.class<-predict(xgb.fit,xgb.test1)
  tp_xgb<-cbind(tp_xgb,sum(xgb.test0$TenYearCHD==1& xgb.test0$pred.class>=0.5))
}

tp_xgb

####################################################3
#### running models through NB categorical data

NB.data.catnew<-NB.data.cat
split<- sample.split(NB.data.catnew, SplitRatio = 0.8 )
NB.trainnew<- subset(NB.data.catnew, split==TRUE)
NB.testnew<- subset(NB.data.catnew, split==FALSE)
str(NB.trainnew)


NB.trainnew$TenYearCHD<- ifelse(NB.trainnew$TenYearCHD==1,"yes", "No")
library(caret)

control <- trainControl(classProbs = T,
                        summaryFunction = twoClassSummary,
                        method="repeatedcv", number=10, 
                        repeats=3)
modellogtrainnew<- caret::train(TenYearCHD~.,data=NB.trainnew,
                             method="glm", family="binomial",
                             metric='ROC',trControl=control)


NB.trainnew$predlogclass<-predict(modellogtrainnew,
                                  NB.trainnew,
                             type="raw")

NB.trainnew$predlogscore<-predict(modellogtrainnew,
                                  NB.trainnew,
                                  type="raw")[,"1"]

caret::confusionMatrix(NB.trainnew$TenYearCHD,NB.trainnew$predlogclass)
view(NB.trainnew)
table(NB.trainnew$TenYearCHD,NB.trainnew$predlogclass)

#####neural network
str(datahrt)
install.packages("neuralnet")
library(neuralnet)

set.seed=800


##########        scaling the data ####################

datahrt.scaled<-datahrt
datahrt.scaled.factor<-datahrt.scaled[,-c(2,5,10:15)]
datahrt.scaled.numeric<-datahrt.scaled[,c(2,5,10:16)]

datahrt.scaled$TenYearCHD=as.factor(datahrt.scaled$TenYearCHD)
datahrt.scaled$Gender=as.factor(datahrt.scaled$Gender)
datahrt.scaled$currentSmoker=as.factor(datahrt.scaled$currentSmoker)
datahrt.scaled$education=as.factor(datahrt.scaled$education)
datahrt.scaled$BPMeds=as.factor(datahrt.scaled$BPMeds)
datahrt.scaled$prevalentHyp=as.factor(datahrt.scaled$prevalentHyp)
datahrt.scaled$prevalentStroke=as.factor(datahrt.scaled$prevalentStroke)
datahrt.scaled$diabetes=as.factor(datahrt.scaled$diabetes)
datahrt.scaledfinal<-datahrt.scaledfinal[,-9]

str(datahrt.scaledfinal1)
datahrt.scaled.numeric<- scale(datahrt.scaled.numeric, center=TRUE, scale=TRUE)
datahrt.scaled.numeric<-as.data.frame(datahrt.scaled.numeric)
datahrt.scaledfinal<-cbind(datahrt.scaled.numeric,datahrt.scaled.factor)


#datahrt.scaled.numeric1<-cbind(datahrt.scaled.numeric,x )
#str(datahrt.scaled.numeric1)

library(tidyverse) 
datahrt.scaledfinal1<-datahrt.scaledfinal %>% filter(complete.cases(.))

split<- sample.split(datahrt.scaledfinal1, SplitRatio = 0.8 )
datahrt.scaledfinal1.train<- subset(datahrt.scaledfinal1, split==TRUE)
datahrt.scaledfinal1.test<- subset(datahrt.scaledfinal1, split==FALSE)


str(datahrt.scaledfinal1.test)
str(nn1$net.result)
x=datahrt.scaledfinal1$TenYearCHD
xtrain=datahrt.scaledfinal1.train$TenYearCHD
xtest=datahrt.scaledfinal1.test$TenYearCHD
#datahrt.scaledfinal1.train<-datahrt.scaledfinal1.train[,-c(16:18)]

nn1<- neuralnet(formula=xtrain~.,
                data=datahrt.scaledfinal1.train,
              hidden=3,
              err.fct = "sse",
              linear.output = FALSE,
              lifesign = "full",
              lifesign.step = 10,
              threshold = 0.1,
              stepmax = 2000)
plot(nn1)
datahrt.scaledfinal1.train$prob<-nn1$net.result[[1]]
#head(datahrt.scaled.numeric2)
datahrt.scaledfinal1.train$class<-ifelse(datahrt.scaledfinal1.train$prob>0.5,1,0)
table(xtrain,datahrt.scaledfinal1.train$class)

(2291+49)/(2291+49+385+19)
str(datahrt.scaledfinal1.test)
datahrt.scaledfinal1.test<-datahrt.scaledfinal1.test[,-c(16)]
predneural<-predict(nn1,datahrt.scaledfinal1.test, type="class")
tab<-table(xtest, predneural>0.5)
table(xtest)
116/(116+798)
tab
head(predneural)
