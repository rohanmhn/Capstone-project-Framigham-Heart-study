#setwd("C:/Users/admin/Desktop/Desktop/BACP/Module 5/Week 1/LR")
library(tidyverse)
library(caret)
library(reshape2)
library(RColorBrewer)

LRdata = read.csv("Coronary_heart_risk_study.csv")
str(LRdata)
summary(LRdata)

############# EDA on the data #############
#display.brewer.all()
##  check that no datapoint is missing, otherwise we need to fix the dataset.
apply(LRdata,2,function(x) sum(is.na(x)))

### filter our NAs
LRdata<-LRdata %>% filter(complete.cases(.))


## list out all the numeric and categorical variables and their levels
split(names(LRdata),sapply(LRdata, function(x) paste(class(x), collapse=" ")))


## Check visual association pattern for continous predictor variables
library(GGally)
ggpairs(LRdata[, c("age","totChol",
                   "sysBP","diaBP",
                   "BMI","heartRate","glucose")],ggplot2::aes(colour = as.factor(LRdata$TenYearCHD)))

# contingency table of dicotomous variables with target variable
ct.data <- subset(LRdata, select = c(male,currentSmoker,
                                     BPMeds,prevalentStroke,
                                     prevalentHyp,diabetes))
# also create a subset of numeric data
num.data <- subset(LRdata, select = c(age,totChol,
                                      sysBP,diaBP,education,
                                      BMI,heartRate,glucose,cigsPerDay))


par(mfrow=c(2,3))
for (i in names(ct.data)) {
  print(i)
  print(table(LRdata$TenYearCHD, ct.data[[i]]))
  barplot(table(LRdata$TenYearCHD, ct.data[[i]]),
          col=c("grey","red"),
          main = names(ct.data[i]))
}
par(mfrow=c(1,1))

########### Chi sq test of catergorical data with target variable
ct.data2 <- cbind(ct.data, LRdata$TenYearCHD)
colnames(ct.data2)[7] <- "TenYearCHD"
ct.data2$TenYearCHD <- as.factor(ct.data2$TenYearCHD)
str(ct.data2)

# convert all columns to factors
fact_ct.data2 = ct.data2 %>% mutate_if(is.integer,funs(factor(.)))
str(fact_ct.data2)

### perform the chi sq test 
ChiSqStat <- NA
for ( i in 1 :(ncol(fact_ct.data2))){
  Statistic <- data.frame(
    "Row" = colnames(fact_ct.data2[7]),
    "Column" = colnames(fact_ct.data2[i]),
    "Chi SQuare" = chisq.test(fact_ct.data2[[7]], fact_ct.data2[[i]])$statistic,
    "df"= chisq.test(fact_ct.data2[[7]], fact_ct.data2[[i]])$parameter,
    "p.value" = chisq.test(fact_ct.data2[[7]], fact_ct.data2[[i]])$p.value)
  ChiSqStat <- rbind(ChiSqStat, Statistic)
}
ChiSqStat <- data.table::data.table(ChiSqStat)
ChiSqStat

# drop the variable currentsmoker
fact_ct.data2 <- subset(fact_ct.data2, select = -c(currentSmoker))

# run a sample model
fact.model <- glm(TenYearCHD~., data = fact_ct.data2, family = binomial)
summary(fact.model)

#wald.test(b = coef(fact.model), Sigma = vcov(fact.model))
regTermTest (fact.model, ~male)$p
regTermTest (fact.model, ~BPMeds)$p 
regTermTest (fact.model, ~prevalentStroke)$p
regTermTest (fact.model, ~prevalentHyp)$p 
regTermTest (fact.model, ~diabetes)$p 

### Few boxplots
boxplot(num.data,
        las=1,
        horizontal = TRUE,
        cex= 0.8,
        par(cex.axis = 0.8),
        col=brewer.pal(8,"Set1"),
        main = "Boxplots of continous variables")

# add the target variable to the numeric data for plotting
num.data2 <- cbind(num.data, LRdata$TenYearCHD)
colnames(num.data2)[10] <- "TenYearCHD"
num.data2$TenYearCHD <- as.factor(num.data2$TenYearCHD)
# stack the data using melt function
nd2.melt <- melt(num.data2, id = c("TenYearCHD"))

#  box plots
zz <- ggplot(nd2.melt, aes(x=TenYearCHD, y=value))
zz+geom_boxplot(aes(color = TenYearCHD), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()

#density/distrbution plots
yy <- ggplot(nd2.melt, aes(value, fill=variable))
yy+geom_density(aes(color = value) ) +
  facet_wrap(~variable,scales = "free")

# next step is to build univariate logistic regression models

#### build univariate logistic regression models and check results
mod.num <- glm(TenYearCHD~age, data = num.data2, family = binomial)
summary(mod.num)
# age is a significant variable
mod.num <- glm(TenYearCHD~totChol, data = num.data2, family = binomial)
summary(mod.num)
# total cholesterol is a sig. var
mod.num <- glm(TenYearCHD~education, data = num.data2, family = binomial)
summary(mod.num)

num.data2 <- subset(num.data2, select = -c(heartRate, TenYearCHD))

full.data <- cbind(num.data2, fact_ct.data2)
names(full.data)
names(LRdata)
# Now we can build the full model

# Split data into test and train datasets
set.seed(300)
library(caTools)
spl = sample.split(full.data$TenYearCHD, SplitRatio=0.65)
train = subset(full.data, spl ==T)
test = subset(full.data, spl==F)

## Check split consistency
#sum(as.integer(as.character(train$TenYearCHD))) / nrow(train)
sum(as.integer(as.character(train$TenYearCHD))) / nrow(train)
sum(as.integer(as.character(test$TenYearCHD))) / nrow(test)
sum(as.integer(as.character(full.data$TenYearCHD))) / nrow(full.data)

# build the model
LRmodel = glm(TenYearCHD ~ ., data = train, family= binomial)
summary(LRmodel)
car::vif(LRmodel)

confint(LRmodel)

anova(LRmodel, test = "Chisq")

predTest = predict(LRmodel, newdata= test, type="response")
table(test$TenYearCHD, predTest>0.5)
(1082+15)/nrow(na.omit(test))
library(ROCR)
ROCRpred = prediction(predTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
perf = performance(ROCRpred, "tpr","fpr")
plot(perf,col="black",lty=2, lwd=2)
plot(perf,lwd=3,colorize = TRUE)


library(blorr) # to build and validate binary logistic models
#blr_bivariate_analysis(fact_ct.data2, TenYearCHD,
#                       male, BPMeds,prevalentStroke,
#                       prevalentHyp,diabetes)

#blr_step_aic_forward(fact.model, details = TRUE)
blr_step_aic_both(LRmodel, details = FALSE)


final.model <- glm(TenYearCHD~age+sysBP+male+cigsPerDay+glucose+totChol+education, data = full.data, family = binomial)
summary(final.model)
predTest = predict(final.model, newdata= test, type="response")
table(test$TenYearCHD, predTest>0.5)
(1082+17)/nrow(na.omit(test))


k <- blr_gains_table(final.model)
plot(k)

blr_ks_chart(k, title = "KS Chart",
             yaxis_title = " ",xaxis_title = "Cumulative Population %",
             ks_line_color = "black")

blr_decile_lift_chart(k, xaxis_title = "Decile",
                      yaxis_title = "Decile Mean / Global Mean",
                      title = "Decile Lift Chart",
                      bar_color = "blue", text_size = 3.5,
                      text_vjust = -0.3)

blr_decile_capture_rate(k, xaxis_title = "Decile",
                        yaxis_title = "Capture Rate",
                        title = "Capture Rate by Decile",
                        bar_color = "blue", text_size = 3.5,
                        text_vjust =-0.3)

blr_confusion_matrix(final.model, data = test)

blr_gini_index(fact.model, data = fact_ct.data2)

blr_roc_curve(k, title = "ROC Curve",
              xaxis_title = "1 - Specificity",
              yaxis_title = "Sensitivity",roc_curve_col = "blue",
              diag_line_col = "red", point_shape = 18,
              point_fill = "blue", point_color = "blue",
              plot_title_justify = 0.5)  

blr_rsq_mcfadden(final.model)
blr_rsq_mcfadden_adj(final.model)


blr_plot_difchisq_fitted(final.model, point_color = "blue",
                         title = "Delta Chi Square vs Fitted Values Plot",
                         xaxis_title = "Fitted Values",
                         yaxis_title = "Delta Chi Square")
