

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
#setwd("C:/Users/David/Documents/Data")
setwd("C:\\Users\\bucka\\OneDrive\\Desktop\\R\\A6")
options(scipen=9)


##################################################
### Install Libraries                           ##
##################################################

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

if(!require(ggplot2)){install.packages("ggplot2")}
library("ggplot2")


##################################################
### Read data and do preliminary data checks    ##
##################################################

Autism_SB <- read.csv("AutismClass.csv", header = TRUE, sep = ",")

#View(Autism_SB)

Autism_SB$Autism_Bin<-as.numeric(Autism_SB$Autism)
Autism_SB$Autism_Bin<-Autism_SB$Autism_Bin-1

Nationality_Dummies_SB <- model.matrix(~Nationality -1, data=Autism_SB)
#View(Nationality_Dummies_SB)
Rel_Dummies_SB<-model.matrix(~Rel -1, data=Autism_SB)
#View(Rel_Dummies_SB)
Autism_SB<-cbind(Autism_SB,Rel_Dummies_SB)
Autism_SB <- cbind(Autism_SB,Nationality_Dummies_SB)


#removing case ID variable, it's just to uniquely identify a case, insignificant to Autism  
#removing Nationality, Rel and Autism variables
#Nationality dummy variables and Autism dummy variables have been merged
#Binary equivalent of the original Autism has been created 
Autism_SB<-Autism_SB[-c(1,15,16,17)]   

names(Autism_SB) <- c("A01","A02","A03","A04","A05","A06","A07",
                      "A08","A09","A10","Age","Gender","Jaundic",
                      "Autism_Bin","HealthCareProf","Others",
                      "Parent","Relative","Self","African",
                      "Asian","European","LatinAmerica",
                      "MiddleEastern","NorthAmerican")


###################################################
## Univariate Descriptive Analysis               ##
###################################################

summary(Autism_SB)
stat.desc(Autism_SB)
hist(Autism_SB[["Age"]], main="Age")

str(Autism_SB)
###################################################
## Find Outliers                                 ##
###################################################

boxplot(Autism_SB[["Age"]], main="Age")

head(sort(Autism_SB$Age,decreasing = T),20)

Autism_SB[Autism_SB$Age>61,'Age']<-61

head(sort(Autism_SB$Age,decreasing = T),20)
str(Autism_SB$Age)

##Check Again

boxplot(Autism_SB[["Age"]], main="Age")
# Histogram for all variables
###################################################
## Comparing Correlation of Predictors           ##
## All                                           ##
###################################################


corrgram(Autism_SB,order=TRUE,lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Autism Factors correlation")

res <- cor(Autism_SB, method="spearman")
round(res, 2)
#others self -0.66
#parent self -0.48
#A3 A4 0.41
#A4 A5 0.33
#A5 A6 0.41
#self relative -0.34
#MiddleEastern NorthAmerican -0.25
###################################################
## Comparing Outcome with Predictors             ##
###################################################

###################################################
## Comparing Outcome with Predictors             ##
## Categorical Data                              ##
###################################################

cross <- table(Autism_SB$Autism_Bin, Autism_SB$Gender)

barplot(prop.table(cross,2), xlab='Class',ylab='Frequency',main="Autism prevelance by gender",
col=c("darkblue","darkred"),legend=rownames(cross),args.legend = list(x = "bottom"))

table(Autism_SB$Gender,Autism_SB$Autism_Bin)   #Contingency Table
prop.table(table(Autism_SB$Gender,Autism_SB$Autism_Bin), margin=1)*100  #Contingency Table Pct.
summary(table(Autism_SB$Gender,Autism_SB$Autism_Bin))   #Chi-Sq
chisq.test(Autism_SB$Gender,Autism_SB$Autism_Bin)       #Chi-Sq - specific

##
cross <- table(Autism_SB$Autism_Bin, Autism_SB$A05)

barplot(prop.table(cross,2), xlab='Class',ylab='Frequency',main="Autism in two A05 cases",
        col=c("darkblue","darkred"),legend=rownames(cross),args.legend = list(x = "bottom"))

table(Autism_SB$Gender,Autism_SB$A05)   #Contingency Table
prop.table(table(Autism_SB$A05,Autism_SB$Autism_Bin), margin=1)*100  #Contingency Table Pct.
summary(table(Autism_SB$A05,Autism_SB$Autism_Bin))   #Chi-Sq
chisq.test(Autism_SB$A05,Autism_SB$Autism_Bin)       #Chi-Sq - specific


###################################################
## Comparing Outcome with Predictors             ##
## Continuous                                    ##
###################################################


###Box Plots by variable

boxplot(Age~Autism_Bin, data=Autism_SB, xlab="Autism prevelance", main="Age") 

#########################################
## Creating Baseline Model             ##
#########################################


Autism_glm_SB = glm(Autism_Bin ~A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+
                      Age+Gender+Jaundic+HealthCareProf+Others+Parent+
                      Relative+Self+African+Asian+European+LatinAmerica+
                      MiddleEastern+NorthAmerican,
             family="binomial", data=Autism_SB, na.action=na.omit)
Autism_glm_SB
summary(Autism_glm_SB)

#Step model

Autism_Step_glm_SB = step(Autism_glm_SB)
Autism_Step_glm_SB
summary(Autism_Step_glm_SB)

#forward selection
# the forwards selection models yields a different model compared to the step selection model
min_model <- glm(Autism_Bin ~ 1, data=Autism_SB, na.action=na.omit)
Autism_Fwd_glm_SB = step(min_model, direction="forward", scope =(
  ~ A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+Age+Gender+Jaundic+HealthCareProf+
    Others+Parent+Relative+Self+African+Asian+European+LatinAmerica+MiddleEastern+
    NorthAmerican), details=TRUE)

summary(Autism_Fwd_glm_SB)




#the model below was obtained by manually eliminating 
#paramters based on the z-value in each iteration 
#and performing the elimination step after each iterations
#untill only significant variables remained

Autism_Model_SB = glm(Autism_Bin ~A01+A02+A03+A05+A07+A08+
                      Gender+Parent+
                      African+Asian+LatinAmerica+
                      MiddleEastern,
                    family="binomial", data=Autism_SB, na.action=na.omit)
Autism_Model_SB
summary(Autism_Model_SB)





#########################################
## Evaluating Logistic Model           ##
#########################################

### Confusion Matrix  ####
# for stepwise selection model  
#NOT THE FORWARD SELECTION MODEL BUT THE STEPWISE SELECTION MODEL
pred <- predict(Autism_Step_glm_SB, Autism_SB)
pred_y <- as.numeric(pred > -.5)
true_y <- as.numeric(Autism_SB$Autism_Bin==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)

conf_mat_step_SB <- matrix(c(sum(true_pos), sum(false_pos),
                             sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat_step_SB) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_step_SB) <- c('Y = 1', 'Y = 0')
conf_mat_step_SB
### Precision, Recall, Specificity and Accuracy ###
#for stepwise selection model
Autism_Step_glm_SB$Precis <- conf_mat_step_SB[1,1]/sum(conf_mat_step_SB[,1])
Autism_Step_glm_SB$Recall <- conf_mat_step_SB[1,1]/sum(conf_mat_step_SB[1,])
Autism_Step_glm_SB$Specif <- conf_mat_step_SB[2,2]/sum(conf_mat_step_SB[2,])
Autism_Step_glm_SB$Accuracy<-(conf_mat_step_SB[2,2]+conf_mat_step_SB[1,1])/
  (conf_mat_step_SB[1,1]+conf_mat_step_SB[1,2]+conf_mat_step_SB[2,1]+conf_mat_step_SB[2,2])

Autism_Step_glm_SB$Precis
Autism_Step_glm_SB$Recall
Autism_Step_glm_SB$Specif
Autism_Step_glm_SB$Accuracy

### ROC Curve ###

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
AUC


#################################################################
#for the iterative obtained model

pred <- predict(Autism_Model_SB, Autism_SB)
pred_y <- as.numeric(pred > -.5)
true_y <- as.numeric(Autism_SB$Autism_Bin==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)

conf_mat_SB <- matrix(c(sum(true_pos), sum(false_pos),
                             sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat_SB) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_SB) <- c('Y = 1', 'Y = 0')
conf_mat_SB

Autism_Model_SB$Precis <- conf_mat_SB[1,1]/sum(conf_mat_SB[,1])
Autism_Model_SB$Recall <- conf_mat_SB[1,1]/sum(conf_mat_SB[1,])
Autism_Model_SB$Specif <- conf_mat_SB[2,2]/sum(conf_mat_SB[2,])
Autism_Model_SB$Accuracy<-(conf_mat_SB[2,2]+conf_mat_SB[1,1])/(conf_mat_SB[1,1]+conf_mat_SB[1,2]+
                                                              conf_mat_SB[2,1]+conf_mat_SB[2,2])

Autism_Model_SB$Precis
Autism_Model_SB$Recall
Autism_Model_SB$Specif
Autism_Model_SB$Accuracy

### ROC Curve ###

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

### AUC ###

AUC <- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
AUC
