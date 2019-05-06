##################################################################################
#DATA DESCRIPTION                                                                #
#These data come from Appendix 1 of Hosmer and Lemeshow (1989), and were         #	
#collected at Baystate Medical Center, Springfield MA, during 1986. 		         #
#Low birth weight is an outcome that has been of concern to physicians	     	   #
#for years. This is due to the fact that infant mortality rates and birth	       #	
#defect rates are very high for low birth weight babies. A woman's behavior 	   #		
#during pregnancy (including diet, smoking habits, and receiving prenatal care)  #				
#can greatly alter the chances of carrying the baby to term and, consequently,   #
#of delivering a baby of normal birth weight. 			                             #
##################################################################################


##################################################################################
#DATA DICTIONARY                                                                 #
#Variable Name	  Description                                                    #
#id	              Patient ID                                                     #
#lbw	            Indicator of low birth weight (< 2500 g); 0=no; 1=yes          #
#age	            Age of mother in years                                         # 
#lwt	            Weight of mother (in pounds) at last menstrual cycle           #
#race_eth	        Race/ethnicity of mother (1=white; 2=black; 3=other)           #
#smoke	          Smoking status during pregnancy (0=no; 1=yes)                  #
#ptl	            History of premature Labor (0=no; 1=yes)                       #
#hyper	          History of hypertension (0=no; 1=yes)                          #
#urirr	          Presence of uterine irritability (0=no; 1=yes)                 # 
#pvft	            Number of physician visits during first trimester              #
#bwt	            Child's birthweight in grams                                   #
##################################################################################

#Load data into R - you have to know the directory where the data are!
load("~/Work Documents/STATCORE/lowbwt.RData")

#Alternate way to create data from Excel spreadsheet
install.packages("readxl")
library(readxl)
lowbwt <- data.frame(read_excel("~/Work Documents/STATCORE/Low Birthweight.xlsx"))

###########################################################
#Two functions:                                           #
#(1) compute AUC of a fitted logistic regression model    #
#(2) make ROC curve of a fitted logistic regression model #
###########################################################
get_auc <- function(mod)
{
  auc <- wilcox.test(fitted(mod)~mod$y)$statistic/prod(table(mod$y))
  auc <- pmax(auc, 1-auc)
  as.numeric(auc)
}

make_roc <- function(mod)
{
  a <- fitted(mod)
  b <- mod$y
  aval <- rev(sort(unique(a)))
  sens <- sapply(aval, function(cutpt, test, disease)
    mean(test[disease==1]>cutpt, na.rm=T), test=a, disease=b)
  spec <- sapply(aval, function(cutpt, test, disease)
    mean(test[disease==0]<=cutpt, na.rm=T), test=a, disease=b)
  x <- c(0, 1-spec, 1)
  y <- c(0, sens, 1)
  plot(0:1, 0:1, ylab="True Positive Rate", xlab="False Positive Rate", pch=" ", 
       cex.axis=1.3, cex.lab=1.3, mgp=c(2.7, 0.7, 0), bty="n")
  lines(x, y, cex=1.2, col="blue3", pch=20, lwd=1.5, type="b")
  abline(a=0, b=1, lty="dashed", lwd=1.5, col="gray50")
}

######################################################
#Simple Logistic Regression with Continuous Predictor#
######################################################

#Fit simple logistic regression model; save results
mymod1 <- glm(lbw~lwt, data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod1)

#Get AUC of fitted model
get_auc(mymod1)

#Make ROC of fitted model
make_roc(mymod1)

#######################################################
#Simple Logistic Regression with Categorical Predictor#
#(Dummy Variables)                                    #
#######################################################

#Fit simple logistic regression model; save results
mymod2 <- glm(lbw~urirr, data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod2)

#Get AUC of fitted model
get_auc(mymod2)

#Make ROC of fitted model
make_roc(mymod2)

##############################################################
#Multiple Logistic Regression - Two Variables; No Interaction#
##############################################################
#Fit a multivariate regression model without an interaction
mymod3 <- glm(lbw~lwt+urirr, data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod3)

#Get AUC of fitted model
get_auc(mymod3)

#Make ROC of fitted model
make_roc(mymod3)

###########################################################
#Multiple Logistic Regression - Two Variables; Interaction#
###########################################################
#Fit a multivariate regression model with an interaction
mymod4 <- glm(lbw~lwt*urirr, data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod4)

#Get AUC of fitted model
get_auc(mymod4)

#Make ROC of fitted model
make_roc(mymod4)

########################################################
#Multiple Logistic Regression - More than Two Variables#
########################################################
mymod5 <- glm(lbw~lwt+urirr+hyper, data=lowbwt, family=binomial)
summary(mymod5)

#Get AUC of fitted model
get_auc(mymod5)

#Make ROC of fitted model
make_roc(mymod5)

#########################################################
#Multiple Logistic Regression - Backward Model Selection#
#########################################################
mymod6 <- glm(lbw~age+lwt+smoke+ptl+hyper+urirr+pvft, data=lowbwt, family=binomial)
mymod6a <- step(mymod6, direction="backward")
summary(mymod5a)

#Get AUC of fitted model
get_auc(mymod6a)

#Make ROC of fitted model
make_roc(mymod6a)

################################################
#Multiple Logistic Regression - Quadratic Model#
################################################
lwt2 <- lowbwt$lwt^2
mymod7 <- glm(lbw~lwt+lwt2, data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod7)

#Get AUC of fitted model
get_auc(mymod7)

#Make ROC of fitted model
make_roc(mymod7)

################################################
#Multiple Logistic Regression - Dummy Variables# 
#For Predictor with More than Two Categories   # 
################################################
mymod8 <- glm(lbw~as.factor(race_eth), data=lowbwt, family=binomial)

#Examine coefficent table and summary 
summary(mymod8)

#Get AUC of fitted model
get_auc(mymod8)

#Make ROC of fitted model
make_roc(mymod8)

