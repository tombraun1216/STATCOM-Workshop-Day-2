##################################################################################
#DATA DESCRIPTION                                                                #
#These data come from Appendix 1 of Hosmer and Lemeshow (1989), and were         #	
#collected at Baystate Medical Center, Springfield MA, during 1986. 		   #
#Low birth weight is an outcome that has been of concern to physicians		   #
#for years. This is due to the fact that infant mortality rates and birth	   #	
#defect rates are very high for low birth weight babies. A woman's behavior	   #		
#during pregnancy (including diet, smoking habits, and receiving prenatal care)  #				
#can greatly alter the chances of carrying the baby to term and, consequently,   #
#of delivering a baby of normal birth weight. 			                     #
##################################################################################


##################################################################################
#DATA DICTIONARY                                                                 #
#Variable Name	  Description                                                  #
#id	              Patient ID                                                   #
#lbw	              Indicator of low birth weight (< 2500 g); 0=no; 1=yes        #
#age	              Age of mother in years                                       # 
#lwt	              Weight of mother (in pounds) at last menstrual cycle         #
#race_eth	        Race/ethnicity of mother (1=white; 2=black; 3=other)         #
#smoke	        Smoking status during pregnancy (0=no; 1=yes)                #
#ptl	              History of premature Labor (0=no; 1=yes)                     #
#hyper	        History of hypertension (0=no; 1=yes)                        #
#urirr	        Presence of uterine irritability (0=no; 1=yes)               # 
#pvft	              Number of physician visits during first trimester            #
#bwt	              Child's birthweight in grams                                 #
##################################################################################

#Load data into R - you have to know the directory where the data are!
load("~/Work Documents/STATCORE/lowbwt.RData")

#Alternate way to create data from Excel spreadsheet
install.packages("readxl")
library(readxl)
lowbwt <- data.frame(read_excel("~/Work Documents/STATCORE/Low Birthweight.xlsx"))

#Examine first 10 rows of data
head(lowbwt, 10)

####################################################
#Simple Linear Regression with Continuous Predictor#
####################################################

#Make a scatterplot of birthweight versus mother's weight at last menstrual cycle
plot(lowbwt$lwt, lowbwt$bwt)

#Make a nicer version of scatterplot
plot(lowbwt$lwt, lowbwt$bwt, ylab="Infant Birthweight (g)", 
     xlab="Weight of Mother at Last Menstrual Cycle (lbs)",
     pch=20, col="blue3", cex=1.5, mgp=c(2.5, 1, 0))

#Add least-squares line
abline(lsfit(lowbwt$lwt, lowbwt$bwt), lwd=2, lty="dashed")

#Fit simple linear regression model; save results
mymod1 <- lm(bwt~lwt, data=lowbwt)

#Examine coefficent table and summary 
summary(mymod1)

#Make histogram of residuals
hist(resid(mymod1), col="blue3", main="", xlab="Residual Value", ylim=c(0, 50))

#Compare actual and fitted values
head(data.frame(lowbwt$lwt, lowbwt$bwt, fitted(mymod1)), 10)

#####################################################
#Simple Linear Regression with Categorical Predictor#
#(Dummy Variables)                                  #
#####################################################

#Make a scatterplot of birthweight versus mother's presence of uterine irritability
plot(lowbwt$urirr, lowbwt$bwt, ylab="Infant Birthweight (g)", 
     xlab="Presence of Uterine Irritability",
     pch=20, col="blue3", cex=1.5, mgp=c(2.5, 1, 0))
#Add least-squares line
abline(lsfit(lowbwt$urirr, lowbwt$bwt), lwd=2, lty="dashed")

#View data using boxplots
boxplot(lowbwt$bwt~lowbwt$urirr, names=c("No", "Yes"), ylab="Infant Birthweight (g)", 
        xlab="Presence of Uterine Irritability", col=c("blue3", "gold3"))

#Fit simple linear regression model; save results
mymod2 <- lm(bwt~urirr, data=lowbwt)

#Examine coefficent table and summary 
summary(mymod2)

#Compute mean birthweight stratified by presence/absence of uterine irritability
mean(lowbwt$bwt[lowbwt$urirr==0])
mean(lowbwt$bwt[lowbwt$urirr==1])

#Perform t-test instead of linear regression - compare to results of regression model
t.test(lowbwt$bwt~lowbwt$urirr, var.equal=T)

############################################################
#Multiple Linear Regression - Two Variables; No Interaction#
############################################################
#Fit a multivariate regression model without an interaction
mymod3 <- lm(bwt~lwt+urirr, data=lowbwt)

#Examine coefficent table and summary 
summary(mymod3)

#Make a scatterplot corresponding to the no-interaction model
plot(lowbwt$lwt, lowbwt$bwt, ylab="Infant Birthweight (g)", 
     xlab="Weight of Mother at Last Menstrual Cycle (lbs)",
     pch=20, col="blue3", cex=1.5, mgp=c(2.5, 1, 0))
ok <- lowbwt$urirr==1
points(lowbwt$lwt[ok], lowbwt$bwt[ok], pch=18, col="gold3", cex=1.6)
legend(200,5000, legend=c("UI Absent", "UI Present"), bty="n", fill=c("blue3", "gold3"))
abline(a=coef(mymod3)[1]+coef(mymod3)[3], b=coef(mymod3)[2], lwd=2, lty="dashed", col="gold3")
abline(a=coef(mymod3[1]), b=coef(mymod3)[2], lwd=2, lty="dashed", col="blue3")

#########################################################
#Multiple Linear Regression - Two Variables; Interaction#
#########################################################
#Fit a multivariate regression model with an interaction
mymod4 <- lm(bwt~lwt*urirr, data=lowbwt)

#Examine coefficent table and summary 
summary(mymod4)

#Make a scatterplot corresponding to the interaction model
plot(lowbwt$lwt, lowbwt$bwt, ylab="Infant Birthweight (g)", 
     xlab="Weight of Mother at Last Menstrual Cycle (lbs)",
     pch=20, col="blue3", cex=1.5, mgp=c(2.5, 1, 0))
ok <- lowbwt$urirr==1
points(lowbwt$lwt[ok], lowbwt$bwt[ok], pch=18, col="gold3", cex=1.6)
legend(200,5000, legend=c("UI Absent", "UI Present"), bty="n", fill=c("blue3", "gold3"))
abline(a=coef(mymod4)[1]+coef(mymod4)[3], b=coef(mymod4)[2]+coef(mymod4)[4], 
       lwd=2, lty="dashed", col="gold3")
abline(a=coef(mymod4[1]), b=coef(mymod4)[2], lwd=2, lty="dashed", col="blue3")

######################################################
#Multiple Linear Regression - More than Two Variables#
######################################################
mymod5 <- lm(bwt~lwt+urirr+hyper, data=lowbwt)
summary(mymod5)

#######################################################
#Multiple Linear Regression - Backward Model Selection#
#######################################################
mymod6 <- lm(bwt~age+lwt+smoke+ptl+hyper+urirr+pvft, data=lowbwt)
mymod6a <- step(mymod6, direction="backward")
summary(mymod6a)

##############################################
#Multiple Linear Regression - Quadratic Model#
##############################################
lwt2 <- lowbwt$lwt^2
mymod7 <- lm(bwt~lwt+lwt2, data=lowbwt)

#Examine coefficent table and summary 
summary(mymod7)

#Visually compare linear and quadratic models                                                                                                       
plot(lowbwt$lwt, lowbwt$bwt, ylab="Infant Birthweight (g)", 
     xlab="Weight of Mother at Last Menstrual Cycle (lbs)",
     pch=20, col="blue3", cex=1.5, mgp=c(2.5, 1, 0))
points(lowbwt$lwt, fitted(mymod1), pch=20, col="gray75")
points(lowbwt$lwt, fitted(mymod7), pch=20)

##############################################
#Multiple Linear Regression - Dummy Variables# 
#For Predictor with More than Two Categories # 
##############################################
mymod8 <- lm(bwt~as.factor(race_eth), data=lowbwt)

#Examine coefficent table and summary 
summary(mymod8)
