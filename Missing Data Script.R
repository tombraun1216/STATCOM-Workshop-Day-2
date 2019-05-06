#####################################################################
#Recall the results of the linear regression model                  
#we created yesterday using backward model selection:               
#                                                                   
#Call:                                                            
# lm(formula = bwt ~ lwt + smoke + hyper + urirr, data = lowbwt)  
#Coefficients:                                                    
#              Estimate   Std. Error  t value  Pr(>|t|)             
#(Intercept)   2575.77        226.82   11.356   < 2e-16 ***         
#lwt              4.51          1.66    2.716  0.007231 **          
#smoke         -240.08        100.14   -2.397  0.017508 *           
#hyper         -649.27        206.35   -3.146  0.001928 **          
#urirr         -548.92        139.44   -3.937  0.000117 ***         
#                                                                   
#Multiple R-squared:  0.1735,	Adjusted R-squared:  0.1555         
#####################################################################

#install.packages("mice")
library(mice)

#Examine all unique missingness patterns in data
md.pattern(lowbwt_miss)

#Impute missing values using predictive mean matching
my_imp_val <- mice(lowbwt_miss, m=10)

#Examine imputed values of birthweight
my_imp_val$imp$bwt

#Fit a the same linear regression model to each imputed dataset
mymod_imp <- with(my_imp_val, lm(bwt~lwt+smoke+hyper+urirr))

#Examine the results of the first imputation
summary(mymod_imp$analyses[[1]])

#Pool results among all imputations
summary(pool(mymod_imp))

#Computed pooled R-squared
pool.r.squared(mymod_imp)

#Instead do a complete case analysis (exclude anyone with at least one missing value)
#R will automatically exclude rows where missing data exist
mymod_cc <- lm(bwt~lwt+smoke+hyper+urirr, data=lowbwt_miss) 
summary(mymod_cc)
