##### ******** Mortgage and Home Sales Data ******** #####

###In this code, I'll perform regression analysis, along with applying a 
#false discovery rate algorithm, to predict housing prices

#Exploratory analysis

#I'll regress log price onto all variables but mortgage.

homes <- read.csv("homes2004.csv")

#View(homes)

reg1 <- lm(log(LPRICE) ~ . - AMMORT, data=homes)
summary(reg1)

pval <- summary(reg1)$coefficients[,4]
View(pval)

#Regressing log(LPRICE) on to all variables but mortgage yields an R-squared of 0.4473.  
#In this model, 42 coefficients (including the intercept) are used.  

#Plot p-values
hist(pval,col="lightblue",breaks=10)

pval_ordered<-pval[order(pval,decreasing=F)]
plot(pval_ordered,pch=19)
abline(0,1/42)

source("fdr.R")
cutoff_reg1 <- fdr_cut(pval,0.10)
cutoff_reg1

plot(pval_ordered,pch=19)
abline(h=cutoff_reg1,lty=2,col=3,lwd=3)
abline(0,.1/42,col=2,lwd=2) #.10/42 corresponds to q/p

signif <- pval_ordered <= cutoff_reg1

points(pval_ordered,
	   col=signif+1,pch=19) # The red dots are discoveries

table(pval_ordered<=cutoff_reg1) # number of discoveries and non-discoveries

(1:42)[pval_ordered<=cutoff_reg1]

t<-table(pval_ordered<=cutoff_reg1)
t

#At 10% FDR (False Discovery Rate), 37 coefficients (including the intercept) are significant.  

pval_ordered$signif_ind <- ifelse(pval_ordered<=cutoff_reg1,1,0)
View(pval_ordered)

homes2 <- homes

homes2$STATEGA <- ifelse(homes$STATE == 'GA',1,0)
homes2$STATEIL <- ifelse(homes$STATE == 'IL',1,0)
homes2$STATEIN <- ifelse(homes$STATE == 'IN',1,0)
homes2$STATELA <- ifelse(homes$STATE == 'LA',1,0)
homes2$STATEMO <- ifelse(homes$STATE == 'MO',1,0)
homes2$STATEOH <- ifelse(homes$STATE == 'OH',1,0)
homes2$STATEOK <- ifelse(homes$STATE == 'OK',1,0)
homes2$STATEPA <- ifelse(homes$STATE == 'PA',1,0)
homes2$STATETX <- ifelse(homes$STATE == 'TX',1,0)
homes2$STATEWA <- ifelse(homes$STATE == 'WA',1,0)
homes2$STATEWA <- ifelse(homes$STATE == 'WA',1,0)

homes3 = subset(homes2,select = -c(ETRANS,STATE,NUNITS,BEDRMS))
#Remove the variables that are not statistically significant per FDR algorithm

names(homes3)

reg2 <- lm(log(LPRICE) ~ . - AMMORT, data=homes3)
summary(reg2)

#Next, I will fit a regression for whether the buyer had more than 20 percent down,
#and will interpret the regression output

View(homes)

percent_down <- (homes$LPRICE - homes$AMMORT)/homes$LPRICE
View(percent_down)

homes_pdown <- homes
homes_pdown$pdown_gt20 <- ifelse(percent_down > 0.20,1,0)
View(homes_pdown)

reg_20d <- glm(pdown_gt20 ~ . - LPRICE - AMMORT,data=homes_pdown, family=binomial)
summary(reg_20d)

#The coefficient on FRSTHOY is -0.3700, which is statistically significant.  
#As such, we expect a .3700 decrease in the log-odds of the dependent variable (pdown_gt20) 
#for first home buyers, holding all other independent variables constant.  
#This is as expected – first home buyers are likely to be less affluent, 
#and therefore less likely to put 20% down on their first home purchase.  
#Lastly, the coefficient on BATHS is 0.2445, which is statistically significant.  
#We expect a 0.2445 increase in the log-odds of the dependent variable for each additional 
#bathroom in the home, holding all other independent variables constant.  
#This is as expected – individuals that purchase homes with more bathrooms tend to be wealthier, 
#and therefore more financially able to make a 20% down payment.  

#Next, I'll add an interaction term between first home buyers and the number of bathrooms

reg_20d_ind <- glm(pdown_gt20 ~ . - LPRICE - AMMORT + BATHS:FRSTHO,data=homes_pdown, family=binomial)
summary(reg_20d_ind)

#The logistic regression with the interaction term demonstrates that first home buyers become less likely 
#to make a 20% as the price of the home (measured by number of bathrooms) increases, which is 
#consistent with intuition.

#Now, I'll focus on homes that are worth more than 100k.
#I'll train the model on homes more than 100k (training sample), and will apply the model to homes that are worth less
#than 100k (validation sample)

homes <- read.csv("homes2004.csv")

subset <- which(homes$VALUE>100000)

homes_training <- subset(homes,homes$VALUE>100000)

reg_training <- lm(log(LPRICE) ~ . - AMMORT, data=homes_training)

homes_validation <- subset(homes,homes$VALUE<=100000)

pred_lte100k <- predict(reg_training,newdata=homes_validation)

predicted <- as.data.frame(pred_lte100k)
View(predicted)
predicted2 <- predicted$pred_lte100k

source("deviance.R")

# Null model has just one mean parameter

ybar <- mean(log(homes_validation$LPRICE))

D0 <- deviance(y=log(homes_validation$LPRICE), pred=ybar)
D <- deviance(y=log(homes_validation$LPRICE), pred=pred_lte100k)

R_squared_OOS <- (D0 - D) / D0

R_squared_OOS

#The R^2 is negative (-0.04988871).  Clearly, a model trained on housing prices above $100,000
#is not suitable for predicting housing prices on homes below $100,000

actual_val <- log(homes_validation$LPRICE)
par(mfrow=c(1,1))
plot(actual_val,predicted2,xlab="Actual Price",ylab="Predicted Price")
abline(h=ybar)

#Houses above/below $100k have very different characteristics.  As such, it is not appropriate to 
#use houses above $100k to model the log(price) of houses below $100k.  Because of this inappropriate 
#training/validation construct, we arrive at a negative R-squared.