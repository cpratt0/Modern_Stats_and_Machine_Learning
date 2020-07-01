#######In this code, we'll explore currency exchange rates,
#######and apply factor models (e.g., PCA) to generate insights 
#######from the data

#First, we'll examine the correlation structure among the FX rates

fx <- read.csv("FXmonthly.csv",header=T)
fx <- (fx[2:120,]-fx[1:119,])/(fx[1:119,])
dim(fx)
View(fx)
names(fx)
dater <- row.names(fx)
dater
fxcor = cor(fx)
View(fxcor)
#write.csv(fxcor,"fxcor.csv")

#The correlation among dimensions of FX are high. 
#As seen in correlation matrix, many FX rates are observed to
#have high correlations with other rates.

#Datasets with regressors highly correlated to each other 
#are not ideal for LASSO as LASSO randomly picks one of 
#the highly correlated variables to fit in the model. 
#However, this type of dataset is a good candidate for factor modeling, 
#where latent factors (lurking variables) are
#created to capture essence of regressors, resulting in much 
#fewer variables are needed in the final regression (i.e. parsimonious model).

#Now, we'll Fit, plot, and interpret principal components.
pcafx <- prcomp(fx,scale=TRUE)

plot(pcafx,main="",xlab="FX Principle Components")
summary(pcafx)

#The first two principle components appear to be the most important, by far.
#The first 12 components explain ~92% of the variance.

fxpc <- predict(pcafx)

plot(fxpc[,1:2])
plot(fxpc[,3:4])

pca1min <- fxpc[order(fxpc[,1])[1:10],1]
pca1max <- fxpc[order(-fxpc[,1])[1:10],1]

pca1min
pca1max

#One end of the PC1 spectrum (pca1min) corresponds to dates that 
#are generally in 2008 and 2010. The other end of the PC1 spectrum 
#(pca1max) corresponds to dates that are generally in 2007 through 2010.
#Considering that the fx data set measures FX rates with the US Dollar, 
#we would initially suspect that PC1 is a measure of the US Dollar’s 
#strength (namely, the strength of returns on the US Dollar index).

PC1 <- fxpc[,1]
View(PC1)

#write.csv(PC1,"PC1.csv")

#In Excel, we plotted "Trade Weighted U.S. Dollar Index" (TWEXB in FRED)
#against the first principle component - we see a very strong relationship
#(correlation = -0.96).  As such, we affirm that PC1 corresponds to 
#strength of the US dollar

pca2min <- fxpc[order(fxpc[,2])[1:10],2]
pca2max <- fxpc[order(-fxpc[,2])[1:10],2]

pca2min
pca2max

PC2 <- fxpc[,2]
View(PC2)

#write.csv(PC2,"PC2.csv")

View(fx)
View(dater)
dater
plot(factor(dater),fxpc[,1])
plot(factor(dater),fxpc[,2])

par(mfrow=c(1,1))
sp  <- read.csv("sp500.csv",header=T)
View(sp)

loadings <- pcafx$rotation[,2]
loadings

#write.csv(loadings,"loadings.csv")

#Mexico-US, Brazil-US, India-US, Korea-US, Japan-US, 
#Switzerland-US, Hong-Kong-US, Euro-US, and Denmark-US exchange 
#rates have the highest loadings. The interpretation is not 
#transparent to us.


#For the next step in our analysis, we will 
#regress SP500 returns onto currency movement factors, 
#using both glm on first K and lasso techniques, and  
#use the results to add to our factor interpretation.

library(Matrix)
library(gamlr) # to get AICc, plus we'll do lasso below

sp500 <- sp$sp500
zfx <- predict(pcafx)
zdf0 <- as.data.frame(zfx)
dim(zdf0)
dim(as.data.frame(sp500))
kfits <- lapply(1:23, 
	function(K) glm(sp500~., data=zdf0 [,1:K,drop=FALSE]))

bic <- sapply(kfits, BIC) 
which.min(bic) ## likes 3

aicc <- sapply(kfits, AICc)
which.min(aicc) ## likes 3

plot(aicc,ylab="AICc",xlab="K",col="red",pch=19)
#View(aicc)
#Based on this plot, the AICc is minimized where K = 3.

#Now, we will use LASSO.
lassoz <- cv.gamlr(x=zdf0, y=sp500)
coef(lassoz,select="min") 
plot(lassoz)

#The model that results in the minimum mean squared error has
#14 coefficients: the intercept, a coefficient for PC1, PC2, PC3, 
#PC5, PC10, PC13,PC15, PC16, PC17, PC18, PC20, PC21, and PC23. 
#Note: randomness may be introduced to the coefficients via the 
#cross validation procedure. This is in agreement with our glm on 
#the first K principal components: we observe a dip in AICc at K = 2,
#and K = 3 - LASSO included those variables in the regression. 
#Additionally, we see large dips in AICc at K = 17 and K = 20, K = 21, 
#and K = 23. Once more, LASSO includes these principle component variables.

lassoz1 <- gamlr(x=zdf0,y=sp500)
plot(lassoz1)

allloadings <- pcafx$rotation[,1:23]
allloadings
write.csv(allloadings,"allloadings.csv")

#Upon inspection of the loadings, the top 10 traded currencies 
#in the world are represented, and in at least one of the principle
#components, each currency has a considerable factor loading. 
#This is intuitive: the state of the economy (measured by returns in S&P 500) 
#is consistent with the health of its strongest currencies.


#Next, we'll fit a LASSO to the original covariates and 
#describe how it differs from PCR here.

View(fx)
dim(fx)

lassoorig <- cv.gamlr(x=fx,y=sp500)
plot(lassoorig)

lassoo <- gamlr(x=fx,y=sp500)
lassooi <- gamlr(x=fx,y=sp500)
coef(lassoorig ,select="min")
lambdaz1 <- log(lassoz1$lambda)
lambdao1 <- log(lassoo$lambda)

lambdao1 <- log(lassoo$lambda)
lambdaz1[which.min(AICc(lassoz1))]

#The object lassoz is a LASSO when using factor loadings as the independent variables
#The object lassoorig is a LASSO when using just the given FX rates

min(lassoz$cvm) 
min(lassoorig$cvm) 

#The mean OOS deviance when using the principle components is slightly
#lower than the mean OOS deviance when using the original covariates. 
#Moreover, when using the original covariates, there are 17
#coefficients in the model (including intercept). Using the principle 
#components, our analysis above, resulted in a model with 14 coefficients 
#(including intercept), which is more parsimonious.

par(mfrow=c(1,2))
lambdaz1

plot(lassoz1,main="Principle Components")
abline(v=log(lassoz$lambda.min), col="blue", lty=2)
abline(v=lambdaz1[which.min(AICc(lassoz1))], col="black", lty=2)
legend("bottomright", bty="n", lwd=1, 
	col=c("blue","black"),
	legend=c("CV.min","AICC"))

plot(lassoo,main="Original Covariates")
abline(v=log(lassoorig$lambda.min), col="blue", lty=2)
abline(v=lambdao1[which.min(AICc(lassoo))], col="black", lty=2)
legend("topright", bty="n", lwd=1, 
	col=c("blue","black"),
	legend=c("CV.min","AICC"))

#Since the principle components are independent, we have a much 
#cleaner spaghetti plot when using the principle components. 
#The intersection in the spaghetti plot when using the original 
#covariates demonstrates collinearity among the covariates.
#Ultimately, we prefer the model specified per PCR – the model 
#specified per PCR has fewer inputs, and has a lower out-of-sample 
#deviance when compared to the model specified using original covariates. 
#Dimension reduction has allowed us to arrive at a simpler, more harmonious model.

#For a final check to our analysis, we will fit a marginal regression and 
#PLS (Partial Least Squares) and compare to our PCA results from above

### marginal regression
dim(fx2)
fx2 <- as.matrix(fx)
dim(sp)
sp500a <- sp$sp500
sp500b <- as.matrix(sp500a)

phi0 <- cor(fx2, sp500b )/apply(fx2,2,sd) 
z0 <- fx2%*%phi0

fwd <- glm(sp500b ~ z0)
dim(fx2)
dim(sp500b)
dim(z0)
fwd$fit
par(mfrow=c(1,1))
plot(fwd$fit, sp500b, pch=21, bg="lightgreen", 
	xlab="marginal regression fit",ylab="S&P 500 returns")

cor(fwd$fit, sp500b)

#The marginal regression, which is effectively the regression between the 
#first principle component and S&P returns, is subpar.
#The correlation between the "marginal" fit and the actual is only 0.50.

#install.packages("textir")
library(textir)

summary( sppls500 <- pls(x=fx2, y=sp500b,  K=4) )

plot(sppls500, pch=21, bg=8)

#With a K = 4 option, the correlation is 0.70, which is naturally
#better than the marginal regression.

#PLS has a huge advantage over PCA in that, for PCA, the ordering 
#of the z’s is relevant only for predicting the x variables, 
#NOT the y (dependent) variable. PLS creates z’s that are related to 
#both our x variables and our y (dependent) variable. With PLS, we
#arrive at a robust model (F-statistic = 25) using only 4 inputs. 
#PLS is not as traditional as PCA, but it is very unique, and 
#incredibly useful!