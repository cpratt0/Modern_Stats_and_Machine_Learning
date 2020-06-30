###***Amazon Reviews Data***###

###In this code, I'll build a model to predict Amazon reviews using 
###LASSO regularization.

# READ REVIEWS

data<-read.table("Review_subset.csv",header=TRUE)
dim(data)

# 13319 reviews
# ProductID: Amazon ASIN product code
# UserID:  id of the reviewer
# Score: numeric from 1 to 5
# Time: date of the review
# Summary: text review
# nrev: number of reviews by this user
# Length: length of the review (number of words)

# READ WORDS

words<-read.table("words.csv")
words<-words[,1]
length(words)
#1125 unique words

# READ text-word pairings file

doc_word<-read.table("word_freq.csv")
names(doc_word)<-c("Review ID","Word ID","Times Word" )
# Review ID: row of the file  Review_subset
# Word ID: index of the word
# Times Word: number of times this word occurred in the text

# Let's define the binary outcome
# Y=1 if the rating was 5 stars
# Y=0 otherwise

Y<-as.numeric(data$Score==5)
View(Y)

#Use only product category as a predictor

library(gamlr)

source("naref.R") 
## naref: make missing (NA) the reference level of a factor

data$Prod_Category<-naref(data$Prod_Category)
View(data$Prod_Category)

#Convert product category to a data frame for usability
products<-data.frame(data$Prod_Category)

#Convert products data frame to a sparse matrix
x_cat<-sparse.model.matrix(~., data=products)[,-1]
colnames(x_cat)<-levels(data$Prod_Category)[-1]

#Run our LASSO logistic regression
lasso1<- gamlr(x_cat, 	y=Y, standardize=FALSE,family="binomial",
lambda.min.ratio=1e-3)

#We used the standardize = FALSE option because we are regressing on Y on 
#Prod_Category, a categorical variable.  Since we are using a categorical variable, 
#the standardization would put higher penalty on more frequently occurring categories 
#(since their standard deviation will be higher), and would put reduced penalty on less 
#frequently occurring categories.  To prevent this scenario, we used the standardize = FALSE option. 

plot(lasso1)

B_prod <- coef(lasso1) ## coefficients selected under AICc

source("deviance.R")

Ybar <- mean(Y)

D0 <- deviance(y=Y, pred=Ybar,family="binomial")
D0

D <- lasso1$deviance[which.min(AICc(lasso1))]
D

R_squared_OOS <- (D0 - D) / D0
R_squared_OOS
#The in-sample R2 for the AICc slide of the LASSO path is 0.1048737.  
#Not bad.

#Now, we Fit a LASSO with all 142 product categories and 1125 words 

View(doc_word)

spm<-sparseMatrix(i=doc_word[,1],
                  j=doc_word[,2],
                  x=doc_word[,3],
                  dimnames=list(id=1:nrow(data),
                  words=words))

dim(spm)

x_cat2<-cbind(x_cat,spm)

dim(x_cat2)
#OK - data were binded as expected

lasso2 <- gamlr(x_cat2, y=Y,lambda.min.ratio=1e-3,family="binomial")

#Run quality tests
#log(lasso2$lambda[which.min(AICc(lasso2))])
#lasso2$lambda[which.min(AICc(lasso2))]

B_lasso2 <- coef(lasso2) ## coefficients selected under AICc
B_lasso2

B_lasso2_1 <- B_lasso2[144:1268,]
B_lasso2_1

B_lasso2_2 <- as.data.frame(B_lasso2_1)
B_lasso2_2

B_lasso2_2$sigdum <- ifelse(B_lasso2_2$B_lasso2_1 == 0,0,1)
View(B_lasso2_2)
sum(B_lasso2_2$sigdum)

#Using AICc, log(lambda) = -8.334091 (which corresponds to lambda = 0.0002401874),  
#1,022 of the 1,125 words were selected as predictive of a 5-star review.  

#plot(lasso2)

B_lasso2_2_ordered <- B_lasso2_2[order(-B_lasso2_1),]
View(B_lasso2_2_ordered)

#These 10 words (in decreasing order) have the most positive effect on odds of a 5-star review:
#1.	"worried"
#2.	"plus"
#3.	"excellently"
#4.	"find"
#5.	"grains"
#6.	"hound"
#7.	"sliced"
#8.	"discount"
#9.	"youd"
#10.	"doggies"

#Now, we'll run cross-validation to obtain the best lambda value that minimizes OOS deviance. 

cv.fit <- cv.gamlr(x_cat2,
				   y=Y,
				   lambda.min.ratio=1e-3,
				   family="binomial",
				   verb=TRUE)

###Min OOS deviance solution
coef(cv.fit, select="min") ## min cv selection
plot(cv.fit)
log(cv.fit$lambda.min)

B_lasso_cvmin <- coef(cv.fit, select="min") ## coefficients selected under MIN Deviance
B_lasso_cvmin2 <- as.matrix(B_lasso_cvmin)

B_lasso2_cvmin3 <- B_lasso_cvmin2[144:1268,]
B_lasso2_cvmin3

B_lasso2_cvmin4 <- as.data.frame(B_lasso2_cvmin3)
B_lasso2_cvmin4
View(B_lasso2_cvmin4)

B_lasso2_cvmin4$sigdum <- ifelse(B_lasso2_cvmin4$B_lasso2_cvmin3 == 0,0,1)
sum(B_lasso2_cvmin4$sigdum)
#The log lambda that minimizes OOS deviance is -6.659484.  Using log lambda = -6.659484 
#results in 871 non-zero coefficients.  

###1se Solution
coef(cv.fit, select="1se") ## 1se cv selection
plot(cv.fit)
log(cv.fit$lambda.1se)

B_lasso_cv1se <- coef(cv.fit, select="1se") ## coefficients selected under MIN Deviance
B_lasso_cv1se2 <- as.matrix(B_lasso_cv1se)

B_lasso2_cv1se3 <- B_lasso_cv1se2[144:1268,]
B_lasso2_cv1se3

B_lasso2_cv1se4 <- as.data.frame(B_lasso2_cv1se3)
#B_lasso2_cv1se4
View(B_lasso2_cv1se4)

B_lasso2_cv1se4$sigdum <- ifelse(B_lasso2_cv1se4$B_lasso2_cv1se3 == 0,0,1)
sum(B_lasso2_cv1se4$sigdum)

#The log lambda under the 1se rule is -6.171057.  Using log lambda = -6.171057 results 
#in 751 non-zero coefficients.  Having fewer non-zero coefficients is expected under the 
#1se rule.  We would use the 1se rule if we want to improve our interpretation of the model, 
#at an expense of the model’s predictive power.