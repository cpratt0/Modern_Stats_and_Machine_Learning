#In this code, we'll explore the "congress109" dataset, and leverage
#clustering techniques to summarize the views of Congress

install.packages("textir")
install.packages("maptpx")
install.packages("wordcloud")

library(textir)
library(maptpx)

data(congress109)

congress <- (congress109Ideology)
View(congress)

congressct <- (congress109Counts)
#View(as.data.frame(as.matrix(congressct)))

#OK - data are loaded successfully

#First, we'll employ a K means algorithm to explain clusters in terms
#of membership and chamber

congscale <- scale(as.matrix(congressct/rowSums(congressct)))

kmcong <- kmeans(congscale,center=5,nstart=100)

summary(kmcong)
table(kmcong$cluster)

congsum <- cbind(congress,kmcong$cluster)
View(congsum)
View(kmcong$centers)
names(kmcong$centers)
table(congsum$cluster,congsum$party)
#write.csv(congsum,file="outcong.csv")
colnames(congscale)

#We summarize each cluster as follows:
#Cluster 1: Predominately Democratic (98%), mostly House representatives (73%)
#Cluster 2: Predominately Republican (93%), mostly Senate representatives (89%)
#Cluster 3: Predominately Republican (68%), mostly House representatives (95%)
#Cluster 4: Predominately Republican (92%), mostly House representatives (92%)
#Cluster 5: Very small sample size, consisting of one House Democrat

#The 10 most important phrases for each cluster are summarized as follows:
print(apply(kmcong$centers,1,function(c) colnames(congscale)[order(-c)[1:10]]))

#The list makes some intuitive sense.  Cluster 1 is heavily Democratic, 
#while cluster 2 is heavily Republican.  For example, Democrats note that 
#Republicans want “tax cuts for the wealthy.”  As such, “tax.cut.wealthy” 
#is one of the most important words in Cluster 1.  The link between words 
#in 2 and Republican party is less obvious, but many of these words deal 
#with judiciary matters.  This jives with our intuition in that Republicans 
#have many matters in the hands of the Circuit and the Supreme Courts.  

#Note, with K-means, results vary with each run.

#Next, we'll fit a topic model for the speech counts - we'll
#use Bayes factors to choose the number of topics, and will interpret our model

xc <- as.simple_triplet_matrix(congressct)
#First, we must use Bayes factors to choose the number of topics.  
tpcc <- topics(xc,K=5) 
tpccs <- topics(xc,K=5*(1:5), verb=10) # it chooses 10 topics 
summary(tpccs)
#summary(tpcc, n=10) 
#Here, max BF selects K = 10 (since log(BF) is maximized when K = 10).

#We will look at words ordered by simple in-topic probability (theta).
View(tpccs$theta)
thetadum <- tpccs$theta
thetadum2 <- thetadum[order(-thetadum[,1]),]
View(thetadum2)

topten <- cbind(rownames(tpcc$theta)[order(tpccs$theta[,1], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,2], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,3], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,4], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,5], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,6], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,7], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,8], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,9], decreasing=TRUE)[1:10]],
rownames(tpccs$theta)[order(tpccs$theta[,10], decreasing=TRUE)[1:10]])
View(topten)
#write.csv(topten, file="top10.csv")

#Our interpretation by topic is as follows:
#Topic 1: Politicians that are most concerned with civil rights, and advocate 
#against issues that impact African Americans.  These are most likely Democrats.

#Topic 2: The interpretation is less clear, but Cluster 2 consists of politicians that 
#are most concerned for the middle class, and for military/veteran issues.  These are most likely Republicans.

#Topic 3: Politicians that are most concerned with illegal aliens/immigration issues.  
#These are most likely Republicans.

#Topic 4: Politicians that are most concerned for military issues.  These are most likely Republicans.

#Topic 5: Politicians that are most concerned with the federal deficient and federal spending.  
#These are most likely Democrats.

#Topic 6: Politicians that are most concerned with climate change, and issues that impact the planet.  
#These are most likely Democrats.  

#Topic 7: Politicians that are most concerned with judicial issues.  Based on above, we hypothesize 
#that these consist primarily of Republicans.
  
#Topic 8: Politicians that are most concerned with gun violence, and with issues impacting those 
#of lower socio-economic status.  We hypothesize that these consist primarily of Democrats.  

#Topic 9: Politicians that are most concerned with trade issues, and with losing jobs to 
#other countries.  These are most likely Republicans.  

#Topic 10: Politicians that discuss stem cell research.  This could consist of Democrats 
#(generally in favor of stem cell research) and Republicans (generally opposed to stem cell research).

#To summarize the data, we'll make a word cloud for topic 3 ("illegal aliens/immigration issues")
#and for topic 5 ("federal deficient and federal spending")

library(wordcloud)

par(mfrow=c(1,2))

wordcloud(row.names(tpccs$theta), 
	freq=tpccs$theta[,3], min.freq = 0.004, max.words = 30, col="maroon")

wordcloud(row.names(tpccs$theta), 
	freq=tpccs$theta[,5], min.freq = 0.004, max.words = 30, col="navy")

#The word clouds give us several useful insights.  For example, Topic 3 consists of 
#numerous buzz words that Republicans use to fire up their base (e.g., “illegal.alien” and “border.patrol”)
#Topic 5 consists of words that are used when discussing the federal deficit, and need for fiscal reform.  
#The word clouds are consistent with our interpretation of the topics (see above).

#Lastly, we'll regress party membership onto topics, and compare to a regression
#onto phrase percentages

library(gamlr)

party <- congress$party
View(party)

tpcsreg <- gamlr(tpccs$omega, party)

#Cross validation of party on "topics" (where topics is quantified by the omega metric).
regtopiccs.cv <- cv.gamlr(tpccs$omega, party,lambda.min.ratio=10^{-4})

xlassoin <- 100*congressct/rowSums(congressct)
dim(xlassoin)

View(as.data.frame(as.matrix(xlassoin)))
test <- rowSums(xlassoin[,1:1000])
sum(test)

#OK - data are populated as expected

##Cross validation of party on "phrases"
regwordscon.cv <- cv.gamlr(xlassoin, party)

par(mfrow=c(1,2))

plot(regtopiccs.cv)

mtext("Topic Regression", font=2, line=2)

plot(regwordscon.cv)

mtext("Phrase Percentages Regression", font=2, line=2)

min(regtopiccs.cv$cvm)

min(regwordscon.cv$cvm)

#Using the topics regression, the min mean squared error is 0.44.  
#Under the phrase percentages regression, the mean squared error is roughly 0.70.  
#In other words, the minimum out-of-sample deviance with the topic regression is LESS 
#than the minimum out-of-sample deviance with the phrase percentages regression.  
#Ultimately, the topic model does better than pure regression on to phrases!!!  
#It is fascinating that we can arrive at a better model by first condensing our data 
#in to topics, as opposed to using the raw phrases!  

#This serves as a reminder to find the most parsimonious model.    
