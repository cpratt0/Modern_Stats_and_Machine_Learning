##In this code, we'll examine actor/movie data, and apply 
###Network theory to generate insights from the data

## actors network example

library(igraph)

### GRAPH

## read in a graph in the `graphml' format: xml for graphs
## it warns about pre-specified ids, but we want this here
## (these ids match up with the castlists in movies.txt)

actnet <- read.graph("actors.graphml",format="graphml")

#plot(actnet)

head(get.vertex.attribute(actnet)$name) # first few names
degree(actnet)

### TRANSACTION
## read in the table of actor ids for movies
## this is a bit complex, because the movie names
## contain all sorts of special characters.

movies <- read.table("movies.txt", sep="\t", 
	row.names=1, as.is=TRUE, comment.char="", quote="")

## it's a 1 column matrix.  treat it like a vector

movies <- drop(as.matrix(movies))

## each element is a comma-separated set of actor ids.  
## use `strsplit' to break these out

movies <- strsplit(movies,",")

## and finally, match ids to names from actnet

casts <- lapply(movies, 
	function(m) V(actnet)$name[match(m,V(actnet)$id)])

#View(as.data.frame(as.matrix(movies)))
casts['True Romance']
#QC Looks good - data are populated as expected

#Convert graph into adjacency

actmat<-as_adjacency_matrix(actnet) 
# this is the adjacency matrix of the graph
# the number of rows and columns is the number of actors
# the matrix has a 1 if the actors played together and . (zero)
# otherwise

# First, we'll explore the data by asking the following questions:
# How many actors, movies and links are in the dataset? 
# How many actors never played with anyone else in our dataset?
# View(as.data.frame(as.matrix(actmat)))

degree_act <- degree(actnet)
degree_ind <- ifelse(degree_act==0,1,0)
dummy_ind <- ifelse(degree_act==degree_act,1,0)
sum(degree_ind)
sum(dummy_ind)

numedges <- E(actnet)
numedges 

#There are 7015 actors and 14,326 movies. 75 actors never played with anyone 
#else in our dataset, identified by having degree of zero.  There are 
#286,360 links (edges) in the dataset.

# Next, we'll find pairwise actor-cast association rules 
# with at least 0.02% support and 50% confidence.
# In other words...
# We'll look for transactions where:
# a) the actors have some fame (in at least 0.02% of movies)
# b) the actors frequently co-star with each other

# format as arules transaction baskets

install.packages("arules")
library("arules")

casttrans <- as(casts, "transactions")

# create a set of movie rules and inspect them

#We'll apply the apriori algorithm
movierules <- apriori(casttrans,parameter=list(maxlen=2,support=0.0002, confidence=0.5))
summary(movierules)
inspect(subset(movierules))

pairs <- labels(movierules)
dum2 <- movierules
sort(dum2,decreasing=FALSE)
inspect(dum2)
dum3 <- as.data.frame(dum2)
pairs <- gsub("\\{|\\}","",pairs)

unclass(dum2)
View(dum2)

pairs <- strsplit(pairs," => ")
pairs <- do.call(rbind,pairs)
pairs <- pairs[pairs[,1]!="",] # no lhs

#The median and mean confidence (conditional probability) is 0.5882 and 0.6168, respectively. 
#The median and mean lift is 651.2 and 697.5, with min of 125.7 and max of 3581.5. 
#The high lift indicates that the association network is rather strong among actors. 
#In other words, the probability of seeing one actor is much higher given that we know 
#another actor is featured.  For example, let’s consider the rule between The Rock 
#(“Rock, The”) and (Vince McMahon (“McMahon, Vince”).  Given that McMahon (The Rock) 
#is in a film, the lift of The Rock (McMahon) being in a film is 685.  This is an 
#incredibly high lift, and is very intuitive, given their involvement in WWE.  
#More tellingly, this lift of 685 is comparable to the mean and median of the sample.  
#There are many rules that generate more lift than the McMahon/Rock rule, which further 
#demonstrates that strong associations exist among actors.

library(igraph)

moviesnet <- graph.edgelist(pairs)

moviesnet <- as.undirected(moviesnet)

V(moviesnet)$color <- "cyan"

plot(moviesnet, vertex.label=NA, vertex.size=3, edge.curved=FALSE,main="Network Plot of Actors where Support >= 0.02% and Confidence >= 50%")

#This plot shows the network of actors with at least 0.02% support and 50% confidence. 
#We can clearly see the communities, which explains the high lift observed.