#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 06-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                                       (DAY 4)                                       #
#######################################################################################

#First, load the igraph library
library(igraph)

getwd() #to get your working directory

#**************************************************************************************#
# Demonstration of properties of real-world networks 
#**************************************************************************************#

#Facebook graph downloaded from https://snap.stanford.edu/data/ego-Facebook.html

#Read the data file and make the graph
datafile <- "facebook_combined.txt"
el =read.csv(datafile, sep = "", head=F)
g=graph.data.frame(el, directed = FALSE)
g
plot(g,layout = layout_nicely,vertex.size=3, vertex.label=NA )
#Count of vertices
vcount(g)    #4039

#Count of edges 
ecount(g)   #88234

#Diameter of the graph 
diameter(g)    #8

#Average Shortest Path 'mu' 
mean_distance(g)     #3.692507

#log n     # n= Number of nodes , logn = 8.303752
log(vcount(g))
#Graph exhibits ultra-small world behaviour mu <<< log n (3.692507<<< 8.303752)


#Demonstration of Scale-Free Property of Graph
#Get the degree of the graph
d <- degree(g,mode="all")

#Fits power-law distribution to degree
power<-power.law.fit(d)

#The power-law exponent
power$alpha #Numeric scalar, the exponent of the fitted power-law distribution.

#Get the degree distribution of the graph
dd = degree.distribution(g, mode = "all", cumulative = FALSE)

#Plot shows that vast majority of nodes have very small degrees,
# whereas there are a few "hub" nodes that have high degrees
plot( dd,ylim=c(0,max(dd)), cex=0.5, col="red",xlab="Nodes Degree", ylab="Frequency")

#Log-log plot of degree (k) versus degree distribution f(k)
plot( dd,log='xy', cex=0.5, col="red",xlab="Degree: k", ylab="Probability: f(k)")


#Define Function to plot and fit a given distribution
fit_power_law = function(dist, deg, graphtitle) {
  # calculate degree
  #d = degree(graph, mode = "all")
  #dd = degree.distribution(graph, mode = "all", cumulative = T)
  degree = 1:max(deg)
  probability = dist[-1]
  probability
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit.new = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  print(paste("Alpha =", round(alpha, 3)))
  # plot
  plot( probability ~ degree,log='xy', cex=0.5, col="red",
        xlab="Degree: log k", ylab="Probability: log f(k)",
        main=graphtitle)
  curve(power.law.fit.new, col = "black", add = T, n = length(d))
}

#Use the newly defined function to plot the log-log plot
fit_power_law(dd,d,'Degree Distribution')


#Note that there is high level of noise for the higher degrees,
#where frequency counts are the lowest.
#To address the problem is to use the cumulative degree
#distribution F(k),

cdd = degree.distribution(g, mode = "all", cumulative = TRUE)
plot( cdd,log='xy', cex=0.5, col="red",
      xlab="Degree: log k", ylab="Probability: log F(k)",
      main="Cumulative Degree Distribution")

#Use the newly defined function to plot the log-log plot
fit_power_law(cdd,d,'Cumulative Degree Distribution')

# Clustering coefficient is the proportion of
# a nodes neighbors that can be reached by other neighbors
# in igraph this property is apparently called "transitivity"
transitivity(g)  # 0.5191743

#log-log plot of k versus C(k) exhibits a straight line
#behaviour with negative slope
d <- degree(g,mode='all')
d
tr <- transitivity(g,'local')
tr
df <- data.frame(d,tr)
df
# Aggregate:Splits the data into subsets, computes summary statistics for each, and returns the result in a convenient form.
averageccf <- aggregate(tr ~ d, data=df, mean) 
averageccf
reg = lm(log(averageccf$tr) ~ log(averageccf$d)) #lm is used to fit linear models.
cozf = coef(reg) #coef is a generic function which extracts model coefficients from objects returned by modeling functions.
power.law.fit.new = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
#log-log plot
plot( averageccf$tr ~ averageccf$d,log='xy', cex=0.5, col="red",
      xlab="Degree: log k", 
      ylab="Average clustering coefficient: log C(k)",
      main='Average Clustering Coefficient')
#Draws a curve corresponding to a function.
curve(power.law.fit.new, col = "black", add = T, n = length(averageccf$d)) 
#n:integer; the number of x values at which to evaluate.
#add:logical; if TRUE add to an already existing plot.



######################## Another Example Graph : CA-AstroPh ############################################################

#Read another data file and make the graph
#Coauthor graph downloaded from https://snap.stanford.edu/data/ca-AstroPh.html
datafile2 <- 'CA-AstroPh.txt'
el =read.csv(datafile2, sep = "", head=F)
cag=graph.data.frame(el, directed = FALSE)

#Remove duplicate edges and self loops
cag <- simplify(cag)

#Count the vertices and edges
vcount(cag)  #18777
ecount(cag)  #198054

#Average shortest path of this graph
mean_distance(cag)  #4.193987

#log n
log(vcount(cag))    #9.840388
#Graph exhibits small world behaviour mu <<< log n


#Now perform the same analysis on this graph as done for the facebook graph



#*******************************************************************************************#
#********************************************************************************************#
#Following commands generate graphs according to the generatve models discussed in the lecture
#*******************************************************************************************#


#********* Random Graph **********#
#Generate random graphs according to the Erdos-Renyi model
#n: The number of vertices in the graph.
#p.or.m	 - Either the probability for drawing an edge between 
#two arbitrary vertices (G(n,p) graph),
#or the number of edges in the graph (for G(n,m) graphs).

?erdos.renyi.game

erg <- erdos.renyi.game(20, 0,directed = FALSE)  #no edges
plot(erg,layout = layout_in_circle,vertex.size=10, vertex.label=NA )

erg1 <- erdos.renyi.game(20, 0.01,directed = FALSE)
plot(erg1,layout = layout_in_circle,vertex.size=10, vertex.label=NA )

erg2 <- erdos.renyi.game(20, 0.05,directed = FALSE)
plot(erg2,layout = layout_in_circle,vertex.size=10, vertex.label=NA )

erg3 <- erdos.renyi.game(20, 0.1,directed = FALSE)
plot(erg3,layout = layout_in_circle,vertex.size=5, vertex.label=NA )

erg4 <- erdos.renyi.game(20, 1,directed = FALSE) #complete
plot(erg4,layout = layout_in_circle,vertex.size=10, vertex.label=NA )




#********* Barabasi-Albert preferential attachment Model **********#
#Generate scale-free graphs according to Barabasi-Albert preferential attachment model for scale-free graphs
# 'n' is number of nodes

?sample_pa

bag <- sample_pa(20)
plot(bag,layout = layout_in_circle,vertex.size=9 )



#********* Watts-Strogatz network Model **********#
#Generate a graph according to the Watts-Strogatz network model.
?watts.strogatz.game

#dim	:Integer constant, the dimension of the starting lattice.
#size	:Integer constant, the size of the lattice along each dimension.
#nei	:Integer constant, the neighborhood within which the vertices of the lattice will be connected.
#p	  :Real constant between zero and one, the rewiring probability.

#gives regular graph with 2 neihbors on each side (p=0)
wsg <- watts.strogatz.game(dim=1,size=10,nei=2, p=0) 
plot(wsg,layout = layout_in_circle,vertex.size=9 )

#gives regular graph with 3 neihbors on each side (p=0)
wsg1 <- watts.strogatz.game(dim=1,size=10,nei=3, p=0)
plot(wsg1,layout = layout_in_circle,vertex.size=9 )

#increasing Probability 'p'=0.05
wsg2 <- watts.strogatz.game(dim=1,size=10,nei=2, p=0.05)
plot(wsg2,layout = layout_in_circle,vertex.size=9 )

#increasing Probability 'p'=0.1
wsg3 <- watts.strogatz.game(dim=1,size=10,nei=2, p=0.1)
plot(wsg3,layout = layout_in_circle,vertex.size=9 )

#increasing Probability 'p'=1 yields a random graph.
wsg4 <- watts.strogatz.game(dim=1,size=10,nei=2, p=1) #random graph
plot(wsg4,layout = layout_in_circle,vertex.size=9 )

