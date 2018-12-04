#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 04-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                                       (DAY 2a)                                       #
#######################################################################################
library(igraph)


################### Create sample Graph from Edge List #####################
#Create a undirected graph from an edge list matrix
el<-matrix( c("V1","V2","V1","V3","V1","V4","V1","V5","V2","V5","V3","V4","V4","V5","V4","V7","V5","V8","V6","V2","V7","V8"), nc = 2, byrow =TRUE)
el
g<- graph_from_edgelist(el,directed = FALSE)
# el: The edge list, a two column matrix, character or numeric.
# directed : Whether to create a directed/undirected graph.
g
plot(g, vertex.size=30)



################################################################################################
############################## COMPUTE GRAPH PROPERTIES ########################################



######################################################################################
#######################################################################################
#   PART 1: Local Properties
#######################################################################################


########## Degree ##########
deg <- degree(g)
deg
sort(deg, decreasing = TRUE) # sort the degree of nodes to get degree sequence of graph.
plot(g, vertex.size=deg*12) # plot graph with node size according to degree of nodes.
min(deg) # minimum degree
max(deg) # maximum degree



########## Computing degree using Adjacency Matrix ##########
am<-get.adjacency(g,sparse=FALSE) # get adjacency matrix 'am' of graph 'g'
am
rowSums(am) # sum of each row gives the degree of that node.
sort(rowSums(am), decreasing = TRUE) # sort degrees in decreasing order



########### Indegree and Outdegree of Directed graph ##################3
g_directed <- graph( c(1,2,2,3,1,3,3,4) , dir=TRUE) 
plot(g_directed)
degree_in <- degree(g_directed,mode="in")
degree_in
degree_out <- degree(g_directed,mode="out")
degree_out


########### Clustering Coefficient of nodes ##################
transitivity(g,type="local")  # gives the clustering coefficient of each node

# CC of star graphs
st <-make_star(5,mode="undirected")
plot (st, vertex.size=10, vertex.label=NA)
transitivity(st,type="local")

# CC of Ring graphs
rg <-make_ring(5)
plot (rg, vertex.size=10, vertex.label=NA)
transitivity(rg,type="local")

# CC of Complete graphs
fg <-make_full_graph(5)
plot (fg, vertex.size=10, vertex.label=NA)
transitivity(fg,type="local")




#######################################################################################
#######################################################################################
#   PART 2: Global Properties
#######################################################################################



########## Degree distribution of graph g ##########
#histogram
hist(deg, breaks=1:vcount(g)-1, main="Histogram ",xlab = "Degree",col= "blue") 

# degree distribution
deg.dist <- degree_distribution(g, cumulative=FALSE, mode="all")
deg.dist
plot( x=0:max(deg), y=deg.dist, pch=19, cex=2.0, col="red",xlab="Degree k", ylab="Probability f(X = k)") # plot degree distribution

# Cumulative degree distribution
degc.dist <- degree_distribution(g, cumulative=TRUE, mode="all")
degc.dist
plot( x=0:max(deg), y=degc.dist, pch=19, cex=1.0, col="red",xlab="Degree", ylab="Probability f(X = k)") # plot degree distribution



########### Degree Distribution of Star graph  ##########
st <-make_star(7)  # make star graph st
plot (st, vertex.size=10, vertex.label=NA) # plot star graph
deg_st <- degree(st) # compute degree
hist(deg_st, breaks=1:vcount(st)-1, main="Histogram ",xlab = "Degree",col= "blue") # histogram
degdist_st <- degree_distribution(st, cumulative=FALSE, mode="all") # degree distribution
degdist_st
# plot degree distribution
plot( x=0:max(deg_st), y=degdist_st, pch=19, cex=2.0, col="red",xlab="Degree k", ylab="Probability f(X = k)")


########### Degree Distribution of complete graph  ##########
c <-make_full_graph(5) #make a complete graph
plot (c, vertex.size=10, vertex.label=NA) #plot complete graph
deg_c <- degree(c) #compute degree
hist(deg_c, breaks=1:vcount(c)-1, main="Histogram ",xlab = "Degree",col= "blue") #histogram
degdist_c <- degree_distribution(c, cumulative=FALSE, mode="all") # degree distribution
degdist_c
# plot degree distribution
plot( x=0:max(deg_c), y=degdist_c, pch=19, cex=2.0, col="red",xlab="Degree k", ylab="Probability f(X = k)")





########### Density of graph ##############
graph.density(g,loop=FALSE)


########### Number of Connected components #############
components(g)


########### Number of Components (in directed graph) ###############
# Strongly Connected Component
g_directed <- graph( c(1,2,2,3,4,2,2,5,4,5,5,4,6,5) , dir=TRUE) # make a directed graph 
plot(g_directed)
components(g_directed,mode="strong")

# Weakly Connected Component
g_directed <- graph( c(1,2,2,3,4,2,2,5,4,5,5,4,6,5) , dir=TRUE) # make a directed graph 
plot(g_directed)
components(g_directed,mode="weak")


########### Shortest Paths ############
# length of all shortest paths in the graph g 
x=distances(g)   
x 
    # or
shortest.paths(g)  


########### Average path length #################
# the mean of the shortest distance between each pair of nodes in the network 
mean_distance(g, directed=F)
    # or
average.path.length(g, unconnected = TRUE)



############ Eccentricity of a vertex  #####################
# shortest path distance from the farthest other node in the graph
eccentricity(g)  # hops to the farthest node


############ Diameter of graph #################
diameter(g)
     # or
max(shortest.paths(g)) 
     # or
max(eccentricity(g))


########### Radius of graph ################
radius(g)
     # or
min(eccentricity(g))


##################################
# install.packages("brainGraph")
library("brainGraph")
##################################


########### Efficiency of vertices #################
# Smaller the distance between the nodes, more efficient is the communication.
efficiency(g, type = c("local"))


########### Efficiency of graph ####################
efficiency(g, type = c("global"))


########### Clustering Coefficient ##################
transitivity(g)  #Clustering Coefficient of Graph
L=transitivity(g,type="local")  # gives the clustering coefficient of each node
L
L[4]



#######################################################################################
#######################################################################################
#   PART 3: Other Properties
#######################################################################################


########## Find Neighbours ##############
plot(g)
neighbors(g, "V1")  # neighbours of vertex V1
incident(g, "V1", mode=c("all")) #incident edges of vertex V1


######### Induced Subgraph ##############
plot(g)
sub = induced_subgraph(g, c(1,2,3,7))
plot(sub)


######## Cliques ############## 
# (complete subgraphs of an undirected graph)
cliques(g, min = NULL, max = NULL)
cliques(g, min = 3, max = NULL)


########### Find the shortest path between specific nodes ################
get.shortest.paths(g,"V1","V8")
get.shortest.paths(g,"V2","V4")

########### Find number of paths of each length #################
L=path.length.hist(g)$res  #starting from path length 1 
L

######## Simple Paths ###########
# (find all simple paths between specified pair of vertices.)
# A path is simple if the vertices it visits are not visited more than once.
plot(g)
all_simple_paths(g, "V1", "V5")  # lists all simple paths between V1 and V5.
get.shortest.paths(g,"V1", "V5") # find shortest path between V1 and V5.



 ################################# END OF DAY 2 #########################################
 #                               ***************
 #                                 ***********
 #                                   *******
 #                                     ***
 #                                      *
 
