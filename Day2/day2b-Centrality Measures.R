#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 05-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                            (DAY 2b: Centrality Measures)                            #
#######################################################################################
library(igraph)

################### Create Graph from Edge List #####################
#Create a undirected graph from an edge list matrix
el<-matrix( c("V1","V2","V1","V3","V1","V4","V1","V5","V2","V5","V3","V4","V4","V5","V4","V7","V5","V8","V6","V2","V7","V8"), nc = 2, byrow =TRUE)
el
g <- graph_from_edgelist(el,directed = FALSE)
# el: The edge list, a two column matrix, character or numeric.
# directed : Whether to create a directed graph.
g
plot(g, vertex.size=30)


#Create a directed graph from an edge list matrix
del<-matrix( c("V1","V2","V1","V3","V1","V4","V1","V5","V2","V5","V3","V4","V4","V5","V4","V7","V5","V8","V6","V2","V7","V8"), nc = 2, byrow =TRUE)
del
dg <- graph_from_edgelist(del,directed = TRUE)
# del: The edge list, a two column matrix, character or numeric.
# directed : Whether to create a directed graph.
plot(dg,vertex.size=30)

#######################################################################################
#   PART 1: Degree based Centrality Measures
#######################################################################################

##  Degree Centrality (number of ties)
degree(g, mode="all") # by default, results are not normalized

degree(g, mode = "all", normalized = TRUE) # To get normalized results, set normalized = TRUE

degree(dg, mode = "in")
##  Eigenvector Centrality (centrality proportional to the sum of connection centralities)
##  Values of the first eigenvector of the graph matrix.
##  Returns unit length vector, if scale is not set TRUE
?eigen_centrality
eigen_centrality(g, directed=F, weights=NA)$vector #undirected graph
eigen_centrality(dg, directed=T, weights=NA)$vector #directed, acyclic graph


#######################################################################################
#   PART 2: Flow based Centrality Measures
#######################################################################################

## Eccentricity Centrality(The less eccentric a node is, the more central it is.)
ec<- 1/eccentricity(g)
ec


##  Closeness Centrality (centrality based on distance to others in the graph)
##  Inverse of the node’s average geodesic distance to others in the network.
closeness(g, mode="all", weights=NA)

closeness(g, mode="all", weights=NA, normalized = TRUE)


##  Betweenness Centrality (centrality based on a broker position connecting others)
##  Number of geodesics that pass through the node or the edge.
betweenness(g, directed=F, weights=NA) # computes node betweenness for each node
betweenness(g, directed=F, weights=NA, normalized = TRUE)

edge_betweenness(g, directed=F, weights=NA) # computes edge betweenness for each edge



#######################################################################################
#   PART 3: Web based Centrality Measures
#######################################################################################

############ Page rank, Hubs: outgoing links, Authority: Incoming links ###############
## Prestige: for directed graph
g_s = erdos.renyi.game(10, .5, "gnp", directed = TRUE)
e_gs = eigen_centrality(g_s, directed=T, weights=NA)$vector #directed graph
e_gs

## PageRank: default computation method is "prpack"
page_rank(g) # return a list: a vector, a value, options

page_rank(g, algo = "arpack") # options is returned if "arpack" algo is used for PageRank computation

page_rank(g)$vector #undirected graph g
page_rank(dg)$vector #directed graph dg


## HITS: Hub and authority score for undirected graphs are same
hs <- hub_score(g, weights=NA)$vector
as <- authority_score(g, weights=NA)$vector
par(mfrow=c(1,2)) #divides the plot into 1 row, 2 cols
plot(g, vertex.size=hs*52, main="Hubs",vertex.color="yellow")
plot(g, vertex.size=as*52, main="Authorities",vertex.color="green")


## HITS: Hub and authority score for directed graphs are different
hs <- hub_score(dg, weights=NA)$vector
as <- authority_score(dg, weights=NA)$vector
par(mfrow=c(1,2)) #divides the plot into 1 row, 2 cols
plot(dg, vertex.size=hs*52, main="Hubs",vertex.color="yellow")
plot(dg, vertex.size=as*52, main="Authorities",vertex.color="green")


# another example of star graph
eg <- make_star(5)
plot(eg)
page_rank(eg)$vector
hub_score(eg, weights=NA)$vector
authority_score(eg, weights=NA)$vector




 ################################# END OF DAY 2 #########################################
 #                               ***************
 #                                 ***********
 #                                   *******
 #                                     ***
 #                                      *
 
