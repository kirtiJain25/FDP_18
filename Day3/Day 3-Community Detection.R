#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 03-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                             (DAY 3: Community Detection)                            #
#######################################################################################

# First we load the igraph package
library(igraph)

#Chose a file from your computer
datafile <- file.choose()          #"./Clique.txt"
el = read.csv(datafile, sep = "", head=F) # Read the file
g = graph.data.frame(el, directed = FALSE) # create a graph from the file
plot(g, vertex.label=V(g)$name, vertex.color="grey",vertex.size=20)

#######################################################################################
#Find cliques (complete subgraphs of an undirected graph)
cliques(g) # list of cliques
sapply(cliques(g), length) # clique sizes
largest.cliques(g) # cliques with max number of nodes
names(unlist(largest.cliques(g)[1]))
## coloring nodes according to cliques
vcol <- rep("grey80", vcount(g))
vcol[unlist(largest.cliques(g)[1])] <- "red"
plot(g, vertex.color=vcol,vertex.size=20)
#tkplot(as.undirected(g), vertex.label=V(g)$name, vertex.color=vcol)
#tkplot(as.undirected(g), vertex.label=V(g)$id,vertex.color=vcol)

#find k-cliques (4-cliques)
C <- cliques(g,max=4,min=4)
C
vcol <- rep("grey80", vcount(g))
vcol[unlist(C[1])] <- "red"
plot(g, vertex.color=vcol,vertex.size=20)
vcol[unlist(C[2])] <- "green"
plot(g, vertex.color=vcol,vertex.size=20)

#Find in-degree and out-degree wrt connected component (clique)
vs<- unlist(C[1])
subgraph <- induced.subgraph(g, vs)
plot(subgraph)
in.degrees <- degree(subgraph)
in.degrees
out.degrees <- degree(g, vs) - in.degrees
out.degrees

#Create a graph of disjoint components
G <- graph.disjoint.union(graph.atlas(1000),graph.atlas(1001),graph.atlas(1002))
plot(G)
#Add edges to connect the components
G <- add.edges(G,c(2,10,11,15,16,1))  
#Plot the Graph
G$layout <- layout.kamada.kawai  # circle type
plot(G)

G$layout <- layout.reingold.tilford # hierarchy type
plot(G)

#Community detection based on edge betweenness (Newman-Girvan)
ceb <- edge.betweenness.community(G)
communities(ceb) #shows community details
#Community membership for each node
membership(ceb)
#Color the vertices according to their membership
V(G)$color <- rainbow(3)[membership(ceb)+1] 
G$layout <- layout.kamada.kawai  # circle type
plot(G)
modularity(ceb)





#######################################################################################

#Demonstration of Community Detection Algorithms using Zachary Karate Club Network

#######################################################################################


#There is a function to create this poular graph !!
g <- make_graph("Zachary")
#number of vertices in the graph
vcount(g)
#number of edges in the graph
ecount(g)

# plot the graph
plot(g, vertex.color="grey",vertex.label=V(g)$name, vertex.size=20, layout=layout.fruchterman.reingold)


#######################################################################################
#Community detection based on edge betweenness (Newman-Girvan)
ceb <- edge.betweenness.community(g) #cluster_edge_betweenness(g)
dendPlot(ceb, mode="hclust")
#plot_dendrogram(ceb)
plot(ceb, g)

#We can also plot the communities without relying on their built-in plot:
V(g)$community <- ceb$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "green"))
plot(g, vertex.color=colrs[V(g)$community])

#Let us examine the community detection igraph object:
class(ceb)
## [1] "communities"
length(ceb) # number of communities
## [1] 5
communities(ceb) #communities object
membership(ceb) # community membership for each node
crossing(ceb, g) # boolean vector: TRUE for edges across communities
modularity(ceb) # how modular the graph partitioning is
## [1] 0.4012985


#######################################################################################
#Community detection based on greedy optimization of modularity
cfg <- fastgreedy.community(g) #cluster_fast_greedy(as.undirected(g))
plot(cfg, as.undirected(g))
modularity(cfg)
communities(cfg)


#######################################################################################
#Community detection based on multi-level optimization of modularity
cl <- cluster_louvain(g)
plot(cl, as.undirected(g))
modularity(cl)
communities(cl)


#######################################################################################
#Community detection based on minimizing expected description length of a random walker trajectory
im <- infomap.community(g)
plot(im,g)
modularity(im)
communities(im)


#######################################################################################
#Community detection based on based on propagating labels
clp <- label.propagation.community(g) #cluster_label_prop(g)
plot(clp, g)
modularity(clp)
communities(clp)


#######################################################################################
#Community detection based on random walks
wc <- walktrap.community(g)
plot(wc, as.undirected(g))
modularity(wc)
communities(wc)


#######################################################################################
### leading.eigenvector.community
lec <- leading.eigenvector.community(g)
plot(lec,g)
modularity(lec)
communities(lec)


#######################################################################################
### spinglass.community
sc <- spinglass.community(g, spins=10)
plot(sc,g)
modularity(sc)
communities(sc)


################################# END OF DAY 3 #########################################
#                               ***************
#                                 ***********
#                                   *******
#                                     ***
#                                      *


