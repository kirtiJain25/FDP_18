#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 03-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                                    (DAY 5: Recap)                                   #
#######################################################################################
# 2. Install package(s)
# i) igraph
#install.packages(igraph)
# or install using GUI


# 3. Load package(s)
library(igraph)   # igraph is a library and R package for network analysis.

################### Create Graph from Adjacency List #####################
#Create a directed graph from an adjacency list 

a_list <- list(NULL,     ## Creating the adjacency list
               NULL,
               c(5,12,16,37),
               c(5,8,12,14,16,21,23,24,27,32,42,45,49),
               c(3,4,6,8,9,12,15,18,23,26,27,30,32,37,39,40,41,42),
               NULL,
               c(20),
               c(4,5,8,11,12,23,28,42,45),
               NULL,
               NULL,
               c(2,8,13,16,19,21,23,27,31,32,38,41,42),
               c(3,4,5,8,11,16,23,27,28,29,32,36,37,45),
               NULL,
               NULL,
               c(8),
               NULL,
               NULL,
               NULL,
               c(5,11,17,18,21,22,23,32,42),
               c(7),
               c(4,5,11,17,18,19,22,23),
               c(5,11,16,23,28,27,30,32,39,40,42,49),
               c(4,5,8,10,11,12,16,21,27,28,32,39,40,41,42,45),
               NULL,
               c(1),
               c(5,6,8,40,43,42),
               c(5,8,11,12,16,23,34,38,39,41,42,49),
               NULL,
               c(5,8,12,43,44),
               NULL,
               NULL,
               c(4,5,8,11,12,23,28,42,45),
               c(46),
               c(27,38,49),
               NULL,
               c(12,37),
               c(36,12),
               c(11,34),
               c(5,8,15,16,23,27,40,41,42),
               c(5,8,15,16,22,23,26,27,39,41,42,43),
               c(5,8,11,12,16,27,29,38,39,40,42),
               c(4,5,8,11,12,15,16,23,26,27,32,39,40,41,43),
               c(5,6,40,29,38,42),
               c(29,3,5,43,45),
               c(4,5,8,12,16,17,21,23,32,44),
               c(33),
               c(1,13,31),
               c(31,47),
               c(4,5,8,12,24,30,32,40))
a_list # Prints the adjacency list
names(a_list) <- c(1:49) # Naming vertices, 1:49 for 49 participants
a_list # Prints named adjacency list
g_alist <- graph_from_adj_list(a_list,mode = "out") # Creates a directed graph with outward links
# a_list : adjacency list
# mode   : Whether to create a directed ("in" or "out")/undirected("all")/weighted graph.
g_alist # Prints graph metadata
comps <- components(g_alist)$membership # Finding components and node membership
colbar <- rainbow(max(comps)+1) # Creating a color vector to assign diff colors to nodes based on membership
V(g_alist)$color <- colbar[comps+1] # Setting vertex color attribute

par(mar=c(0,0,0,0)) # Setting plot to have single plots

##### Plotting the graph
plot(g_alist,asp=0,margin = -0.07,vertex.size2 = 3, layout=layout_nicely,
     vertex.label.cex = 1,vertex.size=6, asp = 0,margin=0,edge.arrow.size=0.4,arrow.size=80)


## Since the data collected are of directed nature (who knows whom network, a knows b
## but b may not know a), we need to create a directed graph first. If we want to have 
## undirected graph (friendship network), then convert the directed graph to undirected
## one by "collapsing" the edges. 
##
## NOTE:: You should not create an undirected graph directly from a directed adjacency
## list/matrix/edgelist because it may create wrong links (symmetric link data is 
## absent in input).

# Convert to undirected graph
ug_alist = as.undirected(g_alist, mode = "collapse") # function for converting
ug_alist
comps <- components(ug_alist)$membership
colbar <- rainbow(max(comps)+1)
V(ug_alist)$color <- colbar[comps]

par(mar=c(0,0,0,0))
plot(ug_alist,asp=0,margin = -0.07,vertex.size2 = 3, layout=layout_nicely,
     vertex.label.cex = 1,vertex.size=6, asp = 0,margin=0,edge.arrow.size=0.4,arrow.size=80)


########################################################################################
## TO DO:                                                                             ##
## ======                                                                             ##
##                                                                                    ##
## Compute topological properties, centrality measures, and communities and their     ##
## properties from the above graphs. Also compute their transitivity and check if the ## 
## graphs exihibits small world property and are scale-free.                          ##
##                                                                                    ##
########################################################################################


################################# END OF DAY 5 #########################################
#                               ***************
#                                 ***********
#                                   *******
#                                     ***
#                                      *

