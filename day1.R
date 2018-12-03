#######################################################################################
#           FDP on NETWORK SCIENCE: FOUNDATION OF SOCIAL NETWORK ANALYSIS             #
#                                (December 03-08, 2018)                               #
#                 Department of Computer Science, University of Delhi                 #
#                                                                                     #
#                                       (DAY 1)                                       #
#######################################################################################


#######################################################################################
#   PART 1: Introduction to R and relevant packages
#######################################################################################

# 1. Install R and RStudio using following links:
# R: https://cran.r-project.org/
# RStudio: https://www.rstudio.com/products/rstudio/


# 2. Install package(s)
# i) igraph
install.packages(igraph)
# or install using GUI


# 3. Load package(s)
library(igraph)   # igraph is a library and R package for network analysis.


###################################
# R Basics
###################################

####### Assignment operation
a <- 3
a

b = 4
b

assign("c", 5)
c


######## Simple arithmetics
a = 1 + 2 # addition
a

a = 3 - 1 # substraction
a

a = 4 * 3 # multiplication
a

a = 4/2 # division
a

a = 5 %% 2 # modulus
a


#   =====================================

######## Data types
######## a) Vectors and sequences
a = c(2,3,4,1,5) # numeric vector
a

?c # help

b = c("a", "b", "c", "d", "e") # character vector
b

c = c("hello", "world") # string vector
c

d = c(TRUE, FALSE, FALSE, TRUE, FALSE) # logical vector
d

v1 = 1:5 # creates a vector containg 1, 2, 3, 4, 5
v2 = seq(10,20,2) # 10, 12, 14, 16, 18, 20
v3 = rep(1, 5) # repeat 1 five times, i.e. 1, 1, 1, 1, 1
v4 = rep(1:3, times = 5) # 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3, repeat 1,2,3 five times
v5 = rep(1:3, each = 2) # 1 1 2 2 3 3, repeat 1,2,3 two times each


length(v1) # check length of vector

a + v1 # element wise addition, vectors need to be of same length
sum(v1) # add all elements of v1

v1[3] # accessing third element of vector v1



#   =====================================

####### b) Factors - used to store categorical data
f = factor(c("blue", "brown", "blue", "green", "blue", "blue", "brown"))
f

levels(f) # distinct factor levels

as.numeric(f) # prints corresponding numeric value of the factor level

as.character(f) # prints character values



#   =====================================

####### b) Matrices and arrays
# create matrix from vector with dimension
m = rep(1, 20) # create a vector of 20 elements, all 1
dim(m) = c(5, 4) # dimension set to 5 rows and 4 columns
m

# create matrix with matrix() command
m1 = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2)
m1

m2 = matrix(data = c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
m2

# create matrix by combining vectors
m3 = cbind(1:5, 6:10, 11:15) # 5X3 matrix
m3
m4 = rbind(1:5, 6:10, 11:15) # 3X5 matrix
m4

# selecting matrix elements
m1[1,2] # first row, second column
m4[,2] # whole second column
m3[3,] # whole 3rd row
m4[1:2, 3:4] # submatrix: rows 1 and 2, columns 3 and 4 
m4[-2,] # all rows except row 2

# matrix arithmetic
t(m4) # transpose
m3 %*% m4 # matrix multiplication
m4 * m4 # element wise multiplication

# arrays: for more than 2 dimensions
a1 = array(data = 1:18, dim = c(3,3,2))
a1



#   =====================================

####### d) Data frames
#Example of Student data
df1 = data.frame(ID=1:4, Name = c("John", "Jane", "Adam", "Sean"), Height = c(1.51, 1.76, 1.65, 1.6), Senior_Citizen = c(F,F, T, F))
df1

df1$Name # Strings are by default considered as factors


df2 = data.frame(ID=1:4, Name = c("John", "Jane", "Adam", "Sean"), Height = c(1.51, 1.76, 1.65, 1.6), Senior_Citizen = c(F,F, T, F), stringsAsFactors = F)
df2

df2$Name # Now strings are not factors

df2[1,] # first row
df2[,1] # first column
df2[c(1,2),] # rows 1 and 2, all columns

df2[which(df2$Height > 1.55),] # select based on a criteria

# Create data frames by combining vectors
a = c(1,2,3)
b = c("abc","pqr","xyz")
c = c(T,F,F)
df3 = data.frame(a,b,c)
df3
names(df3) = c("ID", "Name", "Male?")
df3



#   =====================================

####### c) Lists
l1 = list() # empty list
l1

l2 = list(v1, m1) # list of two elements, one vector and one matrix
l2

names(l2) = c("Vector", "Matrix") # naming a list
l2

l3 = unlist(l2) # unlist list elements into a vector

l2[[3]] = 1 # adding more elements to the list
l2

l2$Vector # accessing Vector from list using names




#   =====================================

####### Other usefull functions
length(l2) # number of elements or components
str(l2)    # structure of an object 
class(l2)  # class or type of an object
names(l2)  # names

l2     # prints the object

ls()       # list current objects

l_dummy = l2
l_dummy # prints the object
rm(l_dummy) # delete an object
l_dummy # Error: object not found


#   =====================================

####### Data Visualization (simple plots)
v = c(1,2,3,1,2,4,3,5,6,7,2,3,4,1,3,4,2,1,1,1)
v

#hist(v_hist)
#hist(v_hist, xlab = "Values", main = "Histogram")


df = data.frame(id = c(1:20), value = v)
plot(df)
plot(df, type = "b")
?plot # help on plot function




#######################################################################################
#   PART 2: Introduction to Network Science Using R (Creating Graphs)
#######################################################################################


################### Create Graph from Edge List #####################
#Create a undirected graph from an edge list matrix
el<-matrix( c("V1","V2","V1","V3","V1","V4","V1","V5","V2","V5","V3","V4","V4","V5","V4","V7","V5","V8","V6","V2","V7","V8"), 
            nc = 2, byrow =TRUE)
el
g_el <- graph_from_edgelist(el,directed = FALSE)
# el: The edge list, a two column matrix, character or numeric.
# directed : Whether to create a directed graph.
g_el
plot(g_el, vertex.size=30)

#-------

#Create a directed graph from an edge list matrix
del<-matrix( c("V1","V2","V1","V3","V1","V4","V1","V5","V2","V5","V3","V4","V4","V5","V4","V7","V5","V8","V6","V2","V7","V8"), 
             nc = 2, byrow =TRUE)
del
dg_el <- graph_from_edgelist(del,directed = TRUE)
# del: The edge list, a two column matrix, character or numeric.
# directed : Whether to create a directed graph.
plot(dg_el,vertex.size=30)




################### Create Graph from Adjacency Matrix #####################
#Create a graph from an adjacency matrix
adjm <- matrix(c(0, 1, 1, 1, 1, 0, 0, 0,
                 1, 0, 0, 0, 1, 1, 0, 0,
                 1, 0, 0, 1, 0, 0, 0, 0,
                 1, 0, 1, 0, 1, 0, 1, 0,
                 1, 1, 0, 1, 0, 0, 0, 1,
                 0, 1, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 1, 0, 0, 0, 1,
                 0, 0, 0, 0, 1, 0, 1, 0),# the data elements
                 nrow=8,              # number of rows
                 ncol=8,              # number of columns
                 byrow = TRUE)        # fill matrix by rows
adjm
g_adjm <- graph_from_adjacency_matrix(adjm,mode = c("undirected"))
# adjm : adjacency matrix
# mode : Whether to create a directed/undirected/weighted graph.
g_adjm
get.adjedgelist(g_adjm)
plot(g_adjm, vertex.size=30)


################### Create Graph from Adjacency List #####################
#Create a graph from an adjacency list 
a_list <- list(c(2,3,4,5),c(1,5,6),c(4,1),c(1,3,5,7),c(1,2,4,8),c(2),c(4,8),c(5,7))
a_list
names(a_list) <- c(1,2,3,4,5,6,7,8) #set the names of list.
a_list
g_alist <- graph_from_adj_list(a_list,mode = "all")
# adjm : adjacency list
# mode : Whether to create a directed/undirected("all")/weighted graph.
g_alist
plot(g_alist, vertex.size=30)


######################### Create Network using "graph" function #################################
# Generate an undirected graph with three edges :
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F )
plot(g1,vertex.size=30)
g1

# Generate a graph with 5 vertices, and directed by default:
g2 <- graph( edges=c(1,2, 2,3, 3, 1), n=5)
plot(g2,vertex.size=30)
g2

# When the edge list has vertex names, the number of nodes is not needed
g3 <- graph( c ("John", "Jim", "Jim", "Jill", "Jill", "John"))  # named vertices
plot(g3)
g3


################################# Graph from literals ########################################
#  “-” for undirected tie, 
#  “+-” or “-+” for directed ties pointing left & right, 
#  “++” for symmetric tie, and 
#  “:” for sets of vertices.
plot( graph_from_literal(a---b, b---c)) # undirected graph
plot( graph_from_literal(a--+b, b+--c)) # directed graph
plot( graph_from_literal(a++b, b++c))   # symmetric graph
plot( graph_from_literal(a:b---c:d))


################################ Loops and Multiple edges ##################################
g <- graph( c(1,2,1,2,3,3) , dir=FALSE)
is_simple(g)  #check if graph is simple (has no loops and multiple edges)
is_simple(simplify(g, remove.loops=FALSE))
is_simple(simplify(g, remove.multiple=FALSE))
is_simple(simplify(g))
plot(g, vertex.size=30)


################################### Types of Graphs #################################
#empty graphs
eg <- make_empty_graph (20)
plot (eg, vertex.size=10, vertex.label=NA)

#full graphs
fg <-make_full_graph(20)
plot (fg, vertex.size=10, vertex.label=NA)

#star graphs
st <-make_star(20)
plot (st, vertex.size=10, vertex.label=NA)

#tree graphs
tr <-make_tree(20, children = 4, mode = "undirected")
plot (tr, vertex.size=10, vertex.label=NA)

#ring graphs
rg <-make_ring(20)
plot (rg, vertex.size=10, vertex.label=NA)


############### Convert graph to Edge list, Adjacency matrix , Adjacency list and Dataframe ################################
gg <- graph( c(1,2,1,3,2,3) , dir=TRUE)
plot(gg)

# graph to Edge list
as_edgelist(gg)

# graph to Adjacency matrix
as_adjacency_matrix(gg)

# graph to Adjacency list
as_adj_list(gg)

# graph to Dataframe
as_data_frame(gg)


######################## Read a small Example Edge list from csv file #############################
edges <- read.csv("graph.csv", header=TRUE) 
edges
head(edges,n=3L)  # retrieves 3 rows from edge list

network<-graph_from_data_frame(edges,dir = FALSE)  #making graph from data frame containing edge list
network
plot(network, vertex.size=30)  # plot the network
V(network) # gives nodes details
E(network) # gives edges details
vcount(network)  # count of vertices
ecount(network)  # count of edges
is.multiple(network)  # check for multiple edges


# Plot the graph
# Interactive plotting of network with tkplot
tkplot(network,edge.label=E(network)$weight,vertex.size=20,vertex.color=c("yellow"),vertex.label.color="black")


g<-network


######################## Graph from built-in dataset #############################3
gz<-graph("Zachary")
plot(gz)


############################### Save graphs ###############################################
# as RData file (.Rda)
g_el = as_edgelist(g, names = TRUE)
saveRDS(g_el, file = "/Save/To/Location/g.Rda")


# as CSV file (.csv)
g_el = as_edgelist(g, names = TRUE)
write.csv(g_el, file = "/Save/To/Location/g.csv", row.names = F, col.names = F)


########################### Read from saved graph  ##########################################
# from .Rda
df = readRDS(file = "/Read/From/Location/g.Rda")
g = graph_from_edgelist(df, directed = FALSE)
plot(g)  


# from .csv
df = as.matrix(read.csv(file = "/Read/From/Location/g.csv", header = TRUE))
g = graph_from_edgelist(df, directed = FALSE)
plot(g)



################################# END OF DAY 1 #########################################
#                               ***************
#                                 ***********
#                                   *******
#                                     ***
#                                      *

