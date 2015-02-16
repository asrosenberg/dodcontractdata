################################################################################
## DOD CONTRACT DATA NETWORK                                                  ##
################################################################################
# load libraries
library(igraph)

# set working directory
setwd("~/Dropbox/Documents/OSU/Spring_2015/Advanced Networks/Data")

# load data (sample of 999 obs from FY 2006)
dod <- read.csv("~/Dropbox/Documents/OSU/Spring_2015/Advanced Networks/Data/dod.csv")

# create an edgelist
el <- dod[, c("vendorname", "piid")]
 
# create an igraph item
net <- graph.edgelist(as.matrix(el))

# create a ``naive" network specification of the DOD contract data
# here the nodes are government contracters and ties are common project

# plot the initial one-mode projection of the network
V(net)$label = V(net)$name
V(net)$label.color = rgb(0,0,.2,.8)
V(net)$label.cex = .8
V(net)$size = 8
V(net)$frame.color = NA
V(net)$color = rgb(0,0,1,.5)

plot1 <- plot(net,
     layout=layout.fruchterman.reingold, 
     main = "Network of DOD Contractors: FY 2006",
     vertex.size=8,
     vertex.label=NULL,
     edge.arrow.size=.15)

# create a bipartite projection/ affiliation network
# nodes: government contracting agency and contractor
el2 <- dod[, c("mod_agency", "vendorname")]

net2 <- graph.edgelist(as.matrix(el2))

summary(net2) # 688 vertices and 999 edges
unique(el2$mod_agency) #  14 unique government agencies issued contracts
unique(el2$vendorname) # 674 contractors won government defense contracts

# plot of bipartite projection
V(net2)$type <- bipartite.mapping(net2)$type

V(net2)$label.cex <- .9

plot2 <- plot(net2, 
     main = "Bipartite Projection of DOD Contract Data",
     layout=-layout.bipartite(net2)[,1:2], 
     vertex.size=10, 
     edge.arrow.size=.15,
     vertex.shape=ifelse(V(net2)$type, "rectangle", "circle"),
     vertex.color=ifelse(V(net2)$type, "red", "green"))

# network attributes for the one-mode affiliation network
# nodes = contractors, ties = common contract

# summary statistics
summary(net) # 1551 nodes and 999 ties
vcount(net) # 1551 nodes
ecount(net) # 999 ties
unique(el$piid) #  877 unique government defense contracts
unique(el$vendorname) # 674 contractors won government defense contracts
graph.attributes(net) # name list()

# first five dyads on the edge list
head(V(net)$name) 
head(E(net))

is.directed(net) # FALSE: no directed ties in the network

# weight edges by contract dollar amount
is.weighted(net) # FALSE
E(net)$weight <- dod$dollarsobligated
is.weighted(net) # TRUE
head(E(net)$weight) # dollar values of the first five defense contracts
# no clue why some contracts have negative contract values?

# plot the initial one-mode network
plot(net,
     layout=layout.fruchterman.reingold, 
     main = "Network of DOD Contractors: FY 2006",
     vertex.size=8,
     vertex.label=NULL,
     edge.arrow.size=.15)

# degree distribution
degree(net) # degree of each node in the network
summary(degree(net)) # 15.13
hist(degree(net), main='Degree Distribution of DOD Contract Data', 
     xlab='degree') # this distribution has a long right tail

V(net)[degree(net)<16] # 1544 nodes less than avg (only 7 nodes have more)

# new network where the degree is less than 16
net_low <- induced.subgraph(net, V(net)[degree(net)<16])
summary(net_low) # 1544 nodes, 828 ties (summary of the induced subgraph)

# new network where the degree is greater than 16
net_high <- induced.subgraph(net, V(net)[degree(net)>16])
summary(net_high) # 6 nodes, 0 edges

vcount(net)-vcount(net_low) # eliminated 7 nodes
ecount(net)-ecount(net_low) # eliminated 171 ties 

# is this a simple graph (i.e., are there no loops and no multi-edges)?
is.simple(net) # FALSE: no loops or multi-edges

# graph structure
is.connected(net) # FALSE: Unconnected

# what is the group membership of the various clusters in the graph?
clusters(net) # a cluster is a set of nodes with full connection to each other 
# 553 isolates (singletons) in our graph

# what is the geodesic distance of the graph (the longest shortest path in the graph)
diameter(net, weights=NA) # accounting for weights (1 step)

# because this is a weighted graph, we can also calculate the strength of 
# the nodes which is simply the degree taking into account edge weights
graph.strength(net) # strength = weighted degree
# weighted value of the edges a node is connected to
summary(graph.strength(net))

cor(graph.strength(net), degree(net)) 
# average weight of the edges -- nearly a 1-to-1 correspondance

cor(graph.strength(net, weights=NA), degree(net)) 
# exactly the same as degree, thus correlation = 1.0

# measures of centrality
# degree centrality
d.net <- degree(net)

# closeness centrality
c.net <- closeness(net) # connection to other important nodes
# one interpretation: diffusion processes (only a few steps to reach many nodes)

# betweenness centrality
b.net <- betweenness(net) # nodes lying on paths between influential
# nodes (ie gatekeepers)

# eigenvector centrality
e.net <- evcent(net) # centrality of self + centrality of those connected
# to

cor(cbind(c.net, b.net))
# What do these correlations tell us?
# moderate correlations = measuring different aspects of the network 
# different ideas about who is central

# next, we can look at hub and authority scores
# first, we can calculate the hub and authority scores
# hub and authority scores necessitate directed ties

# What are hub scores?
# hubs take into account out-degree ties

# What are authority scores?
# in-degree ties

# conglomeration of ties between hubs and authorities
# makes sense in the context of a citation network

hub_net <- hub.score(net)
authority_net <- authority.score(net)

summary(hub_net$vector)
summary(authority_net$vector)

cor(authority_net$vector, hub_net$vector) # -0.00385

# group structure
# we can begin our exploration of the structure of the graphs by looking at the cliques
cliques(net)

table(sapply(cliques(net), length))
# 1551 singletons
# 998 dyads

table(sapply(maximal.cliques(net), length))
# 998 dayds represent the maximal cliques

# or the largest fully-connected clique overall
clique.number(net) # dyad is the largest fully-connected clique overall

# another great place to begin looking at group structure censuses
# particularly for directed graphs
dyad.census(net)
# tells us 0 mutual dyads (undirected = all are mutual ties)
# 1201026 = universe of unrealized ties that could exist
# 999 on asymmetric tells us that this is directed (edge count)

# we can also look at the proportion of possible edges that are realized
graph.density(net) # 0.0004
# porportion of edges that are realized in the network

# similarly, we can look at the transitivity of the graph

# as a first step to studying how much the graph clusters
transitivity(net) # 0.00
# number of fully-connected triads / total triads
# a measure of how closed things are

#######################
# community structure # 
#######################

is.connected(net) # FALSE

comps_net <- decompose.graph(net)

table(sapply(comps_net, vcount))
# list of every fully-connected subgraphs within the network
# one giant component consisting of 40 vertices

vcount(net) # 1551 nodes

table(sapply(comps_net, transitivity))
# how can we select the giant component from our list?

sapply(decompose.graph(net), vcount)
# gives the number of nodes in each one of the decomposed graphs

net.gc <- decompose.graph(net)[[1]]
vcount(net.gc) # 2 nodes in the giant component (just a dyad)

# here's an example of a way that we can "soft" code it such that we get 
# the giant component every time
net.gc <- decompose.graph(net)[[(1:length(decompose.graph(net)))
                        [sapply(decompose.graph(net), vcount)==
                         max(sapply(decompose.graph(net), vcount))]]]

max(shortest.paths(net.gc)) # 11
min(shortest.paths(net.gc)) # 0

# articulation points
net.cut.vertices <- articulation.points(net)
net.cut.vertices 

# we can look at average path length, diameter and transitivity, 
# particularly if we are interested in whether our network fits the 
# small-world categorization
average.path.length(net.gc) # 1 edge to the average path
diameter(net.gc) # 1 longest shortest path (since it's weighted)
transitivity(net.gc) # 0 no triads present in the graph

# we can also look at various forms of community assignment

net.c1 <- edge.betweenness.community(net.gc)
net.c2 <- fastgreedy.community(as.undirected(simplify(net.gc)))
net.c3 <- spinglass.community(net.gc)
net.c4 <- walktrap.community(net.gc)

# next, we can examine the output of 
length(net.c1)
sizes(net.c1)

length(net.c2)
sizes(net.c2)

length(net.c3)
sizes(net.c3)

length(net.c4)
sizes(net.c4)

# and we can look at the membership assignments
# that each of the methods give

membership(net.c1)

membership(net.c2)

membership(net.c3)

membership(net.c4)

par(mfrow=c(1,4))
plot(net.c1,net, vertex.label=NA)
plot(net.c2,net, vertex.label=NA)
plot(net.c3,net, vertex.label=NA)
plot(net.c4,net, vertex.label=NA)

###############################################################################
