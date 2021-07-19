# Compleet network
library(igraph)
library(car)
library(psych)
library(tnet)
library(netseg)
#######################################################################################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse")

node <- read.csv("node_compleet.csv", header = T)
edge <- read.csv("edge_compleet.csv", header = T)

head(node); head(edge)
edge.mat <- as.matrix(edge)
edgegraph <- graph.edgelist(edge.mat, directed = F)
edge2M <- edgegraph
V(edge2M)$type <- bipartite.mapping(edge2M)$type

node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 0] <- "red"
node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
col1 = node$indicatoren_2019.rechtmatigheid
V(edge2M)$color <- ifelse(V(edge2M)$type, "black", col1)
V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")

projection_edge2M <- bipartite.projection(edge2M, multiplicity = T)

#plot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=1)
plot(edge2M, layout = layout.fruchterman.reingold, vertex.label=NA, vertex.size=1, main= "Compleet network diagram")
legend(
  "bottomright",
  pch = 21,
  legend = c("director", "fraugulent healthcare company", "non-fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("Black", "Red", "green"),
  col = "black",
  bty = "n"
)

####################################################################################
####################################################################################

#edge2MB <- cluster_edge_betweenness(edge2M)
#length(edge2MB)
#membership(edge2MB)
#class(edge2MB)
#modularity(edge2MB)
#crossing(edge2MB, edge2M)
#plot(edge2MB, edge2M)

#edge2MC <- cluster_label_prop(edge2M)
#plot(edge2MC, edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)

length(unique(edge$Name))
length(unique(edge$ï..Consern.code))
length(edge$Name)

setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse")
noze <- read.csv("xxx.csv", header = T);noze

length(noze$rechtmatigheid[noze$rechtmatigheid > 0])
length(noze$rechtmatigheid[noze$rechtmatigheid < 0])
median(count(edge$Name))
summary(length(unique(edge$Name)))
max(degree(edge2M))
summary(degree(edge2M))

max(degree(projection_edge2M$proj1))
max(degree(projection_edge2M$proj2))
min(degree(projection_edge2M$proj1))
min(degree(projection_edge2M$proj2))

mean(degree(projection_edge2M$proj2))
hist(degree(projection_edge2M$proj1))
hist(degree(edge2M))

length(node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid == "red"])
length(node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid == "chartreuse3"])

projection_edge2M <- bipartite.projection(edge2M, multiplicity = T)

x <- cluster_edge_betweenness(edge2M)
x

x <- betweenness(projection_edge2M$proj1, directed = F, weights = NA)
mean(x)
xa <- betweenness(edge2M, directed = F, weights = NA)
mean(xa)
xb <- betweenness(projection_edge2M$proj2, directed = F, weights = NA); mean(xb)
projection_edge2M$proj2

xc <- edge_betweenness(projection_edge2M$proj1, directed = F, weights = NA); mean(xc)

betweenness(net, directed=T, weights=NA)
edge_betweenness(net, directed=T, weights=NA)
centr_betw(net, directed=T, normalized=T)

####################################################################################
####################################################################################

length(unique(Edge_Voor2015$Name))
length(unique(Edge_Voor2015$ï..Consern.code))
length(Edge_Voor2015$Name)*2

length(unique(Edge_Na2015$Name))
length(unique(Edge_Na2015$ï..Consern.code))
length(Edge_Na2015$Name)*2

mean(degree(edge2M_Voor2015))
mean(degree(edge2M_Na2015))
mean(degree(edge2M))
mean(degree(edge2M_h2))


mean(degree(Projection_edge2M_Voor2015$proj1))
mean(degree(Projection_edge2M_Voor2015$proj2))

mean(degree(Projection_edge2M_Na2015$proj1))
mean(degree(Projection_edge2M_Na2015$proj2))

mean(degree(projection_edge2M$proj1))
mean(degree(projection_edge2M$proj2))

mean(degree(projection_edge2M_h2$proj1))
mean(degree(projection_edge2M_h2$proj2))


###############################################################################
node <- read.csv("node_compleet.csv", header = T)
edge <- read.csv("edge_compleet.csv", header = T)

head(node); head(edge)
edge.mat <- as.matrix(edge)
edgegraph <- graph.edgelist(edge.mat, directed = F)
edge2M <- edgegraph
V(edge2M)$type <- bipartite.mapping(edge2M)$type


projct_edge2M <- bipartite.projection(edge2M, multiplicity = T)
#plot(projct_edge2M$proj1, vertex.label=NA, vertex.size=3)

V(projct_edge2M$proj1)$recht = node$indicatoren_2019.rechtmatigheid
node$indicatoren_2019.rechtmatigheid


node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 0] <- 5
node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 1] <- 10
node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 6] <- 0
node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid > 7] <- 1

tm <- get.edgelist(projct_edge2M$proj1, names = F)
tm2 <- graph.edgelist(tm, directed = F)
V(tm2)$recht <- node$indicatoren_2019.rechtmatigheid


freeman(tm2, V(tm2)$recht, gsizes = NULL, loops = any(is.loop(tm2)))


freeman(projct_edge2M$proj1, V(projct_edge2M$proj1)$recht)



assortativity(projct_edge2M$proj1, V(projct_edge2M$proj1)$recht,  directed = F)
projct_edge2M

#node$Year[node$Year < 2015] <- "onder"
#node$Year[node$Year > 2015] <- "boven"
#col2 = node$Year
#
tm <- get.edgelist(edge2M, name=F)
deg_tm <- degree_tm(tm)
clustering_w(, measure="mi")

freeman(projct_edge2M$proj, gsizes=965, loopos =  any(is.loop(projct_edge2M$proj1)))


length(unique(edge$Name))
length(unique(edge$ï..Consern.code))
centr_degree(projct_edge2M$proj2, mode= c("all", "out", "in", "total"))
centr_degree(projct_edge2M$proj2)$centrialisation
centr_clo(projct_edge2M$proj2, mode = "all")$centrialisation
centr_degree(projection_edge2M$proj2, mode = "out")
centr_degree(projct_edge2M$proj2, mode = "in")
centr_degree(edge2M, mode="out")
centr_degree(edge2M, mode="in")