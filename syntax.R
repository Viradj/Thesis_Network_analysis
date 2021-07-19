##### Load Library's ###############
library(igraph)
library(tnet)
library(netseg)
##############################################

######################################
############Compleet data#############

################ Render 1 ###########################################################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 0")
edge_R1 <- read.csv("network 1.csv", header =T)

edge.mat_R1 <- as.matrix(edge_R1)
edgegraph_R1 <- graph.edgelist(edge.mat_R1, directed = F)
edge2M_R1 <- edgegraph_R1
V(edge2M_R1)$type <- bipartite.mapping(edge2M_R1)$type

V(edge2M_R1)$shape <- ifelse(V(edge2M_R1)$type, "circle", "square")
V(edge2M_R1)$color <- ifelse(V(edge2M_R1)$type, "black", "green")
plot(edge2M_R1, layout = layout_randomly, vertex.size=16, name="test")

legend(
  "bottomright",
  pch = 21,
  legend = c("directors", "healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "green"),
  col = "black",
  bty = "n"
)


################## Render 2 #################################################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 0")
edge_R2 <- read.csv("network 2.csv", header =T)

edge.mat_R2 <- as.matrix(edge_R2)
edgegraph_R2 <- graph.edgelist(edge.mat_R2, directed = F)
edge2M_R2 <- edgegraph_R2
V(edge2M_R2)$type <- bipartite.mapping(edge2M_R2)$type

V(edge2M_R2)$shape <- ifelse(V(edge2M_R2)$type, "circle", "square")
V(edge2M_R2)$color <- ifelse(V(edge2M_R2)$type, "black", "red")
plot(edge2M_R2, vertex.size=16)

legend(
  "bottomright",
  pch = 21,
  legend = c("directors", "healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "red"),
  col = "black",
  bty = "n"
)

V(edge2M_R2)$recht1 <- 


##################Render 3##########################################################################

setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 0")
edge_R3 <- read.csv("network 3.csv", header =T)

edge.mat_R3 <- as.matrix(edge_R3)
edgegraph_R3 <- graph.edgelist(edge.mat_R3, directed = F)
edge2M_R3 <- edgegraph_R3
V(edge2M_R3)$type <- bipartite.mapping(edge2M_R3)$type

V(edge2M_R3)$shape <- ifelse(V(edge2M_R3)$type, "circle", "square")
V(edge2M_R3)$color <- ifelse(V(edge2M_R3)$type, "black", "red")
plot(edge2M_R3, vertex.size=16)

legend(
  "bottomright",
  pch = 21,
  legend = c("director", "healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "red"),
  col = "black",
  bty = "n"
)



################ Render 4 ###########################################################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 0")
edge_R4 <- read.csv("network 4.csv", header =T)

edge.mat_R4 <- as.matrix(edge_R4)
edgegraph_R4 <- graph.edgelist(edge.mat_R4, directed = F)
edge2M_R4 <- edgegraph_R4
V(edge2M_R4)$type <- bipartite.mapping(edge2M_R4)$type

V(edge2M_R4)$shape <- ifelse(V(edge2M_R4)$type, "circle", "square")
V(edge2M_R4)$color <- ifelse(V(edge2M_R4)$type, "black", "green")

betweenness <- cluster_edge_betweenness(edge2M_R4)
plot(betweenness, edge2M_R4, layout = layout.fruchterman.reingold, vertex.size=16, main="Three communities types")




plot(edge2M_R4, layout = layout.fruchterman.reingold, vertex.size=16, main="Render")
legend(
  "bottomright",
  pch = 21,
  legend = c("directors", "healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "green"),
  col = "black",
  bty = "n"
)


#######################################################################################################


length(unique(edge$Name))
length(unique(edge$ï..Consern.code))

setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse")

node <- read.csv("node_compleet.csv", header = T)
edge <- read.csv("edge_compleet.csv", header = T)
head(node); head(edge)
edge.mat <- as.matrix(edge)
edgegraph <- graph.edgelist(edge.mat, directed = F)
edge2M <- edgegraph
V(edge2M)$type <- bipartite.mapping(edge2M)$type

node$Year[node$Year < 2015] <- "onder"
node$Year[node$Year > 2015] <- "boven"
col2 = node$Year
 
tm <- get.edgelist(edge2M, name=F)
deg_tm <- degree_tm(tm)
clustering_w(, measure="mi")

V(edge2M)$color <- ifelse(V(edge2M)$type, "black", col2)
V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")


node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 0] <- "red"
node$indicatoren_2019.rechtmatigheid[node$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
col1 = node$indicatoren_2019.rechtmatigheid
V(edge2M)$color <- ifelse(V(edge2M)$type, "black", col1)
V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")

#edge2MB <- cluster_edge_betweenness(edge2M)
#length(edge2MB)
#membership(edge2MB)
#class(edge2MB)
#modularity(edge2MB)
#crossing(edge2MB, edge2M)
#plot(edge2MB, edge2M)

edge2MC <- cluster_label_prop(edge2M)
#plot(edge2MC, edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)

#tkplot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=1)

projct_edge2M <- bipartite.projection(edge2M, multiplicity = T)
plot(projct_edge2M$proj1, vertex.label=NA, vertex.size=3)

projct_edge2M


centr_degree(projct_edge2M$proj2, mode= c("all", "out", "in", "total"))
centr_degree(projct_edge2M$proj2)$centrialisation
centr_clo(projct_edge2M$proj2, mode = "all")$centrialisation
centr_degree(projct_edge2M$proj2, mode = "out")
centr_degree(projct_edge2M$proj2, mode = "in")
centr_degree(edge2M, mode="out")
centr_degree(edge2M, mode="in")
###################################################
igraph.options(vertex.color = 'lightblue', vertex.frame.color = 'grey50', 
               vertex.label.family = "Droid Sans", vertex.label.color="black", vertex.label.cex = 0.7, 
               edge.arrow.size = 0.5)

ar(mfrow=c(1,2))
layouts <- list()
layouts[[1]] <- layout.kamada.kawai(gs[[1]])
layouts[[2]] <- layout.kamada.kawai(gs[[2]])
plot(gs[[1]], vertex.label = V(gs[[1]])$title, layout = layouts[[1]], main="Time 1")
plot(gs[[2]], vertex.label = V(gs[[2]])$title, layout = layouts[[2]], main="Time 2")



#####################################################



#####Descriptive#########

#count the number of interlocking directors
#ref: https://statisticsglobe.com/count-unique-values-in-r
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 2")
x <- read.csv("forcounth1.csv")
table(x)
as.data.frame(table(x))
aggregate(data.frame(count = x),
          list(value = x), length)

#count the number of unq. directors
#ref: https://statisticsglobe.com/count-unique-values-in-r
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse")
x2 <- read.csv("forcounth2.csv")
table(x2)
as.data.frame(table(x2))
aggregate(data.frame(count = x2),
          list(value = x2), length)

#visualize compleet network for descriptive
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse")
library(igraph)
library(tnet)
library(netseg)
node_compleet <- read.csv("node_compleet.csv", header = T)
edge_compleet <- read.csv("edge_compleet.csv", header = T)
head(node_compleet); head(edge_compleet)

edge.mat_compleet <- as.matrix(edge_compleet)
edgegraph_compleet <- graph.edgelist(edge.mat_compleet, directed = F)
edge2M_compleet <- edgegraph_compleet
V(edge2M_compleet)$type <- bipartite.mapping(edge2M_compleet)$type

node_compleet$indicatoren_2019.rechtmatigheid[node_compleet$indicatoren_2019.rechtmatigheid < 0] <- "red"
node_compleet$indicatoren_2019.rechtmatigheid[node_compleet$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"

col_compleet = node_B2015$indicatoren_2019.rechtmatigheid

V(edge2M_compleet)$color <- ifelse(V(edge2M_compleet)$type, "black", col_compleet)
V(edge2M_compleet)$shape <- ifelse(V(edge2M_compleet)$type, "circle", "square")
E(edge2M_compleet)$color <- "lightgray"

plot(edge2M_compleet, vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2)
legend(
  "bottomright",
  pch = 21,
  legend = c("directors", "non-frauglent healthcare company", "fraugelent healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "green", "red"),
  col = "black",
  bty = "n"
)



######################### Hypothese 1 ########################################################
#http://www.kateto.net/netscix2016 

############## Before 2015 #####################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 1")

node_B2015 <- read.csv("node_voor2015.csv", header = T)
edge_B2015 <- read.csv("edge_voor2015.csv", header = T)

head(node_B2015); head(edge_B2015)
edge.mat_B2015 <- as.matrix(edge_B2015)
edgegraph_B2015 <- graph.edgelist(edge.mat_B2015, directed = F)
edge2M_B2015 <- edgegraph_B2015
V(edge2M_B2015)$type <- bipartite.mapping(edge2M_B2015)$type

node_B2015$indicatoren_2019.rechtmatigheid[node_B2015$indicatoren_2019.rechtmatigheid < 0] <- "red"
node_B2015$indicatoren_2019.rechtmatigheid[node_B2015$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"

col_B2015 = node_B2015$indicatoren_2019.rechtmatigheid

V(edge2M_B2015)$color <- ifelse(V(edge2M_B2015)$type, "black", col_B2015)
V(edge2M_B2015)$shape <- ifelse(V(edge2M_B2015)$type, "circle", "square")

plot(edge2M_B2015,vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2)

edge2M_Between_B2015 <- cluster_edge_betweenness(edge2M_B2015)
length(edge2M_Between_B2015)
membership(edge2M_Between_B2015)
modularity(edge2M_Between_B2015)
communities(edge2M_Between_B2015)

projct_edge2M_B2015 <- bipartite.projection(edge2M_B2015, multiplicity = T)
plot(projct_edge2M_B2015$proj1, vertex.label=NA, vertex.size=3)


plot(projct_edge2M_B2015$proj1, vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2)
legend(
  "bottomright",
  pch = 21,
  legend = c("non-fraugulent healthcare company", "fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("green", "red"),
  col = "black",
  bty = "n"
)

mean(degree(edge2M_B2015))

##################### After 2015 ###################################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 1")

node_N2015 <- read.csv("node_na2015.csv", header = T)
edge_N2015 <- read.csv("edge_na2015.csv", header = T)

head(node_N2015); head(edge_N2015)
edge.mat_N2015 <- as.matrix(edge_N2015)
edgegraph_N2015 <- graph.edgelist(edge.mat_N2015, directed = F)
edge2M_N2015 <- edgegraph_N2015
V(edge2M_N2015)$type <- bipartite.mapping(edge2M_N2015)$type

node_N2015$indicatoren_2019.rechtmatigheid[node_N2015$indicatoren_2019.rechtmatigheid < 0] <- "red"
node_N2015$indicatoren_2019.rechtmatigheid[node_N2015$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"

col_N2015 = node_N2015$indicatoren_2019.rechtmatigheid

V(edge2M_N2015)$color <- ifelse(V(edge2M_N2015)$type, "black", col_N2015)
V(edge2M_N2015)$shape <- ifelse(V(edge2M_N2015)$type, "circle", "square")

#plot(edge2M_N2015,vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2)

edge2M_Between_N2015 <- cluster_edge_betweenness(edge2M_N2015)
length(edge2M_Between_N2015)#aantal communties
membership(edge2M_Between_N2015)
modularity(edge2M_Between_N2015)
communities(edge2M_Between_N2015)
assortativity_nominal(edge2M_N2015)


projct_edge2M_N2015 <- bipartite.projection(edge2M_N2015, multiplicity = T)
plot(projct_edge2M_N2015$proj1, vertex.label=NA, vertex.size=3)


plot(projct_edge2M_N2015$proj1, vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2)
legend(
  "bottomright",
  pch = 21,
  legend = c("non-fraugulent healthcare company", "fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("green", "red"),
  col = "black",
  bty = "n"
)



##########T-test##




N <- degree(edge2M_N2015)
B <- degree(edge2M_B2015)
t.test(N,B)

mean(degree(edge2M_B2015))
mean(degree(edge2M_N2015))



# Test whether the means of covert and overt are significantly different or not
t.test(mean(degree(edge2M_B2015)), mean(degree(edge2M_N2015)))



##################################### Hypothese 2 ###########################################################


setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 2")

node_h2 <- read.csv("Aantal Name +1 Node.csv", header = T)
edge_h2 <- read.csv("Aantal Name +1 Edge.csv", header = T)
head(node_h2); head(edge_h2)
edge.mat_h2 <- as.matrix(edge_h2)
edgegraph_h2 <- graph.edgelist(edge.mat_h2, directed = F)
edge2M_h2 <- edgegraph_h2
V(edge2M_h2)$type <- bipartite.mapping(edge2M_h2)$type

V(edge2M_h2)$color <- ifelse(V(edge2M_h2)$type, "black", col2)
V(edge2M_h2)$shape <- ifelse(V(edge2M_h2)$type, "circle", "square")

node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0] <- "red"
node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"

col1 = node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid
V(edge2M_h2)$color <- ifelse(V(edge2M_h2)$type, "black", col1)
V(edge2M_h2)$shape <- ifelse(V(edge2M_h2)$type, "circle", "square")

edge2MB_h2 <- cluster_edge_betweenness(edge2M_h2)
length(edge2MB_h2)
membership(edge2MB_h2)
class(edge2MB_h2)
modularity(edge2MB_h2)

freeman(project_edge2M_h2$proj1, vattr = vcount(project_edge2M_h2$proj1), gsizes = NULL, loops = any(is.loop(edge.mat_h2)))


net2 <- graph_from_incidence_matrix(edge_h2)
net2
vcount(net2)
length(net2)

net2.bp <- bipartite.projection(net2)
vcount(net2.bp$proj1)
length(net2.bp$proj1)
freeman(net2, vattr = vcount(net2), gsizes = NULL, loops = any(is.loop(net2)))


vcount(edgegraph_h2)
length(edgegraph_h2)
length(edge2M_h2)
vcount(project_edge2M_h2$proj1)
freeman(edge.mat_h2, gsizes=T, )
data("WhiteKinship")
WhiteKinship

vcount(WhiteKinship);length(WhiteKinship)

Class(WhiteKinship)
#plot(WhiteKinship)

edge2M_h2
project_edge2M_h2$proj1

xx <- as_adjacency_matrix(net2)
vcount(xx)
length(xx)
xx2 <- graph_from_adjacency_matrix(xx)
vcount(xx2)
length(xx2)


freeman(project_edge2M_h2$proj1)

project_edge2M_h2 <- bipartite_projection(edge2M_h2, multiplicity = T)
h2_projection_1 <- project_edge2M_h2$proj1
plot(h2_projection_1,vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=3, main = "Healthcare companies that share at least one director")
legend(
  "bottomright",
  pch = 21,
  legend = c("non-frauglent company", "fraugelent company"),
  pt.cex = 2,
  pt.bg = c("green", "red"),
  col = "black",
  bty = "n"
)



g <- h2_projection_1

types <- V(g)$type               
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

cent_df <- data.frame(types, deg, bet, eig)

cent_df[order(cent_df$type, decreasing = TRUE),]




plot(edge2M_h2,vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=3)


projct_edge2M_h2 <- bipartite.projection(edge2M_h2, multiplicity = T)
plot(projct_edge2M_h2$proj1, vertex.label=NA, vertex.size=4)

edge2MC <- cluster_label_prop(edge2M)
plot(edge2MC, edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)

tkplot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=1)





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

