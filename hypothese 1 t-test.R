##### Load Library's & more ###############
library(igraph)
library(car)
library(psych)
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 1\\new-h1")
##############################################

######(Before) Voor 2015 Fraugulent ###########################################################################################################

Node_Voor2015 <- read.csv("Voor2015_F.csv", header=T)
Edge_Voor2015 <- read.csv("Edge-Voor2015_F.csv", header=T)
#head(Node_Voor2015);head(Edge_Voor2015)

edge.mat_Voor2015 <- as.matrix(Edge_Voor2015)
edgegraph_Voor2015 <-graph.edgelist(edge.mat_Voor2015, directed = F)
edge2M_Voor2015 <- edgegraph_Voor2015
V(edge2M_Voor2015)$type <- bipartite.mapping(edge2M_Voor2015)$type

Node_Voor2015_Color <- Node_Voor2015
Node_Voor2015_Color$indicatoren_2019.rechtmatigheid[Node_Voor2015_Color$indicatoren_2019.rechtmatigheid < 0] <- "red"
Node_Voor2015_Color$indicatoren_2019.rechtmatigheid[Node_Voor2015_Color$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
Node_Voor2015_Color_Sheet <- Node_Voor2015_Color$indicatoren_2019.rechtmatigheid
V(edge2M_Voor2015)$color <- ifelse(V(edge2M_Voor2015)$type, "black", Node_Voor2015_Color_Sheet )
V(edge2M_Voor2015)$shape <- ifelse(V(edge2M_Voor2015)$type, "circle", "square")

plot(edge2M_Voor2015, vertex.label=NA, vertex.size=3, layout = layout_with_graphopt, main = "Fraudulent healthcare companies and directors before 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("Directors", "Fraudulent company"),
  pt.cex = 2,
  pt.bg = c("Black", "Red"),
  col = "black",
  bty = "n"
)

edge2M_Voor2015_Between <- cluster_edge_betweenness(edge2M_Voor2015)
length(edge2M_Voor2015_Between)
#membership(edge2M_Voor2015_Between)
modularity(edge2M_Voor2015_Between)
#communities(edge2M_Voor2015_Between)

Projection_edge2M_Voor2015 <- bipartite.projection(edge2M_Voor2015, multiplicity = T)
plot(Projection_edge2M_Voor2015$proj1, vertex.label=NA, vertex.size=3, layout = layout_with_graphopt, main = "Fraudulent healthcare companies before 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("Fraudulent company"),
  pt.cex = 2,
  pt.bg = c("Red"),
  col = "black",
  bty = "n"
)

summary(degree(edge2M_Voor2015))
summary(degree(Projection_edge2M_Voor2015$proj1))
summary(degree(Projection_edge2M_Voor2015$proj2))


##############(NA)Na 2015 Fraugulent ############################################################################################################
Node_Na2015 <- read.csv("Na2015_F.csv", header=T)
Edge_Na2015 <- read.csv("Edge-NA2015_F.csv", header=T)
#head(Node_Na2015);head(Edge_Na2015)

edge.mat_Na2015 <- as.matrix(Edge_Na2015)
edgegraph_Na2015 <-graph.edgelist(edge.mat_Na2015, directed = F)
edge2M_Na2015 <- edgegraph_Na2015
V(edge2M_Na2015)$type <- bipartite.mapping(edge2M_Na2015)$type

Node_Na2015_Color <- Node_Na2015
Node_Na2015_Color$indicatoren_2019.rechtmatigheid[Node_Na2015_Color$indicatoren_2019.rechtmatigheid < 0] <- "red"
Node_Na2015_Color$indicatoren_2019.rechtmatigheid[Node_Na2015_Color$indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
Node_Na2015_Color_Sheet <- Node_Na2015_Color$indicatoren_2019.rechtmatigheid

V(edge2M_Na2015)$color <- ifelse(V(edge2M_Na2015)$type, "black", Node_Na2015_Color_Sheet )
V(edge2M_Na2015)$shape <- ifelse(V(edge2M_Na2015)$type, "circle", "square")

#plot(edge2M_Na2015, vertex.label=NA, vertex.size=3, layout = layout_with_graphopt, main = "Fraugulent healthcare companies and directors after 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("Director", "fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("Black", "Red"),
  col = "black",
  bty = "n"
)

edge2M_Na2015_Between <- cluster_edge_betweenness(edge2M_Na2015)
length(edge2M_Na2015_Between)
#membership(edge2M_Na2015_Between)
modularity(edge2M_Na2015_Between)
#communities(edge2M_Na2015_Between)

Projection_edge2M_Na2015 <- bipartite.projection(edge2M_Na2015, multiplicity = T)
plot(Projection_edge2M_Na2015$proj1, vertex.label=NA, vertex.size=3, layout = layout_with_graphopt, main = "Fraudulent healthcare companies after 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("Fraudulent company"),
  pt.cex = 2,
  pt.bg = c("Red"),
  col = "black",
  bty = "n"
)

summary(degree(edge2M_Na2015))
summary(degree(Projection_edge2M_Na2015$proj1))
summary(degree(Projection_edge2M_Na2015$proj2))

summary(degree(edge2M_Voor2015))
summary(degree(Projection_edge2M_Na2015$proj1))
summary(degree(Projection_edge2M_Na2015$proj2))


######General node inforamtion#####

#voor
length(Edge_Voor2015$Name)
length(unique(Edge_Voor2015$Name))
length(unique(Edge_Voor2015$ï..Consern.code))

#na
length(Edge_Na2015$Name)
length(unique(Edge_Na2015$Name))
length(unique(Edge_Na2015$ï..Consern.code))



#####################################T-test#####################################
#https://dataz4s.com/r-statistical-programming/independent-two-sample-t-test-in-r/
#https://www.geeksforgeeks.org/levenes-test-in-r-programming/
#
hist(degree(edge2M_Voor2015))
hist(degree(edge2M_Na2015))
boxplot(deg_voor)
boxplot(deg_na)
deg_na <- degree(edge2M_Na2015);deg_voor <- degree(edge2M_Voor2015)
summary(deg_voor);summary(deg_na)
describe(deg_voor)
describe(deg_na)
mean(deg_voor)
mean(deg_na)

levene_NaVoor_T <- data.frame(y=c(deg_voor,deg_na),
                    source=c(rep("Voor",times=length(deg_voor)),rep("Na",times=length(deg_na))))
leveneTest(levene_NaVoor_T$y~levene_NaVoor_T$source)

t.test(deg_voor, deg_na, var.equal = F, na.rm=Tm)
## ? t.test(deg_voor, deg_na, var.equal = F, na.rm=Tm, mu=mean(degree(edge2M_Voor2015)))


############################ Freeman #############################################
#############Werk niet################################################################
Node_Na2015$indicatoren_2019.rechtmatigheid[Node_Na2015$indicatoren_2019.rechtmatigheid < 0] <- 5
Node_Na2015$indicatoren_2019.rechtmatigheid[Node_Na2015$indicatoren_2019.rechtmatigheid < 1] <- 10
Node_Na2015$indicatoren_2019.rechtmatigheid[Node_Na2015$indicatoren_2019.rechtmatigheid < 6] <- 0
Node_Na2015$indicatoren_2019.rechtmatigheid[Node_Na2015$indicatoren_2019.rechtmatigheid > 7] <- 1

projection_Na2015 <- bipartite.projection(edge2M_Na2015, multiplicity = T)
tm_h1_Na2015 <- get.edgelist(Projection_edge2M_Na2015$proj1, names = F)
tm_h1_Na2015_2 <- graph.edgelist(tm_h1_Na2015, directed = F)
V(tm_h1_Na2015_2)$recht <- Node_Na2015$indicatoren_2019.rechtmatigheid
freeman(tm_h1_Na2015_2, V(tm_h1_Na2015_2)$recht, gsizes =NULL, loops = any(is.loop(tm_h1_Na2015_2)))
Node_Na2015$indicatoren_2019.rechtmatigheid

projection_edge2M_h2 <- bipartite.projection(edge2M_h2, multiplicity = T)
tm_h2 <- get.edgelist(projection_edge2M_h2$proj1, names =F)
tm_h2_2 <- graph.edgelist(tm_h2, directed = F)
V(tm_h2_2)$recht <- node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid
freeman(tm_h2_2, V(tm_h2_2)$recht, gsizes =NULL, loops = any(is.loop(tm_h2_2)))




#############################################
############### Renders ###############
#############################################


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

plot(edge2M_B2015, vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2, main = "(Compleet network of) Healthcare companies and directors before 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("directors","non-fraugulent healthcare company", "fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "green", "red"),
  col = "black",
  bty = "n"
)




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

plot(edge2M_N2015,vertex.label=NA, vertex.label.dist=1,layout = layout_with_graphopt, vertex.size=2,  main = "(Compleet network of) Healthcare companies and directors after 2015")
legend(
  "bottomright",
  pch = 21,
  legend = c("directors","non-fraugulent healthcare company", "fraugulent healthcare company"),
  pt.cex = 2,
  pt.bg = c("black", "green", "red"),
  col = "black",
  bty = "n"
)




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





