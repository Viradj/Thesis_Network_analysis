library(igraph)
library(car)
library(psych)
library(tnet)
library(netseg)
##########################
setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 2")

node_h2 <- read.csv("Aantal Name +1 Node.csv", header = T)
edge_h2 <- read.csv("Aantal Name +1 Edge.csv", header = T)
head(node_h2); head(edge_h2)
edge.mat_h2 <- as.matrix(edge_h2)
edgegraph_h2 <- graph.edgelist(edge.mat_h2, directed = F)
edge2M_h2 <- edgegraph_h2
V(edge2M_h2)$type <- bipartite.mapping(edge2M_h2)$type

node_h2;node_h2$coll
node_h2$coll <- node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid
node_h2$coll[node_h2$coll < 0] <- "red"
node_h2$coll[node_h2$coll < 1] <- "chartreuse3"
col1 = node_h2$coll
V(edge2M_h2)$color <- ifelse(V(edge2M_h2)$type, "black", col1)
V(edge2M_h2)$shape <- ifelse(V(edge2M_h2)$type, "circle", "square")

plot(edge2M_h2, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3, main="Directors with more than one healthcare companies")
legend(
  "bottomright",
  pch = 21,
  legend = c("directors", "non-frauglent company", "fraugelent company"),
  pt.cex = 2,
  pt.bg = c("black", "green", "red"),
  col = "black",
  bty = "n"
)




#plot(edge2M_h2, layout = layout.kamada.kawai, vertex.label=NA, vertex.size=3, main="Render")

#plot(projection_edge2M_h2$proj1, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3, main="kdfjalk")


#legend('topright', legend = names(group_ids),
       col = group_color,
       pch = 15, bty = "n",  pt.cex = 1.5, cex = 0.8, 
       text.col = "black", horiz = FALSE)


xxx <- node_h2$coll[node_h2$coll == "red"];xxx
xxxx <- node_h2$coll[node_h2$coll == "chartreuse3"];xxxx

summary(degree(edge2M_h2))
summary(degree(projection_edge2M_h2$proj1))
summary(degree(projection_edge2M_h2$proj2))



projection_edge2M_h2 <- bipartite.projection(edge2M_h2, multiplicity = T)



length(unique(edge_h2$Code))



length(edge_h2$Code)
length(unique(edge_h2$Bestuurder))
length(unique(edge_h2$Code))
mean(degree(edge2M_h2))
mean(degree(projection_edge2M_h2$proj1))

#freeman index segrigation
node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0] <- 5 
node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 1] <- 10
node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 6] <- 0
node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid > 7] <- 1; node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid

length(node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0]) #273
length(node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid[node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid > 0]) #694



projection_edge2M_h2 <- bipartite.projection(edge2M_h2, multiplicity = T)
tm_h2 <- get.edgelist(projection_edge2M_h2$proj1, names =F)
tm_h2_2 <- graph.edgelist(tm_h2, directed = F)
V(tm_h2_2)$recht <- node_h2$Samenvoegen1.indicatoren_2019.rechtmatigheid
freeman(tm_h2_2, V(tm_h2_2)$recht, gsizes =NULL, loops = any(is.loop(tm_h2_2)))

mean(degree(tm_h2_2))
mean(degree(projection_edge2M_h2$proj2))
mean(degree(projection_edge2M_h2$proj1))

V(tm_h2_2)$color <- ifelse(V(tm_h2_2)$type, "black", col1)
V(tm_h2_2)$shape <- ifelse(V(tm_h2_2)$type, "circle", "square")
V(tm_h2_2)$color <- node_h2$coll
V(tm_h2_2)$one <- 1

#plot(tm_h2_2, layout = layout.fruchterman.reingold, vertex.label=NA, vertex.size=4, main="Render")

mean(closeness(edge2M_h2))
closeness_w(tm_h2_2)
mean(betweenness(edge2M_h2))





length(unique(edge$Name))
length(unique(edge$ï..Consern.code))
centr_degree(projct_edge2M$proj2, mode= c("all", "out", "in", "total"))
centr_degree(projct_edge2M$proj2)$centrialisation
centr_clo(projct_edge2M$proj2, mode = "all")$centrialisation
centr_degree(projct_edge2M$proj2, mode = "out")
centr_degree(projct_edge2M$proj2, mode = "in")
centr_degree(edge2M, mode="out")
centr_degree(edge2M, mode="in")
