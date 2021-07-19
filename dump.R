setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\testing-1")
library(igraph)
library(tnet)
nodes2 <- read.csv("nodes-1.csv", header=T)
edge2 <- read.csv("edge-1.csv", header=T)

my_palette <- c("green", "yellow", "red"))
sf <- max(abs(node$test))
node.color <- (node$test) 
tkplot(edge_net, vertex.color=my_palette[node.color])

colrs <- c("gray50", "tomato", "gold")

V(edge2M)$color <- colrs[V(edge2M)$media.type]
plot(edge2M, vertex.color=node$test[!is.na(node$test.type)],  vertex.label.dist=1, vertex.size=7,)

#heads
head(nodes2)
head(edge2)

#make edge2 in to a matrix and bipartite matrix
edge.mat <- as.matrix(edge2)
edge <- graph.edgelist(edge.mat, directed = F)
bipartite.mapping(edge)

edge2M <- edge
V(edge2M)$type <- bipartite.mapping(edge2M)$type
head(edge2M)

nrow(nodes2); length(unique(nodes2$Year))
#nrow(edge2); nrow(unique(edge2(, c("Concern-code", "Name")))
dim(edge2M)

#graph colors lightblue for actors nodes and red is concerns targets
V(edge2M)$color <- ifelse(V(edge2M)$type, "lightblue", "salmon")
V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")
E(edge2M)$color <- "lightgray"

V(edge2M)$color <- ifelse(set.vertex.attribute(edge2M, "Kleur", index = V(edge2M), nodes2$kleur))

V(edge2M)$color <- ifelse(V(edge2M)$)

V(edge2M)$color[nodes2$Kleur]
tkplot(edge2M, vertex.color=V(edge2M)$color)

my_palette <- colorRampPalette(c("green", "white", "red"))(n = 10000)
sf <- max(abs(nodes2$indicatoren_2019.rechtmatigheid))
node.colors <- (nodes2$indicatoren_2019.rechtmatigheid+sf) / (2*sf) * 10000
tkplot(edge2M, vertex.color=my_palette[node.colors])

nrow(edge2M)
nrow(nodes2)

sf <- max(abs(nodes2$indicatoren_2019.rechtmatigheid_missing))
nodes.colors <- (nodes2$indicatoren_2019.rechtmatigheid_missing+sf) / (2*sf)


V(edge2M)[V(edge2M)$Concern_code == "Yellow"]
tkplot(edge2M, vertex.color=my_test(nodes.colors))
tkplot(edge2M)

g_data <-  get.data.frame(edge2M,  what="vertices")$name
data_vert <- apply(nodes2, 1, function(x) tail(x[x!=""], 1))


V(edge2M)$color <- ifelse(nodes2$indicatoren_2019.rechtmatigheid = 1, "Red", "Green")

V(edge2M)$color <- ifelse(nodes2$Kleur == 1, "Red", "Green")

tkplot(edge2M)


tkplot(edge2M)
#set_vertex_attr(edge2M, "name", index = V(edge2M), as.character(nodes2$blad1.c_naam))
set_vertex_attr(edge2M, "kleur", index = V(edge2M), nodes2$Kleur)

tkplot(edge2M, vertex.color=(edge2M)$kleur)


sf <- max(abs(tmp2$S2N))
node.colors <- (tmp2$S2N+sf) / (2*sf) * 10000
plot(g, vertex.color=my_palette[node.colors])

my_palette <- colorRampPalette(c("green", "white", "red"))(n = 10000)
g_data <- get.data.frame(edge2M, what="vertices")$name
V(edge2M)$color <- ifelse(nodes2$
                            
                            
                            V(g)$color <- ifelse(dataset$color_thresh[match(g_data , data_vert)] == 1, "red", "green")
                          
                          
                          st=graph.strength(edge2M)
                          tkplot(edge2M, vertex.color="gold", edge.color="Blue", vertex.size=st*5)
                          
                          '''
#components <- clusters(edge2M, mode="weak")
#biggest_cluster_id <- which.max(components$csize)
#vert_ids <- V(edge2M)[components$membership == biggest_cluster_id]
#induced_subgraph(edge2M, vert_ids)

#et2 <- graph_from_incidence_matrix(edge2)
#table(V(net2)$type)



my_palette <- colorRampPalette(c("green", "white", "red"))(n = 10000)
sf <- max(abs(tmp2$S2N))
node.colors <- (tmp2$S2N+sf) / (2*sf) * 10000
plot(g, vertex.color=my_palette[node.colors])


edgez <- fastgreedy.community(edge2M)
V(edge2M)$color <- edgez$membership +1
tkplot(edge2M)

V(edge2M)$color <- node.colorz


E(edge)
V(edge2M)$Name

my_test <- colorRampPalette(c("Yellow", "red"))
node.colorz <- (nodes2$indicatoren_2019.rechtmatigheid)

tkplot(edge2M, vertex.color=my_test(node.colorz))





sf <- max(abs(nodes2$indicatoren_2019.rechtmatigheid))
nodes.colors <- (nodes2$indicatoren_2019.rechtmatigheid+sf) / (2*sf)


V(edge2M)[V(edge2M)$Concern_code == "Yellow"]
tkplot(edge2M, vertex.color=my_test(nodes.colors))
tkplot(edge2M)
          
          
          ]$shpae <= "square"



clustering_w(edge2M)

'''
                          
                          
                          # hypothese 1
                          # eerst een subsectie op iedereen die een slechte rechtmatigheid score heeft en daarna daarop een analyse draaien
                          V(edge2M)$indeg <- degree(edge2M, mode = "in")
                          V(edge2M)[V(edge2M)$indeg == max(V(edge2M)$indeg)]
                          tez <- degree(edge2M)
                          head(tez)
                          print(tez)
                          
                          deg <- degree(edge2M)
                          hist(rec, breaks = 1:vcount(edge2M)-1, main="Histogram of nodes degree")
                          rec <- reciprocity(edge2M)
                          
                          mean_distance(edge2M, directed = F)
                          distances(edge2M)
                          
                          net.sym <- as.undirected(edge2M, mode= "collapse",
                                                   edge.attr.comb=list(weight="sum", "ignore"))
                          
                          
                          cliques(net.sym)
                          sapply(cliques(net.sym), length) #
                          largest_cliques(net.sym)
                          
                          ceb <- cluster_edge_betweenness(edge2M)
                          plot(ceb, edge2M)
                          
                          assortativity_nominal(edge2M, directed = F)
                          assortativity.degree(edge2M, directed = F)
                          
                          #Plot making documentation: https://igraph.org/r/doc/plot.common.html
                          # plot() tkplot() rglplot()
                          #plot visualisation: https://kateto.net/netscix2016.html
                          
                          
                          #plot general graph
                          #plot(g2M)
                          
                          #plot coloring van better visualization
                          
                          ##
                          write.csv(edge, "filez.csv")
                          
                          data.frame <- get.data.frame(edge2M, what=c("vertices"))
                          write.table(data.frame, file="file.csv", row.names = F, col.names = F)
                          get.adjacency(edge2M)
                          
                          data.m <- as_adj_edge_list(edge2M, mode=c("total"))
                          data.m <- get.data.frame(data.m, what = "edges")
                          
                          
                          write.table(data.frame, file="file.csv", row.names = F, col.names = F)
                          
                          ###
      
                          
                          setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\testing-1")
                          library(igraph)
                          library(tnet)
                          node <- read.csv("Aantal Name +1 Node.csv", header = T)
                          edge <- read.csv("Aantal Name +1 Edge.csv", header = T)
                          head(node); head(edge)
                          
                          edge.mat <- as.matrix(edge)
                          edgegraph <- graph.edgelist(edge.mat, directed = F)
                          edge2M <- edgegraph
                          V(edge2M)$type <- bipartite.mapping(edge2M)$type
                          
                          #V(edge2M)$color <- ifelse(V(edge2M)$type, "yellow", "red")
                          #V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")
                          #E(edge2M)$color <- "lightgray"
                          #head(edge2M)
                          
                          #cols <- c(Yellow="Yellow", Green="green", Red="Red")
                          #V(edge2M)$color <- cols[ node$Kleur[match(V(edge2M)$name, edge2M$Concern.code, nomatch = NA_integer_)]]
                          #  node$Kleur[match(V(edge2M)$name, node$Concern.code)]]
                          #$tkplot(edge2M, vertex.size = 3, vertex.label.font=10, vertex.label.dist=0.2)
                          #V(g)$color <- cols[ indclass$ind[match( V(mst)$name, indclass$values ) ] ]
                          
                          my_palette <- colorRampPalette(c("green", "white", "red"))(n = 10000)
                          sf <- max(abs(nodes2$indicatoren_2019.rechtmatigheid))
                          node.colors <- (nodes2$indicatoren_2019.rechtmatigheid+sf) / (2*sf) * 10000
                          tkplot(edge2M, vertex.color=my_palette[node.colors])
                          
                          palette <- colorRampPalette(c("green", "Yellow", "Red"))
                          
                          tkplot(edge2M, vertex.color(c("Yellow", "green", "Red")))
                          
                          
                          cols <- c(Yellow="Yellow", Green="green", Red="Red")
                          
                          V(edge2M)$color <- cols[node$Kleur[match(V(edge2M)$kleur, edge2M$Concern.code)]]
                          tkplot(edge2M)
                          
                          
                          distance_tm(edge2M)
                          
                          
                          
                          V(edge2M)$color <- ifelse(V(edge2M)$type, "lightblue", "salmon")
                          V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "circle")
                          E(edge2M)$color <- "lightgray"
                          
                          #V(edge2M)$my_palette <- my_palette[V(node)$Samenvoegen1.indicatoren_2019.rechtmatigheid]
                          
                          edge2M.bp <- bipartite.projection(edge2M)
                          head(edge2M.bp)
                          tkplot(edge2M.bp$proj1, vertex.label.color="black", vertex.label.dist=1, vertex.size=7,)
                          
                          cols <- c(Yellow="Yellow", Green="green", Red="Red")
                          
                          V
                          V(edge2M.bp)$color <- c("red", "blue")
                          plot(edge2M.bp$proj1, vertex.label.dist=1, vertex.size=7, vertex.label=NA, layout = layout_with_graphopt)
                          
                          
                          tkplot(edge2M.bp$proj1, vertex.label.color=node$Kleur2, vertex.label.dist=1, vertex.size=7)
                          tkplot(edge2M, vertex.label.color=node$Kleur)
                          
                          tkplot(edge2M, vertex.label.color="black", vertex.label=node$Samenvoegen1.c_naam[!is.na(node$Samenvoegen1.c_naam)])
                          
                          
                          tkplot(edge2M)
                          
                          
                          
                          
                          setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\testing-1")
                          library(igraph)
                          library(tnet)
                          library(netseg)
                          node <- read.csv("Aantal Name +1 Node.csv", header = T)
                          edge <- read.csv("Aantal Name +1 Edge.csv", header = T)
                          head(node); head(edge)
                          
                          edge.mat <- as.matrix(edge)
                          edgegraph <- graph.edgelist(edge.mat, directed = F)
                          edge2M <- edgegraph
                          V(edge2M)$type <- bipartite.mapping(edge2M)$type
                          
                          #recht = node$Samenvoegen1.indicatoren_2019.rechtmatigheid
                          #print(recht)
                          
                          #if(recht < 0){
                          #  print("green")
                          #} else {
                          #    print("blue")
                          #  }
                          
                          data("Classroom")
                          head(Classroom)
                          print(Classroom)
                          print(edge2M)
                          
                          node$Samenvoegen1.indicatoren_2019.rechtmatigheid[node$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0] <- "red"
                          node$Samenvoegen1.indicatoren_2019.rechtmatigheid[node$Samenvoegen1.indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
                          
                          
                          
                          set.vertex.attribute(edge2M, "indication", index = V(edge2M), node$Samenvoegen1.indicatoren_2019.rechtmatigheid[node$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0] <- "red")
                          tkplot(edge2M, vertex.color=V(edge2M)$indication)
                          
                          
                          tkplot(edge2M, vertex.color = c("red", "chartreuse3")[match(V(edge2M)$color, c("girl", "boy"))])
                          print(node$Samenvoegen1.indicatoren_2019.rechtmatigheid)
                          
                          
                          col1 = node$Samenvoegen1.indicatoren_2019.rechtmatigheid
                          
                          V(edge2M)$color <- ifelse(V(edge2M)$type, "black", col1)
                          V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")
                          
                          #check = graph.bfs(edge2M, root=edge$Code, unreachable = T)$check
                          #check
                          
                          orwg(edge2M)
                          
                          tkplot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=7)
                          tkplot(edge2M, layout=layout.bipartite, vertex.label=NA, vertex.size=7 )
                          
                          cols <- c(Green="green", Yellow="Yellow", Red="Red")
                          
                          V(edge2M)$color <- ifelse(cols[node$Kleur[match(V(edge2M)$name, node$value ) ] ])
                          
                          plot(edge2M, vertex.label=NA)
                          
                          plot(edge2M, vertex.color = pal[as.numeric(as.factor(vertex_attr(edge2M, "green")))])
                          
                          
                          
                          
                          
                          
                          library(igraph)
                          
                          dat <- data.frame(col1 = letters[1:20], 
                                            col2 = LETTERS[1:20])
                          
                          g <- graph.edgelist(as.matrix(dat ),directed = T)
                          
                          #assign color information
                          #try head(V(g)) to see the structure of nodes
                          V(g)$color <- c("red", "blue" )
                          
                          plot(g, layout= layout_nicely(g))
                          
                          
                          
                          
                          setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\testing-1")
                          library(igraph)
                          library(tnet)
                          library(netseg)
                          node <- read.csv("Aantal Name +1 Node.csv", header = T)
                          edge <- read.csv("Aantal Name +1 Edge.csv", header = T)
                          head(node); head(edge)
                          edge.mat <- as.matrix(edge)
                          edgegraph <- graph.edgelist(edge.mat, directed = F)
                          edge2M <- edgegraph
                          V(edge2M)$type <- bipartite.mapping(edge2M)$type
                          
                          node$Samenvoegen1.indicatoren_2019.rechtmatigheid[node$Samenvoegen1.indicatoren_2019.rechtmatigheid < 0] <- "red"
                          node$Samenvoegen1.indicatoren_2019.rechtmatigheid[node$Samenvoegen1.indicatoren_2019.rechtmatigheid < 1] <- "chartreuse3"
                          col1 = node$Samenvoegen1.indicatoren_2019.rechtmatigheid
                          V(edge2M)$color <- ifelse(V(edge2M)$type, "black", col1)
                          V(edge2M)$shape <- ifelse(V(edge2M)$type, "circle", "square")
                          
                          edge2MB <- cluster_edge_betweenness(edge2M)
                          length(edge2MB)
                          membership(edge2MB)
                          class(edge2MB)
                          modularity(edge2MB)
                          crossing(edge2MB, edge2M)
                          plot(edge2MB, edge2M)
                          
                          edge2MC <- cluster_label_prop(edge2M)
                          plot(edge2MC, edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)
                          
                          plot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)
                          
                          projct_edge2M <- bipartite.projection(edge2M, multiplicity = T)
                          plot(projct_edge2M$proj1, vertex.label=NA, vertex.size=3)
                          
                          
                          
                          #tkplot(edge2M, layout=layout.bipartite, vertex.label=NA, vertex.size=7 )
                          
                          #cols <- c(Green="green", Yellow="Yellow", Red="Red")
                          
                          #V(edge2M)$color <- ifelse(cols[node$Kleur[match(V(edge2M)$name, node$value ) ] ])
                          
                          #plot(edge2M, vertex.label=NA)
                          
                          #plot(edge2M, vertex.color = pal[as.numeric(as.factor(vertex_attr(edge2M, "green")))])
                          
                          
                          
                          
                          
                          setwd("C:\\Users\\virad\\OneDrive - Universiteit Utrecht\\3. Intership-Afstudeerstage\\Results\\Analyse\\Hypothese 1")
                          library(igraph)
                          library(tnet)
                          library(netseg)
                          
                          node <- read.csv("node_na2015.csv", header = T)
                          edge <- read.csv("edge_na2015.csv", header = T)
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
                          
                          edge2MB <- cluster_edge_betweenness(edge2M)
                          length(edge2MB)
                          membership(edge2MB)
                          class(edge2MB)
                          modularity(edge2MB)
                          crossing(edge2MB, edge2M)
                          plot(edge2MB, edge2M)
                          
                          edge2MC <- cluster_label_prop(edge2M)
                          #plot(edge2MC, edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=3)
                          
                          #tkplot(edge2M, layout = layout_with_graphopt, vertex.label=NA, vertex.size=1)
                          
                          projct_edge2M <- bipartite.projection(edge2M, multiplicity = T)
                          #plot(projct_edge2M$proj1, vertex.label=NA, vertex.size=3)
                          
                          
                          
                          
                          
                          
                          