require(igraph)

l <-layout.fruchterman.reingold(g) 


adj_mat <- read.csv("edges.csv", sep =",")
nodes <- read.csv("vertexes.csv", sep =",") 
net <- as.matrix(adj_mat)
g <- graph.data.frame(net, vertices=nodes, directed=F)
par(mai=c(0.1,0.1,0.1,0.1))

plot(g, layout=l, vertex.label=NA)
plot(g, layout=l , vertex.size=5, vertex.color=V(g)$color, vertex.label.family="Helvetica")
legend("topleft",box.col = 'transparent',fill = c('steelblue1', 'palegreen'),
       legend=c('Patent','Inventor'), lty=0, cex=1.2, y.intersp = 0.7)


V(g)$degree <- igraph::degree(g)
V(g)$degree
plot(g, layout=l , vertex.size=V(g)$degree*5, vertex.color=V(g)$color, vertex.label=NA)
legend("topleft",box.col = 'transparent',fill = c('steelblue1', 'palegreen'),
       legend=c('Patent','Inventor'), lty=0, cex=1.2, y.intersp = 0.7)

--------------------------------------------------------------------------------------------------

adj_mat
colnames(adj_mat) <- c("inventor", "patent")
g_edge_sub <- subset(adj_mat, select=c("inventor", "patent"))
g_twomode <- graph.data.frame(g_edge_sub, directed=F, vertices=NULL)
V(g_twomode)$type <- bipartite.mapping(g_twomode)$type
V(g_twomode)$type
inventor <- bipartite.projection(g_twomode,type=V(g_twomode)$type, which = FALSE, multiplicity = TRUE)
patent <- bipartite.projection(g_twomode,type=V(g_twomode)$type, which = TRUE, multiplicity = TRUE)
patent
inventor
plot(patent , vertex.size=5, vertex.color="steelblue1", vertex.label.family="Helvetica")
plot(inventor , vertex.size=5, vertex.color="palegreen", vertex.label.family="Helvetica")

--------------------------------------------------------------------------------------------------
  

V(g)$quality
V(patent)$degree <- igraph::degree(patent)
V(patent)$degree
V(patent)$quality
linear.1 <- lm(V(g)$quality ~ V(g)$degree)
plot(V(g)$quality, V(g)$degree)
summary(linear.1)

