# "ZACHARY'S KARATE CLUB NETWORK"

#----- INSTALL PACKAGES ------
library(igraph)
library(igraphdata)
library(RColorBrewer)


#---- Read files -----

data(package = "igraphdata") # Ver los datasets incluídos en el paquete
data(karate)

# Ver la información que contiene el dataset "karate"
kar <- karate
head(kar) # Ver la estrctura de la matriz






# ---- Explore iGraph data ------
# igraph summary
gorder(kar) # número de vertices/nodos en la red (personas)
gsize(kar) # número de edges en la red (conexiones entre personas)

# Lista de nombres/labels de los vertices/nodos
V(kar)

# Mapa de las conexiones/edges entre vertices
E(kar)

# Attributes
vertex_attr(kar) # Faction, name, label, color

edge_attr(kar) # weight

# Faction <-  sector de donde los individuos eran originalmente
# Color <- "faction" a la que se unieron después de la pelea
# Label <- Abreviación del numbre del indiduo
# Weight <- Medida en función de actividades en comun que los participantes tienen (¿amigos?)

# Mr. Hi "neighbors" (Adjacent vectors)
Mr.Hi_NB <- neighbors(kar, "Mr Hi", mode = c("all"))

# John A "neighbors"
John.A_NB <- neighbors(kar, "John A", mode = c("all"))

intersection(Mr.Hi_NB, John.A_NB)
# Tienen 4 nodos que coinciden: 9, 14, 20, 32






#------- Measuring Centrality --------

# Degree centrality
kar_deg <- degree(kar, mode = c("All")) 
V(kar)$degree <- kar_deg # Asignar medida como atributo
which.max(kar_deg) # Nodo más importante basado en conexiones directas (John A - 34)


#2 Eigenvector centrality: 
kar_eig <- eigen_centrality(kar)$vector
V(kar)$Eigen <- kar_eig
V(kar)$Eigen
which.max(kar_eig) # Importancia de nodo en relación a nodos importantes


# Betweeness centrality
kar_bw <- betweenness(kar, directed = FALSE)
V(kar)$betweenness <- kar_bw
which.max(kar_bw) #Mr. Hi es el nodo más central

DF_kar <- as_long_data_frame(kar) # Convert igraph object to data frame para ver que tiene


# Edge betweeness: Mide cuántos caminos pasan por un edge; alto edge betweeness son las lineas que conectan cosas cruciales de la red
kar_edge.bw <- edge_betweenness(kar, directed = FALSE)
E(kar)$edge_betweenness <- kar_edge.bw
which.max(kar_edge.bw)


# ----- Meassuring network structure --------

#1 Calcular la densidad de la red
network_density <- edge_density(kar)*100
network_density

## Nota <- densidad cercana a 1: la red está muy conectada.
## Nota <- densidad cercana a 0: la red tiene pocas conexiones comparada con el número máximo posible.
## Interpretación <-  toene 13.9% del 100% de conexiones posibles si todos se conocieran


#2 Assortativity # tendency of nodes in a network to connect to other nodes that are similar to themselves in some way
 

kar.values <- as.numeric(factor(V(kar)$Faction)) # Obtener los valores de "Faction" para cada nodo
assortativity_nominal(kar, types = kar.values) # Calcular assortativity nominal (por "facción"Faction")

#2.1. Calculate the observed assortativity
observed.assortativity <- assortativity_nominal(kar, types = kar.values)
results <- vector("list", 1000) # Calculo de assortativity observada y  muestreo aleatorio
  
for(i in 1:1000){results[[i]] <- assortativity_nominal(kar, sample(kar.values))} #100 muestreos aleatorios para ver dostrobución
unlist(results)

#2.2.  Plot the distribution of assortativity values and add a red vertical line at the original observed value
hist(unlist(results), xlim = c(0, 0.8)) # Histograma de resultados
abline(v = observed.assortativity, col = "red", lty = 3, lwd = 2)  # Línea roja en valor observado

# Los nodos de la misma Faction están mucho más conectados entre sí de lo que sería esperado si las conexiones fueran aleatorias.

# ----- NETWORK VISUALIZATION -----------

# Plotting a network with the degree centrality
set.seed(1001)
pal <- brewer.pal(length(unique(V(kar)$Faction)), "Set3")

plot(kar,edge.color = "black", vertex.label.cex = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(kar, "Faction")))],
     vertex.size = sqrt(kar_deg), edge.width = sqrt(E(kar)$weight),
     layout = layout.fruchterman.reingold) 

# Plotting a network with the eigenvector centrality
set.seed(1001)
plot(kar,edge.color = 'black', vertex.label.cex  = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(kar, "Faction")))],
     vertex.size = sqrt(kar_eig), edge.width = sqrt(E(kar)$weight),
     layout = layout.fruchterman.reingold)

# Plotting a network with the betweenness centrality
set.seed(1001)
plot(kar, edge.color = 'black', vertex.lablel.cex = 0.5,
     vertex.color = pal[as.numeric(as.factor(vertex_attr(kar, "Faction")))],
     vertex.size = sqrt(kar_bw), edge.width = sqrt(E(kar)$weight),
     layout = layout.fruchterman.reingold)






#------ COMMUNITY DETECTION -------

#1 LOUVAIN CLUSTERING 
lc.kar <- cluster_louvain(kar)
communities(lc.kar) 

set.seed(1001) 
plot(lc.kar,
     kar,
     edge.color = "black",
     vertex.label.cex = 0.5, 
     vertex.color = pal[as.numeric(factor(V(kar)$Faction))], 
     vertex.size = sqrt(V(kar)$degree) * 4, 
     edge.width = sqrt(E(kar)$edge_betweenness)/4,  
     layout = layout.fruchterman.reingold)

legend("bottomleft", 
       legend = c("H = Mr. Hi", "A = John A"),  
       col = c("orange", "darkgreen"),  
       pch = 16,  
       cex = .8)  



