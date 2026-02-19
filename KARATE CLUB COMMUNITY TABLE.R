# "ZACHARY'S KARATE CLUB COMMUNITY TABLE"

#----- INSTALL PACKAGES ------
library(igraph)
library(igraphdata)


#---- Read files -----

data(package = "igraphdata") # Ver los datasets incluídos en el paquete
data(karate)

# Ver la información que contiene el dataset "karate"
kar <- karate
head(kar) # Ver la estrctura de la matriz


# ---- TABLA COMUNIDAD --------

# Asignar a cada nodo la comunidad de Louvain
V(kar)$community <- membership(lc.kar) 

# Extraer vector de comunidades por nodo
comm <- V(kar)$community
names(comm) <- V(kar)$name # Poner nombres de los nodos


# Lista de vecinos por nodo
nb_list <- adjacent_vertices(kar, V(kar), mode = "all")

# Lista de total de vecinos por nodo
nb_total <- lengths(nb_list) # Contar cuantos vecinos tiene cada nodo

# Vecinos en la misma comunidad
nb_same.comm <- sapply(seq_along(nb_list), function(i){
  vname <- V(kar)$name[i] # Nombre del nodo
  vcomm <- comm[vname] # community del nodo
  neigh_names <- V(kar)[nb_list[[i]]]$name # nombres de los vecinos
  sum(comm[neigh_names] == vcomm)}) # conteo de vecinos de la misma comunidad

# Vecinos fuera de la comunidad (restar detotal de vecinos entre vecinos de la misma)
nb_out.comm <- nb_total - nb_same.comm

# Calcular porcentajes
P.nb_same.comm <- round(nb_same.comm/nb_total * 100)# Porcentaje de cada uno de los que están dentro
P.nb_out.comm <- round(nb_out.comm/nb_total * 100) # Porcentaje de cada uno de los que están fuera

# Crear tabla
nb_table <- data.frame(
  People = V(kar)$name,
  Community = comm[V(kar)$name],
  Total_neighbors = nb_total,
  NB_same.community = nb_same.comm,
  NB_different.community = nb_out.comm,
  Porcentaje_same.community = P.nb_same.comm,
  Porcentaje_different.community = P.nb_out.comm,
  row.names = V(kar)$name)


View(nb_table)