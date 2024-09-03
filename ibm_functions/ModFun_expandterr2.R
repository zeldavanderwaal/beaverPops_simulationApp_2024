cat("expand.territory  -  ")

expand.territory2 <- function(ter, id, hab) {
  cat("^")
  ter <- terra::unwrap(ter)
  hab <- terra::unwrap(hab)
  current <- which(terra::values(ter) == id)
#   print(id)
 #  print(current) 
  adj <- terra::adjacent(terra::rast(ter), current, directions=8, pairs=FALSE)
 
  
     adj <-  base::unique(adj[which(ter[as.vector(adj)] == 0) ])
     adj <-  base::unique(adj[which( hab [as.vector(adj)] == 2 )] )#exclude occupied territory and null/dispersal habitat
     adj <- na.exclude(adj)
#   print(adj)
 
  
 # adj <-  base::unique(adj[which(ter[as.vector(adj)] == 0 & na.omit( hab [as.vector(adj)]) == 2)]) #exclude occupied territory and null/dispersal habitat

if(length(adj) == 0) { 
  return(terra::wrap(ter))} 
  
  if(length(adj) > 0)  {
  best <- psample(adj, 1) #randomly pick one of the best adjacent pixels
  ter[best] <- id
  return(terra::wrap(ter))
  }
}
 