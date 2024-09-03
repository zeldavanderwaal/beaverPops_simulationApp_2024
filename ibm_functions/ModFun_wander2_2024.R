cat("wander  - ")


wander2 <- function(id, hab, origin,wanderDist) {
   path <- origin #beAut! 
   #hab[is.na(hab)]<-0
   habras <- terra::rast(hab )
   habras <- subst(habras, NA,0)
   ring1 <-  c(terra::adjacent( habras , cells=origin, directions=8, pairs=TRUE, symmetrical=FALSE,include=TRUE) [ ,2])
   ring   <- ring1 [which(values(habras)[ring1] >0)]  
   wanderDist=wanderDist 
  #   wanderDist <- rpois(1, lambda = 3)*10 +5
  cat(paste0("wander: ",wanderDist,"x100m>", origin))
   # wanderDist <- round(runif(1,1,50))
   # wanderDist <- rpois(1,50) 
    
   # It has been estimated that approximately 80% of dispersing beavers attempt to establish territories within 5 km of their natal territory (Nolet & Baveco, 1996), though much greater distances (80km+) have been recorded.
   i=0 # note already one ring so 100m so nly do if wanderdist>0
   prev_ring <- ring
    while(i <(wanderDist-2)){
       #cat(i)
      i <- i+1
      prev_ring <- ring
      ring  <-  unique(  c(terra::adjacent( habras , cells=ring , directions=8, pairs=TRUE, symmetrical=FALSE,include=FALSE) [ ,2])) 
      suitCells <- which(values(habras)[ring] >0)
     if(length(suitCells)>0) {
      ring  <- ring [suitCells] 
      newCells <- which(!ring %in% prev_ring)
      growing <- length(newCells)
      cat(".")
      if(growing==0) {
        cat(paste0(i,"00m  -no longer expanding1 - "))
           wanderDist <- i ##actual wander distance, may be lower than planned 
           i<-wanderDist 
           ring <- prev_ring 
      } else {
        ring  <- ring [newCells] 
      }
     } else { 
       cat(paste0(">",i,"00m  -no longer expanding - ") )
       wanderDist <- i ##actual wander distance, may be lower than planned 
       i<-wanderDist  
       ring <- prev_ring
       }
    }   
       
  path <- ring
  ring_init   <- ring 
  ring<-NULL
  if(i<wanderDist-1){ 
     path <- ring
     ring  <-  unique(  c(terra::adjacent( habras , cells=ring_init, directions=8, pairs=TRUE, symmetrical=FALSE,include=FALSE) [ ,2])) 
     if(length(ring)>0) { 
       ring  <- ring [which(values(habras)[ring] %in% c(1,2))]  
       ring <- ring[!ring %in% ring_init]
       if(length(ring)>0) {path <- ring}
     }
  }
   
   dist <- wanderDist
   if(length(na.exclude(path))==0){ path <- NULL}
   if(!is.null(path)){ path <- sample(na.exclude(path),1)} 
   stepsLeft <- 210-dist  
   cat(" >")
  return(list(path,stepsLeft))
}