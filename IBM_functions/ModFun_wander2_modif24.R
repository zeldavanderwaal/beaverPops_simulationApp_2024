cat("wander2()  -  ")

#    hab <- hab0 <- rast(matrix(sample(c(0,0,1,1,2),500, replace=TRUE), 30,20))
#   plot(hab)
#   hab2 <- terra::as.matrix(hab, wide = T)
#   hab2[5,] <-0
#      hab2[10,] <-0
#   
#      hab2[,12] <-0 
#      hab2[,15] <-0
#     hab <- hab0 <- rast(hab2)
#   origin=215
#      hab[origin] <- 10
#   plot(hab ) 
#   hab[origin]<-1
# path  <- wander2(id=1, hab=terra::as.matrix(hab, wide = T), origin= origin) 
#   plot(hab0)  
#    hab [path] <- 20 
#      hab[origin] <- 10
#    
# plot(hab) 
# 
#  habras0 <- habras
  
wander2 <- function(id, hab, origin) {
  path <- NULL
  cat("w ")
  #hab[is.na(hab)]<-0
  habras <- terra::rast(hab )
   habras <- subst(habras, NA,0)
  ring1 <-  c(terra::adjacent( habras , cells=origin, directions=8, pairs=TRUE, symmetrical=FALSE,include=FALSE) [ ,2])
   ring  <- ring1 [which(values(habras)[ring1] %in% c(1,2))]  
  
    for(i in 1:5){#  inner  
    ring  <-  unique(  c(terra::adjacent( habras , cells=ring , directions=8, pairs=TRUE, symmetrical=FALSE,include=FALSE) [ ,2])) 
    ring  <- ring [which(values(habras)[ring] %in% c(1,2))]  
 #   cat(i)
  }  
   
   
   
   if(any(length(ring)>1 , ring>0)) { 
  ring_first5 <- ring
   
   values(habras)[ring_first5] <- 0
    
  
  for(i in 6:12){ # outer  
  if(length(ring)>1){  
    ring  <-  unique(  c(terra::adjacent( habras , cells=ring , directions=8, pairs=TRUE, symmetrical=FALSE,include=FALSE) [ ,2])) 
    ring  <- ring [which(values(habras)[ring] %in% c(1,2))]  
   ring <-  ring[!ring %in% ring_first5]
   #cat(i)
  }
    ring<- ring[!is.na(ring)] 
    }
   
   
if(length(ring)>1){
 # cat("sample outer")
  path <-  ring 
  }
  
if(length(path)==0 & length(ring_first5)>1){   
 #   cat("sample inner")
    path <-   ring_first5
    } 
   path <-sample(path,1)
 #  print(path) 
   }
  return(path)
}
#was below, i simplified apr2024
#
#  r_id <- floor(terra::rowFromCell(terra::rast(hab), origin))
#  c_id <- floor(terra::colFromCell(terra::rast(hab), origin))
#  rasRows <- as.numeric(terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab)))))  
#  rasCols <- as.numeric(terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))) 
#  rasCells <- c(1:(nrow(hab)*ncol(hab)))
#  print(paste0(length(rasCells)," hab raster cells inc"))
#  otr <- 12; inr <-  5 
#  
#  hab_outer <- rasCells[which(rasRows >(r_id-otr) & rasRows<(r_id+otr) & 
#                              rasCols>(c_id-otr) &  rasCols<(c_id+otr))] #Upper distance
#  hab_inner <- rasCells[which(rasRows>(r_id-inr) &  rasRows<(r_id+inr) & 
#                     rasCols>(c_id-inr) & 
#                     rasCols<(c_id+inr))] #Lower distance
#  hab_ring <- hab_outer[hab_outer %nin% hab_inner] #Habitable area
#  
#  print(paste0("hab ring size :",length( hab_ring ))) 
#  
##  data.frame(index=hab_ring, hab=terra::rast(hab)[hab_ring]) %>% 
##    rename("hab" = "lyr.1") %>% 
##    filter(hab==2) %>% 
##    slice_sample(prop = 1) %>% 
##    pull(index) -> path
#  
#  data.frame(index=hab_ring,  habval=hab [hab_ring])  %>% 
#      rename("habval" = "lyr.1") %>%
#     filter(habval>0 )%>%#==2) %>% #was always ==2 
#     slice_sample(prop = 1) %>% 
#     pull(index) -> path
#  
###hab_outer <- which(terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-otr) & 
#                        terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+otr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-otr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Upper distance
#   hab_inner <- which(terra::rowFromCell(rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-inr) & 
#                        terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+inr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-inr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Lower distance#

