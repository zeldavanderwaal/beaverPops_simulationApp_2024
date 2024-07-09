cat("wander2()  -  ")

 

wander2no <- function(id, hab, origin) {
  r_id <- floor(terra::rowFromCell(terra::rast(hab), origin))
  c_id <- floor(terra::colFromCell(terra::rast(hab), origin))
  rasRows <- as.numeric(terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab)))))  
  rasCols <- as.numeric(terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))) 
  rasCells <- c(1:(nrow(hab)*ncol(hab)))
  print(paste0(length(rasCells)," hab raster cells inc"))
  otr <- 12; inr <-  5 
  
  hab_outer <- rasCells[which(rasRows >(r_id-otr) & 
                     rasRows<(r_id+otr) & 
                     rasCols>(c_id-otr) & 
                     rasCols<(c_id+otr))] #Upper distance
  hab_inner <- rasCells[which(rasRows>(r_id-inr) & 
                     rasRows<(r_id+inr) & 
                     rasCols>(c_id-inr) & 
                     rasCols<(c_id+inr))] #Lower distance
  hab_ring <- hab_outer[hab_outer %nin% hab_inner] #Habitable area
  
  #print(paste0("hab ring size :",length( hab_ring ))) 
  
#  data.frame(index=hab_ring, hab=terra::rast(hab)[hab_ring]) %>% 
#    rename("hab" = "lyr.1") %>% 
#    filter(hab==2) %>% 
#    slice_sample(prop = 1) %>% 
#    pull(index) -> path
  
  data.frame(index=hab_ring, habval=  hab [hab_ring]) %>% 
     filter(habval==2) %>% #was==2 
     slice_sample(prop = 1) %>% 
     pull(index) -> path
  

  
  return(path)
}
#was below, i simplified apr2024
#hab_outer <- which(terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-otr) & 
#                        terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+otr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-otr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Upper distance
#   hab_inner <- which(terra::rowFromCell(rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(r_id-inr) & 
#                        terra::rowFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(r_id+inr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))>(c_id-inr) & 
#                        terra::colFromCell(terra::rast(hab), cell=c(1:(nrow(hab)*ncol(hab))))<(c_id+otr)) #Lower distance

