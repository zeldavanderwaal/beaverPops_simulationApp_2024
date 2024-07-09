  
applyEffect_getCells <- function(hab_rast, geomFeature, geomType, layerEffect ) {
#country <- "Wales"
 # hab_rast <- terra::rast( here::here('data/', country,'/habitat_3857.tif') )# the change!
   
  
   # sf_use_s2(FALSE) 
   Nsuit <- Ndisp <- 0
   modif_cellIds <-   NULL 
   featureName <- layerEffect
   
   print(featureName)
   print(geomType)
    
   
   if(!is.null(geomFeature)){
     if(nrow(geomFeature)>0){
          
          geomdat <- geomFeature[geomFeature$layerEffect ==layerEffect,]
          
 ################ make0         
    if(featureName == "make unsuitable" & geomType %in% c("line", "poly")) { 
             #     if(nrow(geomdat)>0){  
            #       
            #       geomdat <- st_transform(geomdat, st_crs(27700))
            #       if(geomType == "line"){ 
            #      geomdat <- st_buffer( geomdat , 5)#  geomdat_hab <- extractAlong(hab_rast, geomdat, ID=TRUE, cells=TRUE, xy=TRUE, online=TRUE, bilinear=FALSE)
            #        } #else {
            #      #geomdat <- st_buffer(geomdat, 5) 
            #       geomdat <- terra::vect(geomdat) # here was before bracket?! 
            #      #   geomdat_hab <- extractAlong(hab_rast, geomdat, ID=TRUE, cells=TRUE, xy=FALSE, online=TRUE, bilinear=TRUE)
            #      # } else {
            #      
            #       geomdat_hab <- terra::extract(hab_rast, geomdat, ID=TRUE, cells=TRUE, xy=TRUE, bilinear=TRUE)
            #       
            #      print("geomdat_hab")
            #      print(head(geomdat_hab))
            #      
            #      modif_cellIds <- geomdat_hab$cell 
            #      Nsuit <- length(which(geomdat_hab$GB_DispersalHabitat_UoE_v1==2)) # col3 is GB_DispersalHabitat_UoE_v1 - name can vary with provided map
            #      Ndisp <- length(which(geomdat_hab$GB_DispersalHabitat_UoE_v1==1))
            #      } else {
                   modif_cellIds <-NULL
                   Nsuit <- Ndisp <- 0
              #    }
         }#}   

 ################ make terrs from point drawn on map         
    if(featureName == "simulate territories at point location" & geomType == "point") {
 #      cat("make territory from drawn point  - ")
#      print(geomdat)
#       geom_m <-st_transform(geomdat[geomdat$size == "1pt",],st_crs(27700))  # process terrs are in '1terr' unit
#         if(nrow(geom_m)>0) {
#                   geom_ras <- rast(st_union(st_buffer(geom_m  ,700)) %>% st_sf(), res=100)
#                   geom_ras <- crop(hab_rast,geom_ras)
#                  if(!is.null(geom_ras)){
#                   cat("geom non null")
#                       geom_ras <- subst(geom_ras, from=1, to=0)
#                       
#                      if(sum(values(geom_ras ), na.rm=TRUE)>0){ 
#                           settling_ras <- subst(geom_ras, from=2, to=1)
#                           rm(geom_ras)  
#                           geom_m <- geom_m  
#                           init_cell <- cellFromXY(settling_ras,cbind(X=st_coordinates(geom_m)[1],Y=st_coordinates(geom_m)[2]))
#                           cat("cell hab values - ")
#                           print(settling_ras[init_cell])
#                       if(!is.na(settling_ras[init_cell])) {
#                              terr_ras <- spread(settling_ras, loci = init_cell,   persistence = 0, numNeighs = 2,maxSize=10,
#                                                                   spreadProb = settling_ras ,  directions = 8,   id = TRUE) 
#                              terr_ras <- subst(terr_ras,from=0, to=NA)
#                              cellsIDs <- cells(resample(terr_ras,hab_rast)) 
#                              if(length(cellsIDs)==10) {modif_cellIds <- cellsIDs }
#                              print(cellsIDs)
#                         }
#        
#    }}}}
    }         
          
 ################ make terrs from uploaded points shapefiles         
    if(featureName == "each point indicates the location of an observed territory" & geomType == "upload") {
     modif_cellIds <- NULL 
     cat("territory points from upload  - ")
    if(nrow(geomdat)>0){  
      print( geomdat)
                   geomdat <- st_transform(st_cast(geomdat, "POINT"), st_crs(27700))
                   print( geomdat)
          #         geomdat <- terra::vect(geomdat) # here was before bracket?! 
                   geomdat_hab <- terra::extract(hab_rast, geomdat, ID=FALSE, cells=TRUE, xy=FALSE, bilinear=TRUE) 
                   modif_cellIds <- geomdat_hab$cell 
                   print(modif_cellIds) 
                    print(head(geomdat_hab) )
                   Nsuit <- length(which(geomdat_hab$layer==2))
                   Ndisp <- length(which(geomdat_hab$layer==1))
         }} 
     
          
          
}}
   if(!is.null(modif_cellIds)) {
     print("got cells")
   }
   #      sf_use_s2(TRUE)  
  return(list(modif_cellIds,Nsuit,Ndisp))
         }       
          
   