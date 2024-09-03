cat("applyEffect_getCells  - ")  
applyEffect_getCells <- function(hab_rast, geomFeature, geomType, layerEffect ) {
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
                   modif_cellIds <-NULL
                   Nsuit <- Ndisp <- 0
               
         }   

 ################ make terrs from point drawn on map         
    if(featureName == "simulate territories at point location" & geomType == "point") {
 
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
          
   