
 ################ make terrs from a point drawn on map by user
 ################ init coords in longlet from Leaflet
       
 
func_makeTerr_drawnObs <- function(drawnPt,hab3857) {
  pt3857 <- drawnPt %>% st_transform(st_crs(3857))
  buff   <- drawnPt %>% st_transform(st_crs(27700)) %>% st_buffer(700) 
  buff   <- buff %>% st_transform(st_crs(3857))
  hab3857 <- terra::unwrap(hab3857)
  print(hab3857)
  print(buff)
  print("--")
  
  geom_ras <- crop(hab3857,vect(buff))
  geom_ras     <- subst(geom_ras, from=1, to=0)
  settling_ras <- subst(geom_ras, from=2, to=1)
 
  init_cell <- terra::extract(settling_ras, vect(pt3857), ID=TRUE, cells=TRUE, xy=TRUE, method="simple", touches=TRUE)
  print( init_cell)
  init_cell <-  init_cell$cell
  terr_ras <- spread(settling_ras, loci = init_cell,   persistence = 0, numNeighs = 2,maxSize=10,
                                   spreadProb = settling_ras ,  directions = 8,   id = TRUE) 
  terr_ras <- subst(terr_ras,from=0, to=NA)
  print(terr_ras)
  cellsIDs <- cells(resample(terr_ras,hab3857)) 
   if(length(cellsIDs)<8) {
     cellsIDs <- NULL # CANCEL if cant find enough cells for a territory - but quite tolerant in that it always keeps clicked cell as part of terr even if unsuitable
  #    cellsIDs <- terra::extract(settling_ras, vect(terr_ras), ID=TRUE, cells=TRUE, xy=TRUE, method="simple", touches=TRUE)
   #   print(cellsIDs)
    #  cellsIDs <- cellsIDs$cell
  }
  print(cellsIDs)
  return(cellsIDs)
 }
 