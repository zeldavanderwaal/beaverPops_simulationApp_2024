cat("makeTerr_uploadedObs  - ")
#### make terrs from coords uploaded ####
## uploaded = not constrained = could be in unsuitable hab

       
 
func_makeTerr_uploadedObs <- function(uploadedPts,hab3857) {
  hab3857 <- terra::unwrap(hab3857)
  pt3857 <- uploadedPts %>% st_transform(st_crs(3857))
  ptsNames <- uploadedPts$LeafletId
  buff_pts   <- uploadedPts %>% st_transform(st_crs(27700)) %>% st_buffer(1500) 
  buff <- buff_pts %>% st_union() 
  buff   <- buff %>% st_transform(st_crs(3857))
  geom_ras <- crop(hab3857,vect(buff))
  geom_ras     <- subst(geom_ras, from=NA, to=0)
  disp_ras <- geom_ras
  disp_ras     <- subst(disp_ras, from=2, to=1)
  settling_ras     <- subst(geom_ras, from=1, to=0)
  settling_ras <- subst(settling_ras, from=2, to=1)
  cellsIDs <-  vector("list", length=nrow(pt3857))  
  
  
 for(pt in 1:nrow(pt3857)){
   cat("row:")
   print(pt)
        singlePt <- pt3857[pt,]
        cat(".")
        init_cell <- terra::extract(settling_ras, vect(singlePt), ID=TRUE, cells=TRUE, xy=TRUE, method="simple", touches=TRUE)
        #print( init_cell)
        init_cell <- init_cell$cell
        habval <- values(settling_ras)[init_cell]
        adjcells <-  init_cell 
        cellsIDsSingle <- NULL
        numTries<- 0
        stop<-FALSE 
       while(stop==FALSE){
          cat(".")
           if(any(habval==1)){ 
           # cat("some suit cells") 
               init_cell <- adjcells [which(habval==1)] 
           } else {
          # cat("no suit cells")
         while(sum(habval)==0  ){ 
           numTries <- numTries +1  
           if(numTries >2) {
            # cat("disp hab")
             settling_ras <- disp_ras} # try neigh 3 rounds then just use disp as becomes not epreesentative of reality
                   adjcells <- as.numeric(terra::adjacent(settling_ras, adjcells, directions=8))
                   habval <- values(settling_ras)[adjcells]  
                 }
                       init_cell <- as.numeric(adjcells)[which(habval==1)] 
                   } 
                  init_c <-  init_cell 
                  cellsIDsSingle <-   0
                  stop2 <- FALSE
                    while(stop2==FALSE){
                     cat(".") 
                      randomCellIndx <- sample(length(init_cell),1)
                     
                          init_c  <- init_cell[randomCellIndx] 
                          init_cell <- init_cell[-randomCellIndx]
                          terr_ras <- spread(settling_ras, loci = init_c,   persistence = 0, numNeighs = 2,maxSize=10,
                                                           spreadProb = settling_ras ,  directions = 8,   id = TRUE) 
                          cellsIDsSingle <-NULL 
                          if(sum(values(terr_ras))>7){
                            terr_ras <- subst(terr_ras,from=0, to=NA)  
                            cellsIDsSingle <- cells(resample(terr_ras,hab3857))
                                        stop<-stop2<-TRUE
                          }
                                   if(length(init_cell)==0){  
                                      # cat("||")
                                       stop2<-TRUE
                                       habval <-0    
                                   }     }
       }
          cat(":")
          
              if(length(cellsIDsSingle)<8) {
                 cat("!")
                  cellsIDsSingle <- NULL 
                    } else {
                  cat("o")
                  settling_ras[cells(terr_ras)] <- 0 # so next fam cant settle in that spot again
                        
                    } 
             cellsIDs[[pt]] <- cellsIDsSingle
           #  cat("<")
}#each pt
  
names(cellsIDs)<-as.character(ptsNames)   
 
  
  return(cellsIDs)
 }
 