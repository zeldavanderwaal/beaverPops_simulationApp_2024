 cat("generateEffectDf()  -  ")
 
### to generate a table with effect (LeafletId) in first col, and time (min-max) in other cols
### value is whether effect is to be applied that year (1/0)
### 

 
generateEffectDf <- function(metaDat, layerEffect) {
  effdf <- NULL
  if(nrow(metaDat)>0){ 
    if(!is.null(layerEffect)) {
    metaDat <- metaDat[which(metaDat$layerEffect %in% layerEffect),] #%in% so layerEffect can be vector for selected geoms
    print(layerEffect)
    }
  NFeat_metadat <- nrow(metaDat) #length(which(metaDat$layerEffect == layerEffect))
  print( paste0(NFeat_metadat,  "feat in data  -")  ) 
        if(NFeat_metadat>0){
          
          metaDat$start[metaDat$start == "init"] <- 2010  
          
            effdf <- as.data.frame(matrix(ncol=42, nrow=NFeat_metadat)) 
            colnames(effdf)<- c("effectId",  as.character(paste0("y",seq(2010, 2050,1))) )  # == names(ll), keep synced! to avoid dragging reactVals
            effdf$effectId <- metaDat$LeafletId # link via LeafId so name can be changed, link to name for display only
                   for(eff in effdf$effectId){
                      # get years
                      start_y <- as.numeric(as.character(metaDat$start[metaDat$LeafletId == eff]))
                      if(  metaDat$duration[metaDat$LeafletId == eff]=="through"){
                        end_y <- 2050
                      } else {
                      end_y   <- start_y +  as.numeric(gsub("\\D", "",  metaDat$duration [metaDat$LeafletId == eff])) -1
                      }
                      effdf[effdf$effectId == eff,2:ncol(effdf)] <- 0
                      yr_seq <- paste0("y", seq(start_y, end_y,1))
                      effdf[effdf$effectId == eff,colnames(effdf) %in% yr_seq] <- 1
                    } 
        
      }
  }
  #print(effdf)
  return(effdf) 
}

 