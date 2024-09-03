cat("dispersal  - ")

dispersal2 <- function(id, fam, ter, hab, famsize.max, move.max, hab.tot.quality, routes, mode,disp.fail,disp.succ,Ndisp,wanderDist) {
   Ndisp<- Ndisp
   disp.succ <- disp.succ
   disp.fail <-disp.fail
   routes.loc <- NULL 
   wanderDist<- wanderDist
    
   if( mode %in% c("settlement_test_but_not", "settlement_test")) { bvr <- id } else {  bvr <- max(routes$bvr, na.rm=TRUE)+1 }
   
   ter  <- terra::unwrap(ter)
   hab  <- terra::unwrap(hab)
   ter2 <- terra::as.matrix(ter, wide = F) # wide == each row in the SpatRaster becomes a row in the matrix and each column in the SpatRaster becomes a column in the matrix
   hab2 <- terra::as.matrix(hab, wide = F)
 
   
   if( mode %in% c("settlement_test_but_not", "settlement_test")) { 
      depCell <- which(ter2==id) # which( values(ter) ==id) # which(ter2==id) 
      routes.loc <-rbind( routes.loc  ,data.frame(bvr=id, cell=depCell,  move=0, stage="depart"))
      cat(paste0("release #",id," at ",depCell,"  - "))
      startf.natal <- startf <- adjacent.id(ter, cells=depCell  , id=id, directions=8)
      start.here <- psample(find.best.hab(startf.natal, hab2,depCell  ), 1) # KNOWN ORIGIN
   }  else {
       cat( paste0("fam #",id,"///bvr #",bvr,"  - " ))        
       # startf.natal <-startf <- adjacent.id(ter, cells=which(ter2==id) , id=id, directions=8) #which( values(ter) ==id), 
       # print(startf.natal )
        startf.natal <-startf <- which(ter2==id)
         start.here <- psample( startf.natal , 1)  
       }
   path <- NULL
  #  if( mode  %in% c("settlement_test_but_not", "settlement_test")) { # not wandering for sett test but for translocated in model and normal, YES!
     pathSummary <- wander2(id, hab=terra::as.matrix(hab, wide = T), start.here,wanderDist=wanderDist)
  
     path <- pathSummary[[1]]
     stepsLeft <- pathSummary[[2]]
       
   
 
  if(length(path)==0) {path <- start.here}           ### Added Mar 2020
  cat(path,"/",values(hab)[path],":",stepsLeft,"steps left ")
    
         
    
  ######## if no cells around
        if(length(start.here)==0  ) {
            disp.fail <- disp.fail + 1; 
            ter <- terra::wrap(ter)  
            routes  <- rbind(routes.loc, data.frame(bvr=bvr, cell=NA, move= move.max, stage="fail"))
            disp_return <- list(fam,ter, routes,disp.fail,disp.succ,Ndisp)
            cat("failed !")
            return(disp_return)
        } else {  
  #now start looking around destination for territory. 
  start.here <- path[length(path)]
   
  ## cells to start from
  startf <- adjacent.id(ter, cells=start.here, id=id, directions=8)
  startf.natal <- unique( startf) # wasc()
     
   
  
  verbose= FALSE
  
   if(verbose) cat ("\tbeaver from", id, ": natal=", sort(startf.natal), "\n")
  
  success <- 0
  moves   <- 0
  move.max <- stepsLeft
  
  while (success == 0 & moves < move.max) { 
        
      if(length(startf)==0) {
       stage<- "fail"
       success <- -1
       next }
     
    ter.here <-values(ter)[start.here] 
    hab.here <- values(hab)[start.here] 
    if(verbose)  cat ("\tbeaver from", id, ": starting at", start.here, ", hab here =", hab.here, ": \n")
    
   
    # what do I do?
    # unsuitable
    if (is.na(hab.here) | hab.here==0) {
       cat("U") 
      
      #choose again from start
      if(verbose) cat ("\tbeaver from", id, ": no go, starting again\n")
      success <- 0
                   
      routes.loc <- rbind(routes.loc, data.frame(bvr=bvr,  cell=start.here,  move=moves, stage="unsuitable"))
      routes.loc <- rbind(routes.loc, routes.loc[nrow(routes.loc)-1,])

       
      if (length(start.here)>0) { 
        startf <- startf[ which(startf!=start.here)] ###
        start.here<-routes.loc$cell[nrow(routes.loc)]
        start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ###
        
      }
           
    # dispersal only           
    } else if (hab.here==1) {
      routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="disperse"))   
       #choose again from here
      startf <- terra::adjacent( hab , cells=start.here, directions=8, pairs=FALSE)
    
       startf  <- startf[ which(ter2[startf]!=id)] #exclude cells of same family ## values
      
         
      if(length(startf) == 0) {
        if (verbose) cat("\tbeaver from", id, ": can't settle at", start.here, ", returning to natal\n")
        startf <- startf.natal
        start.here <- psample(find.best.hab(startf, hab2, NA), 1) ###
        startf <- startf[ which(startf!=start.here)] ###
        startf.natal <- startf.natal[ which(startf.natal!=start.here)] ### 
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="return" ))
         if(verbose) cat("\t\tchoices (natal):", startf, "\n")
      } else {
        if (verbose) cat("\tbeaver from", id, ": can't settle at", start.here, ", moving on\n") 
         if(verbose) cat("\t\tchoices:", startf, "\n") 
        start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ### 
        startf <- startf[ which(startf!=start.here)] ### 
       }   
      success <- 0

      
    # suitable, occupied  
    } else if (hab.here==2 & ter.here > 0  & !(mode %in%c("settlement_test","settlement_test_but_not")) & ter.here!=id ){ # adde dlas cond feb24 - check?!
          
          #try to join this territory
           if (verbose) cat("\tbeaver from", id, ": discovered territory", ter.here, "\n")  
                famsize <- fam$num.m[fam$fam.id==ter.here ] + fam$num.f[fam$fam.id==ter.here] 
                  
             if (famsize < famsize.max & famsize > 0) { 
             if(fam$num.m[fam$fam.id==ter.here] < fam$num.f[fam$fam.id==ter.here]) {
                   cat("+ male")
                   fam$num.m[fam$fam.id==ter.here] <- fam$num.m[fam$fam.id==ter.here] +1} else {
                   cat("+ female")
                   fam$num.f[fam$fam.id==ter.here] <- fam$num.f[fam$fam.id==ter.here] +1}
              success <- 1
             if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="join"))  
             cat(" from", id, "joined: terr", ter.here)
             } else {
             success <- 0
             if (verbose) cat("\tbeaver from", id, ": too big or empty\n")
             if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="too big"))  
             start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ###
             startf <- startf[ which(startf!=start.here)] ###
            }

            }  else if (hab.here==1 & ter.here > 0  & mode =="settlement_test"){ 
              
     routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="occupied") )
     start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ### 
     startf <- startf[ which(startf!=start.here)] ### 
      success <- 0
    
    
  }  else {
       #try to form own territory
      if (verbose) cat("\tbeaver from", id, ": found empty space", start.here, "\n")
    if( mode %in% c("settlement_test","settlement_test_but_not") )  { fam.id.new <- id } else { fam.id.new <- NROW(fam)+1 } # original
      
   
      
#     if( mode == "settlement_test") {   ter[depCell]  <- 0 } ## allowed jan2 free departure cell before expanding - may resolve issue with patchy growth
      ter.temp <- ter ### added <  here? dec5th
      ter.temp[start.here] <- fam.id.new
          if( mode %in% c("settlement_test","settlement_test_but_not")) { ter.temp[depCell]  <- 0 }  
 
      rast.hab <-  hab # was may 7 terra::rast(hab2 )#why not hab? may 7#
      qual <- hab.here
      #terra::values(rast.hab)[start.here]
#      print(paste0("qual=",qual))
        tries <- 0
        rast.hab <- terra::wrap(rast.hab)
      while (qual < hab.tot.quality & tries < ceiling(hab.tot.quality/2)) {
         #cat("-")
         prev_qual <- qual
          ter.temp <- terra::wrap(ter.temp) 
          ter.temp <- expand.territory2(ter.temp, fam.id.new, rast.hab) ####<
    
    #   print(fam.id.new)
         #  rast.hab <- terra::rast(hab2)
          ter.temp <- terra::unwrap(ter.temp)
       #  rast.hab <- terra::unwrap(rast.hab)
         prev_qual <- qual 
         qual <- sum( values(hab)[which(terra::values(ter.temp)==fam.id.new)], na.rm = TRUE)
         if(qual == prev_qual) {tries <- 20} else { tries <- tries+1}
      }
      if (qual<hab.tot.quality) {
        success <- 0
        if (verbose) cat("\tbeaver from", id, ": failed to find enough space, qual =", qual, "\n")
        if (length(start.here)>0) {
          routes.loc <- rbind(routes.loc,
                              data.frame(bvr=bvr, cell=start.here,  move=moves, stage="insufficient" ) )
          
          routes.loc <- rbind(routes.loc, routes.loc[nrow(routes.loc)-1,])
          
      #    }
        start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ###
        startf <- startf[ which(startf!=start.here)] ###

         # erase attempted ter?
        # ter.temp <- ter # removed this jan10 # added new jan2
      }
       
        
        } else { #add new family
        success <- 2 
        num.m <- num.f <- young <- 0
        if(runif(1) < 0.5) {num.m <- 1} else {num.f <- 1}
        col <- 1
        
          if(! mode %in% c("settlement_test","settlement_test_but_not") )  { # for indexing - in those modes bvr is fam id as dispersing as a fam
        fam <- rbind(fam, data.frame(fam.id=fam.id.new, num.m, num.f, young,  qual, stringsAsFactors=FALSE))
        } else {
        idRow <- which(fam$fam.id==fam.id.new)
          fam[ idRow ,] <-  data.frame(fam.id=fam.id.new, num.m=fam$num.m[idRow], num.f=fam$num.f[idRow], 
                                       young=fam$young[idRow],
                                       qual=qual, stringsAsFactors=FALSE)    
       }
          
         ter  <- ter.temp ####removed < added dec5
 
 
        if (verbose) cat("\tbeaver from", id, ": formed new family", fam.id.new, "\n")
      }
    }        
    #}
    if(success==2) {
      ter  <- ter.temp
     cat("new: terr",fam.id.new,"!")  
      
                    routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="settling"))
                    } 
    
    #do I stop?
    if (length(startf)==0 & length(startf.natal)==0) {   #if all forward spaces are gone and all natal territory is exhausted
      if (verbose) cat("\tbeaver from", id, ": run out of space, fails\n")
      success <- -1
    } 
  #  routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage=stage))
    moves <- moves + 1

    if(moves>=move.max) success <- -1
   # print(moves)
  } 
  
  if(success==-1) {
    if (verbose) cat("\tbeaver from", id, ": unable to settle, dies\n")
    disp.fail <- disp.fail + 1
    if( mode %in% c("settlement_test","settlement_test_but_not")) {
      cat("f")
      ter[depCell]  <- 0 
      fam$qual[fam$fam.id==id]  <-  1    
      }  

  } else {
    disp.succ <- disp.succ + 1
  } 
  
   if (NROW(routes.loc)>0){
      if ( mode == "settlement_test"  ) {
     routes <- rbind(routes, routes.loc)
     } else {
     routes <- rbind(routes, routes.loc[nrow(routes.loc),])  
     }}
#  print(ter)
   cat("*") 
  
  ter <- terra::wrap(ter)
  disp_return <- list(fam,ter,routes,disp.fail,disp.succ,Ndisp) 
  return( disp_return  )
        }
}  

 