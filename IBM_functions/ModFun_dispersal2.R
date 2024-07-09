
dispersal2 <- function(id, fam, ter, hab, famsize.max, move.max, hab.tot.quality, routes, mode,disp.fail,disp.succ,Ndisp) {
   Ndisp<- Ndisp
  disp.succ <- disp.succ
  disp.fail <-disp.fail
   routes.loc <- NULL 
   if( mode %in% c("settlement_test_but_not", "settlement_test")) { bvr <- id } else {  bvr <- max(routes$bvr, na.rm=TRUE)+1 }
   
   ter  <- terra::unwrap(ter)
   hab  <- terra::unwrap(hab)
   ter2 <- terra::as.matrix(ter, wide = F) # wide == each row in the SpatRaster becomes a row in the matrix and each column in the SpatRaster becomes a column in the matrix
   hab2 <- terra::as.matrix(hab, wide = F)
 
#  print(ter)
#  print(ter2)
#  print(id)
#  print(which(ter2==id))
#  print(which( values(ter) ==id))
   
   if( mode %in% c("settlement_test_but_not", "settlement_test")) { 
      depCell <- which(ter2==id) # which( values(ter) ==id) # which(ter2==id) 
      routes.loc <-rbind( routes.loc  ,data.frame(bvr=id, cell=depCell,  move=0, stage="depart"))
      cat(paste0("trans fam #",id," at ",depCell,"  - "))
      startf.natal <- startf <- adjacent.id(ter, cells=depCell  , id=id, directions=8)
      start.here <- psample(find.best.hab(startf.natal, hab2,depCell  ), 1) # KNOWN ORIGIN
   }  else {
       cat( paste0("\nfam #",id,"///bvr #",bvr,"  - " ))        
        startf.natal <-startf <- adjacent.id(ter, cells=which(ter2==id) , id=id, directions=8) #which( values(ter) ==id), 
       # startf.natal <- unique(startf)  ## startf.natal makes sense for pop going from terrs only not transloc?
     
        ##une replace with c(unique (startf)) ? why do that at first and not leave from terr?
      start.here <- psample(find.best.hab(startf.natal, hab2, NA  ), 1)   
      # wander around 
   #     print(start.here)
  #      print(which(ter2==id))
  #      print(which( values(ter) ==id))
   }
   path <- NULL
  #  if( mode  %in% c("settlement_test_but_not", "settlement_test")) { # not wandering for sett test but for translocated in model and normal, YES!
     path <- wander2(id, hab=terra::as.matrix(hab, wide = T), start.here)
 #      } else { 
         cat( paste0(" - from ",start.here," (",values(hab)[start.here],")  - "))
      #  print(start.here)
      #  print(values(hab)[start.here])
#        print(which( values(ter) ==id))
#        print(which(ter2==id))
# #        print("original")
 #      path <- wander2(id, hab=hab2,  which(ter2==id))# start.here)  ### Added Mar 2020
##      print(path)
##      print("test")
#              path <- wander2(id, hab=hab2,  which(ter2==id))# start.here)  ### Added Mar 2020
##      print(path)
#       print("old")
#       print(which(ter2==id))
#       print(which(values(ter)==id))
#    #     path  <- wander2(id, hab=hab, origin=start.here)  ### Added Mar 2020
# print(path)
#       
 #   }
   
 #  print(paste0("start.here: ",start.here))
  ## 
  # if(sum(values(hab)[start.here], na.rm=TRUE)==0) {start.here <- NULL}########################edited out june1 !!!
        
  #  print("path ")
  #  print(length(path ))
  #  print(path)
    
    
  if(length(path)==0) {path <- start.here}           ### Added Mar 2020
 #  routes.loc <- rbind(routes.loc, 
#may8 readd                      data.frame(bvr=bvr, cell=path,  move=0, stage="wander")) 
      

    
  ######## if no cells around
        if(length(start.here)==0  ) {
            disp.fail <- disp.fail + 1; 
            ter <- terra::wrap(ter)  
            routes  <- rbind(routes.loc, data.frame(bvr=bvr, cell=NA, move= move.max, stage="fail"))
            disp_return <- list(fam,ter, routes,disp.fail,disp.succ,Ndisp)
            print("failed !")
            return(disp_return)
        }  
  #now start looking around destination for territory. 
  start.here <- path[length(path)]
 # startf <- adjacent.id(ter, cells=start.here, id=id, directions=8)
#  startf <- adjacent.id(ter, cells=which(ter2==id), id=id, directions=8)
#  startf.natal <- unique(c(startf))
  #ok, surrounding cells (incl all )
 
  ## cells to start from
  startf <- adjacent.id(ter, cells=start.here, id=id, directions=8)
#  startf <- adjacent.id(ter, cells=which(ter2==id), id=id, directions=8)
  startf.natal <- unique( startf) # wasc()
     
   
  
  verbose= FALSE
  
   if(verbose) cat ("\tbeaver from", id, ": natal=", sort(startf.natal), "\n")
  
  success <- 0
  moves   <- 0
  while (success == 0 & moves < move.max) { 
        
#     print(paste0("move #",moves)) 
#    print(startf)
     if(length(startf)==0) {
       stage<- "fail"
       success <- -1
       next }
     
    ter.here <-values(ter)[start.here] #?? as.numeric(ter[start.here])
    hab.here <- values(hab)[start.here] #?? hab2[start.here] # was hab2[here] dec 21  values(hab2)[start.here] 
     if(verbose)  cat ("\tbeaver from", id, ": starting at", start.here, ", hab here =", hab.here, ": \n")
    
#    cat(c("TER=", ter.here," / HAB=" ,hab.here))
    
    
            
#   if( (mode %in% c("settlement_test","settlement_test_but_not"))  &  ter.here >0   & hab.here ==2 ) { # added jan8
#     cat("ter occupied  - ")
#     if(ter.here == id) {hab.here <-1#2
#     cat("by own fam  - ")
#     }    
#     if(ter.here != id) {hab.here <- 1
#     cat("diff fam, pass   - ")
#     }   
#   } ################## wont happen now, hab==1 wherever terrs exist for settl test

    
    
    #what do I do?
# unsuitable
    if (is.na(hab.here) | hab.here==0) {
       cat(".")
            

      #choose again from start
      if(verbose) cat ("\tbeaver from", id, ": no go, starting again\n")
      success <- 0
                   
      routes.loc <- rbind(routes.loc, data.frame(bvr=bvr,  cell=start.here,  move=moves, stage="unsuitable"))
      routes.loc <- rbind(routes.loc, routes.loc[nrow(routes.loc)-1,])
#      routes.loc$stage[nrow(routes.loc)] <- "back"
#      routes.loc$move[nrow(routes.loc)] <- moves
        
      
           
      if (length(start.here)>0) {
#        cat("more options")
   #    startf <- startf[ which(startf!=start.here)] 
    #   print(startf)
      start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ###
        startf <- startf[ which(startf!=start.here)] ###
     #        startf <- startf[ -which(startf==start.here)] #### may7
       #unswaped 2 lines above! jan13
  #  routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="try again"))
      }
           
# dispersal only           
    } else if (hab.here==1) {
      routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="disperse"))   
       #choose again from here
      startf <- terra::adjacent( hab , cells=start.here, directions=8, pairs=FALSE)
  # may7  
    startf  <- startf[ which(ter2[startf]!=id)] #exclude cells of same family ## values
      
  # WAS THIS BUT KEEPS NULL WHEN REMOVING 0!!
  # changed december5th 2023!
 #    print(" startf init version ")
  #   print( startf)
  #   startf <- startf[-which(ter2[startf]==id)] #exclude cells of same family ## values # may7
        
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
   #      if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="step"))   
      }   
      success <- 0

      
# suitable, occupied  
    } else if (hab.here==2 & ter.here > 0  & !(mode %in%c("settlement_test","settlement_test_but_not")) & ter.here!=id ){ # adde dlas cond feb24 - check?!
      
      #try to join this territory
       if (verbose) cat("\tbeaver from", id, ": discovered territory", ter.here, "\n")  
            famsize <- fam$num.m[fam$fam.id==ter.here ] + fam$num.f[fam$fam.id==ter.here] 
#            print(paste0("join? famsize would be:", famsize))
             
      if (famsize < famsize.max & famsize > 0) { 
         if(fam$num.m[fam$fam.id==ter.here] < fam$num.f[fam$fam.id==ter.here]) {
           cat("+ male")
           fam$num.m[fam$fam.id==ter.here] <- fam$num.m[fam$fam.id==ter.here] +1} else {
             cat("+ female")
             fam$num.f[fam$fam.id==ter.here] <- fam$num.f[fam$fam.id==ter.here] +1}
         success <- 1
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="join"))  
        cat("\tbeaver from", id, ": joined territory", ter.here, "\n")
      #  if (verbose) 
      } else {
         success <- 0
        if (verbose) cat("\tbeaver from", id, ": too big or empty\n")
  #       print(start.here )
        if (length(start.here)>0) routes.loc <- rbind(routes.loc, data.frame(bvr=bvr, cell=start.here,  move=moves, stage="too big"))  
        start.here <- psample(find.best.hab(startf, hab2, start.here), 1) ###
        startf <- startf[ which(startf!=start.here)] ###
       }

# not unsuitable, not disp only so is suitable
# not (suitable + not transloc )
#                       
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
      while (qual < hab.tot.quality & tries< ceiling(hab.tot.quality/2)) {
         #cat("-")
          ter.temp <- terra::wrap(ter.temp) 
          ter.temp <- expand.territory2(ter.temp, fam.id.new, rast.hab) ####<
    
    #   print(fam.id.new)
         #  rast.hab <- terra::rast(hab2)
          ter.temp <- terra::unwrap(ter.temp)
       #  rast.hab <- terra::unwrap(rast.hab)
       prev_qual <- qual 
         qual <- sum( values(hab)[which(terra::values(ter.temp)==fam.id.new)], na.rm = TRUE)
#         print(qual)
      #   print(tries)
      #   print(startf)
       #  if(prev_qual==qual) {tries <- hab.tot.quality} # will stop loop and not count as full below
        tries <- tries+1
      }
      if (qual<hab.tot.quality) {
        success <- 0
        if (verbose) cat("\tbeaver from", id, ": failed to find enough space, qual =", qual, "\n")
        if (length(start.here)>0) {
          routes.loc <- rbind(routes.loc,
                              data.frame(bvr=bvr, cell=start.here,  move=moves, stage="insufficient" ),
                              routes.loc[nrow(routes.loc),])
 #         routes.loc$stage[nrow(routes.loc)] <- "post insuff"
          routes.loc$move[nrow(routes.loc)] <- moves
          
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
        #cat("end")
#        print(mode)
          if(! mode %in% c("settlement_test","settlement_test_but_not") )  { # for indexing - in those modes bvr is fam id as dispersing as a fam
        fam <- rbind(fam, data.frame(fam.id=fam.id.new, num.m, num.f, young,  qual, stringsAsFactors=FALSE))
        } else {
        idRow <- which(fam$fam.id==fam.id.new)
          fam[ idRow ,] <-  data.frame(fam.id=fam.id.new, num.m=fam$num.m[idRow], num.f=fam$num.f[idRow], 
                                       young=fam$young[idRow],
                                       qual=qual, stringsAsFactors=FALSE)    
       }
            #     print(ter)
         ter  <- ter.temp ####removed < added dec5
 
 #print(ter.temp)
  #       print(fam)
   #      print(values(ter)[values(ter)>0])
        if (verbose) cat("\tbeaver from", id, ": formed new family", fam.id.new, "\n")
      }
    }        
    #}
    if(success==2) {
      ter  <- ter.temp
     cat("T!")  
      
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
    if( mode %in% c("settlement_test","settlement_test_but_not")) { ter[depCell]  <- 0 }  

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

 