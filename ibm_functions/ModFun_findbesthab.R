cat("find.best.hab()  -  ")


cat("best hab  - ")

find.best.hab <- function(choices.in, hab, origin  ) { # was origin=NA 11
   # this function finds the best habitat from the choices given in choices.in
   # include all cells of habitat2
  hab_ras <- terra::rast(hab)
  ch <- which(hab_ras[c(choices.in)]==2)
  cat("~")#cat("pick  - ")
#    print(c("find best hab amongst: ", choices.in))
#    print( c("suitable cells indx:",ch))
#    print(c("all habvals:", values(hab_ras)[choices.in])) 
 
  hab_ras <- subst(hab_ras,0,NA) # reversed june11 NA,0
  
    # include the four cells furthest away from the supplied origin
  if(is.na(origin)) {
   cat("NA or. - ")
    s <- ifelse(sum(!is.na(hab_ras[c(choices.in)]))<3,sum(!is.na(hab_ras[c(choices.in)])),3)
     ch1  <- psample(which(!is.na(hab_ras[c(choices.in)])), s) 
      if(length(ch )>0){ch <- c(ch ,ch1 )}else{ch <-  ch1  }
    
  } else {
#    cat("known origin  - ")
    dist <- sqrt((floor(terra::colFromCell(hab_ras, origin)) - floor(terra::colFromCell(hab_ras, c(choices.in))))^2 +
                   (floor(terra::rowFromCell(hab_ras, origin)) - floor(terra::rowFromCell(hab_ras, c(choices.in))))^2)
    dist[is.na(hab_ras[c(choices.in)])] <- -999
    dist[ hab_ras[c(choices.in)] ==0] <- -999
    dist.ord <- order(dist, decreasing=TRUE)
    
    ch <- c(ch, dist.ord[1:3])
  }  
  
 
  return(choices.in[unique(ch)])
}

