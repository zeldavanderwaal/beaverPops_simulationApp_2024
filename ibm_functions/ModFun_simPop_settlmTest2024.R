 cat("ssettlmnt test  - ")

################################################################################################################ FUN: simpop_function - simulate pop growth - 
simpop_function_transloc <-function(hab, initValues, #ter.start,    
                                    depCells, cells_make0,departings,
                                    country ){
cat("\n run function: sim_pop transloc\n")
mode <- "settlement_test"   
cat("departing:", departings)
  


## coords in 3857 --> df of coords in 4326 for centre cell in 3857
centreCell3857_to4326 <- function(country, xx, yy,hab3857) { 
xy_init <- data.frame(x=xx, y=yy)
cell <- cellFromXY(hab3857, xy_init)
xy_3857 <- xyFromCell(hab3857, cell) %>% as.data.frame()
pts <- st_multipoint(matrix(c(xy_3857$x , xy_3857$y ), ncol = 2, byrow = FALSE), dim = "XY")   %>% st_sfc() 
st_crs(pts) <-   st_crs(3857)
pts <- pts %>% st_sf() %>% st_cast("POINT") 
pts <-  pts %>% st_transform(st_crs(4326)) %>% st_coordinates() %>% as.data.frame()

return(pts)
}

 
    # ter.start<-  terra::unwrap( ter.start ) 
     hab2 <- terra::unwrap( hab )   
     hab2 <- subst(hab2, NA, 0 ) # test jan13   
     ter.start <-   hab2
     values(ter.start) <- 0    
     ter.start[depCells] <- departings#
     if(!is.null(cells_make0)) {   
       cat("alternative hab ras  - ") 
   #    print(cells_make0)
       hab2[ cells_make0 ] <- 3
     }  
      
     cat(", habitat ready  - ")    
 
      ter.start <- subst(ter.start, NA,0) # keep
      hab2 <- subst(hab2, 3, 0) # test jan13
       
      
      num.fam <- nrow(initValues)
      families <-  data.frame(fam.id=initValues$group, num.m=initValues$males, 
                                  num.f=initValues$females, young=initValues$young,   qual=0, stringsAsFactors=FALSE)
      ter <-  ter.start 
      routes0 <- routes <- NULL
      simpop_transloc <-  NULL       
         
      
       disp.fail <<- deaths.adt <<- deaths.sub <<- deaths.juv <<- births <<- disp.succ <<- 0 # useless here but common scripts needs it for popsims
      
    #  disp <- tidyr::uncount(data.frame(fam.id=families$fam.id , 1),1)$fam.id  
    disp <-  departings
       if(length(disp)>0) {
                       disp <- data.frame(fam.id=disp, ind.id=1:length(disp))  
                       hab2 <- terra::wrap( hab2 ) 
                      # wanderDists <- rpois(n=length(departings),lambda=3) *10
                                  
                              for (ind.id in departings ) { # mar11 WAS fam.id)) { #fam id same as ind.id bu ind.id seqential, famid will have gaps if some not included
                                # id <- departings[ind.id]
                                 id <- ind.id 
                                 cat("\n{")
                                 depCell <- depCells[ind.id] 
                                 depCells[ind.id] <- 0
                                  wanderDist <-  rpois(n=1,lambda=3) *10
                                 valid_depCell <- depCell#which( values(ter.start) ==id) # so can release 2 groups from 1 cll otherwise deletes first group ID
                                 if(!is.null(cells_make0)){
                                   if(valid_depCell  %in% cells_make0) {valid_depCell <- NULL}
                                }
                                 if(length(valid_depCell)==0){
                                               cat("no habitat  - ")
                                               families[id,c(2:ncol(families))] <- 0
                                               routes <- rbind(routes,
                                                               data.frame(bvr=id, cell=depCell,  move=0, stage="no depart") )
                                               values(ter) [which( values(ter) ==id) ] <- 0
                                               if(length(departings[depCells>0])>0){ ter[depCells[depCells>0]] <- departings[depCells>0] }

                                           } else {
                                               cat("hab ok - ") 
                                             ter [ depCell ] <-id
                                                         ter <- terra::wrap(ter) 
                                                         disp_compute <- dispersal2(id, fam=families, ter, hab2 , famsize.max, move.max=210, hab.tot.quality=15, routes, mode=mode,disp.fail=0,disp.succ=0,Ndisp=0,wanderDist =wanderDist )  
                                                         
                                                         families <- disp_compute [[1]] 
                                                         ter <- terra::unwrap(disp_compute [[2]] )  
                                                         routes <-  disp_compute [[3]]  
                    }  
                    cat("}")
          }
      } 
                                  
     routes  <- routes[!is.na(routes$cell),] 
                             
     cat(paste0("\ncompile output: "))
         
         
      if(sum(values(ter), na.rm=TRUE)==0) {
          cat("no group settled ")
                    simpop_transloc <-  NULL       
                    routes0 <- routes   
              
      } else {
      cat("gather terrs ")                                   
                routes0 <- routes
                route <- routes <- simpop_transloc <-  NULL 
                ter  <-  subst(ter , 0, NA )      
                habitat_3857  <-  terra::rast(here::here(country,'habitat3857.tif'))
                ter0    <-  terra::project(ter , habitat_3857,method="near") 
                simpop_transloc <- terra::wrap(ter0) 
     }
           
               
    cat("...")
        
        
        
    if(!is.null(routes0)){
      cat(" gather routes ")  
      routes <- NULL
               for(fam in unique(routes0$bvr) ){
                    route <- na.exclude(routes0[routes0$bvr==fam,])
                    if(nrow(route)>0){
                           moves <- route$move
                           stage <- route$stage
                           ter  <-  subst(ter , 0, NA )      
                           route <-  as.data.frame( terra::xyFromCell(ter,route$cell)) 
                           route$path <- 1:nrow(route)
                           route$move <-  moves
                           route$stage <-  stage
                           routes <- rbind(routes,
                                           data.frame(lng=route$x,  lat=route$y,  id=fam,  path=route$path, stage=route$stage)
                                           )
                           }
            }       
        
       ## routes pts in 3857
       multipoints <- st_multipoint(matrix(c(routes$lng ,  routes$lat), ncol = 2, byrow = FALSE), dim = "XY")   %>% st_sfc() %>% st_sf() %>% st_cast("POINT")
       st_crs(multipoints) <-   st_crs(3857) 
       multipoints3857  <- multipoints  %>% st_coordinates() %>% as.data.frame()
       hab3857 <-  rast(here::here(country,'habitat3857.tif')) 
       pts3857_to4326 <- centreCell3857_to4326(country, xx=multipoints3857[,1], yy=multipoints3857[,2], hab3857)
       cat("...  ok - ")
       routes <-  data.frame(lng=pts3857_to4326$X,  lat=pts3857_to4326$Y,  id=routes$id,  path=routes$path, stage=routes$stage)
      }
     
        
     return(list(simpop_transloc,routes))
 }
 
#ptss <- st_sample(bnd_UK[bnd_UK$country == "Wales",],10) 
#pts <- st_transform(ptss, st_crs(3857)) 
#pts <- as.matrix(pts %>%st_coordinates())
#routes <- data.frame(lng=pts[,1],lat=pts[,2])##

#plot(bnd_UK[bnd_UK$country == "Wales",]$geometry)
#plot(ptss,add=T)
