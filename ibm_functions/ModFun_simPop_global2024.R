cat("sim pop  - ")

################################################################################################################ FUN: simpop_function - simulate pop growth - 
simpop_function<-function(hab, initValues, StartYear,
                        #  ter.start, 
                          fam.start,   
                          depCells, 
                          cells_make0,cells_make50, cells_make75,
                          cells_obs,
                          famDf_obs,
                          country, mgmt.years, mgmt.reps,  
                          reportYears  ) {
 
cat('\nsimulate pop growth, rep',rep,': ')
mode <- "global pop"
routes.all <- out.all <- NULL
ter.all    <- fam.all <- NULL
polys_simulatedTerrs <-  NULL
mgmt.years <- as.numeric(as.character(mgmt.years))
       
      hab2 <- terra::unwrap( hab )   
      hab2 <- subst(hab2, NA, 0 ) # test jan13   
    #  cat("rasters unwrapped  - ")    
      ter.start<- hab2 #ras with only 0 vals 
      values(ter.start) <- 0        # set to 0 here only to dble check hab vals in script 

   
  ##   num.fam <- 0
    # if(!is.null(initValues)) { num.fam <- nrow(initValues) }
      fam.start <-  data.frame(fam.id=0, num.m=0, num.f=0, young=0, qual=0, stringsAsFactors=FALSE)
 
           
      simpop  <-  NULL  
   
    make0_years <- transloc_years <- obsRecs_years <- make50_years <- make75_years <-  NULL  
    if(!is.null(cells_make0)) { make0_years   <-  names(cells_make0)[ which(as.numeric(sapply(cells_make0, is.null))==0) ]   
    } 
    if(!is.null(cells_make50)) { make50_years   <-  names(cells_make50)[ which(as.numeric(sapply(cells_make50, is.null))==0) ]   
    } 
    if(!is.null(cells_make75)) { make75_years   <-  names(cells_make75)[ which(as.numeric(sapply(cells_make75, is.null))==0) ]   
     } 
    if(!is.null(depCells))    {   translocFams.start <- with(initValues, data.frame(fam.id=group, num.m=males, 
                                                     num.f=females, young=young,   qual=0, stringsAsFactors=FALSE))  
                                  transloc_years <- unique(initValues$transYears) 
    } 
   if(!is.null(cells_obs)) {     obsRecs_years <- unique(famDf_obs$obsRecsYears)  
                                 famDf_obs <- unique(famDf_obs)  
    }
    
         
  
 hab.start <- hab2     
    
  for(rep in  mgmt.reps) {  
            
            families <-    data.frame(matrix(ncol = ncol(fam.start), nrow = 0))
            colnames(families) <- colnames(fam.start )
            routes0 <- routes <- NULL
            ter <-  ter.start  
            out <- data.frame( year=1:(mgmt.years+1), 
                              num.fam=0, num.fam.obs=0, num.fam.trans=0,num.fam.sttld=0,
                              num.adt.translocs=0,num.adt.settled=0,num.young.translocs=0,num.young.settled=0,
                              num.adt=0, deaths.adt=0, num.adt.obs=0, num.young.obs=0,
                              num.juv=0, juv.born=0, deaths.juv=0,  
                              num.sub=0, deaths.sub=0, 
                              sub.recruit=0, sub.disp=0, sub.fail=0,
                              num.adt.pairs=0,
                              num.yng.rm=0,  num.adt.rm=0) 
            
            routes <- data.frame(bvr=0, cell=0, move=0,stage=0) 
              
            
            yearcount <-0
            for (year in 1: (mgmt.years+1)) { # was always 1!!
                      yearname <- as.character(paste0("y",StartYear+yearcount))  
                      cat("pop ",yearname,": ")   
                        
                      yng_rm100 <- mal_rm100 <- fem_rm100 <- 0
                      hab2 <- hab.start          
                      if(yearname %in% make0_years)   { # check ncells here taking wrong cells it seems?
                         cat("100%") 
                         hab2[cells_make0[[yearname]]] <- 0  
                         if(year>1){ # makes no sense to obs a terr within unsuitable hab and they dont disperse year1 so only matters for translocs anyway - if dying they diy BY year 2 anyhow
                         fams_0surv <- unique(values(ter)[cells_make0[[yearname]] ])  
                         if(sum(fams_0surv)>0){ 
                           fams_0surv <-fams_0surv[fams_0surv>0]
                           cat(": removing ",fams_0surv)
                             fams_0surv <- families$fam.id[which(families$fam.id %in% fams_0surv)]   
                             yng_rm100 <- sum(families$young[fams_0surv])
                             mal_rm100 <- sum(families$num.m[fams_0surv])
                             fem_rm100 <- sum(families$num.f [fams_0surv]) 
                             families$num.m[fams_0surv]<-0
                             families$num.f[fams_0surv]<-0
                             families$young[fams_0surv]<-0
                             families$qual[fams_0surv] <-0 # could introduce progressive decay or relocation here 
                             ter[cells_make0[[yearname]]]  <- 0 # remove entire terr if overlapping 
                         }
                         ###   ter[cells_make0[[yearname]]] <- 0 # remove entire terr if overlapping - could do adaptation here - when removed cells, remove from map + seek habitat to replace loss
                         }
                         cat(" - ")}      
                        
                         
                       num.fam.obs <- num.adt.obs <- num.young.obs <- 0 
                        if(yearname %in% obsRecs_years)   {
                         cat("obs - ")
                         fam.start.obs <- famDf_obs[famDf_obs$obsRecsYears == yearname,]
                         fam.start.obs <- fam.start.obs %>% subset(select=-fam.id) #[,-which(colnames(fam.start.obs) == "fam.id")] #after first year may be fam.id added beurgh?!
                         fam.start.obs  <- data.frame(fam.id= seq(1:nrow(fam.start.obs)) + nrow(families),fam.start.obs )   # id as annual count from 1 +  prev rows

                         for(fam in 1:nrow(fam.start.obs)){
                                 leafId <- fam.start.obs$LeafletId[fam]
                                 cells <- cells_obs[[leafId]] 
                                 ter[cells] <- fam.start.obs$fam.id[fam]
                                 ### NOTE: if making make0 retro-active, would be here: ter[make0 cells] <- 0 + delete terr in families for remaining qual<20
                         }
                       
                         fam.start.obs <- subset(fam.start.obs, select= c(fam.id, num.m, num.f,young,qual ))
                         num.fam.obs <- nrow(fam.start.obs)
                         num.adt.obs <- sum(fam.start.obs$num.f+fam.start.obs$num.m)
                         num.young.obs <- sum(fam.start.obs$young)
                         families <- rbind(families, fam.start.obs)
                          } 
                      
                         
                  add_Subs_on_ObsYear <- FALSE
                  if(any(add_Subs_on_ObsYear==TRUE, nrow(families)>0 & year>1)){ #i.ei. dont include those cells if not including popdyns
                         
                      fams_50surv <- NULL
                      if(yearname %in% make50_years){ 
                          cat("50% - ") 
                         fams_50surv <- unique(values(ter)[cells_make50[[yearname]] ]) 
                        if(sum(fams_50surv)==0) {
                           fams_50surv<- NULL
                           }else {
                             fams_50surv <- fams_50surv[fams_50surv>0]
                           }
                          }
                      
                      fams_75surv <- NULL
                      if(yearname %in% make75_years){ 
                        cat("75% - ")  
                         fams_75surv <- unique(values(ter) [cells_make75[[yearname]] ]) 
                         if(sum(fams_75surv)==0) {
                           fams_75surv<- NULL
                           }else {
                             fams_75surv <- fams_75surv[fams_75surv>0]
                           }
                            } 
            }
                            yng_rm50 <- mal_rm50 <- fem_rm50 <- yng_rm75 <- mal_rm75 <- fem_rm75 <- 0 # removed counted as included in dead for each - not to be substracted again - for checking numbers
                            deaths.adt <- births <- deaths.juv <-  disp.succ <- disp.fail <- deaths.sub <-  Ndisp<- 0
                            recruit <- matrix(0,ncol =3)
                            sub.disp <- 0     
                           
                
                  #add_Subs_on_ObsYear step WAS NOT IN ORIGINAL MODEL so not adding the now?       
                  if(add_Subs_on_ObsYear==TRUE){  # established families only: year1 make up some subs?    
                      if(nrow(families)>0 & year==1){                      
                        last.years.young <- sapply(families$young, survival, mort=mort.sub) # so the families$young obs in year 1 bcome sub that year 2 only
                        recruit <- t(sapply(families$fam.id, recruitment2, young=last.years.young, fam=families, famsize.max=famsize.max)) #last.years.young obs on year 1 now subs
                        sub.disp <- recruit[,1]
                          hab2 <- subst(hab2, NA, 0 )
                          ter <- subst(ter, NA, 0 )
                          disp <- tidyr::uncount(data.frame(fam.id=1:length(sub.disp), sub.disp), sub.disp)$fam.id 
                               if(length(disp)>0) {
                                 disp <- data.frame(fam.id=disp, ind.id=1:length(disp)) 
                                # print(disp)
                                 for (id in psample(disp$fam.id,length(disp$fam.id))) { 
                                   
                                   ter <- terra::wrap(ter)
                                  # cat("disperse - ")
                                   disp_compute <- dispersal2(id, fam=families, ter, hab2 , famsize.max, move.max, hab.tot.quality, routes, mode=mode,disp.fail=disp.fail,disp.succ=disp.succ, Ndisp=Ndisp)   
                           # print(disp_compute[[1]])
                       # print(families)
                          families <- disp_compute [[1]]
                          ter <- terra::unwrap(disp_compute [[2]] )
                          routes <- disp_compute [[3]]
                          disp.fail <- disp_compute[[4]]
                          disp.succ <- disp_compute[[5]]
                          Ndisp <- disp_compute[[6]]
 
                                 }  }  
                                                          
            }     }                           
                                                          
                                                          
                            
                      # established families only: grow      
                      if(nrow(families)>0 & year>1){    
                        cat("grow(")
                                cat(".")
                                last.years.young <- sapply(families$young, survival, mort=mort.sub) # so the families$young obs in year 1 bcome sub that year 2 only
                                deaths.sub <- sum(families$young) - sum(last.years.young)
                                
                                cat(".")
                                this.years.young <- sapply(families$fam.id, breeding2, fam=families, litter.size=litter.size, breed.prob=breed.prob)
                                births <- this.years.young
                                this.years.young0 <- this.years.young <- sapply(this.years.young, survival, mort=mort.juv) 
                                mal.surv0 <-mal.surv <- sapply(families$num.m, survival, mort=mort.adt)
                                fem.surv0 <-fem.surv <- sapply(families$num.f, survival, mort=mort.adt)
                                cat(".") 
                                
                                         if(!is.null(fams_75surv)){
                                           cat("75%")
                                          fams_75surv  <-  families$fam.id[which(families$fam.id %in% fams_75surv)]  
                                          mal.surv[fams_75surv] <- sapply(mal.surv[fams_75surv], survival, mort=.75)
                                          fem.surv[fams_75surv] <- sapply(fem.surv[fams_75surv], survival, mort=.75)
                                          this.years.young[fams_75surv] <- sapply(this.years.young[fams_75surv], survival, mort=.75)
                                          
                                          yng_rm75 <- sum(this.years.young0)-sum(this.years.young)
                                          mal_rm75 <- sum(mal.surv0) -sum(mal.surv)
                                          fem_rm75 <- sum(fem.surv0) -sum(fem.surv) 
                                          }
                                         if(!is.null(fams_50surv)){
                                          cat("50%") 
                                          fams_50surv  <-  families$fam.id[which(families$fam.id %in% fams_50surv)]
                                           
                                          mal.surv[fams_50surv] <- sapply(mal.surv[fams_50surv], survival, mort=.5) 
                                          fem.surv[fams_50surv] <- sapply(fem.surv[fams_50surv], survival, mort=.5) 
                                          this.years.young[fams_50surv] <- sapply(this.years.young[fams_50surv], survival, mort=.5) 
                                          
                                          yng_rm50 <- sum(this.years.young0)-sum(this.years.young)
                                          mal_rm50 <- sum(mal.surv0) -sum(mal.surv)
                                          fem_rm50 <- sum(fem.surv0) -sum(fem.surv)
                                         }
 
                               deaths.juv <- sum(births)-sum(this.years.young)
                               families$young <- this.years.young
                               deaths.adt <- sum( (families$num.m - mal.surv) + (families$num.f - fem.surv) )
                               families$num.m <- mal.surv
                               families$num.f <- fem.surv
                               cat(".")
                     
                         #   out$num.sub[out$year==year] <- sum(recruit[,1:3])
                        #    out$sub.recruit[out$year==year] <- sum(recruit[,2:3])
                               
                               recruit <- t(sapply(families$fam.id, recruitment2, young=last.years.young, fam=families, famsize.max=famsize.max)) #last.years.young obs on year 1 now subs
                               sub.disp <- recruit[,1]
                               families$num.m <- families$num.m + recruit[,2]
                               families$num.f <- families$num.f + recruit[,3]
                                hab2 <- subst(hab2, NA, 0 )
                               ter <- subst(ter, NA, 0 ) 
                            cat(")")
                            
                           ###
                           ###
                         ##clear empty terrs?
                         ##families         
                         deidIds <- families$fam.id[which(families$num.m+families$num.f+families$young==0  & families$qual>0) ]
                          if(length(deidIds)>0){# is this changing the model?? missing
                           cat( "  deid(",deidIds,")") 
                             families$qual[deidIds] <- 0
                            #families$young[deidIds] <- 0 #???
                            #ter <- subst(ter,deidIds,0) #BeAUT!
                            
                          } ###could easily make the terr removed a non suitable spot for a year or two??
                           ###
                           ###
                           ###
                             
                               ### here wrong attrib of youngs to fam?.id??
                               disp <- tidyr::uncount(data.frame(fam.id=1:length(sub.disp), sub.disp), sub.disp)$fam.id 
                               if(length(disp)>0) {
                                   
                                 disp <- data.frame(fam.id=disp, ind.id=1:length(disp))
                                  
                                cat("  recruit(fams", disp$fam.id,"/beavs",disp$ind.id,"=",length(disp$fam.id)," dispersing)")   
                                 for (id in psample(disp$fam.id,length(disp$fam.id))) { 
                                   wanderDist<- rpois(n=1,lambda=3) *10
                                     
                                    
                                   ter <- terra::wrap(ter)
                                    cat("\n( ")
                                   disp_compute <- dispersal2(id, fam=families, ter, hab2 , famsize.max, move.max, hab.tot.quality, routes, mode=mode,disp.fail=disp.fail,disp.succ=disp.succ, Ndisp=Ndisp,wanderDist=wanderDist)   
                          families <- disp_compute [[1]]
                          ter <- terra::unwrap(disp_compute [[2]] )
                          routes <- disp_compute [[3]]
                          disp.fail <- disp_compute[[4]]
                          disp.succ <- disp_compute[[5]]
                          Ndisp <- disp_compute[[6]]
                          cat( " )") 
                                 }  
                               # cat(" boo" )
                                } 
 }                       
                        translocs <- translocs_stl <- NULL       
                        num.fam.sttld <- num.fam.trans <-num.adt.settled <- num.adt.translocs <- num.young.translocs <-num.young.settled <- 0     
                       ## before they disperse: release translocated fams
                       if(yearname %in% transloc_years) {
                             cat("releases :") 
                             translocs <-  translocFams.start[which(initValues$transYears %in% yearname),]
                             routes0_translocs <- routes_translocs <- NULL
                              translocs$fam.id <-   seq(1, nrow(translocs)) + nrow(families)  
                              num.fam.trans <- length(translocs$fam.id)     
                             disp <- tidyr::uncount(data.frame(fam.id=translocs$fam.id , 1),1)$fam.id  
                             
                             if(length(disp)>0) {
                                   disp <- data.frame(fam.id=disp, ind.id=1:length(disp))  
                                    ter_transloc <- ter             
                                    hab_transloc <- hab2  # includes modifs that year
                                   values(hab_transloc)[values(ter)>0]  <- 0 # include  existing fams that year as unsuitable for joining
                                     
                                   depCell_yr <- depCells [[ yearname ]]
                                   hab_transloc <- terra::wrap(hab_transloc) 
                                   for (id in psample(disp$fam.id,length(disp$fam.id))) {   
                                    wanderDist<- rpois(n=1,lambda=3) *10
                                   cat( "\n{ ") 
                                   depCell <-  depCell_yr[id-nrow(families)]
                                 #   valid_depCell <- which( values(ter) ==id) # filtered for unsuit hab etc
                                     
                                                ter_transloc[depCell] <-  id 
                                               ter_transloc <- terra::wrap(ter_transloc)  
                                               disp_compute <- dispersal2(id, fam=translocs, ter=ter_transloc, hab=hab_transloc, famsize.max, move.max=250, 
                                                                          hab.tot.quality=15, routes=routes0_translocs, mode="settlement_test_but_not", disp.fail = disp.fail,disp.succ=disp.succ,Ndisp=0,wanderDist=wanderDist)  
                                           
                                               translocs <- disp_compute [[1]] 
                                               ter_transloc <- terra::unwrap(disp_compute [[2]] )  
                                               routes0_translocs <- disp_compute [[3]]   # not storing now? should get id ho issue 
                                               ter_transloc[depCell] <-0
                                        cat(" } ")
                                   }}  
                              
                                num.adt.translocs <- sum(translocs$num.m + translocs$num.f ) # <- sum(translocs_stl$num.m + translocs_stl$num.f )
                                num.young.translocs<- sum(translocs$young)
                                translocs$num.m[translocs$qual<15] <- 0
                                translocs$num.f[translocs$qual<15] <- 0
                                translocs$young[translocs$qual<15] <- 0
                                families <-  rbind(families, translocs)
                               
                                
                             if(any(translocs$qual>15) ) {
                               cat("- add newly settled\n")
                                translocs_stl <- translocs[translocs$qual>15,]
                           #      translocs_stl$fam.id <-  nrow(families) + seq(1, nrow(translocs_stl))  
                              #  num.adt.translocs <- sum(translocs$num.m + translocs_stl$num.f ) # <- sum(translocs_stl$num.m + translocs_stl$num.f )
                               # num.young.translocs<- sum(translocs$young)
                                
                                ##??
                                ##num.young.translocs
                                
                                
                                ##apply normal annual survival only, no growth on transloc year? OR NOT?
                             #   translocs_stl$young <- sapply(translocs_stl$young, survival, mort=mort.juv)
                              #  translocs_stl$num.m <-  sapply(translocs_stl$num.m, survival, mort=mort.adt)
                               # translocs_stl$num.f <- sapply(translocs_stl$num.f, survival, mort=mort.adt)  
                                 num.young.settled <- sum(translocs_stl$young)
                                num.adt.settled    <- sum(translocs_stl$num.m, translocs_stl$num.f )
                                 
                                num.fam.sttld <- nrow(translocs_stl) 
                                values(ter_transloc)[!values(ter_transloc) %in% translocs_stl$fam.id] <-0# remove failed
                              #  ter_transloc <- ter_transloc + nrow(families)
                              #  ter_transloc[ter_transloc==nrow(families)] <- 0 
                                translocs_stl<-  data.frame(fam.id= seq_len(nrow(translocs_stl))+ nrow(families),translocs_stl%>%subset(select=-fam.id )  )  
                           
                           values(ter)[values(ter_transloc)>0] <-  values(ter_transloc)[values(ter_transloc)>0]
                          # families <-  rbind(families, translocs_stl)                           
                             } else {
                            cat("none settled\n")
                     }
                       }  else {  cat("\n")}    
                            
                            out$num.fam.obs[out$year==year] <- num.fam.obs
                            out$num.adt.obs[out$year==year] <- num.adt.obs
                            out$num.adt.translocs[out$year==year] <- num.adt.translocs
                            out$num.young.translocs[out$year==year] <- num.young.translocs
                            out$num.adt.settled[out$year==year] <-num.adt.settled
                            out$num.fam.trans[out$year==year] <- num.fam.trans
                            out$num.fam.sttld[out$year==year] <- num.fam.sttld
                            out$num.fam[out$year==year] <- NROW(subset(families, num.m+num.f>0))
                            out$num.adt[out$year==year] <- sum(families$num.m, families$num.f)  
                            out$deaths.adt[out$year==year] <- deaths.adt
                            out$juv.born[out$year==year] <- sum(births)
                            out$num.young.settled[out$year==year] <- num.young.settled
                            out$num.juv[out$year==year] <- sum(births) - deaths.juv
                            out$deaths.sub[out$year==year] <- deaths.sub
                            out$deaths.juv[out$year==year] <- deaths.juv
                            out$num.young.obs[out$year==year] <- num.young.obs
                            out$num.sub[out$year==year] <- sum(recruit[,1:3])
                            out$sub.recruit[out$year==year] <- sum(recruit[,2:3])
                            out$sub.disp[out$year==year] <- disp.succ
                            out$sub.fail[out$year==year] <- disp.fail  
                            out$num.adt.pairs[out$year==year] <- NROW(subset(families, num.m+num.f>1))
                            out$num.yng.rm[out$year==year] <- yng_rm50 +yng_rm75+yng_rm100
                            out$num.adt.rm[out$year==year] <- fem_rm50 + mal_rm50 +fem_rm75 + mal_rm75 +fem_rm100 + mal_rm100 
                             
                       if(length(which(values(ter)>0))){
                              ter.all <- rbind(ter.all, data.frame(cell=which(values(ter)>0), ter=values(ter)[values(ter)>0], rep.id=rep, year=year ))
                              fam.all <- rbind(fam.all, data.frame(families, rep.id=rep, year=year))
                       }
             yearcount <- yearcount+1      
              
             
           }# year loop 
 
             out.all    <- rbind(out.all, data.frame(rep.id=rep(rep, NROW(out)), out)) 
           
          
         }#rep loop  
    simpop <- list(ter.all,fam.all, out.all)
 
return(simpop)
   }
 
       