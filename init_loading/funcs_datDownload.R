cat("data export -  ")
   

FUN_save_metadata <- function(simName , filename,file, modify_landscape, obs_records, area_of_interest, group) {
        owd <- setwd( tempdir())
        on.exit( setwd(owd))            
           if(!is.null(modify_landscape)){
         shp_loc <- paste0(file.path(tempdir()) , "/metadat_modifLandscape")#,simName) 
         my_dsn <- paste0( shp_loc,".shp")
         sf::st_write(modify_landscape,   dsn =  my_dsn, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
           }
        
           if(!is.null(obs_records)){
         shp_loc2 <- paste0(file.path(tempdir()) , "/metadat_obsRecords")#,simName)
         my_dsn2 <- paste0( shp_loc2,".shp")
         sf::st_write(obs_records, dsn =  my_dsn2, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
           }
        
           if(!is.null(area_of_interest)){
         shp_loc3 <- paste0(file.path(tempdir()) , "/metadat_areaOfInterest")#,simName)
         my_dsn3 <- paste0( shp_loc3,".shp")
         sf::st_write(area_of_interest, dsn =  my_dsn3, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
           }
        
      filelist <-   list.files(tempdir(),pattern="metadat")    
      zipfile   <- zip( file, filelist)
      if(group == "only") {return( zipfile)}
      if(group == "all")  {return( filelist)} 
}
 



FUN_save_layout <- function(simName ,
                            filename,file, initValues, sitePoly , group) {
        owd <- setwd( tempdir())
        on.exit( setwd(owd))            
         if(!is.null(initValues)){  
         shp_loc <- paste0(file.path(tempdir()) , "/layoutGroups")#,simName) 
         my_dsn <- paste0( shp_loc,".shp")
         sf::st_write(initValues, dsn =  my_dsn, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
         }
        ## release site is minimum input required, button is disabled if there is no rel site so no need to condition
         shp_loc2 <- paste0(file.path(tempdir()) , "/layoutSite")#,simName)
         my_dsn2 <- paste0( shp_loc2,".shp")
         sf::st_write(sitePoly, dsn =  my_dsn2, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)

      filelist <-   list.files(tempdir(),pattern="layout")    
      zipfile <- zip( file, filelist)
     
      if(group == "only") {return( zipfile)}
      if(group == "all")  {return( filelist)} 
}
 

FUN_save_settleTest <- function(simName , filename,file, settleTest , group) {
        owd <- setwd( tempdir())
        on.exit( setwd(owd))            
           
         shp_loc0 <- paste0(file.path(tempdir()) , "/settlmtTest_routes",simName) 
         my_dsn0 <- paste0( shp_loc0,".shp")
         sf::st_write(settleTest, dsn =  my_dsn0, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
          
          filelist <-   list.files(tempdir(),pattern="settlmtTest")    
          zipfile <- zip( file, filelist)
 
      if(group == "only") {return( zipfile)}
      if(group == "all")  {return( filelist)} 
}
  

FUN_save_simpop <- function(simName ,
                            filename,file, 
                            simpop_shp,  recapPlot_summAdt,recapPlot_summFam, recTable_annual_stats,simParams,group ) {
        owd <- setwd( tempdir())
        on.exit( setwd(owd))            
          if(!is.null(simpop_shp)){ 
         shp_loc <- paste0(file.path(tempdir()) , "/simOutput_spatial")#,simName) 
         my_dsn <- paste0( shp_loc,".shp") 
         sf::st_write(simpop_shp , dsn =  my_dsn, driver = "ESRI Shapefile", quiet = TRUE,append=FALSE)
         }
         gg_loc1 <- paste0(file.path(tempdir()) , "/simOutput_plot_NAdt")#,simName) 
         my_dsn1 <- paste0( gg_loc1,".pdf")
         ggsave(filename=my_dsn1 , plot=recapPlot_summAdt, width = 17, height =10, unit = "cm", dpi = 300)
         
         gg_loc2 <- paste0(file.path(tempdir()) , "/simOutput_plot_NFams")#,simName) 
         my_dsn2 <- paste0( gg_loc2,".pdf")
         ggsave(filename= my_dsn2, plot=recapPlot_summFam, width = 17, height =10, unit = "cm", dpi = 300)
          
         tab_loc <- paste0(file.path(tempdir()) , "/simOutput_detTable_annual")#,simName) 
         my_dsn3 <- paste0(tab_loc,".csv")
         write.csv(recTable_annual_stats, my_dsn3)
         
         if(!is.null(simParams)){
         tab_loc0 <- paste0(file.path(tempdir()) , "/simOutput_simConfig")#,simName) 
         my_dsn0 <- paste0(tab_loc0,".txt") 
         write.table(simParams,my_dsn0, quote=FALSE , sep=": ", row.names = TRUE  )
         }
         
      filelist <-   list.files(tempdir(),pattern="simOutput")    
      zipfile <- zip( file, filelist)
 
      if(group == "only") {return( zipfile)}
      if(group == "all")  {return( filelist)} 
 
}
  
 