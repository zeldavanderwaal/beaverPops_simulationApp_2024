cat("newDrawnFeature  - ")
#### make geom from drawn shape ####



newDrawnFeature <- function(newFeat, run,sim_Nyrs) {
 
      sf_use_s2(FALSE)
     # Leaflet ID to add to the shapefile dataframe
      id = newFeat$properties$`_leaflet_id`
      
      # Site name combined with run # for new polygon feature
      featType = newFeat$geometry$type
      name_      = paste(c(featType, run), collapse='_')
      names = c( names, name_)
       
      # Grabbing lat/lon values for new leaflet polygon
      coor      = unlist(newFeat$geometry$coordinates )
      lng = coor[seq(1, length(coor), 2)]
      lat  = coor[seq(2, length(coor), 2)]

      
      # Building Dataframe with points from newly created leaflet feature
      c = 0
      df <- NULL 
      for (x in lng){
        c       = c + 1
        df  =  rbind(df,data.frame(Name = name_, lng = x, lat = lat[c], LeafletId = paste0("Leaflet", id)) )
        } 
      ## if line
      if(featType == "LineString") {
      newlineFeature <- st_as_sf(data.frame(df),   coords = c("lng","lat"), crs=st_crs(4326)) %>% st_combine()  %>%
                              st_cast("LINESTRING")  %>% st_sf()
                        newlineFeature$LeafletId = paste0("Leaflet", id)
                        newlineFeature$name <- "line"
                        newlineFeature$type <-paste0("<img src='penciled.png' height='14' width='14'></img>") 
                        newlineFeature$size <- paste0(round(as.numeric(st_length(newlineFeature))/1000,1), "km")
                        newlineFeature$start=0
                        newlineFeature$duration=sim_Nyrs
                        newlineFeature$layerType=NA
                         newlineFeature$layerEffect=NA
       newFeaturedat= list(data = newlineFeature,shapeGeom="line") 
       }
      
      if(featType == "Polygon") { 
         newpolyFeature <- st_as_sf(data.frame(rbind(df, df[1,])),   coords = c("lng","lat"), crs=st_crs(4326)) %>% st_combine()  %>%
                           st_cast("POLYGON")  %>% st_sf() # %>% st_make_valid()?
                        newpolyFeature$LeafletId = paste0("Leaflet", id)
                        newpolyFeature$name <- "zone"   
                        newpolyFeature$type <-paste0("<img src='penciled.png' height='14' width='14'></img>") 
                        newpolyFeature$size <- paste0(round(as.numeric(st_area(newpolyFeature))*1e-4,1),"ha") #paste0(round(as.numeric(st_area(newpolyFeature)*1e-4),-2),"ha")
                        newpolyFeature$start=0
                        newpolyFeature$duration=sim_Nyrs
                        newpolyFeature$layerType=NA
                        newpolyFeature$layerEffect=NA
       newFeaturedat= list(data = newpolyFeature,shapeGeom="polygon")
       }
        
      if(featType == "Point") { 
         newpointFeature <- st_as_sf(data.frame( df  ),   coords = c("lng","lat"), crs=st_crs(4326)) %>% st_combine()  %>%
                           st_cast("MULTIPOINT")  %>% st_sf()
                        newpointFeature$LeafletId = paste0("Leaflet", id)
                        newpointFeature$name <- "fam "   
                        newpointFeature$type <-paste0("<img src='penciled.png' height='14' width='14'></img>") 
                        newpointFeature$size <- paste0(nrow(newpointFeature) ,"pt")
                        newpointFeature$start=NA
                        newpointFeature$duration=sim_Nyrs
                        newpointFeature$layerType="observation records"
                        newpointFeature$layerEffect="territory identified on map"
       newFeaturedat= list(data = newpointFeature,shapeGeom="point")
       }
      
        
      return(newFeaturedat)
  
 
      # Updating the select input for the download availability of created leaflet features
  #    updateSelectInput(session, 'shapefiles', choices = unique(data$df$Name))

      # Building the polygon table from the data$df dataframe containing all of the leaflet polygon data
     # build_polygon_table()

      # print (data$df)
      # sets highlight pixel to on
     
          sf_use_s2(TRUE) 
   }

## longlat to "crs("EPSG:4326")" jan8 - more complete?