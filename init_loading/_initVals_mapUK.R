

cat("load map uk  - ")
                                          country_names <- c("Scotland","Wales","England","demo")
                                          cdat <- data.frame( country =  country_names,
                                                              lng_countries =  c(-3.98,-3.44, -1.29,-3.33)     ,
                                                              lat_countries = c(56.6,52.49,52.97,52.54  ) ,
                                                              zoom_countries= c(10,8,10,11)) 
                                            
                                          
                                               
                                          bnd_UK <- st_read(here::here('metadata/bnd_UK.shp' ), quiet=TRUE)
                                          bnd_UK <- st_transform(bnd_UK, st_crs(4326))  
                                          bnd_UK$country <- factor(bnd_UK$country, levels=country_names)
                                          bnd_UK <- bnd_UK[order(bnd_UK$country),]
                                          bnd_UK$country <- as.character(bnd_UK$country) 

 sf_use_s2(FALSE) 