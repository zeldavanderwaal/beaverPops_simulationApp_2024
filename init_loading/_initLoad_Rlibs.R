  

cat("R packages  - ") 
  
options("rgdal_show_exportToProj4_warnings"="none")
# load before sp or rgdal - mute warnings of possible GDAL/OSR exportToProj4() degradation


# R packages (excl shiny)
suppressPackageStartupMessages({
      library(xfun)
      library(leafpm) 
      library(showtext)
      library(shinyGizmo) # anims
      library(leaflet.extras)
      library(leaflet.extras2)
      library(tibble)
      library(tidyr)
      library(zip)
      library(colorspace)
      library(here)
      library(spatstat)
      library(Rfast) 
      library(ggplot2)
      library(ggtext)
      library(shinyWidgets)  
      library(shinyjs) 
      library(lwgeom) 
      library(dplyr)
      library(viridis)
      library(png)
      library(grid)
      library(shinybusy)
      library(stringr)
      library(shinyBS)
      library(colourpicker) 
      library(plotly)
      library(magrittr)
      library(terra) 
      library(stars)  
      library(htmlwidgets)
      library(htmltools)
      library(exactextractr)
      library(DT)   
      library(sf)  
      library(leafem)
      library(leaflet) 
      library(leaflegend)    
      library(lubridate)
      library(dplyr)  
      library(purrr)
      library(SpaDES.tools)   
     # library(sysfonts)
        library(promises)
        library(future)                                          
        library(future.callr)
  library(foreach)
  library(shinytitle)
        
})   
 
terra::terraOptions(tempdir=here::here("temp_files") )
terra::terraOptions(memmax = 1e+09)

      
## dev only ####      
#library(bslib) ############?
## renv::dependencies()
### forked repo for terra implemnting SpatRaster etc into leaflet  
#remotes::install_github("https://github.com/rhijmans/leaflet")  
#  devtools::install_github("https://github.com/rhijmans/leaflet")  
#library(leaflet)  
#print(.libPaths())
#library(rmapshaper) # not for app but for dev
#remotes::install_github("https://github.com/Nowosad/supercells")
#remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")# panes withrasterimage leaflet
####library(config)
#library(supercells) 
#install.packages("supercells", repos = "https://nowosad.r-universe.dev")
#library(shiny) 
#library(leaflet.extras)
#library(philentropy)
#library(HatchedPolygons)
#install.packages("leaflet.extras", lib = "C:/Users/Adm!n/AppData/Local/R/win-library/4.4")   
#library(shinythemes) 
#library(ggnewscale)   
#library(ggspatial)
#library(svMisc)###
#library(shinyEffects)
#library(ggrepel)
#library(rgeos) 
#library(nngeo)
#library(shinydashboard) 
#library(rgee) 
# terra::terraOptions(tempdir=here::here("temp_files"))  
##changed in print(.libPaths())
#[1] "C:/Users/Adm!n/AppData/Local/R/win-library/4.3" "C:/Program Files/R/R-4.3.0/library" 

#file.edit(".Rprofile")
 

 
#install.packages("usethis")
# console
# usethis::edit_r_profile()

