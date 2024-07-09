  

cat("loading  R packages  .. ") 
#  mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"
 
options("rgdal_show_exportToProj4_warnings"="none")
# before loading sp or rgdal. 
# To mute warnings of possible GDAL/OSR exportToProj4() degradation
# R packages (excl shiny)
#library(HatchedPolygons)
suppressPackageStartupMessages({
  library(xfun)
library(leafpm)
#library(leaflet.extras)
#library(philentropy)
library(showtext)
#install.packages("supercells", repos = "https://nowosad.r-universe.dev")

library(shinyGizmo) # anims
#remotes::install_github("https://github.com/Nowosad/supercells")
#remotes::install_github("rstudio/leaflet", ref="joe/feature/raster-options")# panes withrasterimage leaflet

#library(supercells) 

#install.packages("leaflet.extras", lib = "C:/Users/Adm!n/AppData/Local/R/win-library/4.4")
library(leaflet.extras)
library(leaflet.extras2)
library(tibble)
library(tidyr)

#library(rmapshaper) # not for app but for dev
   
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
#library(bslib) ############?
library(plotly)
library(magrittr)
library(terra) 
library(stars)  
library(htmlwidgets)
library(exactextractr)
library(DT)  
 #library(shiny) 
 library(sf)  
library(leafem)
library(leaflet) 
 library(leaflegend)    
library(lubridate)
library(dplyr)  
### forked repo for terra implemnting SpatRaster etc into leaflet  
#remotes::install_github("https://github.com/rhijmans/leaflet")  
#  devtools::install_github("https://github.com/rhijmans/leaflet")  
#library(leaflet)  
 #print(.libPaths())
 library(purrr)
library(SpaDES.tools)   
})   
 
  ## renv::dependencies()
  
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














terra::terraOptions(memmax = 1e+09)
terra::terraOptions(tempdir="temp_files")  
cat(".. ok  - ")
##changed in print(.libPaths())
#[1] "C:/Users/Adm!n/AppData/Local/R/win-library/4.3" "C:/Program Files/R/R-4.3.0/library" 