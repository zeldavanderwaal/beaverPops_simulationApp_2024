leafletOptions(attributionControl = FALSE)

addLayersCont_list <- function(cur_layers,new_layer,type) { 
 if(type=="clear"){
   for(new_lay in new_layer){
   if(new_lay %in% cur_layers) { cur_layers <- cur_layers[-which(cur_layers == new_lay)] }
     }
 }
  if(type=="add"){cur_layers <- unique(c(cur_layers, new_layer)) }
  return(cur_layers)
}



create_btns <- function(x) {
  x %>% 
    purrr::map_chr(~
    paste0(
      '<div class = "btn-group">
                   <button  id="delete_',.x, '" 
                   type="button"
                   class="buttontab"
                   onclick=get_id(this.id)>
                   -
                   </button>
                   </div>'
    )) 
}


# used in
# indx <-  which(c("modify habitat", "observation records", "control local abundance","area of interest") == featureName) 

 markerIcons <- iconList( 
                       red = makeIcon(here::here("www/pin-red.png"), iconWidth = 30, iconHeight =30),
                       green = makeIcon(here::here("www/pin-black.png"), iconWidth = 30, iconHeight =30),
                       purple = makeIcon(here::here("www/pin-purple.png"), iconWidth = 30, iconHeight =30),
                       black = makeIcon(here::here("www/pin-black.png"), iconWidth = 30, iconHeight =30))
  

houseIconWee <- makeIcon(
  iconUrl = here::here('www/house2-lab.png'),
  iconWidth = 28, iconHeight = 28,
  iconAnchorX = 0, iconAnchorY = 0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

beaverIconWee <- makeIcon(
  iconUrl = here::here('www/beaver-facing-right_sh.png'),
  iconWidth = 40, iconHeight = 24,
  iconAnchorX = 0, iconAnchorY = 0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

timeIcon <- makeIcon(
  iconUrl = here::here('www/clock-solid.png'),
  iconWidth = 17, iconHeight = 17,
  iconAnchorX =  -7, iconAnchorY =  0,
   shadowWidth = 0, shadowHeight =0 ,
  shadowAnchorX = 0, shadowAnchorY =0 ,  className ="uppedIcons_clock"
) 


beaverIcon <- makeIcon(
  iconUrl = here::here('www/beaver-facing-right_sh.png'),
  iconWidth = 60, iconHeight = 36,
  iconAnchorX = 0, iconAnchorY = 0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0,  className ="uppedIcons"
)
 
beaverIcon_init_stranded<- makeIcon(
  iconUrl = here::here('www/beaver-facing-right_sh.png'),
  iconWidth = 30, iconHeight = 18,
  iconAnchorX =  0, iconAnchorY = 0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

beaverIconSmall_init_stranded<- makeIcon(
  iconUrl = here::here('www/beaver-facing-right-small_sh.png'),
  iconWidth = 17, iconHeight = 11,
  iconAnchorX =  16, iconAnchorY = 0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0,  className ="uppedIcons"
)
 
departFlag <- makeIcon(
  iconUrl = here::here('www/flag.png'),
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY =  0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

 
skull <- makeIcon(
  iconUrl = here::here('www/flag2-simple.png'),
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY =  0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0,  className ="uppedIcons"
)
skull2 <- makeIcon(
  iconUrl = here::here('www/skull.png'),
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY =  0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

beaverIconSmall <- makeIcon(
  iconUrl = here::here('www/beaver-facing-right-small_sh.png'),
  iconWidth = 33, iconHeight = 20,
  iconAnchorX =  24, iconAnchorY = 15,#-2,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

# change legend width
# https://stackoverflow.com/questions/46287443/creating-legend-with-custom-absolute-position-in-leaflet-within-r-shiny
# https://stackoverflow.com/questions/60838128/shiny-r-how-to-make-a-leaflet-legend-horizontal

         
      
#### disable click on plot 
#click_disabled <- FALSE
# disablePlotClick <- function(session, id){
#  session$sendCustomMessage(type = 'disablePlotClick', message = c("ID"=id))
# }
 

#### custom legend
          addLegendCustom <- function(map, title, colors, labels, sizes, shapes, borders, opacity=.7  , layerId ){

            make_shapes <- function(colors, sizes, borders, shapes) {
                shapes <- gsub("circle", "50%", shapes)
                shapes <- gsub("square", "0%", shapes)
                paste0(colors, "; width:", sizes, "px;height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
            }
            make_labels <- function(sizes, labels) {
                paste0("<div style='display:inline-block;margin-left:",10-sizes/2,"px;height: ", 
                       sizes, "px;margin-top: 4px;line-height: ", 
                       sizes, "px;'>", labels, "</div>")
            }
  
            return(addLegend(map,"topright",  
                               className = "info legend legend-larger",
                             title=title, colors = make_shapes(colors, sizes, borders, shapes), labels = make_labels(sizes, labels), layerId=layerId))
          }    
          
 

makeLabels_ind <- function(csvData_ind) {
  labs <- NULL
for(si in 1:nrow(csvData_ind)) {
   lab <- sepsymb <-"" 
        if(!is.na(csvData_ind$male[si]+csvData_ind$female[si]+csvData_ind$young[si])) {
     
    
  if(csvData_ind$female[si]>0) 
    {lab <- paste0(csvData_ind$female[si], " female")
    sepsymb <- " + " }
   
   if(csvData_ind$male[si]>0) {
     lab <- paste0(lab, sepsymb , paste0(csvData_ind$male[si]," male"))
     sepsymb <- " + " }
   
   
   if(csvData_ind$young[si]>0) {
     lab <- paste0(lab, " + ", paste0(csvData_ind$young[si], " young"))
     sepsymb <- " + " }

        }
   
   
   labs <- c(labs, lab)
   }
  return(labs)
}
  



js <- c(
  "$('[id^=checkb]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 5, value: value}];",
  "  Shiny.setInputValue('coordsTable_cell_edit:DT.cellInfo', info);",
  "})"
)



makeLabels_releasePts <- function(csvData_ind) {
    labs <- NULL

for(si in 1:nrow(csvData_ind)) {
   lab <- sepsymb <-"" 
        if(!is.na(csvData_ind$male[si]+csvData_ind$female[si]+csvData_ind$young[si])) {
     
    
  if(csvData_ind$female[si]>0) 
    {lab <- paste0(csvData_ind$female[si], " female")
    sepsymb <- " + " }
   
   if(csvData_ind$male[si]>0) {
     lab <- paste0(lab, sepsymb , paste0(csvData_ind$male[si]," male"))
     sepsymb <- " + " }
   
   
   if(csvData_ind$young[si]>0) {
     lab <- paste0(lab, " + ", paste0(csvData_ind$young[si], " young"))
     sepsymb <- " + " }

        }
   
   
   labs <- c(labs, lab)
   }
  return(labs)
}

 
        
#### palettes          
# palette_intercatchK <- leaflet::colorFactor(rev(heat.colors(nlevels(intercatch$cat))),domain= levels(intercatch$cat))
  palette_demog <- leaflet::colorNumeric( c("#DFFF00","#FFBF00") ,domain= c(0,1))
 #    factpal <- colorFactor(viridis(3),unique(bnd_UK$country))
colors_yr <- rev(rep(c("black", "darkred","red",  "red", "orange", "beige") , each=2))
   getMarkerColor <- function(csvData) {  colors_yr [csvData$year-2012]   } 
  getIconColor  <- function(csvData) {  c("black","yellow")[as.numeric(as.factor(csvData$type))] }

  
  
  
  

### map labels style (individuals and legend)
### font-size:clamp(11px,.8vw,1vw)!important;
labstyle_legend <-   " 
    color: #555; 
    font-family:Comfortaa; 
    text-indent: 12px; 
    vertical-align:middle!important;
    max-width:13vw;
    margin: 0!important;"
# line-height: 1.8em!important; 
 #   width:12vw!important;
# 
# 
labstyle_plotLabs <- c(
    "border-radius" = "20px", 
    "width"="auto",
    "font-family"="Comfortaa",
    "text-align"="center",
    "vertical-align"="middle!important",
    "z-index"="2000")

labstyle_routeLabs <- c(
    "border-radius" = "20px", 
    "width"="auto",
    "font-family"="Comfortaa",
    "text-align"="center",
    "vertical-align"="middle!important",
    "z-index"="2000", 
    "font-size"="clamp(11px,.8vh,1vh)",
    "background-color"="#0000008f")
 
      
  
### for mapUK (initial screen)  
palette_cntry <- colorFactor(palette = c( "#fcfa5b",  "#d8e470", "#fcde5b"),domain= levels(bnd_UK$country))
labstyle_plotLabs_init <-  c(
   "border-radius" = "20px",
    "font-size"="clamp(11px,.8vh,1vh)",
    "background-color"="#0000008f",
    "width"="auto",
    "font-family"="Comfortaa",
     "line-height"="2.7vh",
    "vertical-align"="middle!important" ,
    "z-index"="2000")
  
  
  
  
  cat("map funcs ok  - ")