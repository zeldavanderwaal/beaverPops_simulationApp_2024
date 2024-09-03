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
  
 
 
stranded<- makeIcon(
  iconUrl = here::here('www/flag2-spl.png'),
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 0, iconAnchorY =  0,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0,  className ="uppedIcons"
) 

beaverIconSmall <- makeIcon(
  iconUrl = here::here('www/beaver-facing-right-small_sh.png'),
  iconWidth = 33, iconHeight = 20,
  iconAnchorX =  24, iconAnchorY = 15,#-2,
   shadowWidth = 0, shadowHeight = 0,
  shadowAnchorX = 0, shadowAnchorY =0
)

 
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
          
 
 
  



js <- c(
  "$('[id^=checkb]').on('click', function(){",
  "  var id = this.getAttribute('id');",
  "  var i = parseInt(/checkb(\\d+)/.exec(id)[1]);",
  "  var value = $(this).prop('checked');",
  "  var info = [{row: i, col: 5, value: value}];",
  "  Shiny.setInputValue('coordsTable_cell_edit:DT.cellInfo', info);",
  "})"
)

 
col_succ <- "#a7ca35d9"
col_info <- "#2299aacc"
col_fail <- "#fb8861d4"
col_warn <- "#e9d852de"   
txtCol_notif <- "darkmagenta"
txtCol_notif2 <- "yellow" 

  
  

### map labels style (individuals and legend)
labstyle_legend <-   "color: #555; 
                      font-family:Comfortaa; 
                      text-indent: 12px; 
                      vertical-align:middle!important;
                      max-width:13vw;
                      margin: 0!important;"

labstyle_plotLabs <- c( "border-radius" = "20px", 
                        "width"="auto",
                        "font-family"="Comfortaa",
                        "text-align"="center",
                        "vertical-align"="middle!important",
                        "z-index"="2000")

labstyle_routeLabs <- c( "border-radius" = "20px", 
                        "width"="auto",
                        "font-family"="Comfortaa",
                        "text-align"="center",
                        "vertical-align"="middle!important",
                        "z-index"="2000", 
                        "font-size"="clamp(11px,.8vh,1vh)",
                        "background-color"="#0000008f")
 
      
  
palette_cntry <- colorFactor(palette = c( "#fcfa5b",  "#d8e470", "#fcde5b"),domain= levels(bnd_UK$country))
labstyle_plotLabs_init <-  c( "border-radius" = "20px",
                              "font-size"="clamp(11px,.8vh,1vh)",
                              "background-color"="#0000008f",
                              "width"="auto",
                              "font-family"="Comfortaa",
                               "line-height"="2.7vh",
                              "vertical-align"="middle!important" ,
                              "z-index"="2000") 
  
  cat("map aes opts  - ")