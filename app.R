 
 
checkShinyOutput <- function(){
  tryCatch({
      parsed <- getParseData(parse(file = rstudioapi::getSourceEditorContext()$path))
      shinyOutput <- parsed[parsed$token=='SYMBOL_FUNCTION_CALL'& grepl("^[a-z]+[A-z]+Output$",parsed$text),]
      shinyOutput <- merge(shinyOutput,parsed,by='line1')
      shinyOutput <- shinyOutput[shinyOutput$token.y == "STR_CONST",]
      warn <- table(shinyOutput$text.y)
      warn <- warn[warn>=2]
      warnname <- names(warn)
      if (length(warn>1)) {
       warning(mapply(function(warn,nb){paste("Output",warn,"is used",nb,"times")},warnname,warn))
      }
    },
    error = function(){},
    warning = function(cond) {
      message("Shiny UI : check following warnings to avoid unexpected UI behaviour")
      message(cond)
    } 
  )
} 
  
#perfTable <- read.csv( paste0(here::here("testing/perfTable.csv")))
#options(shiny.reactlog=TRUE) 

 
#   
#   
#   
 
                     
initLoading_scripts <- paste0(here::here("init_loading"),"/",list.files(here::here("init_loading")) )
initData_scripts <- paste0(here::here("init_data"),"/",list.files(here::here("init_data")) )
UI_scripts <- paste0(here::here("ui_functions"),"/",list.files(here::here("ui_functions")) )
IBM_scripts <- paste0(here::here("IBM_functions"),"/",list.files(here::here("IBM_functions")) )
GIS_scripts <- paste0(here::here("GIS_functions"),"/",list.files(here::here("GIS_functions")) )
   
                                          for(scr in initLoading_scripts){   source(scr) }
                                          for(scr in initData_scripts)   {   source(scr) }
                                          for(scr in UI_scripts)         {   source(scr) }
                                          for(scr in IBM_scripts)        {   source(scr) }   
                                          for(scr in GIS_scripts)        {   source(scr) } 
                                          font_add_google("Comfortaa", "Comfortaa")
                                          showtext_auto()
 
                                     
                                           
                                          

 #plan(multisession, workers = 4)
#message("Number of parallel workers: ", nbrOfWorkers())
shiny::addResourcePath("www", paste0(here::here(),"/www"))
 Vectorize(require)(package = c("promises","future"), character.only = TRUE)
  library(promises)
  library(future)                                          
  library(future.callr)
  plan(callr) 
                           
                     
# plan(multisession ) #, workers=10 comes with race issues # max numb to use would be future::availableCores() anywa (cores
  options(future.globals.onReference = "ignore")
#extra overhead "error" introduces, which can be significant for very large nested objects
#gets triggered by error in somefuncs
#https://cran.r-project.org/web/packages/future/vignettes/future-4-non-exportable-objects.html

  
# object.size( list.files(here("data/global")))
#336 bytes  
 
#object.size(list.files(here("data/England")))
#2328 bytes

#object.size(list.files(here("data/Scotland")))
#2328 bytes

#object.size(list.files(here("data/Wales")))
#2328 bytes
  
# allow 3G??  
 # object.size(here::here())
 # 90 016
   
options(future.globals.maxSize= 3145728000)# for 3g limit891289600) #limit calculated  for an 850mb limit: 850*1024^2 = 891289600 
                                      
                                       
################################ https://stackoverflow.com/questions/62916053/r-asynchronous-parallel-lapply?rq=3  #####################################                                          
                                          
                                          
                                         
#
#
#
#
#
 
 #library(shinyalert)                               
####  
#### UI 
####  
 
                                          
 
                                          
                                          
ui <- fluidPage(    
             shinyjs::useShinyjs(),
             tags$head(tags$script(paste0("Shiny.addCustomMessageHandler('activate_tooltips', function(x) {","$('[data-toggle=\"tooltip\"]').tooltip();})"))  ), 
             includeCSS("www/aesthetics.css"),
             tags$head(tags$link(rel="stylesheet",href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css")),
             tags$style(HTML('#password {max-width:100%!important;font-family:monospace;}')), 
             tags$style(HTML('#password::placeholder {color:black}')), 
             tags$head(tags$style(HTML(".navbar .navbar-nav    {float: right}
                                        .navbar .navbar-header {float: left}"))), 
             tags$style(HTML('table.dataTable.selected {box-shadow: inset 0 0 0 9999px #ffe62cc2!important;}')),                 
             tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {background-color:#b0bed9!important; } "))),  
             tags$head(tags$script(src="./script_current_id.js")),             
             tags$head(tags$script(src="./script_JSconflict.js")), 
             tags$head(tags$script(src="./script_removeLeaflet.js")),      
             tags$head(tags$link(rel = "icon", type = "image/png", href = "beaver-legend.png" )),  
                          
             shinybusy::add_busy_spinner(  spin = "fading-circle",  onstart=TRUE,height = "44px",color="magenta",position = "top-left",margins = c("4px","62vw"))  ,
     
    
             
################### LAUNCH PAGE             
   tags$div(id="show_launchPage",  style="z-index:20;position:absolute;background-color:#ffe62c52!important;",
   navbarPage(title=div( img(src="beaver-facing-right-small_sh - Copy.png", width="auto", height="auto",id="beav0", class="animate__animated animate__bounce animate__delay-2s",
                               style="margin-left:6vw;width:clamp(.7*50px,.8vw,1vw) ;height:clamp(.7*30px,.8vw,1vw);"),
                            "A BEAVER POPULATION SIMULATION APP"
                           ),
  position = "fixed-top", 
     tabPanel(div(style="font-weight: 100; font-size: 81%;", "v1-june2024" , 
                   actionButton("ask_app0", "?", inline=TRUE, class="btn_Ask" , class="btn_AskApp",style="margin-top: 0 !important;") )
, id="tab0", 
            
 hidden(  
  tags$div(id="show_author", div(style="font-size: 11px;position:absolute;color:magenta;top:10px;z-index:40;margin-left:66vw;font-weight: bolder;", "an app developed by .zelda.", span( "github.com/zeldavanderwaal", style="font-weight:normal!important;margin-left:17px;color:yellow;") ))
  ),

  
    br(),
    tags$div(id="prelaunchPage_ui", 
             p( "initialising data..", style="padding-top:46vh;margin-left: 47vw; width: 300px;color: magenta;position: absolute;z-index:0;"),
             div(img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'),  
                 style="padding-top:52vh;padding-left:48vw;z-index:0;position:absolute") 
              ), 
      launchPage_ui_txt 
)) )# big div launch page
 
   
 ,  
   
  
################### APP START PAGE             
    
hidden(
  tags$div(id="show_panels",   
  
  navbarPage(title=div(img(src="beaver-facing-right-magenta.png", width="auto", height="auto",id="beav", class="animate__animated animate__bounce animate__delay-5s", 
                               style="margin-left:6vw;width:clamp(.7*50px,.8vw,1vw) ;height:clamp(.7*30px,.8vw,1vw);"),
                            "A BEAVER POPULATION SIMULATION APP"
                           ),
  id="navBar", 
  position = "fixed-top",
  tabPanel("spatial layout",   
    
           div(id = 'placeholder_timing'), # sound effects
   
   hidden( tags$div(id="pretab2_ui",
             p(id="initLoadingTxt2","loading country data..", style="padding-top:47vh;padding-left:47vw;color:#ffe62cc2;z-index:0;position: absolute;"),
             div(img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'),  style="padding-top:52vh;padding-left:49vw;position:absolute") 
             )), 

# uiOutput("mapReleaseSite_UI" )  , 
          tags$div(id="leafmap", leaflet::leafletOutput("mapReleaseSite" , height="100vh")  ),  

 
#conditionalPanel( condition ="output.countryOK == 1",
#absolutePanel(id = "leafcontrol_panel",
#                  class = "panel panel-default", fixed = TRUE, class="animate__animated animate__fadeIn animate__delay-2s", 
#              style = 'background-color:transparent;border-color:transparent;position:absolute',
#                  draggable = FALSE, top ="auto", left ="1vw", right = "auto",bottom="51px", width="auto", height = "auto",
#             div(id="contrPanelTitle", p("Mapped layers controls",style=" margin-bottom: 0;"), class= "leafLegendTitle", style="padding: 4px 1vw; border-radius: 4vh;") 
#             )),

###################################################################################################################  model running panel tab #1
 conditionalPanel( condition ="output.modelRunning == 1",
      absolutePanel( id = "prog_panel_model",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
          uiOutput("modelRunning_panel")  
)),
  conditionalPanel( condition ="output.modelPrep == 1",
      absolutePanel( id = "prog_panel_prep",top ="11px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
         div(textOutput("modelPreping_panel"),style="padding: 12px 27px;" )
)), 

conditionalPanel( condition ="output.modelOut == 1",
      absolutePanel( id = "prog_panel_out",top ="11px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor: grabbing;width: fit-content;',
                         div( style="display:flex;align-items: baseline;justify-content: space-between;padding-right: 11px;padding-left: 11px;",
                            div(textOutput("modelOutput_panel"),  style="display:inline;padding: 12px 17px;" ))
)),
   
 
###################################################################################################################  buttons panel
 
conditionalPanel( condition = "output.SiteSelected0 == 1",    
             absolutePanel(id = "buttons_panel",
                                       class = "panel panel-default", fixed = TRUE,  #class="animate__animated animate__fadeIn animate__delay-1s", 
                           style = 'display:flex;background:transparent;border: 0px;margin-left: max(480px, 22vw);top:max(59px,8vh);',
                                       draggable = FALSE, top = 0, left ="auto", right = "auto",bottom="auto", width="auto", height = "auto",
                          
                           
                           actionButton("ask_simBtns", "?", inline=TRUE, class="btn_Ask", style="margin:10px 0 !important;"  ), 
                            
                            div( style="display:inline-flex;background:#555555c4;border-radius: 50px;",
                             actionButton("adjust_view",  
                                          div(style="text-align:center;padding:0px;","adjust", br(),icon("binoculars" ),br(),"view" )   ,
                                        class="btn_adj",class="btn_Simulate" )    ,  
                            div( actionButton("view_input",  div(style="text-align:center;padding:0px;","check",br(),icon("list-check"),br(),"input"),  
                                                            class = "btn_check",class="btn_Simulate" ) ,style="margin-left:7px;display:inline-flex;")
                           ), 
                             p("", style="margin:10px;"),            
                           conditionalPanel("output.but_initTerrs == 1", style="display:inline-flex;background:#555555c4;border-radius:50px;",
                                        
                                        div( actionButton("try_settling",  div(style="text-align:center;padding:0px;","try",br(),icon("house"),br(),"settling"),  
                                                            class = "btn_simterrs",class="btn_Simulate" ) ),
                                       div(style="margin-left:7px;",actionButton("undo_settling",  div(style="text-align:center;padding:0px;","undo",br(),icon("undo"),br(),"settling"),  
                                                            class = "btn_simterrsUndo",class="btn_Simulate" ) ),
                             
                                conditionalPanel("output.but_sTestOutput == 1" , 
                                              style="margin-left:10px;padding:0 17px; min-width: fit-content;display: flex; flex-direction: column; background:#13b3a94f;  border-radius: 50px;  text-align: center; color: turquoise;",
                                 p("viewing", style="margin:0!important;"),#margin: 4px; text-align: center;,
                                 p("settlement test", style="margin:0!important;"),
                                 div(uiOutput("modelsTestOutput_panel"),  style="text-align:center;padding-top:4px;" )
                                 ),         
                                       
                                       
                                       
                                                  ),
                             p("", style="margin:10px;"),            
                             conditionalPanel("output.but_simCanStart == 1" , style="display:inline-flex;background:#555555c4;border-radius: 50px;",
                                        div( actionButton("start_sim",  div(style="text-align:center;padding:0px;","start",br(),icon("play"),br(),"growing"),  
                                                            class="btn_Simulate" ,class = "btn_simPop") ),
                                        actionButton("undo_sim",  div(style="text-align:center;padding:0px;","undo",br(),icon("undo"),br(),"growing"),  
                                                            class = "btn_simPopUndo",class="btn_Simulate" )  ,
                              conditionalPanel("output.but_simOutput == 1" , style="margin-left:10px;padding: 0 17px;min-width: fit-content;display:flex;flex-direction: column;background:#ff00ff3d;border-radius: 50px;",
                                 div(uiOutput("modelSimOutput_panel"),  style="text-align:center;color:#fd87fd;padding-top:4px;" ), 
                              tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hide minor ticks
                             div(uiOutput("uishow_simYrs"))
     )   
  )     

  )) ,
# make those hidden and disaply only after map is on         
conditionalPanel( condition ="output.countryOK == 1", 
hidden(tags$div(id="div_map_titles",
         absolutePanel(id = "leaflegend_panel",
                                class = "panel panel-default", fixed = TRUE,    
                            style = 'background-color:transparent;border-color:transparent;position:absolute',
                                draggable = FALSE, top = 0, left ="auto", right = "1vw",bottom="auto", width="auto", height = "auto", 
                   
                               div(id="legendTitle",
                                   
                                   actionButton("ask_legend", "?", inline=TRUE, class="btn_Ask" , class="btn_AskOut"), 
                                   p("Mapped layers legend",style=" margin-bottom: 0;"), class= "leafLegendTitle", class= "leafLegendTitletop", 
                                style="text-wrap: nowrap;padding: 4px 1vw; border-radius: 4vh;display: flex;") 
               ),
              
              absolutePanel(id = "simParams_panel",
                                class = "panel panel-default", fixed = TRUE,   
                            style = 'background-color:transparent;border-color:transparent;position:absolute;',
                                draggable = FALSE, top =0, left ="1vw", right = "auto",  width="auto", height = "auto",
                           div(id="parsPanelTitle",  p("Simulation parameters",style=" margin-bottom: 0;"), 
                               class= "leafLegendTitle",  class= "leafLegendTitletop1", 
                                style="padding: 4px 1vw; border-radius: 4vh;") 
               ),
         absolutePanel(id = "leafcontrol_panel",
                  class = "panel panel-default", fixed = TRUE, #class="animate__animated animate__fadeIn animate__delay-2s", 
              style = 'background-color:transparent;border-color:transparent;position:absolute',
                  draggable = FALSE, top ="auto", left ="1vw", right = "auto",bottom="51px", width="auto", height = "auto",
             div(id="contrPanelTitle", p("Mapped layers controls",style=" margin-bottom: 0;"), class= "leafLegendTitle", style="padding: 4px 1vw; border-radius: 4vh;") 
             )

))),
 
 tags$style(HTML('#SimParamsTabs {font-size:90%;display:flex;align-content:center;align-items:center;flex-wrap:wrap;justify-content:space-between;
                           background-color:#999!important;border-radius: 0 7vh 0 0;}')),                  
 
###################################################################################################################  params panel (topleft)
  conditionalPanel(condition = "output.countryOK == 1", 
      hidden(tags$div(id="div_paramsPanel",             
          absolutePanel( id = "buffersize", 
                      style = "min-width: 390px;  width:20vw; background:transparent;border: 0!important; color:black;opacity:.82;", 
                      class = "panel panel-default",  class= "leafLegendTitletop1a", fixed = TRUE, draggable = FALSE, top =0  , left ="2vw", right = "auto", bottom = "auto",  
          
             tabsetPanel(id="SimParamsTabs",    
                    tabPanel("site",
                             id="candSiteTab",  align="center", style="padding-bottom:6px;background:white;opacity:.96;border-radius:0 0 20px 20px;",
                                      fluidRow(class="hdr_darkBack", 
                                                column(10,div("Exploratory site geometry",class="hdr_darkText")),
                                                column(2, div(actionButton("ask_siteGeometry", "?", inline=TRUE, class="btn_Ask" ), style="float:left"))
                                                 ), 
                              
                                      br(),
                                      fluidRow(column(4, syle="top: 0.8vh;",
                                                      div("Operation",class="hdr_params", style="margin-left:12px;top:0.8vh;" )), 
                                               column(7,
                                                      div(shinyWidgets::radioGroupButtons(inputId ='operation', label=NULL, choices=c("observation","translocation"), 
                                                                        selected="observation", width="100%"), style="margin-bottom:17px;"))),
                             
                                      br(),
                                      fluidRow(column(4, syle="top: 0.8vh;",
                                                       div("Base geometry",class="hdr_params", style="margin-left:12px;" )), 
                                               column(7,
                                                      div(shinyWidgets::radioGroupButtons(inputId ='shape', label=NULL, choiceNames=c("none","buffer","catchment"), choiceValues=c("none","buffer","catchment"),  individual=FALSE, 
                                                                        selected="none", width="100%"), style="margin-bottom:17px;"))),
                             
                                      br(),  
                  
                               hidden(tags$div(id="div_buffer_sel",
                                           fluidRow(column(4, syle="top: 0.8vh;",
                                                       div("Zone radius",class="hdr_params", style="margin-left:12px;margin-right:0px; " )), 
                                               column(7,  
                                       div( shinyWidgets::radioGroupButtons(inputId ='buffer_sel', label=NULL, choiceNames=c( "2km", "5km", "10km", "25km"), choiceValues=c(2000, 5000,10000, 25000), individual=FALSE, 
                                                                        selected=2000, width="100%" ), style="margin-bottom:17px;white-space:wrap;"))
                                    ))),  
                                      conditionalPanel("output.catchment_selection==1" , style="margin-top:12px",
                                                 fluidRow( uiOutput("catchSelection_title")  )),
                                       
                               fluidRow( column(10, offset=1, 
                                      conditionalPanel("output.catchment_selection==1", 
                                               fluidRow( 
                                                  column(2,  div( img(src="check.png", width="auto", height="auto", style="margin-top: 8px;width:1.7vh;height:1.7vh;" ))), 
                                                  column(10,  style="margin:0;padding:6px;background-color:#ffe62cc2ba;border-radius: 20px 20px 20px 20px;",
                                                         div(uiOutput( "selected_catchments_names" ) ,style="font-size:90%;max-height:20vh;overflow-y:auto;"))
                                                  )
                                                )   ,
                                      conditionalPanel("output.catchment_selection==1 && output.catchment_selection_noSite==1" , style="margin-top:12px;margin-bottom:6px;",
                                                
                                                fluidRow( 
                                                  column(2,  div( img(src="decline.png", width="auto", height="auto", style="margin-top: 8px;width:1.7vh;height:1.7vh;" ))), 
                                                  column(10 , style="margin:0;padding:6px;background-color:#e28ada29;border-radius:20px 20px 20px 20px;",
                                                         div(uiOutput("nonselected_catchments_names" ) ,style="font-size:90%;max-height:13vh;overflow-y:auto;")
                                                ))
                                                      
                                      ))),
                                     
                             
                                conditionalPanel("output.SiteSelected == 1",  
                                                          div(  shinyWidgets::prettyCheckbox(inputId = "lock_site" ,  label = "exploratory site unlocked", 
                                                                                             shape = "round",  value = FALSE, fill=TRUE, 
                                                                                             icon=icon("lock")), style="padding:12px 12px 0 12px;")  
                                                ) 

                             ), 
       
                             
                             
                     tabPanel("points",
                             id="relPointsTab",  align="center", style="background:white;opacity:.96;padding-bottom:6px;border-radius:0 0 20px 20px;",
                                      fluidRow(class="hdr_darkBack", 
                                                column(10,div("Release points coordinates",class="hdr_darkText")),
                                                column(2,actionButton("ask_relPoints", "?", inline=TRUE, class="btn_Ask" ), style="float:left") 
                                      ),
                                      br(),  
                      
                                       fluidRow( 
                                                column(3,  
                                                         div(id="target" ,p("target") ,style="text-align:center;margin-top:14px;"),
                                                         div(numericInput(inputId="Nfams_init", label=NULL, value=3, min = 1,  max = 10, step = 1, width="100%")  )),
                                                column(3,
                                                       div(id="actual",p("actual"),style="text-align:center;margin-top: 14px;"),
                                                        uiOutput("Nfams_init_actual_UI")   #max-height: 50vh;
                                                       ) ,
                                                column(4, style = "margin-left:7px;",
                                                       div(shinyWidgets::radioGroupButtons(inputId ='relPts_method', label=NULL, individual=TRUE,
                                                                                           choiceNames=c("automated", "click on map", "manual" ),choiceValues=c("random_location_across","each_pt_on_map", "manual_entry"),
                                                                                           selected="random_location_across", width="100%") ,style = "margin-top:2px!important;")
                                                   ),
                                                column(1, 
                                                       actionButton(inputId = "simRandPts", label=NULL, 
                                                                     style = "padding: 0!important;width:3vh;height:3vh;border-radius:1.5vh;background:url('beaver-yellowteal2.png');background-size:cover;
                                                                    background-position:center;color:transparent;margin: 4px 0 0 clamp(-1.9vw,-1.5vw,-1vw);max-width: 30px;max-height: 30px;")
                                                       
                                                        )
                                       ),
                                       br(), 
                         hidden(tags$div(id="div_coordsTable",
                                        uiOutput("show_relPtsCoords") )),
                                        
                                      conditionalPanel("output.releasePointsOK_tolock == 1" , 
                                                          div(  shinyWidgets::prettyCheckbox(inputId = "lock_pts" ,  label = "release locations unlocked", 
                                                                                             shape = "round",  value = FALSE, fill=TRUE, 
                                                                                             icon=icon("lock")), style="margin: 18px 12px 12px 12px" ) 
                    )
                             
                    ),   
                    
                    tabPanel("groups",
                             id="beaversTab",align="center",style="background:white;border-radius:0 0 20px 20px;",
                     
                                      fluidRow(class="hdr_darkBack", 
                                               column(10, div("Demographics",class="hdr_darkText")),
                                               column(2,actionButton("ask_groupChars", "?", inline=TRUE, class="btn_Ask" ), style="float:left")  
                                               ), 
                                      br(),
                                       fluidRow(#column(4, div("Demographics",class="hdr_params", style="padding-top:18px;text-align:right;")),
                                                column(7,  shinyWidgets::radioGroupButtons(inputId = "demog",label=NULL, 
                                                                        choices = c("all adults","all families","combination"), individual=TRUE,
                                                                        selected = "all adults", width="100%", direction = "horizontal") )  ,
                                 
                              column(5,  div(textOutput("Nyg_btnpreText"), style="font-size:90%;margin:0;margin-top:7px;text-align:center;"),
                                         div(textOutput("Nyg_btnText"), style="font-size:90%;text-align:center;"),
                                         div( uiOutput("downNyg_btns"), style="display: inline-flex;"),   
                                         div( uiOutput("upNyg_btns"), style="display: inline-flex;")    
                                                )),
                             
                             br(),
                             
                          
                                      conditionalPanel("output.releasePointsOK==1" , 
                                                          #       div( uiOutput("show_relPtsFams"), style="max-width:100%;") 
                                                       div(DTOutput('famTable',  width='90%'), style="max-height: 40vh;overflow-y: auto;") 
                                                ),#condpanel
                                      br()
                                    ),  
                                       
                     tabPanel("timing",
                              id="timingTab",align="center",style="background:white;border-radius:0 0 20px 20px;",
                     
                                      fluidRow(class="hdr_darkBack", 
                                               column(10, div("Releases through time",class="hdr_darkText")),
                                               column(2, actionButton("ask_timing", "?", inline=TRUE, class="btn_Ask" ) , style="float:left")  
                                               ),
                                      br(), 
                              
                          fluidRow(
                            column(5,
                                      div("Chronology",class="hdr_params", style="text-align:center;" ) , 
                                      shinyWidgets::radioGroupButtons(inputId = "timing",label=NULL, individual=FALSE,
                                                             choiceNames = c("same year","lags" ), 
                                                             choiceValues=c("same_year", "several_years"),
                                                             selected = "same_year" , width="100%", direction = "horizontal")   
                          ), 
                            column(7,
                                     div("Initial settlement test year",class="hdr_params", style="text-align:center;" ) , 
                                     div(numericInput(inputId="startYr_trySettl", label=NULL, value=as.numeric(format(Sys.Date(),"%Y")), 
                                                                min = 2000,  max = as.numeric(format(Sys.Date(),"%Y"))+10, step = 1, width="100%"),
                                                   style="text-align:center;width:82%;display:flex;") 
                                          
                          )
                          ) ,  
 
                               
 
                                      br(),
                                      conditionalPanel("output.releasePointsOK==1" ,  
                                                             uiOutput("show_relPtsTiming")),
                                      br() 
                                   ), 
              
                     tabPanel("landscape",
                              id="metadataTab",style="background:white;text-align:center;border-radius:0 0 20px 20px;",
                                     fluidRow(class="hdr_darkBack", 
                                              column(10, div("Simulation landscape and scenarios",class="hdr_darkText")),
                                              column(2, actionButton("ask_landscape", "?", inline=TRUE, class="btn_Ask" ), style="float:left")  
                                            ),  
                 
                                    fluidRow(style="margin-top:12px;",
                                      column(9, 
                                           div("view layers", style="font-size: 90%;margin-bottom: -3px!important;"),
                                           div(class="btns-panel-metadat", style="font-size: 90%;",
                                               fluidRow(column(3, 
                                                      p(id="div_lcm","land cover", style="text-align: center;"),
                                                      materialSwitch(inputId = "landCoverMap", label = NULL, status = "danger")
                                                      ), 
                                               column(3, 
                                                      p(id ="div_NBNAtlas","NBN records", style="text-align: center;"),
                                                      materialSwitch(inputId = "NBNAtlas", label = NULL, status = "danger")) , 
                                               column(3,  
                                                     div(id ="div_customLayer", p("custom", style="margin-right:4px"),actionButton("deleteCustomLayer","↻", inline=TRUE, class="buttontab", style="height:1.4vh;min-width:1.4vh;width: 1.4vh;"), style="display: flex;text-align: center;"),
                                                      materialSwitch(inputId = "view_custom", label = NULL, status = "danger"))
                                               ) 
                                               ) ,
                                           div("view catchments", style="font-size: 90%;margin-bottom: -3px!important;margin-top:17px;"),
                                           div(class="btns-panel-metadat", style="font-size: 90%;", 
                                           fluidRow(column(3, 
                                                      p(id ="div_rivC", "river", style="text-align: center;"),
                                                      materialSwitch(inputId = "rivCatch", label =  NULL, status = "danger")),
                                                column(3, 
                                                      p(id ="div_opeC","operational", style="text-align: center;"),
                                                      materialSwitch(inputId = "opeSurf", label =  NULL, status = "danger")), 
                                                column(3, 
                                                      p(id ="div_mgmtC","management", style="text-align: center;"), 
                                                      materialSwitch(inputId = "mgmtSurf", label = NULL, status = "danger"))
                                         )
                                           )
                                      #,
                                #       column(6,
                               #            div("view catchments", style="font-size: 90%;margin-bottom: -3px!important;"),
                              #             div(class="btns-panel-metadat", 
                              #                 bsButton("rivCatch", label= div(style="text-align:center;padding:0px;magenta;","river" )  ,
                              #                  class="btn_lcm" ), 
                              #                bsButton("opeSurf", label= div(style="text-align:center;padding:color:green;","operational" )    ,
                              #                  class="btn_lcm" ),  
                              #                bsButton("mgmtSurf", label= div(style="text-align:center;padding:0px;color:orange;","management" )   ,
                              #                  class="btn_lcm" ) 
                              #                 )    
                                 #      )
                                     ), 
                              
 
                              
                              
                    #    fluidRow(style="margin-top:12px;", 
                                    column(3 , style="display: inline-grid;justify-content: space-evenly;",
                                          div("add", style="font-size: 90%;margin-bottom: -3px!important;"),
                                           div(class="btns-panel-metadat",style="flex-direction: column;width: fit-content !important;",
                                          div(actionButton(inputId ='metadata_upload',
                                                          div(style="text-align:center;padding:0px;",img(src="upload.png", height='14px', width='14px') ),                                    
                                                       class = "btn_upload",class="btn_Simulate" ) ), 
                                          div(actionButton(inputId ='metadata_draw', 
                                                       div(style="text-align:center;padding:0px;",img(src="pencil.png", height='14px', width='14px') ),
                                                       class = "btn_draw",class="btn_Simulate" ) ),
                                          div(actionButton(inputId ='metadata_select', 
                                                       div(style="text-align:center;padding:0px;",img(src="map-select.png", height='14px', width='14px') ),
                                                       class = "btn_select",class="btn_Simulate" ) ) 
                                               ), 
                                          
                                     )
                                 ),
          
                       br(),
               div(style="max-height: 43vh;    overflow-y:auto;scrollbar-width: thin; height: -webkit-fill-available;",
                       div(style="display:inline;",shinyWidgets::radioGroupButtons(inputId = "addMetadata_modifHab",label=NULL,   width="100%",
                                                                choices = c("use current landscape","modify mapped habitat"), 
                                                                selected = "use current landscape" )),
  
                                                                                      
                       conditionalPanel("output.show_metamodifHab==1" ,   style="text-align:center;",
                                                        uiOutput('metaTable_modifHab' ), 
                                                        hr()
                                       ),
                       br(), 
                       div(style="display:inline;",shinyWidgets::radioGroupButtons( inputId = "addMetadata_records",label=NULL, individual = FALSE, width="100%",
                                                                choices = c("no existing population nearby", "incorporate records" ), 
                                                                selected = "no existing population nearby" )), 
                                                             
                        conditionalPanel("output.show_metaRecords==1" ,   style="text-align:center;",
                                                        uiOutput('metaTable_records' ),  
                                                        hr()
                                       ), 
 
                      br(),
                      div(style="display:inline;",shinyWidgets::radioGroupButtons(inputId = "addMetadata_areaOfInt",label=NULL,  individual = FALSE, width="100%",
                                                                  choices = c("report overall growth", "area of interest" ), selected = "report overall growth" )),
                                             
                         conditionalPanel("output.show_metaAreaOfInt==1" ,  
                                                               style="text-align:center;",
                                                                 uiOutput('metaTable_areaOfInt' ) 
                                        ),
                      br() 
               )
      )#tabpanel  
      )#tabset
  )#abspanel
))), # condpanel
   
 
###################################################################################################################  help panel  - text
#conditionalPanel(condition = "output.countryOK == 1",
hidden(tags$div(id="div_helpPanel",
  
   absolutePanel(id = "Release_site_info",   # class="animate__animated animate__bounce animate__delay-3s",
                      class = "panel panel-default", fixed = FALSE, style = 'opacity:0.82;z-index: 9999;padding: 14px 25px;border-radius: 5vh 0vh 5vh 5vh;max-width: max(370px, 18vw);overflow-wrap:normal; text-wrap: wrap;',
                                    draggable = FALSE, top = "auto", right = "22vw", left = "auto", bottom =0, width = "auto", height = "fit-content" ,
      
       div(id = 'placeholder', style = 'background-color:black;border-color:transparent;border-radius:7px;' ),
        tags$head(tags$style("#text{padding:0;margin-bottom:7px;font-size:12px;color:#ffe62cc2;font-family:monospace;text-align:center;}"))  ,
       div( div(uiOutput("infotext_init"),style="color:darkmagenta;"),
            hidden( tags$div(id="div_help", img(src="arrow-left_teal.png", width="auto", height="auto", style="width:3vh;height:3vh;"),  class="animate__animated animate__bounceOut animate__delay-2s animate__repeat-2",
                             style="animation-fill-mode: none;" )),
           style="display: flex;align-items: center;" ),   
       
       
       
       
  HTML('<button data-toggle="collapse" data-target="#demo" style="background-color: #ddca3c;border-radius: 40px;border: 0;float: right;color: #555;font-weight: 800;">+/-</button>'),
        tags$div(id = 'demo',  class="collapse",   
       
  conditionalPanel(condition = "output.show_helpBox == 1",         
         div( uiOutput("infotext_instructions_siteSel"),style="color:darkmagenta;"), 
         div(uiOutput("infotext_def_shape",style="color:#a7ca35;")), 
         div( uiOutput("infotext_instructions_resample") ,style="color:darkmagenta;"), ##coords are define for        
         div( uiOutput("infotext_instructions_NlocsOK"),  style="color:#a7ca35"),   
         div( uiOutput("infotext_instructions_pointout") ,style="color:magenta;text-align:center;font-size:80%!important"),
         div( uiOutput("infotext_simError") ,style="color:magenta;text-align:center;font-size:80%!important"),
         div( uiOutput("infotext_instructions_habLowqual") ,style="color:magenta;text-align:center;"),  
         div( uiOutput("relSite_infotext_demog"),  style="color:darkmagenta"),
         div(uiOutput("relSite_infotext_Npts_comp"),  style="color:#a7ca35"),
         div( uiOutput("relSite_infotext_timing"),  style="color:darkmagenta"),
         div(uiOutput("relSite_infotext_timingdet"),  style="color:#a7ca35"),
         div( uiOutput("relSite_infotext_landscape"),  style="color:darkmagenta"),
         div(uiOutput("relSite_infotext_landscapedet"),  style="color:#a7ca35;margin-left: 24px;"), 
         div( uiOutput("relSite_instructions_draw") ,style="color:magenta;text-align:center;font-size:80%!important"),
         div(uiOutput("infotext_instructions_shape"),  style="color:#c59137;text-align:right;max-height: 10vh!important;overflow-y: auto;"), 
         div( uiOutput("infotext_instructions_lockSite") ,style="color:#770;text-align:center;font-size:80%!important;max-height: 10vh!important; overflow-y: auto;"),
         div( uiOutput("infotext_instructions_ptmethod") ,style="color:#c59137;max-height: 10vh!important;    overflow-y: auto;"),
         div( uiOutput("infotext_instructions_lockPts") ,style="color:#770;text-align:center;font-size:80%!important;max-height: 10vh!important;overflow-y: auto;") 
                          
  ))  
)#abspanel
 )) # condpanel
),#tabpanel
 
  

###################################################################################################################  second main panel (metadata)
 tabPanel( "input summary", style="background-color:gray20;margin:0px;height:90vh", 
     

###################################################################################################################  help panel  - y/n btns

#conditionalPanel(condition = "output.countryOK == 1",   
#      absolutePanel(id = "show_btn", 
#                       class = "panel panel-default", fixed = TRUE, style = 'background-color:transparent;border-color:transparent;margin:0;opacity:.82;margin-right:-32px!important;float:right!important;',
#                       draggable = FALSE, top = "8.5vh", right = "16vw", left = "auto", bottom = "auto", height = "4vh" , 
#                div( prettyToggle(inputId = "playSound", label_on = "sound", label_off="no sound",
#                                  icon_on=icon("volume-low" ), icon_off=icon("volume-xmark" ),  shape="round", 
#                                  plain=FALSE, bigger=TRUE,
#                                  value=TRUE )) ,          
#                    shinyWidgets::radioGroupButtons(inputId = "show_help" ,  label = NULL, 
#                                                  choices =c("show", "hide"), selected="show")
#       
#      )),                          

#################################################################################  model running panel tab #2
 conditionalPanel( condition ="output.modelRunning == 1",
      absolutePanel( id = "prog_panel_model2",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
          uiOutput("modelRunning_panel_inputTab")  
)),
 conditionalPanel( condition ="output.modelPrep == 1",
      absolutePanel( id = "prog_panel_prep2",top ="8px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
         div(textOutput("modelPreping_panel_inputTab"),style="padding: 12px 27px;")
)), 
 
  
 #textOutput("sim_in_progress_inputTab"),
 fluidRow(style="padding:130px 2vw 0 2vw;", 
         
  
 column(2,         
            fluidRow(style="text-align:left;color:turquoise;", 
               div('Display settings'),
                fluidRow( class="input_summary_row",style="border-color:turquoise;",
                       div("process alerts",class="hdr_simParams",  style="color:turquoise;"),
                       div( prettyToggle(inputId = "playSound", label_on = "sound", label_off="no sound",
                                  icon_on=icon("volume-low" ), icon_off=icon("volume-xmark" ),  shape="round", 
                                  plain=FALSE, bigger=TRUE,
                                  value=TRUE ), style="padding: 12px;") ,    
                ),
                 fluidRow( class="input_summary_row",style="border-color:turquoise;",
                       div("progress & instructions",class="hdr_simParams",  style="color:turquoise;"),

                  div(  shinyWidgets::radioGroupButtons(inputId = "show_help" ,  label = NULL, 
                                                  choices =c("show", "hide"), 
                                                  selected="show"),style="padding: 12px 0; display: inline-flex;" )
            ),
                 fluidRow( class="input_summary_row",style="display:none;border-color:turquoise;", # some code linked to those vars! undo if deleting
                       div("download alerts",class="hdr_simParams",  style="color:turquoise;"), 
                  div(  shinyWidgets::radioGroupButtons(inputId = "show_alerts" ,  label = NULL, 
                                                  choices =c("warn when output changes", "hide"), 
                                                  selected="warn when output changes"),style="padding: 12px 0; display: inline-flex;" ), 
                  div(  shinyWidgets::radioGroupButtons(inputId = "show_save" ,  label = NULL, 
                                                  choices =c("save on complete", "hide"), 
                                                  selected="save on complete"),style="padding: 12px 0; display: inline-flex;" )
            ),
              fluidRow( class="input_summary_row",style="border-color:turquoise;",
                        div("project reference name",class="hdr_simParams",  style="color:turquoise;"),
                        div( textInput("save_name", label=NULL, placeholder= "project name", value=""), style="padding:12px 0;") 
                      #  div(uiOutput("uiSave_name") ,style="padding:12px 0;")  
        
              )
          ) ),
 column(3, offset=1,
          fluidRow(style="color:yellow;", 
                               
                   div(id="siteRow",  
                       div('Layout' ),  
                               fluidRow(class="input_summary_row",style="text-align:center;border-color:yellow;",
                                      div("exploratory site",class="hdr_simParams", style="color:yellow;text-align:left!important;"), 
                                      div(uiOutput("relSite_infotext_loc"), style="color:beige;margin-left:17px;"))) ,  
                             
                             absolutePanel( id ="input_summary_transloc", style ="width: -webkit-fill-available;",
                                         fluidRow( class="input_summary_row",style="border-color:yellow;",
                                                 div(textOutput("operation_ttl"),class="hdr_simParams", style="color:yellow;text-align:left!important;") , 
                                                 tags$div(id ="inputSum1",textOutput("relSite_infotext_Npts_summary"), style="color:beige;margin-left:17px;")   , 
                                                 tags$div(id ="inputSum2",textOutput("relSite_infotext_demog_summary"), style="color:beige;margin-left:17px;")   , 
                                                 tags$div(id ="inputSum3",textOutput("relSite_infotext_timing_summary"), style="color:beige;margin-left:17px;padding-bottom:10px;")  ,
                                                 hidden(tags$div(id ="inputSum4",p("only existing beaver populations are included in the simulation"), style="color:beige;margin-left:17px;")))
                              )
              )
              ),
           
           
 column(3,          
     
    ## metadata
             fluidRow(style="text-align:left;color:orange;", 
                               div('Metadata'),    
                               
                               fluidRow( class="input_summary_row",style="border-color:orange;",
                                      div(textOutput("metaTitle_modifHab"),class="hdr_simParams", style="color:orange;") , 
                                      div(uiOutput( "metaSummary_modifHab" ),style="text-align:left;color:beige;margin-left:17px"),
                                      div(uiOutput( "metaSummary_modifHab2" ), syle="color:orange!important;width: 75%;text-align: center;margin-left:17px;") 
                              ),      
                               
                               fluidRow( class="input_summary_row",style="border-color:orange;",
                                      div(textOutput("metaTitle_records"),class="hdr_simParams", style="color:orange;") , 
                                      div(uiOutput( "metaSummary_records" ),style="text-align:left;color:beige;margin-left:17px")
                              ), 
                               fluidRow( class="input_summary_row",style="border-color:orange;",
                                      div(textOutput("metaTitle_areaOfInt"),class="hdr_simParams", style="color:orange;")   ,  
                                      div(uiOutput( "metaSummary_areaOfInt" ),style="text-align:left;color:beige;margin-left:17px") 
                              )
                      )
    ),    
             
    
       ## simulation params
    column(3,
           fluidRow(style="text-align:left;color:#d8e470",  
                    div('Population growth simulation' ,style="color:#d8e470;display:inline;" ), 
                                 fluidRow(  class="input_summary_row",style="border-color:#d8e470",
                                               div(textOutput("sim_assumption_txtStartYr"),class="hdr_simParams"  ,style="text-align:left;float:left;color:#d8e470;"), 
                                               div(textOutput("simStartYear_txt") ,style="display:inline-flex;padding:7px 10px 12px 12px;") 
                                              ), 
     
                    
                                    fluidRow(  class="input_summary_row",style="border-color:#d8e470",
                                               div(textOutput("sim_assumption_txtNyr"),class="hdr_simParams"  ,style="display:inline-block;text-align:left;float: left;color:#d8e470;"), 
                                           div(shinyWidgets::radioGroupButtons(inputId = "sim_Nyrs" ,  label = NULL, individual = TRUE,
                                                           choiceNames =c("1 years","3 years","5 years","10 years"), choiceValues =c(1,3,5,10),
                                                           selected=1),style="display:inline-block;margin-right:72px;float:right;margin-top:1vh;")
                              
                                              ),
                                    fluidRow( class="input_summary_row",style="border-color:#d8e470",
                                              div(textOutput("sim_assumption_txtNrep"),class="hdr_simParams"  ,style="display:inline-block;text-align:left;float: left;color:#d8e470;"), 
                                           div(shinyWidgets::radioGroupButtons(inputId = "sim_Nreps" ,  label = NULL, individual = TRUE,
                                                           choiceNames =c("1","5","15","30" ), choiceValues =c(1,5,15,30 ), 
                                                           selected=1),style="display:inline-block;margin-right:22px;float:right;margin-top:1vh;")
                                               )   
                    )  
))
),
  

################################################################################################################### third main panel (output mapped)
 tabPanel( "simulation output", 
        #################################################################################  model running panel tab #3
 conditionalPanel( condition ="output.modelRunning == 1",
      absolutePanel( id = "prog_panel_model3",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
          uiOutput("modelRunning_panel_outputTab")  
)),
 conditionalPanel( condition ="output.modelPrep == 1",
      absolutePanel( id = "prog_panel_prep3",top ="8px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
        div( textOutput("modelPreping_panel_outputTab"),style="padding: 12px 27px;")
)), 
    
    
                                               
                                                
                                                
             
 fluidRow(style="position: absolute; top: 8vh; left: 3vw; right: 3vw;", 
 column(2,
          div(id="siteRow_out",
          fluidRow(style="text-align:left;color:yellow;", 
                   div('Layout',style="display:inline;" ), 
                   div(downloadButton("save_layout", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;"),
                   fluidRow( class="input_summary_row",style="width: fit-content;min-height:4vh;text-wrap: wrap;padding: 12px 6px",
                                div(uiOutput("dlText_layout"), class="hdr_simParams", style="color:yellow;"), 
                                      )                
                   )),
     ## metadata
             fluidRow(style="text-align:left;color:orange;", 
                      div('Metadata',style="display:inline;" ), 
                   div(downloadButton("save_metadata", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;"),
                  
                   fluidRow( class="input_summary_row",style="border-color:orange;width: fit-content;min-height:4vh;text-wrap: wrap;padding: 12px 6px",
                            div(uiOutput("dlText_metadata"), class="hdr_simParams", style="color:orange;")
      )),     
                               
       ## init settlement params       
   conditionalPanel("output.but_sTestOutput == 1" ,
           uiOutput("ui_save_settleTest") 
   )
     ),  
       ## simulation params
    column(10, 
            
           fluidRow(style="text-align:left;left:2vw;top: 8vh; position: relative;", 
                       div( uiOutput("popGrowth_title")    ,style="display:inline-flex;")  , 
                   div(downloadButton("save_simpop", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;padding-left: 12px;"),
 

                        div(actionButton("ask_resOutput", "?", inline=TRUE, class="btn_Ask" ), style="display:inline-flex;float:none;margin-left:4px;   ")  , 

                       fluidRow( class="input_summary_row",style="border-color:#d8e470;margin-right:2vw ;min-height: 7vh;border-radius: 0 7vh 6vh 8vh!important;",
                                  div(uiOutput("outputReadYN_txt"),
                                      style="position:absolute;padding:1vh 0 0 6vh;left: 25vw;"),
     
 
 
 
 
 
conditionalPanel( condition ="output.simOutput == 1",
absolutePanel(id = "simOutput_panel",
                  class = "panel panel-default", fixed = TRUE, 
                 style = 'position: relative;background-color: transparent;border-color: transparent;',
                  draggable = FALSE, top =0, left =0, right = 0,bottom= 0,
              
               
   
     
              
     fluidRow(style="display:block;padding-top: 2vh", #max-height:80vh!important
             
           ########TABLES   
              column(7, 
              div(style="overflow:hidden;border-radius:30px;overflow-y: clip; height: fit-content;",# 76vh;max-height: 800px;",      
              div(style="overflow-x:auto;display: grid;overflow-y: clip;scrollbar-width: thin;",  
                   tags$div(id="nonNULLheaders_Output",uiOutput("uirecapTableOut_headers"), style="background:transparent!important;"), 
                   
                    div(p("OCCUPANCY: BEAVER TERRITORIES", style="margin: 0"), style="margin: 37px 0 0 0;color: #e7d236;line-height:.2em;"),
  
           hidden( tags$div(id="noTerrs_inOutput",
                       p("no simulated territory in output!" , style="padding: 2vh;") )), 
                    tags$div(id="nonNULLTerrs_inOutput",  DTOutput("recapTableOut_fams" , height="31em") ,  #height = "auto",  
                         style ="line-height: 90%; font-size: 90%; color:#555555cf!important; 
                        border-radius:30px;text-wrap: nowrap;width: fit-content;position:relative;") 
                    , 
    
             absolutePanel(id ="abundPanel",  style="text-align:center;position:relative;top:3em;", #bottom: 355px
                            div(p("ABUNDANCE: INDIVIDUAL BEAVERS")
                                                    , style="color: #e7d236;height:30px;")
), 
 
                    
               hidden( tags$div(id="noAdlt_inOutput",
                       p("no simulated abundance in output!" , style="padding: 5vh;") )),    
              tags$div(id="nonNULLAdlt_inOutput",style="border-radius:12px;top:1em;height: 32em;",  
                  DTOutput("recapTableOut_N", height="31em"),
                        style ="line-height: 90%; font-size: 90%; color:#555555cf!important; 
                        border-radius:30px;text-wrap: nowrap;width: fit-content;position:relative;" ) 
             ))),      
                    
             ########PLOTS       
                     
      
 column(5,  style="height:-webkit-fill-available;",# height:76vh;max-height:800px;  
        
  hidden( tags$div(id="noPlotOutput",
                       img(src="beav-nulltoo.png", width="auto", height="auto",id="beavnull", class="animate__animated animate__hinge animate__delay-1s",
                               style="width:60px;margin:2vh"),
                       img(src="beav-null.png", width="auto", height="auto",id="beavnull2", class="animate__animated animate__hinge animate__delay-2s",
                               style="width:50px;padding-top: 13vh;"),
                       img(src="beav-nullhey.png", width="auto", height="auto",id="beavnull3", 
                               style="width:40px;") )),
 
                  
                          
              tags$div(id="nonNULL_plotOutput",
                       style="display:grid;display:flex;flex-direction: column;align-items: stretch;justify-content: space-between;padding-top:2em;",      
                        
              div(radioGroupButtons(inputId = "show_outputType", label=NULL, choices= c("each repetition", "summary values") , 
                                             selected="each repetition", individual = FALSE ),
                           style="text-align:center;padding-bottom:5em;"),
                          
          conditionalPanel( condition ="output.showEachRep == 1",
              absolutePanel(id = "showEachRep_fams", class = "panel panel-default", fixed = TRUE, 
                  style = 'position: relative;background-color: transparent;border-color: transparent;height:24em;',
                  draggable = FALSE, top =0, left =0, right = 0,bottom= 0,  
             # match ttle
             div(
             class = "NTimeLinePlots_fams",
             div( plotOutput("NTimeLinePlot_fams", height="25em" )   ),
             div( plotOutput("NTimeLinePlot_fams_hilights" ,  height="25em")  ))
             ),
    tags$style(
        "
        .NTimeLinePlots_fams {
            position: relative;  
        }
        #NTimeLinePlot_fams {
            position: absolute;border-radius: 12px;background-color:beige;   
        }        
        #NTimeLinePlot_fams_hilights {
            position: relative;border-radius: 12px;background-color: transparent;  
        }
        ")
  ) ,
            conditionalPanel( condition ="output.showEachRep == 0",
              absolutePanel(id = "showSumm_fams", class = "panel panel-default", fixed = TRUE, 
                  style = 'position: relative;background-color: transparent;border-color: transparent; height:24em;',
                  draggable = FALSE, top =0, left =0, right = 0,bottom= 0,
                          
             div(
             class = "NSummTimeLinePlots_fams",
             div( plotOutput("NSummTimeLinePlot_fams", height="25em") ),
             div( plotOutput("NSummTimeLinePlot_fams_hilights", height="25em"))
             ),
    tags$style(
        "
        .NSummTimeLinePlots_fams {
            position: relative;   
        }
        #NSummTimeLinePlot_fams {
            position: absolute;border-radius: 12px;background-color:beige;  
        }        
        #NSummTimeLinePlot_fams_hilights {
            position: relative;border-radius: 12px;background-color: transparent;  
        }
        ")
 )),
     
     
      
          # one panels with 2 so tooltip is at top of plot
                       
     
      absolutePanel(id = "show_N",style="position:relative;max-height:300px;height:30vh;",
        
          conditionalPanel( condition ="output.showEachRep == 1",
              absolutePanel(id = "showEachRep_N", class = "panel panel-default", fixed = TRUE, 
                style = 'position:relative;background-color: transparent;border-color: transparent; padding: 0;', 
                  draggable = FALSE, top ="5em", left =0, right =0,bottom= 0,
                     
             
                div(
             class = "NTimeLinePlots_N",
             div( plotOutput("NTimeLinePlot_N", height="25em" ) ),
             div( plotOutput("NTimeLinePlot_N_hilights", height="25em" ))
             ),
    tags$style(
        "
        .NTimeLinePlots_N {
            position: relative;  
        }
        #NTimeLinePlot_N {
            position: absolute;border-radius: 12px;background-color:beige; 
        }        
        #NTimeLinePlot_N_hilights {
            position: relative;border-radius: 12px;background-color: transparent;  
        }
        "
    ) )),
 
  
            conditionalPanel( condition ="output.showEachRep == 0",
              absolutePanel(id = "showSumm_N", class = "panel panel-default", fixed = TRUE,
                style = 'position:relative;background-color: transparent;border-color: transparent; padding: 0; ', 
                  draggable = FALSE, top ="5em", left =0, right = 0,bottom= 0,
                     
             
                div(
             class = "NSummTimeLinePlots_N",
             div( plotOutput("NSummTimeLinePlot_N", height="25em" ) ),
             div( plotOutput("NSummTimeLinePlot_N_hilights", height="25em" ))
             ),
    tags$style(
        "
        .NSummTimeLinePlots_N {
            position: relative;  
        }
        #NSummTimeLinePlot_N {
            position: absolute;border-radius: 12px;background-color:beige; 
        }        
        #NSummTimeLinePlot_N_hilights {
            position: relative;border-radius: 12px;background-color: transparent; 
        }
        "
    )),  
    br()
    )
))
)         
 ) #panel
            # row
 )) 
      
  ##
                                                    
                                   )
              
              
    ) ,
          conditionalPanel( condition ="output.metadataTline == 1",            
          fluidRow(style="text-align:left;color:#FB8861FF;top: 8vh; position: relative;left: 2vw;", 
                   div('Added metadata timeline',style="display:inline;" ),
                    fluidRow(class="input_summary_row",style="min-height:4vh;text-wrap: wrap;padding: 12px 6px; border-color: #FB8861FF;border-radius:0 7vh 20px 46px!important;",
                      column(7,style="vertical-align:middle;",
                           div(style="display: flex; align-items: center;",
                                 div(   textOutput("effTimeLineYrTxt") ,style="margin-left:8vw;float: inline-start;") , 
                                 div(   uiOutput("effTimeLineTxt") ,style="display: inline-block;text-align:left;margin: 8px 47px;"))
                         ),
                      column(5,  
                            div(  class = "effTimeLinePlot",
            div( plotOutput("effTimeLinePlot", height="auto"  ) ),
            div( plotOutput("effTimeLinePlot_hilights" , height="auto" ))
             ),
    tags$style(
        "
        .effTimeLinePlot {
            position: relative;
        }
        #effTimeLinePlot {
            position: absolute;border-radius: 12px;background-color:beige; 
        }        
        #effTimeLinePlot_hilights {
            position: relative;border-radius: 12px;background-color: transparent; 
        }
        ")  )  
))) ,
           conditionalPanel( condition ="output.areaOfInterestTline == 1",            
          fluidRow(style="text-align:left;color:#b6b612;top: 8vh; position: relative;left: 2vw;margin-left:-15vw;",  
                   div('Area of interest Observations',style="display:inline;" ),
                   div(actionButton("proximTo", label="compute", inline=TRUE, class="btn_aOfInt"), 
                       style="display:inline-flex;margin-left:32px;"),
                     fluidRow(class="input_summary_row",style="min-height:4vh;text-wrap: wrap;padding: 12px 6px;border-radius: 0 7vh 15vh 42px;border-color: #b2b212;",
                     
   conditionalPanel( condition ="output.recapProximToNonNuLL==1 ",
                          fluidRow(  column(width=7,
                         div(  DTOutput("recapProximTo", height="100%") ,
                        style ="overflow:auto;top:1em;height:fit-content;max-height:100vh;line-height:90%;font-size:90%;color:#555555cf!important;
                        text-wrap:wrap;width:fit-content;position: relative;padding-bottom: 7px;visibility: visible;border-radius:30px;" )   
                                 )  ))
                 )
                   )
) 
 
 )  
)

   
           
),
 

###################################################################################################################  fourth main panel (dl)
ui_aboutTab   # all in ui_aboutTabText.R
 

#shiny::includeScript("script_current_id".js")  
#   tags$script(HTML( "Shiny.addCustomMessageHandler(  'removeleaflet',  function(x){
#     console.log('deleting',x)
#     // get leaflet map
#     var map = HTMLWidgets.find('#' + x.elid).getMap();
#     // remove
#     map.removeLayer(map._layers[x.layerid])
#  })")), # to remove layers drawn - removeDToolbar doesnt work in deployed (known bug)
)
#) # panels div group
 
#,  ui_tab2Tooltips ,
,bsTooltip( id ="save_name", "name the project to attribute its ID to downloaded data",placement = "right", trigger = "hover")
))
)
 

   
 
####  
#### SERVER 
####  
 server   <- function(input, output, session) {
 #shinyjs::addClass(id = "navBar", class = "navbar-right")   
col_succ <- "#a7ca35d9"
col_info <- "#2299aacc"
col_fail <- "#fb8861d4"
col_warn <- "#e9d852de"   
txtCol_notif <- "darkmagenta"
txtCol_notif2 <- "yellow"##dcc93b

launchTab2 <- reactiveVal(FALSE) 
mapExists <- reactiveVal(FALSE)  
 
    
loadLocal_scripts <- paste0(here::here("load_local"),"/",list.files(here::here("load_local")) )
for(scr in loadLocal_scripts){   source(scr, local=TRUE) }
                                   #       source(here::here("serv_initVals.R"), local=TRUE)  
                                  #        source(here::here("condPanelsValues.R" ), local=TRUE) 
                                  #        source(here::here("moduleFilter.R"), local=TRUE) 
      



country_boundaries <- reactive({ 
   req(rv_mapctry$bnd_cntry$country) 
   req(!is.null(rv_mapctry$bnd_cntry )) 
#   print(paste0("retrieve country_boundaries for ",rv_mapctry$bnd_cntry$country, "  - "))
  return( load_cntryBdry_4326(rv_mapctry$bnd_cntry$country))
})


country_indx <- reactive({ 
  req( rv_mapctry$bnd_cntry )  
  return(which(country_names == rv_mapctry$bnd_cntry$country) ) 
}) 
  
 
country <- reactive({
  NULL
  req(!is.null(rv_mapctry$bnd_cntry))
  return(rv_mapctry$bnd_cntry$country)
})
    
      
   
#runjs('document.getElementById("lock_site-label").setAttribute("aria-labelledby", "lock_site-value")')     
 
     
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))   
 bnd_UKsession <- bnd_UK
# bnd_UKsession$country <- as.factor(bnd_UKsession$country)
##################################### map UK  ####  
 output$mapUK_init <- renderLeaflet({
    #bnd_UKsession$country <- as.factor(bnd_UKsession$country)
  req(bnd_UKsession)
   leaflet(options = leafletOptions(zoomControl = FALSE ,dragging = FALSE  )) %>% 
      
     addTiles(
    urlTemplate = "https://tiles.stadiamaps.com/tiles/{variant}/{z}/{x}/{y}.jpg?api_key={apikey}",
    attribution = paste('&copy; <a href="https://stadiamaps.com/" target="_blank">Stadia Maps</a> ' ,
                            '&copy; <a href="https://stamen.com/" target="_blank">Stamen Design</a> ' ,
                            '&copy; <a href="https://openmaptiles.org/" target="_blank">OpenMapTiles</a> ' ,
                            '&copy; <a href="https://www.openstreetmap.org/copyright" target="_blank">OpenStreetMap</a>'),
    options = tileOptions(variant='stamen_watercolor', apikey = '81b8a26e-a608-4799-b0db-cab582a5c6a1')
    ) %>%
     
      
     
     
     
   #        addProviderTiles(provider = "Stadia.StamenWatercolor" ,
  #                          options = providerTileOptions( maxZoom = 7, minZoom=6  , 
  #                       attribution = as.character(c(" Stadia Maps | https://stadiamaps.com,   OpenStreetMap | www.openstreetmap.org/copyright"))
   #                                          )) %>%
                            setView( -11.8 , 54.9 , zoom=6) %>%
           addPolygons(data=bnd_UKsession , fillColor ="transparent" ,  fillOpacity = 0.3, weight = 2,   color="transparent", opacity =1,
                              highlight = highlightOptions( fillColor ="yellow" ,color ="black",   bringToFront = FALSE ,sendToBack=TRUE ),
                                              label =~paste0("set-up a simulation in ", country ) , group="bnd_UK",
                                              labelOptions = labelOptions(direction = c("left"),offset=c(-50,0),
                                                              style = as.list(c("color" =  "yellow" ,labstyle_plotLabs_init))  )) 
 
  
                    
 })  
 
hideUKmap <- reactiveVal(FALSE)

 
# output$mapUK_init_UI <- renderUI({   
 #    if(! is.null(mapReleaseSite())  )  {return(NULL)}
  #   return(div(leaflet::leafletOutput("mapUK_init" , height="98vh", width="99vw" ), style="border-radius: 0 51px;overflow: hidden;", class="animate__animated animate__fadeIn"  )   ) #, style="height:97vh;width:97vw;"
  #}) 
  

launchPage_ui <- reactiveVal(launchPage_ui_txt) 
output$launchPage_ui <-renderUI({  launchPage_ui()  })   
 

passwordProtect <- FALSE   
startApp <- reactiveVal(FALSE)
input_mapUK_init_shape_click <- reactiveVal(NULL)

 
observe ({ #mapUK_init_shape_click,{   
 req(mapExists()==FALSE)
  req(!is.null(input$mapUK_init_shape_click))#undefined_shape_click))
  cat("click")
   if(passwordProtect==FALSE){ 
     startApp(TRUE)
     input_mapUK_init_shape_click( input$mapUK_init_shape_click)
     }
   if(passwordProtect==TRUE){
           showModal(
                     modalDialog( size =  "s",   footer=NULL,   easyClose = TRUE,  position =  "centered",
                     div(style="display:inline",
                         div(img(src="beaver-facing-right.png", width="37px", height="19px"),id="beavpw", class="animate__animated animate__headshake animate__delay-1s"),
                           textInput('password', NULL, placeholder=  "enter password to launch the app" ) ) 
         ))
   }
}) 


observeEvent(input$password,{
  cat("check pw?")
  req(passwordProtect==TRUE)
  cat("aye - ")
  if(input$password == "bb") {
    startApp(TRUE)
    input_mapUK_init_shape_click( input$mapUK_init_shape_click)
    removeModal()}
})


observeEvent(startApp(),{
    if( !is.null(input_mapUK_init_shape_click())) {
     shinyjs::hide(selector ="#show_launchPage", anim=TRUE, animType="fade",time=2) # first fade then erase
     cat("\n--HIDE LAUNCH PAGE* ")
      } 
 },ignoreInit=TRUE)
 

#### on click over country (mapUK) ####
 observeEvent(input_mapUK_init_shape_click(),{ 
cat("country click - ")
   click <- input_mapUK_init_shape_click() 
  click_pt <- st_sfc(st_point(c( click$lng, click$lat) ), crs = st_crs(4326)) 
#  st_agr(bnd_UKsession)<- "constant"
#  click_pt <- click_pt%>%st_as_sf()
#  st_agr(click_pt)  <- "constant"
  if( any(lengths(st_intersects(bnd_UKsession, click_pt )%>%suppressMessages()) >0) ){ 
     rv_mapctry$bnd_cntry <-  bnd_UKsession[ click_pt,] %>%suppressMessages()
   cat(paste0("selected: ",rv_mapctry$bnd_cntry$country, "  - "))
    }
}, ignoreNULL=TRUE)
        
  

  


  
 # leaflet icons
beaverIcon_init <-  beaverIcon  
beaverIconSmall_init <-  beaverIconSmall 
timeIcon_init <-  timeIcon 
   
 


# info text random pts  sampling
infotext_instructions_habLowqual<- reactiveVal(NULL)
warning_noSuitHab <- reactiveVal(0)  



disable("undo_settling")
disable("undo_sim")

 

 
exploSite_catchments <- reactiveVal(NULL)       
exploSite <- reactiveVal(NULL)                       
 


habtif3857_path  <- reactiveVal(NULL) 
intercatch <- reactiveVal(NULL)
intercatch_merc <- reactiveVal(NULL)
suitablePatches  <- reactiveVal(NULL)
hab_3857 <- reactiveVal(NULL)  
hab_3857w  <- reactiveVal(NULL) 
 

progMsges_start <- reactiveVal() 
local_suitras <- reactiveVal()     
local_habw <- reactiveVal(NULL)       
local_hab  <- reactiveVal(NULL)     
try_settling <- reactiveVal(NULL)
  
 
## sim output storing vars 
palette_Nruns <- reactiveVal()
palette_dens  <- reactiveVal()
fam.start <- reactiveVal()
ter.start <- reactiveVal()
sim_out <- reactiveValues() 
## reset trigger values post sim output for next sim
reset_simTriggers <- reactiveVal(0) 


## counter change in terrs   
 terrs_labs <- reactiveVal(NULL)
  
   


## highlighting params for transloc data tables
ptdf <- reactiveVal()
dfrows_selected <- reactiveVal(NULL)
df_selected <- reactiveVal(NULL)
markerClick_id_hilight <- reactiveVal(NULL) # hilight grp


## bckgrnd color for selected rows in transloc data tables
dt_rowShowCol <- "#ffe62cc2"
dt_rowNoshowCol <- "transparent"   
dt_row  <- reactiveVal() 
dt_backgroundColours_0 <- reactive({
            NULL
            req(relPoints_coordsBNG_display())
            rep(0,nrow(relPoints_coordsBNG_display())) })

dt_backgroundColours <- reactive({
           NULL
           req(dt_backgroundColours_0())
                   dt_back <- dt_backgroundColours_0()  
                   dt_back[dfrows_selected()] <- 1  
            return(dt_back)
}) 
    

showEachRep <- reactive({as.numeric(input$show_outputType == "each repetition")})
metadataTline <- reactive({as.numeric(!is.null(ganttdf()) ) })                
areaOfInterestTline <- reactive({as.numeric(  input$addMetadata_areaOfInt == "area of interest" & !is.null(datSave_sim_pop()) ) }) 
  

##  interactive dis/enabling of transloc params  
disable("Nsites") 
disable("down")
disable("up") 
disable("demog")  
## can change vals from table or buttons - this trigger makes difference between user and script updated value to not re-trigger update..
rerun_demog <- reactiveVal(TRUE)     

disable("save_settleTest")  
disable("save_layout")
disable("save_simpop") 
disable("save_metadata")

##  dis/enabling of download buttons provided output type exists  
observe({
      if( any(!is.null(initValues()) , !is.null(exploSite()))) { 
        enable("save_layout") 
         txt <- "site " 
         if( !is.null(initValues())) {txt <- paste0(txt,"+ released groups ")}
         txt <- paste0(txt,  "data")
         HTML(as.character(  p(as.character(txt))  ))  %>% dlText_layout() 
    #     paste0("_",format(Sys.time(), "%d%b%y"),"_",format(Sys.time(), "%H%M")) %>% saveName_placeholder()
        } else { 
         p("no data")  %>% dlText_layout()
        disable("save_layout")  }
})
 
observe({ 
  newFeaturesDat_shp()
      if( !is.null(newFeaturesDat_shp() ) ) {
        newFeaturesDat_shp <- newFeaturesDat_shp()
        txt <- NULL
          if(nrow(newFeaturesDat_shp )>0) {
          txt <-  paste0(nrow(newFeaturesDat_shp ), " spatial features available to download" )
          txt <- as.character(txt,br(),  "dl as set?")
     #     txt <- as.character(txt,br(),nrow(newFeaturesDat_shp ), " features")
      #  HTML(as.character(tagList( p(as.character(txt)) , br(),  p("(1x .shp)") ))) %>% dlText_metadata()  
        p(as.character(txt)) %>% dlText_metadata() 
        enable("save_metadata")
         paste0(format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")) %>% saveName_placeholder()
          } else {   
         p("no metadata included")  %>% dlText_metadata()
       disable("save_metadata")        
        } } else {   
         p("no metadata included")  %>% dlText_metadata()
           disable("save_metadata")  
          }  
        })  
  


observe({
  datSave_settleTest()
 if( !is.null(datSave_settleTest() )) {
         enable("save_settleTest") 
        } else { 
          p("no output") %>% dlText_settleTest()
          disable("save_settleTest") 
          }
})  
 

 
 


observeEvent(datSave_sim_pop(),{
      if( !is.null(datSave_sim_pop())) {
        enable("save_simpop")
        paste0(format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")) %>% saveName_placeholder()
        } else {    disable("save_simpop")  } 
},ignoreNULL=FALSE, ignoreInit=TRUE)

  

 
dlText_metadata <- reactiveVal()
dlText_layout <- reactiveVal()
dlText_settleTest <- reactiveVal() 
######################## downloadables text
 
output$dlText_layout <- renderUI({
   div( dlText_layout(),style="color:beige;padding: min(7px, 0.9vh);line-height: min(17px, 3vh);") 
})
 
output$dlText_metadata <- renderUI({
   div( dlText_metadata(),style="color:beige;padding: min(7px, 0.9vh);line-height: min(17px, 3vh);") 
})  
output$dlText_settleTest<- renderUI({
   div( dlText_settleTest(),style="color:beige;padding: min(7px, 0.9vh);line-height: min(17px, 3vh);") 
})   
 
 observe({ if(is.null(initValues()) )  { disable(id="demog") } else { enable(id="demog") } })
 
## lock site and pts when simulaton triggered
observe({
  req(is.null(newdrawnShapetemp())) 
  if(any(base::isTRUE(model_running()), modelPrep()==1) & operation() == "translocation") {
      updatePrettyCheckbox(session, "lock_pts",  label="release locations locked", value=TRUE  )
      updatePrettyCheckbox(session, "lock_site",  label="exploratory site locked", value=TRUE ) 
    }
})
 

show_help <- reactive({as.numeric(input$show_help!="hide")}) 

observeEvent(input$demog,{
  if(input$demog == "all adults") { 
    disable(id="down")
    disable(id="up") 
    } else { 
    enable(id=input$down)
    enable(id="up")}
}, ignoreNULL=FALSE)



 

superDispcells <- reactiveVal(NULL)
init_terrsPolys <- reactiveVal()  
    
## progress ui text 
notSettling_pars<- reactiveVal(NULL)
notSettling <- reactiveVal(NULL) 
startGrowing <- reactiveVal(NULL)
startGrowing_pars <- reactiveVal(NULL)
cellId_rast <- reactiveVal()
model_running <- reactiveVal(FALSE)  
sim_in_progress <- reactiveVal(NULL)
sim_in_progress2 <- reactiveVal(NULL) 
sim_in_progress_inputTab <- reactiveVal(NULL)
sim_in_progress2_inputTab <- reactiveVal(NULL)
sim_in_progress_outputTab <- reactiveVal(NULL)
sim_in_progress2_outputTab <- reactiveVal(NULL)
sim_in_progress_aboutTab <- reactiveVal(NULL)
sim_in_progress2_aboutTab <- reactiveVal(NULL)

output$sim_in_progress <- renderText({ paste0(sim_in_progress(), " in progress")})
output$sim_in_progress2 <- renderText({ paste0(sim_in_progress2() )})

output$sim_in_progress_inputTab <- renderText({ paste0(sim_in_progress_inputTab(), " in progress")})
output$sim_in_progress2_inputTab <- renderText({ paste0(sim_in_progress2_inputTab() )})

output$sim_in_progress_outputTab <- renderText({ paste0(sim_in_progress_outputTab(), " in progress")})
output$sim_in_progress2_outputTab <- renderText({ paste0(sim_in_progress2_outputTab() )}) 
 
output$sim_in_progress_aboutTab <- renderText({ paste0(sim_in_progress_aboutTab(), " in progress")})
output$sim_in_progress2_aboutTab <- renderText({ paste0(sim_in_progress2_aboutTab() )})
 


## record perf times for speed report
simTime <- reactiveVal(NULL)   # to record perf times
#perfTable <- read.csv( paste0(here::here("testing/perfTable.csv")))
#perfTable <- data.frame(date =as.character(Sys.Date()), nreps=NA, nyrs=NA, duration=NA) 
#write.csv(perfTable, paste0(here::here("testing/perfTable.csv")), row.names=FALSE)
 

# reactives to store latest outputs - s can run new sim and download visualised output at same time
out_simpop     <- reactiveVal(NULL)
out_settleTest <- reactiveVal(NULL)
out_metadata   <- reactiveVal(NULL)
out_layout  <- reactiveVal(NULL)
  
  
## obsRecs points - user input when clicking on metatable for obsRecs
obsRecs_famDf  <- reactiveVal(NULL)           # user-informed or default vals for demographics and timing (modal dial)
obsRecs_famDf_display  <- reactiveVal(NULL)   # syncs display and saved data to allow edits via modal table


 

relPoints_coordsBNG_display <- reactiveVal(NULL) 



## matadata input - show/hide
#show_metamodifHab <- reactiveVal(FALSE)
#show_metaRecords <- reactiveVal(FALSE)
#show_metaAreaOfInt <- reactiveVal(FALSE) 

#init_superCellsPalN <- reactiveVal() 
#show_addMetadata<- reactiveVal(0)

## n features per type, for summary text
Nfeat_modifHab <- reactiveVal(0)
Nfeat_records <- reactiveVal(0)
Nfeat_areaOfInt <- reactiveVal(0) 
#metadata_layer_type <- reactiveVal(NULL)


## params to show/hide sim output/progress in top navbar
#simModelOut <- reactiveVal(0) # slider panel
#modelOut    <- reactiveVal(0) 
#modelPrep   <- reactiveVal(0)
#modelRunning<- reactiveVal(0)
#simOutput   <- reactiveVal(0) # simulation otput tab - panel with output plots/tables
#sTestOutput <- reactiveVal(0)
#observe({
#   req(mapExists()==TRUE)
#   print("--------------------------------- output display params ------------------------------------------")
#   
#   print(paste0("modelPrep():", modelPrep()))   
#   print(paste0("model_running(): ",model_running()))
#   print(paste0("model_running_UItxt(): ",model_running_UItxt()) ) 
#   print(paste0("modelRunning(): ",modelRunning()))
#   print(paste0("modelOut(): ",modelOut()))
#   print(paste0("simModelOut():", simModelOut()))
#   print(paste0("model_running_UItxtout():",model_running_UItxtout()))
#   print("--------------------------------------------------------------------------------------------------")
# }) 

 
 
observeEvent(country_indx(), { 
  req(country_indx())
  shinyjs::show(selector = "#pretab2_ui")
  country_indx <- country_indx()
  country <-  country_names[country_indx]
  rv_mapctry$country_infotext <-  country
  rv_init$infotext_init <-   paste0("The simulation will be located in ",span(style="font-weight:bold;", country),".",br(), 
                              "Select an operation to start building a scenario for beaver population growth.",br(),
                              "Refer to ", span(class='aboutBtnTxt','about',style='color:orange;border-color:orange;background-color:#000000d1!important;'),'>',
 span(class='aboutBtnTxt',"General",style="color:#ffe62cc2;background-color:#000000d1!important;"), " for step-by-step instructions.", br(),
 
 "Click +/- on this panel to show progress with the set-up."  
 
 ) 
}, ignoreNULL=TRUE, ignoreInit=FALSE)
   
  

####################################### load country-specific data in session but save global first time
observeEvent(country_indx(), { 
  req(country_indx())
  country_indx <- country_indx()
  country <- c("Scotland","Wales","England") [country_indx]
   cat(paste0("\nload ",country," data: "))
           here::here('data',country,'habitat3857.tif')  %>% habtif3857_path()
          load_intercatch_4326(country) %>% intercatch()
          cat("intercatch_4326    - ")  
            load_intercatch_27700(country) %>% intercatch_merc()
       #    future_promise({load_intercatch_27700(country)}, seed=NULL) %...>% intercatch_merc()
 
       #   load_intercatch_27700(country) %>% intercatch_merc()
          cat("intercatch_27700  - ")  
       #   load_suitPerCatch_4326(country) %>% suitablePatches()
            #      future_promise({load_suitPerCatch_4326(country)}, seed=NULL) %...>% suitablePatches()
load_suitPerCatch_4326(country) %>% suitablePatches()
             cat("suitablePatches_4326  - ")  
     #     load_habitat_3857(country)  %>% hab_3857() 
                  #        future_promise({ load_habitat_3857(country) }, seed=NULL) %...>% hab_3857w()
 load_habitat_3857(country) %>% hab_3857w()
          cat("hab raster   - ")   
          launchTab2(TRUE) 
          shinyjs::show(selector ="#div_about") 
          notify_info("click on the 'about' tab for more instructions" ,timeout=12000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="fade",pauseOnHover = TRUE ,clickToClose=TRUE, 
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl",showOnlyTheLastOne=FALSE,     
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
         
          cat("\n--SHOW TAB2* ")

}, ignoreNULL=TRUE, ignoreInit=FALSE)
    
observeEvent(launchTab2(),{
 req(launchTab2()==TRUE)
 shinyjs::show(selector ="#div_helpPanel")  
         shinyjs::show(selector ="#div_help")   
         notify_info("click +/- on the bottom-right panel to display the simulation set-up progress",timeout=12000,  
                  config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="fade",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                                fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                                background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
}, ignoreNULL=TRUE, ignoreInit=TRUE)  




hab_3857 <- reactive({
  NULL
req(!is.null(hab_3857w()))
  hab_3857w <-hab_3857w()
return( terra::unwrap(hab_3857w ) )
})



 
   
  
observeEvent({
  input$shape
  input$operation
  1
  },{
 if(input$operation == "translocation" & input$shape == "none") {
  updateRadioGroupButtons(session,"shape", selected = "buffer") 
  updateRadioGroupButtons(session,"buffer_sel", selected = "2000") 
  notify_info("simulating a translocation requires an exploration site" ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
 }
#,closeButton=TRUE  cssAnimation=TRUE,
},ignoreInit=TRUE)

##################################### testing version filters 
observe({  
  req(mapExists()==TRUE)
  if( input$operation == "observation") {
  cat("adjust ui options for operation = obs  - ")  
  hideTab(inputId="SimParamsTabs", target="points",   session = session)
  hideTab(inputId="SimParamsTabs", target="groups",   session = session)
  hideTab(inputId="SimParamsTabs", target="timing",   session = session) 
  hideTab(inputId="setup_ask", target= "groups",   session = session) 
  hideTab(inputId="setup_ask",target=  "timing",   session = session)   
  hideTab(inputId="setup_ask", target= "points",   session = session) 
 }
  if( input$operation == "translocation"){
  cat("adjust ui options for operation = translocation  - ")  
 showTab(inputId="SimParamsTabs", target="points", select = FALSE, session = session)  
 showTab(inputId="SimParamsTabs", target="groups", select = FALSE, session = session)  
 showTab(inputId="SimParamsTabs", target="timing", select = FALSE, session = session) 
 showTab(inputId="setup_ask", target="points", select = FALSE, session = session)  
 showTab(inputId="setup_ask", target="groups", select = FALSE, session = session)  
 showTab(inputId="setup_ask", target="timing", select = FALSE, session = session) 
  }
})
  
  
observe({  
  toggleElement(id= "siteRow_out", condition =  input$shape != "none"  )  
  toggleElement(id= "inputSum1",condition =  input$operation != "observation" ) 
  toggleElement(id= "inputSum2",condition =  input$operation != "observation" ) 
  toggleElement(id= "inputSum3",condition =  input$operation != "observation" ) 
  toggleElement(id= "inputSum4",condition =  input$operation != "translocation"  )   
  toggleElement(id= "div_buffer_sel",condition =  input$shape != "none"  ) 
  
  toggleElement(id="div_uplFormat", condition = current_drawnShapeGeom() == "upload" & input$metadata_effect =="incorporate records")
  toggleElement(id="div_uplFormat_ttl", condition =current_drawnShapeGeom() == "upload" & input$metadata_effect =="incorporate records")
 
})                              
                  
observe({  
  req(mapExists()==TRUE)
  toggleElement(id="div_coordsTable",condition =  Nfams_actual_display() == input$Nfams_init) 
  cat("toggle coords tab  - ")
  toggleElement(id= "div_effTimeLine", condition =!is.null(ganttdf())) 
})
  
observeEvent(mapExists(),{
  req(mapExists()==TRUE) 
delay(3000,  shinyjs::show(selector ="#div_map_titles", anim=TRUE, animType="fade",time=2) )
delay(3000,  shinyjs::show(selector ="#div_paramsPanel", anim=TRUE, animType="fade",time=2) )
  shinyjs::show(selector ="#leafmap", anim=TRUE, animType="fade", time=1)
})
shinyjs::show(selector ="#show_panels") # loads at first while navbarpage 1 appears but not showing
shinyjs::hide(selector = "#prelaunchPage_ui", anim=TRUE, animType="fade", time=3) 

 

observe({ 
  show_metaselectModal()
  btn <-1 
  if(!is.null(input$metaselect_listGlob)){ 
        if(input$metaselect_listGlob == "none" & is.null(input$metaselect_listCustom)) {
           btn<- 0}
        if(input$metaselect_listGlob == "none" & !is.null(input$metaselect_listCustom) ){
             if(length(input$metaselect_listCustom)==0) {
 btn<- 0} }
        if(input$metaselect_listGlob == "river catchment" & length(selectedCatch_meta())==0 ){ 
 btn<- 0 }
             if(length(selectedCatch_meta())==1 & "none selected" %in% selectedCatch_meta() ){ 
 btn<- 0 } 
       } else {
         if(is.null(input$metaselect_listCustom)) { 
 btn <-0  } else {
         if( length(input$metaselect_listCustom)==0){ 
 btn <-0  } }
       } 
   toggleElement(id="div_metaselect_btn",condition = btn ==1) #button()==1) ###FFS!!!! needs to work or crashes following geoms
})

observe({ 
   req(mapExists()==TRUE)
   cat("toggle sim display  - ")
   toggleElement(id="noAdlt_inOutput",condition = !is.null(recapPlot_fams()) & is.null(recapTableOut_fams())) 
   toggleElement(id="noTerrs_inOutput",condition = !is.null(recapPlot_fams()) & is.null(recapTableOut_fams())) 
   toggleElement(id="noPlotOutput",condition = !is.null(recapPlot_fams()) & is.null(recapTableOut_fams()))  
   toggleElement(id="nonNULLAdlt_inOutput",condition = !is.null(recapTableOut_fams())) 
   toggleElement(id="nonNULLTerrs_inOutput",condition = !is.null(recapTableOut_fams())) 
   toggleElement(id="nonNULL_plotOutput",condition = !is.null(recapTableOut_fams()))   
   toggleElement(id="nonNULLheaders_Output",condition = !is.null(recapTableOut_fams()))
 
}) 
 
##################################### allow prog spinner - country selected
output$progress_spinner <- renderUI({ 
    # req(!is.null(rv_mapctry$bnd_cntry) )
     add_busy_spinner(  spin = "fading-circle",  onstart=TRUE,height = "44px",color="magenta",position = "top-right",margins = c("4px","38vw"))        
}) 
outputReadYN_txt <- reactiveVal(  p("no output")  )
output$outputReadYN_txt<- renderUI({  outputReadYN_txt() }) 
      
 
 

 



##################################### filter functionalities available depending on test mode
modFilter ="none"
observe({
  if(modFilter ==  "site selection") { #session 1 4oct23
      cat(" filter value auto-input: stay on site tab -------------")

     shinyjs::disable(selector = '.navbar-nav a[data-value="input summary"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="simulation output"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="export output"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="about"')
    
     shinyjs::disable(selector = '.nav-tabs a[data-value="points"')
     shinyjs::disable(selector = '.nav-tabs a[data-value="groups"')
     shinyjs::disable(selector = '.nav-tabs a[data-value="timing"')
     shinyjs::disable(selector = '.nav-tabs a[data-value="landscape"')
  }
  if(modFilter ==  "locating release points") { #session 2 18oct23
      cat("------------- filter value auto-input: stay on site/points tabs -------------")

     shinyjs::disable(selector = '.navbar-nav a[data-value="input summary"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="simulation output"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="export output"')
     shinyjs::disable(selector = '.navbar-nav a[data-value="about"')
      disable(id="start_sim")  
 #    shinyjs::disable(selector = '.nav-tabs a[data-value="groups"')
#     shinyjs::disable(selector = '.nav-tabs a[data-value="timing"')
     shinyjs::disable(selector = '.nav-tabs a[data-value="landscape"')
  }  
  
if(modFilter =="metadata") { 
  cat("layers list  - ")
  layers_list <- addLayersCont_list (cur_layers=cur_layers(),new_layer="beaver habitat",type="add")  
  layers_list %>% cur_layers() 

  }
}) 

 


 



   
##################################### user locks the exploratory site  
observeEvent(input$lock_site, {
  cat("site locking input  - ")
  input$lock_site %>% click_Site_disabled()
         if(input$lock_site== FALSE) {  
             cat("unlock site - ")
           updatePrettyCheckbox(session, "lock_site",  label="exploratory site unlocked" )
           updatePrettyCheckbox(session, "lock_pts",  label="release locations unlocked", value=FALSE ) # unlock pts if unlocking site
         #  initValues(NULL) # so doesnt bug
                                  shinyjs::enable("buffer_sel")
                                  shinyjs::enable("shape")
                                  shinyjs::enable("operation") 
                                  rv_sim$candsite_lab <- "exploratory site"
                                  rv_sim$candsite_col <- "black"
                                  rv_sim$candcatch_col <- "black"
                                  rv_sim$candsite_weight <- 4
                                  rv_sim$candsite_yellOp <- 1
           } else {
             cat("lock site - ")
                                 updatePrettyCheckbox(session, "lock_site",  label="exploratory site locked" )
                                 shinyjs::disable("buffer_sel")
                                 shinyjs::disable("shape") 
                            #      shinyjs::disable("operation") 
                                 rv_init$infotext_instructions_pointout <- NULL 
                                # rv_sim$candsite_lab <-NULL
                                 rv_sim$candsite_col <- "#5555559c"
                                  rv_sim$candcatch_col <- "#5555559c"
                                 rv_sim$candsite_weight <-2
                                 rv_sim$candsite_yellOp<- 0
                                 if(input$shape == "catchment"){click_catch_disabled(TRUE)}
            } 
}, ignoreInit=TRUE )   

 
##################################### user locks the release points
observeEvent(input$lock_pts, {
  cat("trigger - ")
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("table_hilight")
  input$lock_pts %>% click_Pts_disabled()
  if(input$lock_pts== TRUE) {  
                                 cat("lock pts  - ")
                                 updatePrettyCheckbox(session, "lock_pts",  label="release locations locked" )
                                 updatePrettyCheckbox(session, "lock_site",  label="exploratory site locked", value=TRUE ) # lock site if locking pts
                                 shinyjs::disable("simRandPts") 
                                 shinyjs::disable("Nfams_init")
                                 shinyjs::disable("relPts_method")
                                 rv_sim$candsite_lab <-NULL # unplots the explo site boundary for clarity
  } else {       
            cat("unlock pts  - ")
            if(input$relPts_method == "random_location_across"){shinyjs::enable("simRandPts")}
            shinyjs::enable("Nfams_init")
            shinyjs::enable("relPts_method")
            rv_sim$candsite_lab <- "exploratory site"

            }
}, ignoreInit=TRUE ) ### keep points locked when chnagin site geom if pts are still inside? - warning msge if not
    

 
################################################################################################################ infotexts - initial settlement 
output$infotext_settlmntest <- renderUI({ 
  if(any(!is.null(notSettling_pars()),!is.null(notSettling()))){  
return(tagList(
          div("initial settlement test",style="color:teal;"), 
          div(textOutput("infotext_notSettling_pars"),style="text-align:center;color:#26bcad;" ),
          div( uiOutput("infotext_trySettling"), style="float:right;width: 100%;")   ))
  }
})     

output$infotext_notSettling_pars <- renderText({  
   if(is.null(notSettling_pars()))  {return(NULL) } else { notSettling_pars() }
})                                                 
   
output$infotext_trySettling <- renderUI({  
   if(is.null(notSettling()))  {return(NULL) } else {
   div(HTML(paste0(notSettling())),class="summ_txt_output", style="color:turquoise;" )  
}
})
  
              

output$modelRunning_panel_inputTab  <- renderUI({ 
  req(base::isTRUE(model_running()) & launchTab2()==TRUE)
  return(tagList(div( style="z-index:3;display:flex;align-items: baseline;margin-top:-5px;justify-content: space-between;padding-right: 11px;padding-left: 11px;",
                                                    div( textOutput("sim_in_progress_inputTab"), style="display:inline;"),
                                                    div(tags$img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'), style="display:inline;padding-left:23px;"),
                                                    div( textOutput("sim_in_progress2_inputTab"), style="margin-left:7px;display:inline;") 
                      )) 
)  }) 

output$modelRunning_panel           <- renderUI({ 
  req(base::isTRUE(model_running()) & launchTab2()==TRUE)
  return(div( style="z-index:3;display:flex;align-items: baseline;margin-top:-5px;justify-content: space-between;padding-right: 11px;padding-left: 11px;",
                                                    div( textOutput("sim_in_progress"), style="display:inline;"),
                                                    div(tags$img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'), style="display:inline;padding-left:23px;"),
                                                    div( textOutput("sim_in_progress2"), style="margin-left:7px;display:inline;") 
                      ) 
) }) 
output$modelRunning_panel_outputTab <- renderUI({
  req(base::isTRUE(model_running()) & launchTab2()==TRUE)
  return(tagList(div( style="z-index:3;display:flex;align-items: baseline;margin-top:-5px;justify-content: space-between;padding-right: 11px;padding-left: 11px;z-index:1000;",
                                                    div( textOutput("sim_in_progress_outputTab"), style="display:inline;"),
                                                    div(tags$img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'), style="display:inline;padding-left:23px;"),
                                                    div( textOutput("sim_in_progress2_outputTab"), style="margin-left:7px;display:inline;")  
)) 
) })

output$modelRunning_panel_aboutTab <- renderUI({
  req(base::isTRUE(model_running()) & launchTab2()==TRUE)
  return(tagList(div( style="z-index:3;display:flex;align-items: baseline;margin-top:-5px;justify-content: space-between;padding-right: 11px;padding-left: 11px;z-index:1000;",
                                                    div( textOutput("sim_in_progress_aboutTab"), style="display:inline;"),
                                                    div(tags$img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'), style="display:inline;padding-left:23px;"),
                                                    div( textOutput("sim_in_progress2_aboutTab"), style="margin-left:7px;display:inline;")  
)) 
) })

output$modelPreping_panel           <-  renderText({ model_running_UItxt() }) #div(), style="display:inline;") }) 
output$modelPreping_panel_inputTab  <- renderText({ model_running_UItxt() })
output$modelPreping_panel_outputTab <- renderText({ model_running_UItxt() })
output$modelPreping_panel_aboutTab <- renderText({ model_running_UItxt() })
output$modelOutput_panel            <-  renderText({ model_running_UItxtout() }) # output summary up in nav bar
output$modelSimOutput_panel         <-  renderUI ({ 
  div(HTML(paste0(model_sim_UItxtoutTtl() ,model_sim_UItxtout())))}) # output slider for mapping each year output
                          
model_sim_UItxtoutTtl <- reactiveVal()  
 
output$modelsTestOutput_panel         <-  renderUI({ HTML( model_sTest_UItxtout())  }) # output slider for mapping each year output
                           
 

model_running_UItxt <- reactiveVal(paste0("compiling GIS layers") )
model_running_UItxtout <- reactiveVal(NULL)
model_sim_UItxtout<- reactiveVal(NULL)
model_sTest_UItxtout <- reactiveVal(NULL)
observeEvent({
  model_running()    ### those are updated when sim starts
  modelPrep()
  model_running_UItxtout()
  model_sim_UItxtout()
  1
},{
  if(base::isFALSE(model_running()) &  is.null(model_running_UItxtout())) {  ## note model_running_UItxt never NULL from start
                modelOut(0)
              #  modelPrep(0)  ### those contro display of update panel ontop
                modelRunning(0)
    } 
   if(base::isTRUE(model_running()) & modelPrep()==0 )   {modelRunning(1)} else {modelRunning(0)}
   if(base::isFALSE(model_running()) & !is.null(model_running_UItxtout())) {modelOut(1)} else {modelOut(0)}
   if(!is.null(model_sim_UItxtout())) {simModelOut(1)} else {simModelOut(0)}
}, ignoreInit=FALSE, ignoreNULL=FALSE)  
   

 
   

observeEvent(try_settling() ,{
  try_settling <- try_settling()
  try_settling(NULL)
  req(try_settling == 1)
  cat("\n try_settling  - ")
  updatePrettyCheckbox(session, "lock_pts",  label="release locations locked", value=TRUE  ) 
    settling_output(NULL)
   trying_settling(1) 
    notSettling(NULL) 
   notSettling_pars(NULL)
    init_terrsPolys(NULL) 
   Nsettled_txt(NULL)
   model_running_UItxtout(NULL) # becoes null with val above but making sure to avoif race issue # but have to leave if visualising simpop!
   try_settling_params(1)  
 })
    

 
anim_sim_prog <- reactiveVal(NULL)
         
   

 
 
try_settling_now<- reactiveVal(NULL)
try_settling_params <- reactiveVal()
settling_output <- reactiveVal()

 
  



Nsettled_txt <- reactiveVal(NULL)

output$Nsettled_txt <- renderText ({
  if(is.null(Nsettled_txt())) return("initial territories not simulated.")
  Nsettled_txt()
})

 
  

    


cur_layers <- reactiveVal(NULL) # selected legend controls
 
 

released_labs_stranded <- reactiveVal()
trying_settling <- reactiveVal(NULL)
 
 

  




 
 


################################### assumptions  sim & init - UI
    

output$settling_assumptionArea_txt <- renderUI({  paste0("Initial settlement area: ", input$geom_settling)  })
output$settling_assumptionDist1_txt <- renderUI({  paste0("Options for territories located more than ", input$dist_settling, "km from release location will not be considered.")  })
 
reptxt <- reactiveVal() 


output$sim_assumption_txtNyr <- renderText({  paste0("Simulation duration: ", input$sim_Nyrs, " years")  })
output$sim_assumption_txtNrep <- renderText({  paste0("Number of simulation runs: ", input$sim_Nreps)  })
output$sim_assumption_txtStartYr <- renderText({  paste0("Simulation start year: ", simStartYearParams$minyear)  })


observeEvent(input$sim_Nyrs,{
  if(input$sim_Nyrs=="10 years"){
   notify_warning("see caveat regarding simulation duration beyond 5 years",timeout=5100,   
                     config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                                   fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                                   background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
   }
},ignoreNULL=TRUE, ignoreInit=TRUE)  


popGrowth_txt <- reactive  ({
  datSave_sim_pop()
   if(is.null(datSave_sim_pop())) {
     return(NULL) 
     } else {  
       mgmt.years <- as.numeric(as.character(mgmt.years()))
       mgmt.reps  <- as.numeric(as.character(mgmt.reps()))
       mgmt.startYear<- as.numeric(as.character(mgmt.startYear())) 
       
       yrTxt <- as.character(paste0(mgmt.startYear," - ",mgmt.startYear+mgmt.years))
       repTxt <- as.character(paste0(mgmt.reps," repetitions"))
  return(list(yrTxt,repTxt))
}
})
 
output$proximTo_title <- renderUI({ 
  req(proximToTable())
  geomNams <- as.character(unique(proximToTable()$name))
  as.character(paste0(  "PROXIMITY TO",geomNams)) 
})

proximToTable <- reactiveVal(NULL)
dataTables.rowsGroup_path <- here("www") # folder containing dataTables.rowsGroup.js
dataTables.rowsGroup_dep <- htmltools::htmlDependency( "RowsGroup", "2.0.0", dataTables.rowsGroup_path, script = "dataTables.rowsGroup.js")
outDat_areaOfInterest <- reactiveVal(NULL)
  
GIS_running <- reactiveVal(FALSE)

input_proximTo <- reactiveVal(0)



observeEvent(input$proximTo,{
  cat("proximTo") 
   
  if(is.null(datSave_sim_pop())){ 
         notify_warning("simulate a population to assess its spatial spread",timeout=5100,   
                     config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                                   fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                                   background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
       GIS_running(FALSE)
  } else {
 req(!is.null(datSave_sim_pop()[[1]])  ) 
cat(c(as.numeric(Nfeat_areaOfInt())," feat area of interest  - " ))
      if(as.numeric(Nfeat_areaOfInt())==0) {
        # delete distto table 
        outDat_areaOfInterest(NULL)
        GIS_running(FALSE)
     #   input_proximTo(0)
      }  

if(as.numeric(Nfeat_areaOfInt())>0) {
  # delete distto table 
 outDat <- outDat_areaOfInterest() # update: remove deleted geoms since last computing values
 
 outDat[outDat$LeafletId %in% metaTable_areaOfInt()$LeafletId, ] %>% outDat_areaOfInterest()
 input_proximTo(input_proximTo()+1)
 print(input_proximTo()+1)
 GIS_running(TRUE)
 cat("GIS_running(TRUE)  - ")
}
}
}, ignoreInit=TRUE, ignoreNULL=TRUE)

     

output$GISrunning_areaOfInterest <- renderUI({ 
  req(GIS_running()==TRUE) 
  return(tagList(div( style="display:flex;justify-content: center;padding-right:0 4px;",
                    div(tags$img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='20px', width='20px'), style="display:inline;padding-left:7px;"),
                    div( "processing GIS layers" , style="display:inline;padding: 0 17px;margin-top: 2px;")
)))  
}) 
 

observeEvent(input_proximTo(),{
   req(as.numeric(input_proximTo())>0 ) 
  cat("\n compute proxim summary: ")

geomsOfInt <- newFeaturesDat_shp()[newFeaturesDat_shp()$layerType == "area of interest" ,] # if leaving so and post hoc calc, can do anyafter computation 
 
# only new ones
 outDat_areaOfInterest <-  outDat_areaOfInterest() # already computed for those geoms
if(!is.null(outDat_areaOfInterest)){
  geomsOfInt <- geomsOfInt [!geomsOfInt$LeafletId %in% outDat_areaOfInterest$LeafletId,] # ie dont recompute
}
#print(geomsOfInt)

if(nrow(geomsOfInt)==0){
    paste0("geomsOfInt 0 rows") 
    geomsOfInt <- NULL 
    GIS_running(FALSE)
    notify_info("no new data",timeout=5100,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
  
     cat("GIS_running(FALSE)")
  }
req(!is.null(geomsOfInt))
 
#make density plot and take edges for min distance, per frequency level
## proximity of beavs to geom
## nearest geom
## count of points within distance - buffer
pts <- datSave_sim_pop()$simpop_shp %>% st_centroid()
cat("pts  - ")
#print( pts) 
 # year_dat <- t(data.frame(year=c( seq(1,mgmt.years)-1+simStartYearParams$minyear)))
 

### crop geoms of int by 30kmbuffered simpop because no point and takes ages if too many/far geoms

cropZone <- pts %>% st_transform(27700) %>% st_buffer(30000)  %>% st_transform(4326)
sub_geomsOfInt <- st_crop(geomsOfInt,cropZone)

 geomsOfInt$LeafletId <- as.factor(geomsOfInt$LeafletId)
 pts$Nruns <- as.factor(pts$Nruns)
 
  #choiceValues=c("distTo","occ1km","tToReach")  
    vars <-c("distTo","occ1km","tToReach","occOver")# input$metadata_layerEffect  
        
   outDat <-outVar<-NULL
  for(id in levels(geomsOfInt$LeafletId)){
  outDat0 <- expand.grid(LeafletId =id,  
                        Nruns=c("any",">0-39%","40-59%","60-79%","80-100%"  ), 
                        vars= c("distTo","occ1km","occOver"))#[which(c("distTo","occ1km") %in% vars)]  )   # Nruns=c(">0-39%","40-59%","60-79%","80-100%"  ), year=seq(2024, 2025))
  outDat <- rbind(outDat, outDat0)
  }
   for(id in levels(geomsOfInt$LeafletId)){
  outVar0 <- expand.grid(LeafletId =id,  
                        Nruns=c(">0-39%","40-59%","60-79%","80-100%"  )  )   # Nruns=c(">0-39%","40-59%","60-79%","80-100%"  ), year=seq(2024, 2025))
  outVar <- rbind(outVar, outVar0)
   }
   outVar$tToReach <- as.numeric(NA)
      
    
 for (year in unique(pts$year)){
    outDat$year=0#as.numeric(NA) #add year column 
    print(year)  
    for(geom in  unique(geomsOfInt$LeafletId)){
      
      if(geom %in% sub_geomsOfInt$LeafletId) { 
        
       datgeom <-sub_geomsOfInt[sub_geomsOfInt$LeafletId == geom,] 
        if(geom %in% lineFeature()$LeafletId){geomtype="poly"}
        if(geom %in% mapGeomFeature()$LeafletId){geomtype="poly"} # beacuse can only be catchments for now
        if(geom %in% polyFeature()$LeafletId){geomtype="poly"}
        if(geom %in% pointFeature()$LeafletId){geomtype="point"}
        if(geom %in% uploadFeature()$LeafletId){geomtype="poly"}
        
      daty <- pts[pts$year == year,]
     for (run in unique(daty$Nruns)){
       print(run)
        datgeom_dis <- datgeom
       dat <- daty[daty$Nruns %in% levels(daty$Nruns)[4:which(levels(daty$Nruns)== run)],]
      
        if(geomtype=="poly" & any(lengths(st_intersects(dat,datgeom_dis)%>%suppressMessages())>0)){ # if overlapping
          
          outDat$year[outDat$vars=="distTo" & outDat$Nruns %in% run & outDat$LeafletId == geom] <- 0
          }  else { 
               distances <- st_distance(dat, datgeom_dis)
                 
               if("distTo" %in% vars) {
                 dis <- round(min(as.numeric(distances) )/1000,1)
                 if (dis>0) { dis <-  paste0(dis,"km")}
               outDat$year[outDat$vars=="distTo" & outDat$Nruns == run & outDat$LeafletId == geom] <- dis
               }
              } 
         
        if("occ1km"   %in% vars) {  
       dat_buf1km <- datgeom  %>% st_transform(st_crs(27700)) %>% st_buffer(1000) %>% st_transform(st_crs(4326))
       if(geomtype=="poly"){ dat_buf1km <- st_erase(dat_buf1km, datgeom)} # buffer OUTSIDE poly here
       npts_1km   <- length(which(lengths(st_intersects(dat, dat_buf1km)%>%suppressMessages())>0) )
       if(npts_1km>0) {npts_1km<- paste0(npts_1km,"ha")}
       outDat$year[outDat$vars== "occ1km" & outDat$Nruns == run & outDat$LeafletId == geom] <-  npts_1km   
       } 
        
       if("occOver" %in% vars & geomtype=="poly") {
       dat_occOver <- datgeom  
       npts_occOver   <- length(which(lengths(st_intersects(dat, dat_occOver)%>%suppressMessages())>0)) 
       if(npts_occOver>0) {npts_occOver<- paste0(npts_occOver,"ha")}
        outDat$year[outDat$vars== "occOver" & outDat$Nruns == run & outDat$LeafletId == geom] <-npts_occOver 
      }   
     } 
    } else { # if geom is >30km away, was cropped off
      outDat$year[outDat$vars=="distTo" & outDat$Nruns == "any" & outDat$LeafletId == geom] <- ">30km"
   }
   }
     colnames(outDat)[which(colnames(outDat) == "year")]<- paste0("y",year)
    
    }   
  geomsOfInt$name <-  as.character( paste0(
                  "<p style='font-weight:bold;'>",geomsOfInt$name,"<br><span style='font-weight:normal'>",
                   geomsOfInt$layerEffect ,"</span></p>"))
  
 

   
    geomsOfInt$LeafletId <-  as.character(geomsOfInt$LeafletId)
     
# print(outDat)
 outDaty <- outDat[,c(4:ncol(outDat))]
 
  
 outDatp <- data.frame(LeafletId=as.character(outDat$LeafletId), 
                       name=as.character(outDat$LeafletId), 
                       vars=as.character(outDat$vars),
                       Nruns=as.character( outDat$Nruns ))
 outDatyLighter <- data.frame(lapply(outDaty,as.numeric) ) # becomes all NA except for 0 values
 
 
 outDatp$Nruns[which(!is.na(rowSums(outDatyLighter, na.rm=FALSE)))] <- NA# that way it filters also distance to between overlapping zones as returns 0- BeauT! as.character("any")
 outDat <- cbind(outDatp,outDaty) 
 outDat <- na.exclude(outDat) 
 #outDat <- outDat  %>% distinct() # one row per "any" prob level
 #print(  paste0( "<p style='color:",outDat$fillCol,";'>", outDat ,"</p>"))
 
# print(outDat)
       
  for(ro in 1:nrow(outDat)){ 
   outDat$name[ro] <- geomsOfInt$name[which(geomsOfInt$LeafletId == outDat$LeafletId[ro] )][1] 
  } 
 outDat$vars <- as.character(outDat$vars)
 outDat$vars[outDat$vars == "distTo"] <- "distance to"
 outDat$vars[outDat$vars == "occ1km"] <- "1km surrounding"
 outDat$vars[outDat$vars == "occOver"] <- "shared area"
 
 
   
 ##outDat$vars=distTo
 
 cat("fillCols  - ")
 makoColz <- c( rev(alpha(mako(4),.1))[1:3],"#b6b612")
  hiliColorz <- c( "#b6b612","#ffe62c6b", "#ffe62ca1","#ffe62cc2" )
 fillCols <- hiliCols <- txtCols <-outDat$Nruns 
  indx=4
  outDat$Nruns <- as.character(outDat$Nruns)
  for(lev  in as.character(c( ">0-39%","40-59%","60-79%","80-100%"))){ 
   fillCols[outDat$Nruns==lev] <- as.character(makoColz[indx])
   hiliCols[outDat$Nruns==lev] <- as.character(hiliColorz[indx])
  indx<- indx-1
  }
    fillCols[outDat$Nruns=="any"] <- as.character("#b6b612")
   hiliCols[outDat$Nruns=="any"] <- as.character("#b6b612")
 
 outDat$Nruns[outDat$Nruns == ">0-39%" &  outDat$vars!="distance to"] <- "potentially occupied"
# outDat$Nruns[outDat$Nruns == "40-59%" &  outDat$vars!="distance to"] <- ">40%"
# outDat$Nruns[outDat$Nruns == "60-79%" &  outDat$vars!="distance to"] <- ">60%"
 outDat$Nruns[outDat$Nruns == "80-100%" &  outDat$vars!="distance to"] <- "occupied >80%"
 
 outDat$Nruns[outDat$Nruns == ">0-39%" &  outDat$vars=="distance to"] <- "nearest occupied cell"
# outDat$Nruns[outDat$Nruns == "40-59%" &  outDat$vars=="distance to"] <- ">40%"
# outDat$Nruns[outDat$Nruns == "60-79%" &  outDat$vars=="distance to"] <- ">60%"
 outDat$Nruns[outDat$Nruns == "80-100%" &  outDat$vars=="distance to"] <- "nearest cell occupied >80%"
 
 outDat$Nruns[outDat$Nruns == "any" &  outDat$vars=="distance to"] <- "nearest occupied cell"
 
 txtCols <- rep(as.character("beige"),length(fillCols))
 txtCols[fillCols %in% c(makoColz[3],makoColz[4])] <- "beige"
   
 
  
      
  
 outDat$fillCols <- fillCols
 outDat$txtCols <- txtCols
 outDat$hiliCols <- hiliCols 
 # for filtering
 # 
 outDat <- outDat[!outDat$Nruns %in% c("40-59%",">40%"),]
 outDat <- outDat[!outDat$Nruns %in% c("60-79%",">60%"),] 
 outDat$Nruns <- as.factor(outDat$Nruns)
 outDat$vars <- as.factor(outDat$vars)
 outDat$name <- as.factor(outDat$name)
  
   outDat_areaOfInterest <-  rbind(outDat,outDat_areaOfInterest )
#   outDat_areaOfInterest <-  outDat_areaOfInterest[ outDat_areaOfInterest$Nruns != "any",]
 outDat_areaOfInterest %>% outDat_areaOfInterest()
  
}, ignoreInit=FALSE, ignoreNULL=FALSE )

firstcolW <- reactiveVal()
anycolW <- reactiveVal()

 
  
  

observeEvent(outDat_areaOfInterest(),{
outDat <- NULL
   
  if(!is.null(outDat_areaOfInterest())) { 
   outDat <- outDat_areaOfInterest()   
      
         firstcolW <- firstcolW()
           anycolW  <- anycolW() # note - config for years 3-5-10
          firstcolW2 <-4
          firstcolW1 <- 4
          firstcolW3 <- 4
cat("display output - ")

#12 or 13 firstcolW
if(firstcolW==13) {firstcolW1 <- firstcolW1+1}


          fillColnum <- which(colnames(outDat) == "fillCols") -1
          txtColnum <- which(colnames(outDat) == "txtCols") -1
          hiliColnum <- which(colnames(outDat) == "hiliCols") -1
          ttlCellStyle <- as.character("font-weight: 100;margin: 8px 0 0 0 !important;")

          
            
          dat <-data.frame(lapply(outDat,as.factor) ) # becomes all NA except for 0 values
  
colN <- rep("", ncol(dat))
colN[2] <-as.character(p("feature of interest",style=c("color:#b6b612;",ttlCellStyle)))
colN[3] <- as.character(p("metric",style=c("color:#b6b612;",ttlCellStyle)))
colN[4] <- as.character(p("simulated",style=c("color:#b6b612;min-width:120px;",ttlCellStyle )))
colN[length(colN)-5] <-   as.character(p("annual summary",style=c("color:#b6b612;min-width:120px;",ttlCellStyle )))
  
  # it does not reflect he potential general spread of the population  
  # it represents the potential locations being occupied
  # the output is to be considered at the cell level
  # i.e. this cell is most likely to be occupied 
  # as the general spread is population and time-depdt 
  # this represents all opions for a populaton to take, that it couldnt possibly take as a single pop
  # interprete as such errr
  # do row sums? over 10 years, Ncells potentially occupied?
          #  dat <- outDat
 
# dat0 <-dat
# if(length(unique(dat0$LeafletId))>1){
#   for(id in unique(dat0$LeafletId)){
#     dat1 <- dat0[dat0$LeafleId == id,]
#     dat <- rbind(dat,c( rep("", ncol(dat) )),dat1) 
#}}  
         outDat <- datatable( dat,
               colnames = colN,                  
              rownames = FALSE,  selection = "none",escape=FALSE,
              editable = FALSE,  filter = "top",
             extensions = c('KeyTable', 'FixedColumns'),
              options = list(dom = 't', autowidth = TRUE,ordering=F, keys = TRUE,pageLength=-1, 
                              rowsGroup = list(1,2,3), # merge cells
                              fixedColumns = list(leftColumns = c(1,2,3)),
                                 columnDefs = list(  
                                  list(type = 'html', targets = c(1:(ncol(dat)-1))), #c(1,2,3)), # to escape HTML in filtering tags  
                                  list(targets =1, searchable = FALSE), 
                                  #list(targets =c(4: (ncol(dat)-1)  ), searchable = FALSE),
                                  list(width = paste0(anycolW ,"vw"),  targets =c(4: (ncol(dat)-1)  )) ,#
                                  list(width =  "12vw" ,  targets =c(1 )) ,  #paste0(firstcolW1 ,"vw") ,  targets =c(1 )) , 
                                  list(width =  "6vw" ,  targets =c(2 )) , 
                                  list(width = "6vw" ,  targets =c(3)) , 
                                  list(className ='dt-first', targets = c(1,2,3)) ,
                                  list(visible = FALSE, targets = c(0, fillColnum,txtColnum,hiliColnum)))
                              ) )   
  }      
  GIS_running(FALSE) 
 outDat  %>% proximToTable()  
 cat("proximToTable()   -") 
},ignoreNULL=FALSE, ignoreInit=TRUE) # ignoreInit added ap28  test init

 
 
  
output$filterProximTo_ui <- renderUI({
  req(outDat_areaOfInterest())
  varlevs <- levels(outDat_areaOfInterest()$vars)
  geomlevs <- levels(outDat_areaOfInterest()$name)
  return(tagList(
   div(checkboxGroupButtons(inputId= "filter_geom",  label = NULL,   
                            choices =geomlevs, selected =varlevs,individual =TRUE,
           size = "normal", direction = "vertical"), style="text-wrap:wrap;display:inline;"),
  div(checkboxGroupButtons(inputId= "filter_vars",  label = NULL,   
                            choices =varlevs, selected =varlevs,individual =TRUE,
           size = "normal", direction = "vertical"), style="text-wrap:wrap;display:inline;"),
  div(checkboxGroupButtons(inputId= "filter_Nruns",  label = NULL,   
                            choices =c(">0-39%","40-59%","60-79%","80-100%"  ), selected =c(">0-39%","40-59%","60-79%","80-100%"  ),
           size = "normal", direction = "vertical"), style="text-wrap:wrap;display:inline;")  
  ))                 
                             
})


output$recapProximTo <-  DT::renderDT({  
  req(!is.null(proximToTable()))# should be null when no data
  req(!is.null(outDat_areaOfInterest()))#
  req(nrow(outDat_areaOfInterest())>0)#
  tbl <-  proximToTable() 
 colz <-  outDat_areaOfInterest()$fillCols
 valz <-  as.character(outDat_areaOfInterest()[,outColSelected()+3]) 
 txtz <-  as.character(outDat_areaOfInterest()$txtCols)
 hilicolz <-  as.character(outDat_areaOfInterest()$hiliCols)
 rowz <- seq(1,length(colz))

 
 
ttbcolz <- seq(1:nrow(outDat_areaOfInterest()))
ttbcolz[which( outDat_areaOfInterest()$vars == "distance to")]<-"#cacabc"  
ttbcolz[which( outDat_areaOfInterest()$vars == "1km surrounding")] <- "#f9f96ecc"
ttbcolz[which( outDat_areaOfInterest()$vars == "shared area")] <- "#f9f96ea8"
    

 tbl$dependencies <- c(tbl$dependencies, list(dataTables.rowsGroup_dep))
 tbl  <- tbl %>% formatStyle(columns=3, target = "cell",   
                                 backgroundColor =  styleRow(rowz,ttbcolz), color=styleRow(rowz,"#525452")) 
      
   tbl %>% formatStyle(columns=outColSelected()+3,target = "cell", borderRadius = "50px",
                        backgroundColor = styleRow(rowz,ttbcolz) )

                       

},  server=FALSE)

  

output$popGrowth_title <- renderUI({ 
 if(is.null(popGrowth_txt())) {return( div("Population growth simulation",style="color:#d8e470;") )
 } else {
     popGrowth_txt <- popGrowth_txt()
yrTxt<- popGrowth_txt[[1]]
repTxt <- popGrowth_txt[[2]]
 return(tagList(    div("Population growth simulation",style="padding-right:13px;color:#d8e470;display: inline;"),
            div(yrTxt , style="color:#d8e470;background-color: #777;border-radius: 20px; display: inline;padding: 2px 13px;margin: 0 22px;"),
            div(repTxt ,style="color:#d8e470;background-color: #777;border-radius: 20px; display: inline;padding: 2px 13px;margin: 0 22px;")) 
 ) }
 })
                   
 
        

        
  



 
######## map layers controls toggles  ############

addLayersCont_list <- function(cur_layers,new_layer,type) { 
 if(type=="clear"){
   for(new_lay in new_layer){
   if(new_lay %in% cur_layers) { cur_layers <- cur_layers[-which(cur_layers == new_lay)] }
     }
 }
  if(type=="add"){cur_layers <- unique(c(cur_layers, new_layer)) }
  return(cur_layers)
}
  
 titl <- as.character( paste0( "<span style='strong'>beaver habitat suitability</span>
                                                <span style='line-height:1.2em!important;text-align:center;display:inline-block;font-size:90%;font-style:italic;font-weight:normal;'>
                                                as SDM habitat category output per cell</p>")) 
 titlHab_hiZoom <- sprintf(  "%s", titl  )   %>%  lapply(htmltools::HTML)  
          
 titl <- as.character( paste0( "<span style='strong'>beaver habitat suitability</span>
                                              <span style='line-height:1.2em!important;text-align:center;display:inline-block;font-size: 90%;font-style:italic;font-weight:normal;'>
                                              as the proportion of catchment suitable for settlement</p>")) 
 titlHab_lozoom <- HTML(titl)
 
showHab <- reactiveVal(NULL)
#  if(is.null(mapReleaseSite())& "beaver habitat" %in% input$mapReleaseSite_groups)
 # if(mapExists()==TRUE & "beaver habitat" %in% input$mapReleaseSite_groups) {return(1)} else {return(0)} })


observeEvent(input$mapReleaseSite_groups,{ 
  cat("\nleaflet groups  - ")
  if(is.null(input$mapReleaseSite_groups)){showHab(NULL)} else{
    if("beaver habitat" %in% input$mapReleaseSite_groups) {showHab(1)} else {showHab(0)} }
 },ignoreNULL=FALSE,ignoreInit=TRUE)


hiZoom_prev <- reactiveVal(2)
zoomRefresh <- reactiveVal(NULL)

 


 
  
hiZoom <- reactive({ 
# cat("hihi")
 # if( is.null( mapReleaseSite())) {  return(NULL)  }  
#  if(!is.null(mapReleaseSite()) & !is.null(input$mapReleaseSite_zoom)){
#           if(input$mapReleaseSite_zoom>11){return(TRUE)}else {return(FALSE)}}
#  if(!is.null(mapReleaseSite()) &  is.null(input$mapReleaseSite_zoom)){return(NULL)}
 input$mapReleaseSite_zoom
   if(  mapExists()==FALSE) {  return(NULL)  }  
  if(mapExists()==TRUE & !is.null(input$mapReleaseSite_zoom)){
           if(input$mapReleaseSite_zoom>11){return(TRUE)}else {return(FALSE)}}
  if(mapExists()==TRUE  &  is.null(input$mapReleaseSite_zoom)){return(NULL)} # for a second but allows trigger 
     
})

observeEvent(hiZoom(),{
    cat("check zoom  - ")
 if(hiZoom_prev()==0){
   hiZoom_prev <-TRUE
 } else {
 hiZoom_prev <- as.numeric(hiZoom_prev()>11)
}
  hiZoom<- as.numeric(hiZoom()) # TRUE/FALSE trigger
  
  if(hiZoom_prev != hiZoom) {zoomRefresh(TRUE)} #else{zoomRefresh(NULL)}
 hiZoom_prev(input$mapReleaseSite_zoom)
},ignoreNULL=TRUE,ignoreInit=FALSE)  

changeHabLegend <- reactive({ return(sum(as.numeric(showHab()), as.numeric(hiZoom())))  })
show_catchLegend <- reactive({ return( as.numeric(any(!is.null(input_mapReleaseSite_click()),!is.null(input$mapReleaseSite_click) ) & sitedat()$shape!="none"))})

 

observeEvent(showHab(),{ #showHab(),{   
         cat("mapzoom  - ")
 if( showHab()==0   ) {
  cat("hide ")
    leafletProxy("mapReleaseSite", session) %>%  hideGroup("local suitable") %>% hideGroup("habitat loZoom") %>% hideGroup("habitat hiZoom")
} else {
 #if(showHab()==1 & launchTab2()==TRUE )  {
  cat("show  ")
    if( hiZoom()==TRUE) {
              cat("zoom>10 ") 
                leafletProxy("mapReleaseSite", session)  %>%   hideGroup("habitat loZoom") %>% showGroup("habitat hiZoom") # %>% 
              #     removeControl("beaver habitat")  
            #      addLegend("topright",  layerId="beaver habitat", #may9 layerId="habitat hiZoom", group ="beaver habitat",
            #                colors =c(  alpha("grey20",.2),alpha("grey20",.8)) , values = ~c(1,2), labels=c("dispersal","settlement"),
            #                title =titlHab_hiZoom,  opacity = .7 ) 
             } else {
               cat("zoom<10 ")
              leafletProxy("mapReleaseSite", session) %>%   showGroup("habitat loZoom") %>% hideGroup("habitat hiZoom") %>% removeControl("habitat hiZoom")#  
              #              addLegendImage(position = 'topright',  layerId="beaver habitat", 
              #                        title=titlHab_lozoom,  
              #                        images= 'www/grayscaleGrad.png', labels="lower to higher",
              #                        width = 54, height = 18, labelStyle = as.list(c("margin"="0",labstyle_legend,"margin-right"="12px!important")) )  
             } 
  }
 
   cat("hab layer - ")
 
},ignoreInit=TRUE, ignoreNULL=FALSE) # showHab(NULL) while map doesnt exists so starts plotting after rendering] 
 
                                 
 
observeEvent(mapExists(),{
   req(mapExists()==TRUE)
delay(4000,   shinyjs::hide(selector ="#div_help",    anim=TRUE, animType="fade", time=4))
delay(4000,   shinyjs::hide(selector ="#div_about",    anim=TRUE, animType="fade", time=4))
delay(8000,    notify_success("ready !",timeout=4000,   
                 config_notify(height="2vh", 
                               width="9vw",opacity =1,borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE, 
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", showOnlyTheLastOne=TRUE,  
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )))
#delay(8000,shinyjs::show(selector ="#div_askAbout",    anim=TRUE, animType="fade", time=1   ) )
})
 
    
############add: reset/hide output visualising ui when switching operation as it includes/remves sim-pop buttons
 observeEvent({
   show_metaRecords()
   pointFeature_terrPols()
   1
   },{
   req(mapExists()==TRUE) 
   req(show_metaRecords()==TRUE & !is.null(pointFeature_terrPols()))
   req(nrow(pointFeature_terrPols())>0)
   cat("update mapped terrs   -")
   pols <- pointFeature_terrPols() 
   leafletProxy("mapReleaseSite", session) %>%  
                  addMarkers(data=mapAbleObs(),lng = ~X, lat = ~Y,
                                icon=makeIcon(iconUrl = "house2-pointer.png", iconWidth = 40, iconHeight = 40, iconAnchorY =  20  ), 
                                group = ~layerType, layerId= ~LeafletId, 
                                label= ~ sprintf(  "%s %s ",
                                     paste0("<p style='margin-top:0;font-size:16px;'>",featureSymb, "<span style='font-size:11px;text-align:left;color:",featureCol,";'>   ", layerType,": ",  name,"</span></p>"),
                                     paste0("<p style='color:beige;'>(",layerEffect,")") )   %>%  lapply(htmltools::HTML) ,  
                           labelOptions = labelOptions(offset=c(-50,0), direction="left",  style = as.list(c( "background-color" = "#0000008f",labstyle_plotLabs))  ) ,
                          options = pathOptions(pane = ~zPane )   ) %>% 
                    addPolygons(data=pols ,color= "black", opacity=1, fillOpacity=1,  group = ~layerType,  layerId= ~LeafletIds,# sometimes nultipolys means more geoms as pols than pts 
                                        fillColor="#dac838" , weight=1, options = pathOptions(pane ="metadataPt2")) 
     mapNewObs(1)
  },ignoreNULL=TRUE) # trigger replot after poly produced?
 
  
observeEvent( zoomRefresh(),{
  cat("refresh legend: ") 
 # req(mapExists()==TRUE & !is.null(hiZoom()))  
  if( hiZoom()==TRUE) { #>11
              cat("zoom>10 - ") 
                leafletProxy("mapReleaseSite", session)  %>%  removeControl("habitat loZoom") %>% hideGroup("habitat loZoom") %>% showGroup("habitat hiZoom") %>%
                  addLegend("topright",   group="habitat hiZoom",  layerId="habitat hiZoom",  
                            colors =c(  alpha("grey20",.2),alpha("grey20",.8)) , values = ~c(1,2), labels=c("dispersal","settlement"),
                            title =titlHab_hiZoom,  opacity = .7 ) 
             } else {
               cat("zoom<10 - ")
              leafletProxy("mapReleaseSite", session) %>% removeControl("habitat hiZoom") %>% hideGroup("habitat hiZoom") %>% showGroup("habitat loZoom") %>%
                            addLegendImage(position = 'topright',  layerId="habitat loZoom", 
                                      title=titlHab_lozoom,  
                                      images= 'www/grayscaleGrad.png', labels="lower to higher",
                                      width = 54, height = 18, labelStyle = as.list(c("margin"="0",labstyle_legend,"margin-right"="12px!important")) )  
             } 
   zoomRefresh(NULL)
   refresh_legendTxt(refresh_legendTxt()+1)
},ignoreNULL=TRUE,ignoreInit=FALSE )    #bEauT!
  
 


refresh_legendTxt <- reactiveVal(0)
 
observeEvent({
  refresh_legendTxt()
  cur_layers()
  1
  },{
 #  zoomRefresh()
 # cur_layers()
 # LCM_display() 
 # NBNAtlas_display()
#  1
#  },{
  cat("legend - ") 
   if ("simulated occupancy" %in% cur_layers()) {
     datSave_sim_pop <- datSave_sim_pop()
     if(length(levels(datSave_sim_pop$simpop_shp$Nruns))>0){
     YlOrBr <- rev(mako(4)) 
     colorFactor(palette = YlOrBr,domain=levels(datSave_sim_pop$simpop_shp$Nruns)) %>%  palette_Nruns() 
     mgmt.reps <- mgmt.reps()
     titl <- as.character( paste0( "<span style='strong'>simulated occupancy</span><br><span style='line-height:1.2em!important;text-align:center;display:inline-block;font-size:90%;font-style:italic;font-weight:normal;'>
                                    frequency over ",mgmt.reps," reps.</p>")) 
     titl <- sprintf(  "%s", titl  )   %>%  lapply(htmltools::HTML)   
     leafletProxy("mapReleaseSite", session)  %>%  
             addLegend("topright", colors =rev(mako(4)) , values = ~levels(datSave_sim_pop$simpop_shp$Nruns), labels=levels(datSave_sim_pop$simpop_shp$Nruns),
                             title = titl ,  opacity = .7, group= "simulated occupancy legend", layerId="simulated occupancy legend") 
  } 
  }    
   if( "catchments features" %in% cur_layers()){
    leafletProxy("mapReleaseSite", session) %>% #showGroup("catchments features")  %>%
           addLegendImage(images=c(  'www/line-magenta.png','www/square-suit.png'),
                                       title="catchments features", layerId="catchments features", 
                                       labels=c( "boundaries","settlement habitat"),
                                       width = 18, height = 18, # layerId=c("local suitable","catchments features"),
                                       labelStyle = as.list(c("margin"="0",labstyle_legend )),position = 'topright') 
  }   
   
     
   select <- which(c("modify habitat","observation records","area of interest") %in% cur_layers())
   nolays <- c("modify habitat","observation records","area of interest")
     if( length(select)>0 ) {
      lays <- c("modify habitat","observation records","area of interest")[select]
      nolays <- c("modify habitat","observation records","area of interest")[-select]
     cat(paste0("\n------------CLEAR: ",nolays))
#     mapNewFeature(1)
 #    mapNewObs(1)
  #   add_labelOnly(TRUE)
     cat("add augmented landscape legend  - ")
     leafletProxy("mapReleaseSite", session)   %>%   removeControl(layerId= "augmented landscape") %>%  
                  addLegendImage(images=c(  'www/square-modif.png','www/house2.png','www/square-areaOfInt.png')[select], 
                                 title="augmented landscape" ,
                                 labels=c(paste0("modify habitat (",as.numeric(Nfeat_modifHab()),")"),paste0("observation records (",as.numeric(Nfeat_records()),")"),paste0("area of interest (",as.numeric(Nfeat_areaOfInt()),")"))[select],
                                 width = 18, height = 18,  
                                 labelStyle = labstyle_legend,position = 'topright',
                                 group= "augmented landscape", 
                                 layerId="augmented landscape")  # %>%  hideGroup(nolays) # %>%  showGroup(lays)  
    }  else {
       
          leafletProxy("mapReleaseSite", session)     %>%   removeControl(layerId= "augmented landscape")   %>% clearGroup(nolays) 
   }
 
   


  if( "translocation set-up" %in% cur_layers() )   {  
  npts <- nyng <- nlags <- 0
  if(!is.null(initValues())){
    npts <-nrow(initValues())
    nyng <- length(which( initValues()$young>0))
    if(nrow(initValues())>0){nlags  <- length(which( initValues()$year>min( initValues()$year)))}
  }
   leafletProxy("mapReleaseSite", session) %>% #showGroup("translocation set-up") %>%
                       addLegendImage(images=c(here::here('www/beaver-legend.png') ,
                                              here::here('www/beaver-facing-right-small_sh.png'),
                                              here::here('www/clock-solid.png') ) ,
                                      title="translocation set-up", 
                                      labels=c(paste0("release location (",npts,")") , paste0("with young (",nyng,")"), paste0( "lagged release (",nlags,")") ), 
                                      width = c(18, 18, 16 ), height = c(17,9,16 ),  layerId="translocation set-up", 
                                      labelStyle =   labstyle_legend  , position = 'topright') 
} else {
   leafletProxy("mapReleaseSite", session) %>%  clearGroup("translocation set-up") %>% removeControl("translocation set-up")  
}    
        

if( "initial settlement test" %in% cur_layers()  ){
  leafletProxy("mapReleaseSite", session) %>%    
        addLegendImage( images=c('www/wanderline.png', 'www/rout.png','www/square-initSetlmnt.png' , 'www/flag2.png'  )   ,
                        title="initial settlement test",#titl, 
                        labels=c( "initial wander",paste0("dispersal route (",Ngoing(),")"),paste0("settlement (",Nsettling(),")"),paste0("failed to settle (",Nstranded(),")")), 
                        width = c(18,18,18,18), height = c(18,18,18,18),   
                                           labelStyle = labstyle_legend,  position = 'topright', 
                                            layerId="initial settlement test",
                                            group="initial settlement test") 

} else {
     leafletProxy("mapReleaseSite", session) %>% removeControl("initial settlement test")  
}  

if (NBNAtlas_display() ==1){  
    req(provider_ID())
    req(NBNAtlas())
    cat("NBN legend - ") 
    palette_providers <- colorFactor(provider_ID()$provider_col,domain= NBNAtlas()$provider) 
    leafletProxy("mapReleaseSite", session) %>%     
     addLegend("topright",pal=palette_providers, values=NBNAtlas()$provider,
                                       title = "NBN Atlas data providers",   opacity = .7,  layerId="NBN Atlas records",  
                                       className = "info legend legend-larger")    %>% 
    addTiles(urlTemplate = "", group="NBN Atlas records",  
                                      attribution = as.character(c(" NBN records | NBN Atlas website at http://www.nbnatlas.org contributors: ",  
                                                                         as.character(paste(provider_ID()$provider, sep="   ")))))  
}  
if (LCM_display()  ==1){  
    cat("LCM legend - ") 
   leafletProxy("mapReleaseSite", session) %>%      
             addControl(img(src=landCoverMapLegend, style='width:12vw;min-width:180px!important;'),position = "topright",layerId="land cover map")
   }  #no leg control for that lay but keep here for maintining same legend order
    
 } , ignoreNULL=TRUE,ignoreInit=TRUE)

#WMS of wooded riparian zones in Great Britain</Title>
# <Abstract>This web map service provides a 1km resolution gridded coverage of wooded areas in riparian zones (river- or streamsides) across Great Britain. The areas classified as riparian in this dataset are defined by a 50 metre buffer applied to the CEH 1:50000 watercourse network. Wooded areas within this zone are identified as those classified by the Land Cover Map of Great Britain 2007 as either coniferous or deciduous woodland. The data are aggregated to a 1km resolution.</Abstract
   
observeEvent(cur_layers(),{   
     cat("\ncontrols  - ") 
     leafletProxy("mapReleaseSite", session) %>%      
           addLayersControl( overlayGroups  =  cur_layers()  ,
                             position = "bottomleft", options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)   )
},ignoreNULL=TRUE, ignoreInit=TRUE)   

  

observe({ 
  req(!is.null(input$mapReleaseSite_groups)) 
  mapLays <- NULL   
  show_metaRecords()
  show_metamodifHab()
  
  if(!is.null(datSave_sim_pop())) {  mapLays <- c(mapLays,"simulated occupancy")   } 
   
   if(show_catchLegend()==1) { mapLays <- c(mapLays,"catchments features") }   
   
  
if(any(show_metamodifHab()==TRUE,show_metaAreaOfInt()==TRUE)){
  mapNewFeature <- 0
if( show_metamodifHab()==TRUE) {    #input$addMetadata_modifHab == "modify mapped habitat") {  
   mapLays <- c(mapLays,"modify habitat") 
   mapNewFeature <- 1
  } else {
  mapLays <-  mapLays[mapLays !="modify habitat"]
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("modify habitat")
  }
  
if(show_metaAreaOfInt()==TRUE) {
  mapLays <- c(mapLays,"area of interest")  
  mapNewFeature <- 1
 } else {
mapLays <-  mapLays[mapLays !="area of interest"]
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("area of interest")
} 
 if(mapNewFeature>0){  mapNewFeature(1)}
  }  else {
  mapLays <-  mapLays[mapLays !="area of interest"]
  mapLays <-  mapLays[mapLays !="modify habitat"]
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("modify habitat")%>%  clearGroup("area of interest")
  }

  
 

if(show_metaRecords()==TRUE) {     #input$addMetadata_records  == "incorporate records")   {  
   mapLays <- c(mapLays,"observation records")
  if(!is.null(pointFeature()) & !is.null(pointFeature_terrPols()) & is.null(add_labelOnly())) { #obsRecs_famDf())){
        mapNewObs(1)
       add_labelOnly(TRUE) # was false # true means when toggling doesnt replot terrs..
    ### bEAut! - just replot saved polys for terrs and pts +labes - way faster 
}
     } else {
    mapLays <-  mapLays[mapLays !="observation records"]
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("observation records")
}
     
  
  if(input$operation == "translocation")   {    mapLays <- c(mapLays,"translocation set-up")  
    } else {
  mapLays <-  mapLays[mapLays !="translocation set-up"] 
}    
         
if(!is.null(datSave_settleTest()) ) {  
  mapLays <- c(mapLays,"initial settlement test")  
} else {
  mapLays <-  mapLays[mapLays !="initial settlement test"]
    leafletProxy("mapReleaseSite", session) %>% removeControl("initial settlement test")  
}  
   
 c("beaver habitat" , mapLays ) %>% cur_layers() 
 cat( c("\nmap layers : beaver habitat" , mapLays,"  - " ) )
})



 
































 



##################################### draw on map 
removeDrawToolbar <- function(map){
  invokeMethod(map,getMapData(map),method =  'removeDrawToolbar') # known bug - doesnt work once deployed
}
 
 
observeEvent(input$addMetadata_modifHab,{
  if(input$addMetadata_modifHab == "use current landscape") { ##  
      cat("hide any added modifHabfeatures  - ") 
        show_metamodifHab(FALSE) 
  }   
  if(input$addMetadata_modifHab == "modify mapped habitat") { ## reset all pars
        cat("recover modifHab features  - ") 
        show_metamodifHab(TRUE) #show table
  } 
    
}, ignoreInit=TRUE)

observeEvent(input$addMetadata_records,{
      if(input$addMetadata_records =="no existing population nearby") { ## reset all pars
         cat("exclude any added records  - ") 
         show_metaRecords(FALSE)
  } else { ## reset all pars
      cat("include added records  - ") 
         show_metaRecords(TRUE) #show table
        # add_labelOnly(NULL) 
  } 
}, ignoreInit=TRUE)
 
 
observeEvent(  input$addMetadata_areaOfInt ,{
  featureName <- "area of interest"  
       if(input$addMetadata_areaOfInt =="report overall growth") { ## reset all pars
         cat("hide area of interest - ") 
         show_metaAreaOfInt(FALSE) 
  }
  
  if(input$addMetadata_areaOfInt =="area of interest") { ## reset all pars
      cat("recover area of interest  - ") 
       show_metaAreaOfInt(TRUE) #show table 
  }
}, ignoreInit=TRUE)
   



 








  

#################################################################################### matadata input
observeEvent(metaTable_modifHab(),{ 
  req(mapExists()==TRUE) 
  refresh_legendTxt(refresh_legendTxt()+1)
  cat("update metadata modify habitat UI - ") 
  if( is.null(metaTable_modifHab()))  { 
      Nfeat_modifHab(NULL) 
   } else {
            if(nrow(metaTable_modifHab())== 0){  
               Nfeat_modifHab(0) 
            } else { # when adding a row, recompute metadata effect
                    updateRadioGroupButtons (session,"addMetadata_modifHab", selected="modify mapped habitat")
                    show_metamodifHab(TRUE)
                    print(paste0( nrow(na.exclude(metaTable_modifHab()))," features  - "))
                    as.numeric(nrow(na.exclude(metaTable_modifHab()))) %>% Nfeat_modifHab()  
              }} 
},ignoreNULL=FALSE, ignoreInit=FALSE)   
  

observeEvent(metaTable_records(),{  
 req(mapExists()==TRUE)
  cat("update metadata sightings UI - ")  
   refresh_legendTxt(refresh_legendTxt()+1) 
if( is.null(metaTable_records())) {
    Nfeat_records(NULL) } else {
  if(nrow(metaTable_records())==0) { 
               Nfeat_records(0) 
       } else { 
              updateRadioGroupButtons (session,"addMetadata_records", selected="incorporate records")
              show_metaRecords(TRUE)
              nrow(na.exclude(metaTable_records())) %>% Nfeat_records()   
    }}

},ignoreNULL=FALSE, ignoreInit=FALSE)   
   

observeEvent(metaTable_areaOfInt(),{  
 req(mapExists()==TRUE)
   refresh_legendTxt(refresh_legendTxt()+1)
   cat("update metadata areaOfInt UI - ")  
if( is.null(metaTable_areaOfInt())) {
  Nfeat_areaOfInt(0) 
  } else {
  if(nrow(metaTable_areaOfInt())==0) { 
               Nfeat_areaOfInt(0) 
       } else { 
              updateRadioGroupButtons (session,"addMetadata_areaOfInt", selected="area of interest")
              show_metaAreaOfInt(TRUE)
              nrow(na.exclude(metaTable_areaOfInt())) %>% Nfeat_areaOfInt()  
    }}

},ignoreNULL=FALSE, ignoreInit=FALSE)   
  
    
  
 #lulu
output$metaSummary_modifHab  <- renderUI({  
  if(input$addMetadata_modifHab == "use current landscape") { txt <- paste0(as.character(tagList(div(p("use current landscape, no management scenario"),style="text-align:center!important;")))) 
  } else {
   txt <-  featTxt_modifHab()  
  }
  return(HTML(txt))
  })
 

metaTitle_modifHab <- reactive({  
  Nfeat_modifHab <- Nfeat_modifHab() 
  txt <- "modify mapped habitat: no" 
    if(input$addMetadata_modifHab == "modify mapped habitat") { 
    if(is.null(Nfeat_modifHab)) {Nfeat_modifHab <- 0}
     if(Nfeat_modifHab>1) {txt <- paste0("modify mapped habitat: ",Nfeat_modifHab," features")} else {
       txt <- paste0("modify mapped habitat: ",Nfeat_modifHab," feature")
     }  }
  return(txt)
}) 


metaTitle_areaOfInt <- reactive({ 
  txt <- "area of interest: no" 
  Nfeat_areaOfInt<- Nfeat_areaOfInt()
    if(input$addMetadata_areaOfInt == "area of interest") {
    if(is.null(Nfeat_areaOfInt)) {Nfeat_areaOfInt <- 0}
     if(Nfeat_areaOfInt>1) {txt <- paste0("area of interest: ",Nfeat_areaOfInt," features")} else {
       txt <- paste0("area of interest: ",Nfeat_areaOfInt," feature")
     }  }
  return(txt)
})


metaTitle_records <- reactive({ 
  txt <- "incorporate records: no" 
  Nfeat_records <- Nfeat_records()
    if(input$addMetadata_records == "incorporate records") {
      if(is.null(Nfeat_records)) {Nfeat_records <-0}
     if(Nfeat_records>1) {txt <- paste0("incorporate records: ",Nfeat_records," features")} else {
       txt <- paste0("incorporate records: ",Nfeat_records," feature")
     }  }
  return(txt)
})
 
 
 
 
output$metaTitle_records<- renderText({metaTitle_records() }) 
output$metaTitle_areaOfInt <- renderText({metaTitle_areaOfInt() }) 
output$metaTitle_modifHab  <- renderText({ metaTitle_modifHab() }) 
  
output$metaSummary_modifHab2  <- renderUI({ # recap tetx of features added for modifHab
  txt<- NULL
   if(input$addMetadata_modifHab == "modify mapped habitat") { 
     Nunsuit_txt_new <- Nunsuit_txt_new()
     
     txt<- HTML(Nunsuit_txt_new()  )   
   }  else {  
     if(!is.null(Nfeat_modifHab())) {
       if(Nfeat_modifHab()>0) {
       txt <- paste0("(1 feature available)")
       if(Nfeat_modifHab()>1) {txt <- paste0("(",Nfeat_modifHab()," features available)")  }}}
   }
return(txt)     
  }) 
  
 featTxt_areaOfInt <- reactiveVal()
featTxt_records <- reactiveVal()
featTxt_modifHab <- reactiveVal()
Nunsuit_txt_new <- reactiveVal() 

####################################################### summary metadata                               

observeEvent(metaTable_areaOfInt(),{
  req(mapExists()==TRUE)
    featTxt_areaOfInt<- NULL
    cat(paste0(Nfeat_areaOfInt(), " area of interest feat in table  - "))
  if(!is.null(Nfeat_areaOfInt())) {
    Nfeat <- Nfeat_areaOfInt()  
            if(Nfeat==0) {featTxt_areaOfInt <- paste0("no metadata")}
            if(Nfeat>0){  featTxt_areaOfInt <- paste0("   - ", metaTable_areaOfInt()$name ," (", metaTable_areaOfInt()$size ,")",br()  )  }
  } 
   featTxt_areaOfInt %>% featTxt_areaOfInt() 
}, ignoreNULL=FALSE)   

 

     

observeEvent(metaTable_records(),{ 
  req(mapExists()==TRUE)
  featTxt_records<- NULL
  if(!is.null(Nfeat_records())) {
    Nfeat <- Nfeat_records()  
    if(Nfeat==0) {featTxt_records <- paste0("no metadata")}
    if(Nfeat>0){
    for(feat in 1:Nfeat){ ### here make ui less cumbersome if many pts? not meant  to happen though..
     featTxt_records0<- paste0("   - ", metaTable_records()$name[feat],": ", metaTable_records()$layerEffect[feat],br()  )
     featTxt_records <- paste0(featTxt_records,featTxt_records0 )
  } }
  }
  cat(paste0(Nfeat, " recs in table  - "))
  paste0( featTxt_records,br() ) %>% featTxt_records() 
}, ignoreNULL=FALSE)  

observeEvent(metaTable_modifHab(),{ 
  req(mapExists()==TRUE)
    Nfeat <- Nfeat_modifHab()  
  featTxt_modifHab <- NULL
  if(!is.null(Nfeat)) { 
    if(Nfeat==0) {featTxt_modifHab <- paste0("no metadata")}
    if(Nfeat>0){ 
    for(feat in 1:Nfeat){
     featTxt_modifHab0<- paste0("   - ", metaTable_modifHab()$name[feat], " (", metaTable_modifHab()$size[feat],"): ",  metaTable_modifHab()$layerEffect[feat],br()  )
     print(featTxt_modifHab0)
     featTxt_modifHab <- paste0(featTxt_modifHab,featTxt_modifHab0 )
  } }
  } 
 cat(paste0(Nfeat, " modified hab feat in table  - "))
  paste0( featTxt_modifHab,br() ) %>% featTxt_modifHab() 
}, ignoreNULL=FALSE)  

 

output$metaSummary_records   <- renderUI({  
  if(input$addMetadata_records  == "no existing population nearby") { txt <- paste0(as.character(tagList(div(p("no existing population nearby, no observation included"),style="text-align:center!important;")))) 
  } else {
   txt <-  featTxt_records()  
  }
  return(HTML(txt))
  })

output$metaSummary_areaOfInt   <- renderUI({  
  if(input$addMetadata_areaOfInt  == "report overall growth") { txt <- paste0(as.character(tagList(div(p("report overall growth"),style="text-align:center!important;")))) 
  } else {
   txt <-  featTxt_areaOfInt()  
  }
  return(HTML(txt))
  })
  





   


newFeaturesDat_shp <- reactiveVal(NULL) 

lock_addedMetadata <- reactive({
  val <- FALSE
  #if(!is.null(input$lock_addedMetadata)) {val <- input$lock_addedMetadata}
  return(val)
})
  
  

adding_Metadata <- reactive({
#  req(input$addMetadata_modifHab )
#  req(input$addMetadata_records )
#  req(input$addMetadata_areaOfInt)
    val <- FALSE
#  if(any(  input$addMetadata_modifHab== "modify mapped habitat", 
##          input$addMetadata_records == "incorporate records", 
#          input$addMetadata_areaOfInt == "area of interest")
#  ) {val <- TRUE}
   return(val) 
})  
 

output$ui_metadata_makeWhat <- renderUI ({   
   req(input$metadata_effect)
   if(input$metadata_effect == "incorporate records") { 
     req(metadata_effect_sichoices())
    return(  div( shinyWidgets::prettyRadioButtons(inputId="metadata_layerEffect", label=NULL, 
                        choices= metadata_effect_sichoices() , 
                        selected=metadata_effect_sichoices()[1], inline=TRUE,
                        shape = "round",   fill=TRUE, 
                        icon=icon("check")), 
                        style="padding:12px 0 0 0;text-align:center;text-align:center;display:inline;")) 
   } 
  
   if(input$metadata_effect == "modify mapped habitat") {  
            if(!is.null(current_drawnShapeGeom())){ 
           current_drawnShapeGeom <- current_drawnShapeGeom()  
           modChoices <- c("make unsuitable", "50% mortality", "75% mortality") 
           if(current_drawnShapeGeom ==  "mapGeom" & !is.null(input$metaselect_listGlob)) {    
                           if(input$metaselect_listGlob == "whole area"){ # if mapgeom,  dont include nonsense options  
                                               modChoices <- modChoices [c(2,3)] }} 
        
                return( div(  shinyWidgets::prettyRadioButtons(inputId="metadata_layerEffect", label=NULL, 
                                    shape = "round", fill=TRUE, inline =TRUE, icon=icon("check"), selected=modChoices[1],
                                    choices=modChoices), style="padding:12px 12px 0 12px;text-align: center;")  )  
           }}

    if(input$metadata_effect == "area of interest") {   
      choicename<- "summarise the simulated population's spatial spread in relation to the geometry"
          return(   div(fluidRow(column(width=10,offset=1,
                                        shinyWidgets::prettyRadioButtons(inputId= "metadata_layerEffect",  label = NULL,   
                             icon=icon("check"), fill=TRUE, choices=choicename, selected=choicename, shape = "round"   )))) 
                   ) 
    }                    
})
    
observe({
  req(input$metadata_effect)
  if(input$metadata_effect == "area of interest") { disable("metadata_layerEffect")  } else { enable("metadata_layerEffect")}
})
  
  
  
col_succ <- "#a7ca35d9"
col_info <- "#2299aacc"
col_fail <- "#fb8861d4"
col_warn <- "#e9d852de"   
txtCol_notif <- "darkmagenta"
txtCol_notif2 <- "yellow"##dcc93b

 
  


output$metadata_effect_textSub  <- renderUI({
 req(input$metadata_layerEffect) 
  if(input$metadata_effect== "modify mapped habitat") {  
      req(input$metadata_layerEffect)
           if(input$metadata_layerEffect== "make unsuitable") { 
             return(tagList(fluidRow( style="border: 1px solid #2299aacc;border-radius:20px;margin: unset !important;padding: 1vh;",
                            fluidRow(column(width=7, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("make unsuitable:", style="color:#2299aacc!important;display:inline;")  ),
                                      column(width=5,    
                                            div(img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                            div("application", style="color:#2299aacc!important;display:inline;"))),
                            fluidRow(column(width=7,
                                                div("no dispersal or settlement will be possible within the created area or across the created line", style="padding-left:6px;") ,
                                                div("families whose territory overlaps with the spatial feature will be removed from the population entirely" , style="padding-left:6px;")) ,
                                       column(width=5,
                                                 div("use to create barriers or geographical areas licensed for beaver removal assuming a 100% removal rate", style="padding-left:6px;")))   
                            ))) 
       }
           if(input$metadata_layerEffect== "50% mortality") {            
                          return(tagList(fluidRow( style="border: 1px solid #2299aacc;border-radius:20px;margin: unset !important;padding: 1vh;",
                            fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("50% mortality:", style="color:#2299aacc!important;display:inline;")  ),
                                      column(width=6,    
                                            div(img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                            div("application", style="color:#2299aacc!important;display:inline;"))),
                            fluidRow(column(width=6,
                                             div("a 50% reduction in survival will be imposed on beaver families settled within the area")),
                                       column(width=6,
                                                 div("use to represente geographical areas licensed for beaver removal, assuming a 50% removal rate", style="padding-left:6px;")))   
                            ))) 
             }
                                                                         
           if(input$metadata_layerEffect== "75% mortality") { 
             return(tagList(fluidRow( style="border: 1px solid #2299aacc;border-radius:20px;margin: unset !important;padding: 1vh;",
                            fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("75% mortality:", style="color:#2299aacc!important;display:inline;")  ),
                                      column(width=6,    
                                            div(img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                            div("application", style="color:#2299aacc!important;display:inline;"))),
                            fluidRow(column(width=6,
                                             div("a 75% reduction in survival will be imposed on beaver families settled within the area")),
                                       column(width=6,
                                                 div("use to represent geographical areas licensed for beaver removal, assuming a 75% removal rate", style="padding-left:6px;")))   
                            ))) 
             }
 }
  if(input$metadata_effect== "incorporate records") {  
                 return(tagList(   fluidRow( style="border: 1px solid #2299aacc;border-radius:20px;margin: unset !important;padding: 1vh;",
                            fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("observation records:", style="color:#2299aacc!important;display:inline;")  ),
                                     column(width=6,    
                                            div(img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                            div("application", style="color:#2299aacc!important;display:inline;"))  
                                     ),
                            fluidRow( column(width=6,
                                             div(metadata_effect_sitxt_sim(), style="padding-left:6px;")),
                                       column(width=6,
                                                 div(metadata_effect_sitxt(), style="padding-left:6px;"))),
                            
                          fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("about", style="color:#2299aacc!important;display:inline;")  ),
                                     column(width=6,    
                                      hidden(    tags$div(id="div_uplFormat_ttl", 
                                              div(img(src="attention-teal.png", width="17px", height="17px"),style="display:inline"),
                                              div( "input format", style="color:#2299aacc!important;display:inline;")) ) )
                                     ),
                            fluidRow( column(width=6,
                                             div(metadata_effect_sitxt_sim0(), style="padding-left:6px;")),
                                       column(width=6,
                                                 hidden(tags$div(id="div_uplFormat",metadata_effect_siFor, style="padding-left:6px;"))))    
                            
                            ))) 
  }
     
   
   
  if(input$metadata_effect== "area of interest") {  
                 return(tagList(   fluidRow( style="border: 1px solid #2299aacc;border-radius:20px;margin: unset !important;padding: 1vh;",
                            fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                             div("area of interest:", style="color:#2299aacc!important;display:inline;")  ),
                                     column(width=6,    
                                            div(img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                            div("application", style="color:#2299aacc!important;display:inline;"))  
                                     ),
                            fluidRow( column(width=6,
                                             div( "generate metrics to describe how the spatial spread of the simulated population changes annually in relation to a location of interest", style="padding-left:6px;")),
                                       column(width=6,
                                                 div("use to assess how near and how soon a population may get to a spatial geometry such as infrastructure, adjacent catchment, licensed area", style="padding-left:6px;"))),
                            
                          fluidRow(column(width=6, div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                         div( "reported metrics", style="color:#2299aacc!important;display:inline;"    ) , 
                                       
                                              div(span("distance to:", style="color:#2299aacc!important;"),"is the minimum distance oberved between occupied cells and the feature of interest", style="padding-left:12px;"),
                                              div(style="padding-left:12px;",span("1km surrounding: ", style="color:#2299aacc!important;")," describes the area occupied by the simulated population within 1km of a feature of interest (i.e. around but not within the geometry)", style="padding-left:12px;padding-top:7px;"),
                                              div(span("shared area: ", style="color:#2299aacc!important;")," quantifies the overlap of both features to describe the surface occupied within the area of interest", style="padding-left:12px;padding-top:7px;")
                                              
                                            
                                           ) ,
                                  column(width=6,
                                             
                                           div(div( img(src="info-teal.png", width="17px", height="17px"),style="display:inline"),
                                              div( "units", style="color:#2299aacc!important;display:inline;"),style="padding-top:17px;"),
                                              div("annual values for distances in km and for area occupied in ha", style="color:darkmagenta!important;"),
                                        div("values do nt descrive an 'average population' but the potential for space to be occupied as the population would grow. it adds movemen", style="color:darkmagenta!important;"),

                                              div("the number of occupied 1ha cells is returned for any level of occupancy (binary, any non-null number of repetitions) and for cells frequently occupied(>80% repetitions) when applicable", style="color:darkmagenta!important;"),
                                              div("only relevant metrics are displayed depending on whether the geometries overlap at any point", style="color:darkmagenta!important;") 
                                             
                                         
                                  )),
                                         
                           
                            fluidRow(column(width=12,
                                            div(div( img(src="idea-teal.png", width="17px", height="17px"),style="display:inline"),
                                      div( "interpretation", style="color:#2299aacc!important;display:inline;"),style="padding-top:17px;"),              
                                         
                           div(span(style="color:#2299aacc!important","output type:"),
                        "zone 1 - shared area, year 2026, any: 20ha  - >80%: 10ha", style="color:darkmagenta!important;"), 
                           div(span(style="color:#2299aacc!important","an interpretation:"), 
    "30 repetitions of the local beaver population growth simulation in conditions representative of our strategy generated a spatial ouput  
    suggesting the area of interest may be colonised by year 2026. 
    More particularly, 20ha were identified with potential for occupancy (including 10ha of higher recurrence in the simulated population spread)", style="color:darkmagenta!important;padding-top:7px;")
                           )))
 )) 
  }
   
})  
   
                      


                             
metadata_effect_sitxt_sim0 <- reactive({
 txt <- ".."
  req(current_drawnShapeGeom()) 
 current_drawnShapeGeom <- current_drawnShapeGeom()
 if(current_drawnShapeGeom == "point")  {  txt <- "if local habitat is insufficient, the simulation will fail"}
 if(current_drawnShapeGeom == "upload") {  txt <-  "if local habitat is insufficient, a territory will be simulated at the nearest suitable location or within dispersal habitat if the distance >?CHECK?km" }
  return(txt)
})   


metadata_effect_siFor <-   "either a table (.csv) or shapefile (.shp/.shx, .dbf, .sbn/sbx, .prj) containing one point or row per territory location" 
    
 

 
metadata_effect_sitxt_sim<- reactive({
 txt <- ".."
  req(current_drawnShapeGeom()) 
 current_drawnShapeGeom <- current_drawnShapeGeom()
 if(current_drawnShapeGeom == "point")  {  txt <- "a territory will be simulated at the point location"}
 if(current_drawnShapeGeom == "upload") {  txt <-   "a territory will be simulated at each point location " }
 return(txt)
})  
 
 
metadata_effect_sitxt <- reactive({
  txt <- ".."
  req(current_drawnShapeGeom()) 
 current_drawnShapeGeom <- current_drawnShapeGeom()
 if(current_drawnShapeGeom == "point")  { txt <- "use to incorporate an existing beaver territory into the simulation"}
 if(current_drawnShapeGeom == "upload") { txt <- "use to incorporate one or more existing beaver territories into the simulation using coordinates" }
 return(txt)
})  
 





  
metadata_effect_sichoices <- reactive({
  req(current_drawnShapeGeom()) 
 txt<- "territory identified on map"
  #if(current_drawnShapeGeom == "point")  {txt <- "territory identified on map"}
  if(current_drawnShapeGeom() == "upload") {txt <- "territory from uploaded coordinates"}
 return(txt)
})    ### check bug -----update obsRecs
   

 
output$metadata_effect_text  <- renderUI({
  req(input$metadata_effect)
 if(input$metadata_effect== "modify mapped habitat") { return("modify beaver habitat quality to reflect loss of habitat or population management via zonal licensing")}
 if(input$metadata_effect== "incorporate records") { return("incorporate observed beaver territories into the simulation model")}
 if(input$metadata_effect== "area of interest") { return("generate a detailed output report over an area of interest")}
  })

output$metadata_duration_title <- renderUI({ 
    req(metadata_duration_slider() ) # updated with matching vals
    req(input$metadata_effect != "area of interest")
    cat("slider ttl  - ")
  
    if(input$metadata_effect== "incorporate records"){  return( as.character(paste0("year of observation"))) }
    if(input$metadata_effect== "modify mapped habitat") { return(  as.character(paste0("effect duration"))) }
    if(input$metadata_effect== "area of interest") { return(NULL) }
})  
  
  
output$metadata_duration_text <- renderUI({ 
  req(mapExists()==TRUE)
  req(input$metadata_effect != "area of interest")
  req(metadata_duration_slider() ) # updated with matching vals
  req(input$metadata_layerEffect) 
  req(input$metadata_duration)
  cat("slider txt   - ") 
   
    if(input$metadata_effect== "incorporate records"){  
    return( as.character(paste0("beaver territory most recently observed on year ",input$metadata_duration[1])))
              } else {  
                req(input$metadata_duration[2])
      startY <- input$metadata_duration[1]
      endY   <- input$metadata_duration[2] 
      if(startY ==  endY ) { return( paste0("apply effect on year ",startY , " only")) 
      } else {
             return( paste0(input$metadata_layerEffect," from year ", startY, " to ", endY, " included ( ",length(seq(startY,endY)) , " years)")) }   
               }
})
 

output$new_metadata_title <- renderText({  newfeature_title()  })

####################################################################### start vals uploaded feature
observeEvent(input$metadata_upload, {
  cat("click new upload  - ")  
  "new feature upload" %>% current_metaShape_placeholder()
  "new feature upload" %>% newfeature_title()
  "upload" %>% current_drawnShapeGeom()
    invalidGeom_warn(NULL) 
}) 
     
output$ui_metadata_upload <- renderUI ({
 req( newfeature_title()=="new feature upload")
  div(fileInput("fileUpload_metadata", label="spatial information upload", buttonLabel=  div( img(src="upload.png", width="16px", height="16px")), 
                          placeholder ="new feature upload",
              accept= c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj",".csv"), multiple=TRUE,width ="100%") ) #  %>% ui_metadataUpload()
   })   # accept zipped files to and unzip here?

 outputOptions(output, "ui_metadata_upload", suspendWhenHidden = FALSE)  
#output$ui_metadata_upload <- renderUI ({
 #   fileInput("fileUpload_metadata", label="spatial information upload", buttonLabel=  div( img(src="upload.png", width="16px", height="16px")), placeholder =current_metaShape_placeholder(),
#              accept= c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj",".csv"), multiple=TRUE,width ="100%") 
#})   # accept zipped files to and unzip here?
   
############################################################### effect names to select for metadata 
uploadGeomType <- reactiveVal(NULL)  
  
## c( "modify mapped habitat", "incorporate records",  "area of interest"),

 

observeEvent({ 
   uploadGeomType()
   input$metadata_upload
   current_drawnShapeGeom()
   1
},{ 
   req(current_drawnShapeGeom())
   req(mapExists()==TRUE)
   cat("filter available effects  - ")
  effectNames <- c("modify mapped habitat",  "incorporate records", "area of interest")
  current_drawnShapeGeom <- current_drawnShapeGeom()
 if(current_drawnShapeGeom == "line") {
effectNames <- c( "modify mapped habitat", "area of interest")
 } 
 if(current_drawnShapeGeom ==  "polygon") {
effectNames <- c("modify mapped habitat", "area of interest")
 } 
 if(current_drawnShapeGeom ==  "upload") {
 if(is.null(uploadGeomType())){
   effectNames <- c("modify mapped habitat",  "incorporate records", "area of interest")
} else {
  if(uploadGeomType() == "point") {  effectNames <-   "incorporate records" }
  if(uploadGeomType() == "poly")  {  effectNames <- c( "modify mapped habitat", "area of interest") }
}}

 if(current_drawnShapeGeom ==  "mapGeom") {
   layersIncluded <- c(input$metaselect_listGlob, input$metaselect_listCustom)
   if( "whole area"  %in% layersIncluded)  {
   effectNames <- c( "modify mapped habitat")
   } else {  
   effectNames <- c( "modify mapped habitat", "area of interest")
   }
 }
 if(current_drawnShapeGeom ==  "point") {
effectNames <- c( "incorporate records")
 } 
 effectNames %>% effectNames()
},ignoreNULL=TRUE) 


 

############################################################### modal dialogue for metadata

########### if uploading metadat while sim running - warning message
output$UImodel_running <- renderUI({
  model_running <- model_running()
  warning_model_running  <- NULL
  if(base::isTRUE(model_running)) {warning_model_running <- "simulation in progress - changes will not be included in the output"}
  return(warning_model_running)
})
 
 

observeEvent(input$use_yearinDat,{
  if(is.null(input$use_yearinDat)){
    enable("metadata_duration") 
  } else { 
      if(input$use_yearinDat==TRUE) {disable("metadata_duration")}
      if(input$use_yearinDat==FALSE){enable("metadata_duration")} 
  }  
},ignoreInit=FALSE, ignoreNULL=FALSE)




output$saveShape_text<- renderText({ saveShape_text()   }) 
saveShape_text <- reactiveVal(NULL)

output$metadata_duration_slider <- renderUI({ metadata_duration_slider() })

thisYear <- reactive({isolate(as.numeric(format(Sys.time(),"%Y")) )})


metadata_duration_slider <- reactive({ 
   req(input$metadata_layerEffect)
  cat("\nsim duration  - ")  
  req(input$metadata_effect != "area of interest")
 sli <- NULL
 val <- thisYear()
 if(input$metadata_effect== "incorporate records" ) { 
    sli <-  div(class="effDurationSlider",
                sliderInput(inputId = 'metadata_duration',
                  label = NULL,  sep = "", dragRange =FALSE, 
                  value =  val  , round = TRUE, step=1,  
                  min = val-10  , max = val ) ) 
  } 
    if(input$metadata_effect== "modify mapped habitat" ) {
    val <- c(val,val+10) 
    sli <-  div( sliderInput(inputId = 'metadata_duration',
                  label = NULL,  sep = "", 
                  value =  val  , round = TRUE, step=1, #post ="yr",
                  min = val[1]-5, max = val[2]) )  
    } 
  return(sli)
}) 

new_metadata_lays <- renderUI({ 
 req(current_metaShape_placeholder()  == "selected map geometry")  
effNames <- c(input$metaselect_listGlob, input$metaselect_listCustom)
if("none" %in% effNames) {effNames <- effNames[-which(effNames=="none")]}
return(paste0("apply to ",effNames))  
}) 







 
 




 



input_fileUpload_metadata <- reactiveVal(NULL) 

observeEvent(input$fileUpload_metadata,{
 disable("metadata_submit")

  cat("\nnew upload  : ")
  print(input$fileUpload_metadata)
  req( input$fileUpload_metadata) 
  geom <-  NULL 
  error_message1 <- NULL
 
  if(is.null(input$fileUpload_metadata)) { 
    error_message1 <- "no data"
    } else {  
    shpdf <-input$fileUpload_metadata
    cat("check input  - ")  
    
   if(any(file_ext(shpdf$name) %in% c( "shp", "dbf", "sbn", "sbx", "shx", "prj"))) {
      cat("shapefile  - ")  
     NmandatoryGISFiles <- sum(as.numeric(c( "shp", "dbf", "shx") %in% file_ext(shpdf$name))) 
      print(NmandatoryGISFiles) 
     if( NmandatoryGISFiles<3) { 
       cat("shapefile not complete -  ")
        error_message1 <- "shapefiles not complete - check file formats and try again!"
         input_fileUpload_metadata <- NULL  
          } else {
          cat(" ok  - ")       
              tempdirname <- dirname(shpdf$datapath[1])
                for (i in 1:nrow(shpdf)) {
                  file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]) )
                }
                indx <- grep(pattern = "*.shp$", shpdf$name)
                if(length(indx)>0){  
                  shpData  <- st_read(paste(tempdirname,shpdf$name[indx],sep = "/" )) 
                   if( any(c("POINT","MULTIPOINT") %in% st_geometry_type(shpData) ) ) {geom <-   "point"}
                shpData  <- shpData  %>%st_as_sf()
                 input_fileUpload_metadata <- shpData 
                 }
                }
    } 
 
   if(is.null(error_message1) & any(file_ext(shpdf$name) %in%  "csv" & length(shpdf$name)<4))  {
     if(length(shpdf$name)>1) {
        error_message1 <- "more files than required - subsetting single .csv" 
        print("more files than required - subsetting .csv")
        shpdf <- shpdf[which(file_ext(shpdf$name) %in%  "csv")[1],]
     }
        input_fileUpload_metadata <- shpdf   
         cat(" csv file   - ") 
         geom <-"point" 
        }      
    }  
if(!is.null(error_message1)) {
      notify_failure("upload cancelled" , timeout=4000,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl", 
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
      notify_warning(error_message1 ,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
      
     if(base::isTRUE(playSound())){
     insertUI(selector = "#placeholder_timing",  # beep.wav should be in /www of the shiny app
                 where = "afterEnd", 
                 ui = tags$audio(src = "beep-problem.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")  ) }
 } else {  
 geom %>% uploadGeomType()   
    
}
input_fileUpload_metadata %>% input_fileUpload_metadata()
},ignoreInit=FALSE,ignoreNULL=TRUE)
    

pointUpload <- reactive({ as.numeric(base::isTRUE(newfeature_title() == "new feature upload")) +as.numeric(input$metadata_effect == "incorporate records") ==2})
 #### newfeature_title
###effectNames() doesnt refresh after first upload!!
###
output$uimetadata_effect <- renderUI({
effectNames()
      tags$div(  selectInput(inputId= "metadata_effect",
                                  label = 'effect type',  choices=effectNames(), 
                                  selected =effectNames()[1],
                                  multiple = FALSE, selectize = TRUE, width = "auto"),style="padding-top:2vh;")
})
effectNames <- reactiveVal(NULL)
observeEvent(effectNames(),{
   updateSelectInput(session,inputId= "metadata_effect",   choices=effectNames(),  selected =effectNames()[1]) 
},ignoreNULL=TRUE)

 

observeEvent(current_metaShape_placeholder(),{  
   current_metaShape_placeholder <- current_metaShape_placeholder()
  cat(paste0("\nadding metadata: ", current_metaShape_placeholder, "  - "))
 

    showModal(modalDialog( size =  "m",
              tags$h3(textOutput("new_metadata_title"), style="text-align:center;"), 
               div(uiOutput("invalidGeom_warn_ui")  , style="height:17px;text-align:center;color:#2299aacc;font-size:90%;"),

               div(uiOutput("new_metadata_lays")  ,style="height:17px;text-align:center;color:magenta;font-size:90%;"),
              div(uiOutput("UImodel_running")  ,style="height:17px;text-align:center;color:magenta;font-size:90%;"),

    
              # drop-down menu with category preselected but same for all - saves time etc! 
              fluidRow(style="height:14vh;",
                       column(6,  uiOutput("uimetadata_effect")),
                       column(5,  tags$p(uiOutput("metadata_effect_text"), style="padding-top:2vh;vertical-align: middle;") )),  
              fluidRow( uiOutput("ui_metadata_makeWhat")),
              fluidRow( div(uiOutput("metadata_effect_textSub"), style="color:darkmagenta;vertical-align: middle;font-size:90%;text-align:left;padding: 2vh 2vw;" )),
                 
           #   fluidRow(column(10,offset=1 , div(style="width: 50%;",uiOutput("ui_metadata_upload")) )),
                
              fluidRow( column(6, 
                           # uiOutput("ui_metadata_upload") ,
                             uiOutput("ui_metadata_upload") ,
                            div(uiOutput("metadata_duration_title" ) ,style="font-weight:bold;margin-left:30px;"),
                            div(uiOutput("metadata_duration_text")  ,style="margin-left:30px;font-size:90%;color:darkmagenta;"),
                            tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hide minor ticks
                            div(uiOutput("metadata_duration_slider")  ,style="width:80%;float:right;")  ),
                       column(6, 
                             div( textInput('metadata_drawName', 'spatial feature name' , placeholder=  current_metaShape_placeholder()), style="padding:0!important;margin-right: 12px;"),
                             
             conditionalPanel(condition = "output.pointUpload == 1",
                     
                    div(  style="display:inline-flex;align-items: center;font-size:90%;padding-left:12px;padding-top:24px;",
                           p("look for year values in data?" ),
                           materialSwitch(inputId = "use_yearinDat", label =  NULL, status = "danger")) ,
                          
                     div(  style="display:inline-flex;align-items: center;font-size:90%;padding-left:12px;",
                           p("look for demographics values in data?"),
                           materialSwitch(inputId = "use_demoginDat", label =  NULL, status = "danger")) 
             ),
             
                              ) ), 
                
               
              
            footer=tagList( div( actionButton("metadata_submit",label="submit") 
                          ,style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;")  , #  
                          div(actionButton('metadata_cancel','cancel' ),style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;") 
                        )#,
              
  #  bsTooltip( "metadata_cancel","metadata_cancel",placement = "top", trigger = "hover"), 
  #  bsTooltip( "metadata_submit","metadata_submit",placement = "top", trigger = "hover"),
  #  bsTooltip( "metadata_effect","metadata_effect",placement = "left", trigger = "hover"),
  #  bsTooltip( "metadata_drawName","metadata_drawName",placement = "bottom", trigger = "hover"), 
  #  bsTooltip( "metadata_duration_title","metadata_duration_title",placement = "left", trigger = "hover"), 
  #  bsTooltip( "ui_metadata_makeWhat","ui_metadata_makeWhat",placement = "left", trigger = "hover") 
            ))    
      
 
})
     
reset_fileInput <- reactiveVal(0)
output$invalidGeom_warntxt <- renderText({invalidGeom_warn()})


output$invalidGeom_warn_ui <- renderUI({  
 req( invalidGeom_warn() ) 
 # textOutput("invalidGeom_warntxt")
   return(   div(img(src="check_@2X.png", width="12px", height="12px"), div(style="margin-left:7px;color: magenta;", textOutput("invalidGeom_warntxt")),   style="display:inline-flex;"))  

  })

 
    

observe({
  req(newfeature_title())
  input$metadata_drawName
   if( newfeature_title() == "new drawn geometry") { 
     print("boooya________________")
    enable("metadata_submit")
    invalidGeom_warn(NULL) 
      }  
      
}) 
  
 
 
newFeature_tempName <- reactiveVal()


newdrawnShape <- reactiveVal()
current_drawnShapeGeom <- reactiveVal(NULL) # type of geom being submitted - created with default vals before submit - use name for placeholder
current_metaShape_placeholder <- reactiveVal()
     
 
######### process new metadata input ######## 
observeEvent(input$metadata_submit,{
 req(!is.null(current_drawnShapeGeom()))
  #req(input$metadata_duration) #  Sys.sleep(1) # avoids crash if clicking before all input loaded?
  cat("\nsubmit metadata  - ") 
    newdrawnShapegeom <- current_drawnShapeGeom() # shape only - attributes not yet informed by user until modal
    cat(paste0("geom source: ",newdrawnShapegeom,"  - "))
      if(!newdrawnShapegeom %in% c("mapGeom","upload")){    deleteDrawnShape(drawnshapes()) }
      
   
    if(newdrawnShapegeom == "upload" &  is.null(input_fileUpload_ready())){ # foolproof user cicked submit but no file
      notify_failure("no file selected",timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_sgl",  
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))   
    } else {
    input_fileUpload_metadata(NULL)
 #         reset_fileInput(reset_fileInput()+1) 
 #     cat("fileInput reset  -")
    }
    req( newdrawnShapegeom != "upload" | !is.null(input_fileUpload_ready()))  #foolproof if submit with no uploaded file
    input_fileUpload_ready(NULL)
     
   ## if cicked submit but no data: disabled btn from starts
    
 newdrawnShape <- newdrawnShape_geoms <- NULL  
if(newdrawnShapegeom == "mapGeom")   {
  mapGeomFeature <-  mapGeomFeature()
  effNames <- c(input$metaselect_listGlob, input$metaselect_listCustom) 
  print(effNames)
  print(input$metadata_effect)
 #Ngeoms <- length(effNames) -as.numeric("none" %in% effNames)
  ## global mapped features
  #  if( any( c("river catchment" ) %in% effNames )) {  #,"whole area"
    
 
  if("river catchment" %in% effNames) {
    Nst <- length(which( substring(mapGeomFeature$LeafletId,1,1) == "c"))+1
    selectedCatch_meta <- selectedCatch_meta()
    print(paste0("catchments included  :",c(selectedCatch_meta)))
      newdrawnShape <- data.frame(
      LeafletId =  "cLeaflet" ,# intercatch_on_map()$LeafletId[intercatch_on_map()$WB_NAME %in% selectedCatch_meta ],
      name =intercatch()$WB_NAME[intercatch()$WB_NAME %in% selectedCatch_meta ],
      type =  paste0("<img src='map-select.png' height='14' width='14'></img>")  ,
      size =   paste0(round(as.numeric(st_area(intercatch() [intercatch()$WB_NAME %in% selectedCatch_meta, ])*1e-4),-2),"ha"),
      start =1,
      duration=1,  
      layerType= c("modify habitat", "observation records", "area of interest")[ 
                        which( c( "modify mapped habitat", "incorporate records", "area of interest") %in% input$metadata_effect)] ,
      layerEffect=  "seeBelow") 
      print(newdrawnShape )
      
      newdrawnShape$LeafletId = paste0(newdrawnShape$LeafletId,Nst+seq(1:nrow(newdrawnShape)))
      
     if( newdrawnShape$layerType[1] != "area of interest"){
      newdrawnShape$layerEffect <-  input$metadata_layerEffect   
       } else {
      newdrawnShape$layerEffect <- paste0("river catchment, ",newdrawnShape$size)#no need but will show up in labs and recap tables
       }
      newdrawnShape <- cbind(newdrawnShape,st_geometry(intercatch() [intercatch()$WB_NAME %in% selectedCatch_meta, ]))%>% st_as_sf()
}
     print(newdrawnShape)
      
     
  ## user custom mapped features   
  if(length(input$metaselect_listCustom)>0 ){ 
    Nst <- length(which( substring(mapGeomFeature$LeafletId,1,1) == "s"))+1
    if(!is.null(newdrawnShape)) {Nst <- Nst + nrow(newdrawnShape)} 
    newFeature <- newFeaturesDat_shp()  ##
    #newFeature <- newFeature[] ## remove duplicated names for duplicated features selected on map
    print(newFeature)
    newdrawnShape_geoms <- newFeature[newFeature$name %in% input$metaselect_listCustom, ] 
    newdrawnShape_geoms$LeafletId <-  paste0("s",newdrawnShape_geoms$LeafletId)
    newdrawnShape_geoms$layerType= c("modify habitat",  "area of interest")[ 
                        which( c( "modify mapped habitat",   "area of interest") %in% input$metadata_effect)]  
     if( newdrawnShape_geoms$layerType[1] != "area of interest"){
      newdrawnShape_geoms$layerEffect <-  input$metadata_layerEffect   
       } else {
         sizes <-
      newdrawnShape_geoms$layerEffect <- paste0( newdrawnShape_geoms$layerEffect,", ",newdrawnShape_geoms$size)#no need
       } 
  } 
  } 

if(!is.null(newdrawnShape_geoms)){
      newdrawnShape <- rbind(newdrawnShape, newdrawnShape_geoms)
}

      
if(newdrawnShapegeom == "upload")   {
       cat("upload  df - ")
       newdrawnShape <-  uploadFeature()[1,] }  
if(newdrawnShapegeom == "line")   {
       cat("line  df - ")
       newdrawnShape <-   lineFeature()[1,] } 
if(newdrawnShapegeom == "polygon")   {
      cat("polygon  df - ")
    newdrawnShape <-  polyFeature()[1,]}
if(newdrawnShapegeom == "point")   { 
  cat("point  df - ") # drawn shape is a point (flag or pin)
 newdrawnShape <-  pointFeature()[1,] %>% st_cast("POINT")
 }
    ## note if  newdrawnShapegeom == "mapgeom")    newdrawnShape + newdrawnShape_geoms computed above  
     
 cat("add attributes - ")
if(input$metadata_drawName != "") {  
     if(nrow(newdrawnShape)>1) {
       newdrawnShape$name <- paste0(input$metadata_drawName,"(",newdrawnShape$name,")" )
     } else {
       newdrawnShape$name <- input$metadata_drawName 
}}#  only combine names if naming a bunch of geoms with one (user-selected) Id  

     if(input$metadata_effect == "incorporate records")   { dur <- 1 }   #pt per terr shp, only 1 year obs
     if(input$metadata_effect == "modify mapped habitat")   {     dur <- paste0(length(seq(input$metadata_duration[1],input$metadata_duration[2])) , "yr") }
     if(input$metadata_effect == "area of interest")   { 
        dur <- "through" 
        newdrawnShape$start <- "init" } else {  newdrawnShape$start <- input$metadata_duration[1] }
    
  newdrawnShape$duration <-  dur
  newdrawnShape$layerType <- c("modify habitat", "observation records", "area of interest")[ 
                        which( c( "modify mapped habitat", "incorporate records", "area of interest") %in% input$metadata_effect)] 
 newdrawnShape$layerType[1] %>% featureName() 
 if( newdrawnShape$layerType[1] != "area of interest"){
 newdrawnShape$layerEffect <-  input$metadata_layerEffect   
} else {
  if(newdrawnShapegeom != "mapGeom")   {
    newdrawnShape$layerEffect <- paste0("detailed output, ",newdrawnShape$size)#"custom"#no need for effect name for this feature, may be more than 1 selected geom at
  }
}
   
 if(newdrawnShapegeom == "line")   { 
   lineFeature <- lineFeature()
   lineFeature[1,] <- newdrawnShape
   lineFeature %>%   lineFeature()
        } 
 if(newdrawnShapegeom == "polygon")   {
    polyFeature <- polyFeature()
    polyFeature[1,] <- newdrawnShape
    polyFeature %>%  polyFeature()
 } 
 if(newdrawnShapegeom == "point")   {
   pointFeature <- pointFeature()
   newdrawnShape$size <- "1terr"
   newdrawnShape$LeafletId <- pointFeature$LeafletId[1]
   pointFeature[1,] <- newdrawnShape
   pointFeature %>%  pointFeature()
 } 

 if(newdrawnShapegeom == "upload")   {
    uploadFeature <-  uploadFeature()
    if(newdrawnShape$layerType != "observation records"){
     cat("----  uploading landscape geom  - ")
      uploadFeature[1,] <- newdrawnShape
      uploadFeature %>%   uploadFeature()
 }  else {
 cat("----  uploading obs pts  - ") 
  pointFeature <- pointFeature()
   newdrawnShape$size <- "1terr" 
  st_agr(newdrawnShape)<- "constant" # avoids sf message
    newdrawnShape <- newdrawnShape %>% st_cast("POINT")
   newdrawnShape$LeafletId <-  uploadFeature$LeafletId[1]
   if(nrow(newdrawnShape)>0) {
   newdrawnShape$LeafletId <- paste0(newdrawnShape$LeafletId,"_", seq(1:nrow(newdrawnShape)))
   newdrawnShape$name   <- str_replace(newdrawnShape$name,"upload_","fam ") # remove id past _ symbol for different selection sets
   newdrawnShape$name   <- paste0(newdrawnShape$name,"_", seq(1:nrow(newdrawnShape)))
   }
   Nterrs <- nrow(newdrawnShape)
   
     
    colW <- round(80/sum(as.numeric(input$use_yearinDat)+as.numeric(input$use_demoginDat)*2))
    yearinDat <- feminDat <- malinDat <- p("")
    nval <-"value"
    if(Nterrs>1){nval<- "values"}
  if(input$use_yearinDat==TRUE){
            if(!is.null(newuploadFeature_yearColumn())){
              minYr <- min(newuploadFeature_yearColumn(), na.rm=TRUE)
              maxYr <- max(newuploadFeature_yearColumn(), na.rm=TRUE)
               
              if(minYr!=maxYr){
                yrstxt <- paste0("(",minYr,"-",maxYr,")")} else {yrstxt <- paste0("(",minYr,")")}
              
              yearinDat <- div(img(src="check_@2X.png", width="12px", height="12px"),    
                               p(paste0("found ",nval," for 'year' in data ",yrstxt) ), 
                               style=paste0("'width:",colW,"%;display:inline-block;vertical-align: middle;"))  
                } else {
              yearinDat <-  div(img(src="attention.png", width="12px", height="12px"),    
                               p(paste0("year not found in data, amend default value in the table if required") ), 
                               style=paste0("'width:",colW,"%;display:inline-block;vertical-align: middle;"))                }
  } 
    
  if(input$use_demoginDat==TRUE){
     if(!is.null(newuploadFeature_femColumn())){
              feminDat <- tagList(div(img(src="check_@2X.png", width="12px", height="12px"), style="display:inline;text-align:center;vertical-align: middle;"),
                     p(paste0("found ",nval," for female abundance in data") , style=paste0("'width:",colW,"%;display:inline-block;vertical-align: middle;'") ) )
                } else {
              feminDat <-  div(img(src="attention.png", width="12px", height="12px"),    
                               p(paste0("female abundance not found in data, amend default values in the table if required") ), 
                               style=paste0("'width:",colW,"%;display:inline-block;vertical-align: middle;")) 
                }
    
    if(!is.null(newuploadFeature_malColumn())){
              malinDat <- tagList(div(img(src="check_@2X.png", width="12px", height="12px"), style="display:inline;text-align:center;vertical-align: middle;"),
                     p(paste0("found ",nval," for male abundance in data") , style=paste0("width:",colW,"%;display:inline-block;vertical-align: middle;") )) 
                } else {
              malinDat <- div(img(src="attention.png", width="12px", height="12px"),    
                               p(paste0("male abundance not found in data, amend default values in the table if required") ), 
                               style=paste0("'width:",colW,"%;display:inline-block;vertical-align: middle;"))                }
  }
    
    if(Nterrs==1){ Nterrs <- "1 beaver territory"  }else{ Nterrs <- paste0(Nterrs," beaver territories") }
    
      beavTxt <- div( p(style="color:#2299aacc;text-align:center;", paste0("creating ",Nterrs," from uploaded point coordinates"),
                      p(style="color:darkmagenta;text-align:center;", paste0("..this may take a while..")) , style="padding: 2vh 10px;font-size:12px;vertical-align: middle;text-align:center;")) 
     
           showModal(modalDialog( size =  "s",       
                     div(img(src="beaver-facing-right.png", width="37px", height="19px"), style="padding:4vh 0 0 0;text-align:center;vertical-align: middle;") ,
                     beavTxt  , 
                     div( yearinDat,feminDat,malinDat ,
                         style="padding:9px; vertical-align: middle; text-align: center; color: darkmagenta;"),
                        br(),
          footer=tagList(
             div(actionButton('dismiss','ok'),style="width: fit-content;display: inline-block;") 
          )))   
         
   if(input$use_yearinDat==TRUE & !is.null(newuploadFeature_yearColumn())){newdrawnShape$start <-  newuploadFeature_yearColumn()[1:nrow(newdrawnShape)]}
   newuploadFeature_yearColumn(NULL)   
   newuploadFeature_malColumn(NULL)
   newuploadFeature_femColumn(NULL)
   ### uploaded pts land into the pointfeature df (no longer in upload df)

   pointFeature <- na.exclude(pointFeature)#[-1,] #### check this? removes first row when upload after click only if incomplete -so works with both upload and drawn pts
   pointFeature  <- rbind(newdrawnShape,pointFeature)
   pointFearure <- na.exclude(pointFeature)
   pointFeature %>%  pointFeature()
   newdrawnShapegeom <- "point_upload" # to trigger terr computation below
   if(nrow( uploadFeature)>1){ uploadFeature  <- uploadFeature[-1,]} else  {uploadFeature <- NULL} # remove from uploadfeature df
      uploadFeature %>%   uploadFeature()
  }}
 if(newdrawnShapegeom == "mapGeom")   {
    mapGeomFeature <-  mapGeomFeature() 
    cat("ba")  
    mapGeomFeature <- rbind(newdrawnShape,mapGeomFeature)  
    mapGeomFeature %>% mapGeomFeature()  
  selectedCatch_metaTxt(NULL)
  selectedCatch_meta(NULL) # reset to reset maps and lsit
 }   # new created geoms in df now independent from those used to create them
      
        
  #  hab3857 <-  hab_3857() 

  if( newdrawnShapegeom  %in% c("point_upload","point") ){#newdrawnShape$layerType == "observation records" ){#
    disable("addMetadata_records") #vava
               if( newdrawnShapegeom == "point_upload") { 
                        cat(paste0(nrow(newdrawnShape),"uploaded point(s): making  geoms (...) - "))
                            
                            # terrFrom_uploadedPts <-  func_makeTerr_uploadedObs(uploadedPts=newdrawnShape,hab3857=hab3857) 
                           #  print(terrFrom_uploadedPts)   ## note terrFrom_uploadedPts is a list here, names are leafletIds  
                            #  hab3857 <- terra::wrap(hab3857)   
                              hab3857w <-  hab_3857w()
                               future_promise({func_makeTerr_uploadedObs(uploadedPts=newdrawnShape,hab3857=hab3857w)}, seed=NULL) %...>%  terrFrom_uploadedPts() 
                            #  terrFrom_uploadedPts <- func_makeTerr_uploadedObs(uploadedPts=newdrawnShape,hab3857=hab3857)
                             # terrFrom_uploadedPts %>%  terrFrom_uploadedPts() 
                      if(model_running()==FALSE){ # ie doing that while model running  
    sim_in_prog2 <- paste0(nrow(newdrawnShape), " observed territories" ) 
    sim_in_prog2 %>%  sim_in_progress2()
    sim_in_prog2 %>%  sim_in_progress2_inputTab()
    sim_in_prog2 %>% sim_in_progress2_outputTab() 
    sim_in_prog2 %>% sim_in_progress2_aboutTab() 
   
    sim_in_prog <- "simulating from upload"
    sim_in_prog %>% sim_in_progress_inputTab()
    sim_in_prog %>% sim_in_progress() 
    sim_in_prog %>% sim_in_progress_outputTab()
    sim_in_prog %>% sim_in_progress_aboutTab()
    
                              modelRunning(1)
                              model_running(TRUE) ## to show pael but locks UI argh! pane
                              terrSimRunning(1)
                      }
                             newdrawnShape %>% newdrawnShapetemp() 
                             disable("metadata_draw")
                             disable("metadata_upload")
                             disable("metadata_select") 

                              } # if point(s) uploaded
           
          
          if( newdrawnShapegeom == "point") { 
                        cat("single drawn point: make obsTerr geom  - ")
                            hab3857 <-  hab_3857w() 
                            terrFrom_drawnPt <-  func_makeTerr_drawnObs(drawnPt=newdrawnShape,hab3857=hab3857)    
                            print(terrFrom_drawnPt) # note - function returns NULL when didnt find enough cells for a territory
                          if(!is.null(terrFrom_drawnPt)) {  
                            input$metadata_layerEffect %>% featureEffect() #  to trigger recompute hab modfied only if new one in that category - nice
                            current_metaShape_placeholder(NULL)
                            removeModal() 
                            add_labelOnly(FALSE)  # map labs + terr polygons
                            cat("computed newCells3857  -")
                            newdrawnShape %>% newdrawnShape() ##jan22
                            terrFrom_drawnPt      %>% newCells3857()  ## returns cellsIDs for territory
                          } else { # cancel territory!
                            cat("cancel territory!")
                       gather_metadata(NULL) # not useful but reminder
                       current_metaShape_placeholder(NULL)
                       removeModal() 
                       enable("addMetadata_records") 
                       newFeature_deleteId(newdrawnShape$LeafletId)
                       leafletProxy("mapReleaseSite", session)  %>%  removeMarker(layerId =newdrawnShape$LeafletId)  
                       mapNewObs(NULL)  
                       print(paste0("not enough suitable hab, deleting ",newdrawnShape$LeafletId))
                       add_labelOnly(FALSE)
                       error_message2 <-  "local suitable habitat is insufficient"  
                       error_message1 <- "simulation cancelled"  
   notify_failure(error_message1,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_dbl",  
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
   notify_warning(error_message2,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",   
                               background =col_warn,textColor=txtCol_notif ,notiflixIconColor=txtCol_notif  )) 
                       
                          }
     } # if point

 } else {
            input$metadata_layerEffect %>% featureEffect() #  to trigger recompute hab modfied only if new one in that category - nice
           current_metaShape_placeholder(NULL)
          #  gather_metadata(1)
             removeModal()  
           cat("newdrawnShape:")
           print(newdrawnShape)
             newdrawnShape %>% newdrawnShape() ##jan22
  
 if(!newdrawnShapegeom %in% c("point_upload", "mapGeom")) {
   newFeature3857 <- st_transform(st_buffer(st_transform(newdrawnShape,st_crs(27700)),25),st_crs(3857))
   print(newFeature3857)
    cat("use vector dat  - ")
    hab3857 <-  hab_3857() 
      extractedNew <- terra::extract(   hab3857, vect(newFeature3857), ID=TRUE, cells=TRUE, xy=TRUE, method="simple", touches=TRUE)
      extractedNew <-  extractedNew[!duplicated(extractedNew$cell),]
      extractedNew$cell %>% newCells3857()
      length(which(extractedNew[,2]==2)) %>% NsuitCellsNew()
      length(which(extractedNew[,2]==1)) %>% NdispCellsNew()
      length(which(is.na(extractedNew[,2])| extractedNew[,2]==0)) %>% NunsCellsNew()
       
 }
}
#gather_metadata(1)
 })

newdrawnShapetemp <- reactiveVal(NULL)
terrFrom_uploadedPts<- reactiveVal(NULL)
terrSimRunning<- reactiveVal(0)
observeEvent(terrFrom_uploadedPts(),{
 if(terrSimRunning()==1){
   model_running(FALSE)  
   modelRunning(0)
   terrSimRunning(0)
 } # only resets if was running not simultaneous with model sim
  
    input$metadata_layerEffect %>% featureEffect() #  to trigger recompute hab modfied only if new one in that category - nice
    current_metaShape_placeholder(NULL)
    removeModal() 
    add_labelOnly(FALSE)  # map labs + terr polygons
    terrFrom_uploadedPts <- terrFrom_uploadedPts()
    newdrawnShape <- newdrawnShapetemp()
    newdrawnShape %>% newdrawnShape() ##jan22
    terrFrom_uploadedPts   %>% newCells3857()  ## returns cellsIDs for territory = here it is a list!
    cat("ok get cells in table  - ")
    Nterrs <- nrow(newdrawnShape)
    txtterrs <- "territories" 
    if (Nterrs==1){txtterrs <-"territory"}
 error_message1 <- paste0(Nterrs," beaver ",txtterrs," simulated from input file!")  
 notify_success( error_message1  ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
    enable("metadata_draw")
    enable("metadata_upload")
    enable("metadata_select")  
    enable("addMetadata_records")
    reset_simTriggers(1) ##does it allow ui ok after this is complete
  # smthg about      click_Site_disabled(FALSE)
}, ignoreNULL=TRUE)  
 
     
                             
                             
                        


NsuitCellsNew <- reactiveVal()
NunsCellsNew  <- reactiveVal()
NdispCellsNew <- reactiveVal()
newdrawnShape <-reactiveVal()
 
  

 
  

  
 
  


NareaOfInt_txt_new <- reactiveVal(0)
applyEffect_areaOfInt <- reactiveVal(0)
applyEffect_trigger <- reactiveVal(0)
habCells_make0 <- reactiveVal(NULL) ################## values - cell index in global raster NOW A LIST PER YEAR!
hab_rast_make0 <- reactiveVal(NULL)
 
  
   ll <-vector("list", length=length(seq(2010, 2050,1))) 
   names(ll) <-as.character(paste0("y",seq(2010, 2050,1))) 
   hab_rast_make0_tmp <- habCells_make0_tmp <- ll
   metadataFeatType_annual <- reactiveVal() # simple list per year with names of metadata included each year - user defined name! need to make reactive throughout..
   metadataFeatCells_perId <- reactiveVal(list(NULL))
   
   metadataMake0_cells <- reactiveVal() 


################################################################## new dranw shape into geom
observeEvent(newdrawnShape(),{
  cat("\nnew feature: ")
   newdrawnShape <- newdrawnShape()  
   metadataFeatType_annual <- metadataFeatType_annual() 
   newFeature_amendId <-    newFeature_amendId()
   newFeature_deleteId <- newFeature_deleteId()
   if(!is.null(newFeature_amendId)) {newFeature_deleteId <- newFeature_amendId  } ## amend = delete + add
    
  if(is.null(newdrawnShape) | !is.null(newFeature_amendId)) { # was triggered for deleting shape
 cat("---- delete cells  - ")
    
     req(newFeature_deleteId())
     newId <- newFeature_deleteId 
     cat(newId)
     newYears <- paste0("y",seq(2010,2050,1) ) 
      ## delete
      for (yrname in newYears) { 
                   metadataFeatType_thatYr <- metadataFeatType_annual[[yrname]]
                   if(newId %in% metadataFeatType_thatYr) {
                   metadataFeatType_thatYr <-  metadataFeatType_thatYr [metadataFeatType_thatYr != newId] 
                   metadataFeatType_annual[[yrname]] <- metadataFeatType_thatYr }
             }
  
            metadataFeatCells_perId <- metadataFeatCells_perId()
            featIndx <- which(names(metadataFeatCells_perId)==newId)
            metadataFeatCells_perId[featIndx]  <-  NULL 
          #  names(metadataFeatCells_perId)[featIndx]  <-  newId  
          #  not deleted just null but should work the same
  
       leafletProxy("mapReleaseSite", session)  %>%  removeShape(layerId=newId )  %>%  removeMarker(layerId =newId )   %>% clearGroup("temp")
    }  
    
   if(!is.null(newdrawnShape) ) { #!is.null(newFeature_amendId)
  cat("--- incorporate cells  for ",nrow(newdrawnShape)," geoms  - ")  
   
  
     metadataFeatCells_perId <- metadataFeatCells_perId()
     newdrawnShapegeom <- current_drawnShapeGeom()
      if(newdrawnShapegeom != "mapGeom")   { # for geoms newly drawn, retrieve rster cells
      newIds <-  newId <- as.character(newdrawnShape$LeafletId )
      newCells <- newCells3857<- newCells3857() 
      newdrawnShape_all <- newdrawnShape
       
          for(feat in 1:length(newIds)){
        if(is.list(newCells3857)) {
            # cat("list (upload) :")
             newId <- newIds[feat]
             newCells      <- newCells3857[[newId]]
             newdrawnShape <- newdrawnShape_all[newdrawnShape_all$LeafletId ==  newId,]
        }  
      cat(paste0(newId,": ",length(newCells),"cells   -  "))
      startYear <- newdrawnShape$start[newdrawnShape$LeafletId == newId] # need as numeric for duration = throughout
      if(startYear == "init"){startYear <- 2010} 
      startYear <- as.numeric(as.character(startYear))
      
      duration <- newdrawnShape$duration [newdrawnShape$LeafletId == newId] # need as numeric for duration = throughout
      if(duration == "through"){
        endYear <- 2050
      } else {
       endYear <-  startYear + as.numeric(gsub("\\D", "",  duration)) -1
      }  
       newYears <- paste0("y",seq(startYear,endYear,1) )
           ## store name of effect on years it is to be applied:   
             for (yrname in newYears) { 
                   metadataFeatType_thatYr <- metadataFeatType_annual[[yrname]]
                   metadataFeatType_thatYr <- c(metadataFeatType_thatYr, newId) 
                   metadataFeatType_annual[[yrname]] <- metadataFeatType_thatYr
             }
       
            featIndx <- length(metadataFeatCells_perId)+1 # not names() as no names initially? was error
      
            metadataFeatCells_perId[[featIndx]] <-  newCells 
            names(metadataFeatCells_perId)[featIndx]  <-  newId  
            metadataFeatCells_perId<- metadataFeatCells_perId[ which(as.numeric(sapply(metadataFeatCells_perId, is.null))==0) ] 
          cat("|")
     } 
 }
     
     
    if(newdrawnShapegeom == "mapGeom")   {# dont recompute - cells already know, retrieve by id
    metadataFeatCells_perId  <- metadataFeatCells_perId() 
    newCells  <- NULL
    # type of area according to name given
    print(newdrawnShape)
    areaType <- unique(substring(newdrawnShape$LeafletId,1,1) ) # w for wholeArea, c for catchments, s for selected drawn geoms
 print(paste0("areaTypes : ",areaType))
  if("w" %in% areaType ) { ## whole area
    cat("whole area  - ") ## only add name in list of effects per year
                startYear<- newdrawnShape$start[1]
                endYear  <-  startYear + as.numeric(gsub("\\D", "",  newdrawnShape$duration[1])) -1
                newYears <- paste0("y",seq(startYear,endYear,1) )
                # add in time effect table
                for (yrname in newYears) { 
                             metadataFeatType_thatYr <- metadataFeatType_annual[[yrname]]
                             metadataFeatType_thatYr <- c(metadataFeatType_thatYr, newdrawnShape$LeafletId) 
                             metadataFeatType_annual[[yrname]] <- metadataFeatType_thatYr
                }
                ## rather than cells, add effect to be appled EVERYWHERE (can only be about survival)  
                      featIndx <- length(names(metadataFeatCells_perId))+1
                      metadataFeatCells_perId[[featIndx]] <-  c(.5,.75)[which(c("50% mortality","75% mortality") %in% newdrawnShape$layerEffect)]
                      names(metadataFeatCells_perId)[featIndx]  <-  newdrawnShape$LeafletId 
  }
    
  if("c" %in% areaType ) { ## catchments
    cat("catchments  - ") # incorporation into metadata happens elsewhere - more efficient not to use cells just here
  }
    
  if("s" %in% areaType ) { ## selected geoms = fetch cells from original geoms
   newSelShapes <- newdrawnShape[which( substring(newdrawnShape$LeafletId,1,1) =="s"),]
   prev_LeafletId <- gsub("sLeaflet", "Leaflet", newSelShapes$LeafletId ) # remove s for "geom sel" id
   while(any(substring(prev_LeafletId,1,1) =="s") ){
     cat("clean s again  - ")
        prev_LeafletId <- gsub("sLeaflet", "Leaflet", prev_LeafletId ) # remove s for "geom sel" id for sel of sel in alternative effect
    }
   prev_LeafletId <- str_replace(prev_LeafletId,"\\_[0-9]","") # remove id past _ symbol for different selection sets
    
   cat("process geoms selected :" )
   if(length(prev_LeafletId)>0){
               for(id in 1:length(prev_LeafletId)) {
                prevId   <-  prev_LeafletId[id] 
                newId    <-  newSelShapes$LeafletId[id]
                newCells <- metadataFeatCells_perId [[prevId]] # retrieve cells already computed 
                startYear <- newSelShapes$start[newSelShapes$LeafletId == newId] # need as numeric for duration = throughout
                if(startYear == "init"){startYear <- 2010} 
                startYear <- as.numeric(as.character(startYear)) 
                duration <- newSelShapes$duration [newSelShapes$LeafletId == newId] # need as numeric for duration = throughout
              if(duration == "through"){
                endYear <- 2050
              } else {
              endYear <-  startYear + as.numeric(gsub("\\D", "",  duration)) -1
              }  
                
                newYears <- paste0("y",seq(startYear,endYear,1) )
                # add in time effect table
                for (yrname in newYears) { 
                             metadataFeatType_thatYr <- metadataFeatType_annual[[yrname]]
                             metadataFeatType_thatYr <- c(metadataFeatType_thatYr, newId) 
                             metadataFeatType_annual[[yrname]] <- metadataFeatType_thatYr
                } 
                ## add old cells under new id in the df per effect
                      featIndx <- length( metadataFeatCells_perId) +1 # not names()
                      metadataFeatCells_perId[[featIndx]] <-  newCells 
                      names(metadataFeatCells_perId)[featIndx]  <-  newId  
        
               print(paste0(prevId,"/",newId))
               }
   }} # for selected feature customs
        
    }
 
 
   } 
  print("metadataFeatCells_perId updated   - ") 
 
      metadataFeatCells_perId <-  metadataFeatCells_perId[unique(names(metadataFeatCells_perId) )]
       
      metadataFeatType_annual %>% metadataFeatType_annual() 
      metadataFeatCells_perId %>% metadataFeatCells_perId()
    
if(pSc==TRUE){
  cat("SHOW DF EFFECTS  - ")
 print(metadataFeatType_annual )
 print(metadataFeatCells_perId) 
}
   if(is.null(newFeature_deleteId())){ 
    notify_success("metadata updated !",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  # for deletions too - too much?
  #   trigger_undo_settling(trigger_undo_settling()+1)
   #  trigger_undo_sim(trigger_undo_sim()+1)
}
  newFeature_deleteId(NULL) # done
  gather_metadata(1)
   current_drawnShapeGeom(NULL)#reset
   uploadGeomType(NULL) # reset eff choices
}, ignoreNULL=FALSE, ignoreInit=TRUE)
 
      

################################################### metadata - effect summary tables in simulation

metaEff_make0_SumTab <- reactiveVal()
metaEff_make50surv_SumTab <- reactiveVal()
metaEff_make75surv_SumTab <- reactiveVal()
metaEff_areaOfInt_SumTab <- reactiveVal()
metaEff_obsRecs_SumTab <- reactiveVal()


observeEvent(metaTable_modifHab(),{ 
   req(featureName() == "modify habitat")
  cat("generate effect df modif hab - ") 
   print(input$metadata_layerEffect)
   
  req(metaTable_modifHab())
  req(nrow(metaTable_modifHab())>0)
  print(metaTable_modifHab())
  metaTable_modifHab <- metaTable_modifHab() 
  if(input$metadata_layerEffect == "make unsuitable"){
       metaEff_make0_SumTab(NULL)
       generateEffectDf(metaDat=metaTable_modifHab,layerEffect="make unsuitable")  %>% metaEff_make0_SumTab()
  }
  if(input$metadata_layerEffect == "50% mortality"){
    metaEff_make50surv_SumTab(NULL)
    generateEffectDf(metaDat=metaTable_modifHab,layerEffect="50% mortality")   %>% metaEff_make50surv_SumTab()
  }
  if(input$metadata_layerEffect == "75% mortality"){
   metaEff_make75surv_SumTab(NULL)
    generateEffectDf(metaDat=metaTable_modifHab,layerEffect="75% mortality")   %>% metaEff_make75surv_SumTab()
  }
    cat("ok")
}, ignoreNULL=FALSE)

 

observeEvent(metaTable_records(),{ 
 
   metaEff_obsRecs_SumTab(NULL) 
  req(metaTable_records())
  req(!is.null(add_labelOnly()))
  if( add_labelOnly()==FALSE)
  cat("-------------------------------------change in rec tab")
  metaTable_records <- metaTable_records()
   
   generateEffectDf(metaDat=metaTable_records,layerEffect=c("territory identified on map","territory from uploaded coordinates"))  %>% metaEff_obsRecs_SumTab() 
   cat("done")
   
}, ignoreNULL=FALSE)
 

observeEvent(metaTable_areaOfInt(),{ 
   metaEff_areaOfInt_SumTab(NULL) 
  req(metaTable_areaOfInt())
  metaTable_areaOfInt <- metaTable_areaOfInt()
  print(metaTable_areaOfInt )
  print("-------")
 # layerEffectz <- metaTable_areaOfInt()$layerEffect #mar26
   generateEffectDf(metaDat=metaTable_areaOfInt,layerEffect=NULL)  %>% metaEff_areaOfInt_SumTab() 
}, ignoreNULL=FALSE)
 
metaEff_all_SumTab <- reactive({
  rbind(metaEff_areaOfInt_SumTab(),metaEff_obsRecs_SumTab(),metaEff_make50surv_SumTab(),metaEff_make75surv_SumTab(),metaEff_make0_SumTab() )
})
  
 
metaEff_in_sim <- reactiveVal(NULL) 
ganttdf <- reactiveVal(NULL)
ganttdf_dat <- reactiveVal(NULL)
ganttdf_datHili <- reactiveVal(NULL)
ganttdf_hili <- reactiveVal(NULL)   
nrow_ganttdf <- reactiveVal(0)
ganttdf_noTxt <- reactiveVal(NULL) 

ganttdf_txt <- reactive({
  ganttdf_dat()
  yearViz()
  req(ganttdf_dat())
#  req(yearViz())
     if(!is.null(yearViz())) {  
    year <- yearViz()
  
  
  effcols <- c( "modify habitat" = "#FB8861FF", "incorporate records" = "magenta", "translocation"="turquoise")
    
      
  effTyps <- as.character(ganttdf_dat()$effType[ganttdf_dat()$StartDate<year+1 &  ganttdf_dat()$EndDate+1 >year]  )
  effIds  <-as.character( ganttdf_dat()$effectId[ganttdf_dat()$StartDate<year+1 &  ganttdf_dat()$EndDate+1 >year]  )
  effCols <-   effcols[effTyps]  
    
  if(length(effIds)>0) { # effectNames <-  lapply(effIds, function(x) p(x,style="color:beige;display:list-item"))  
  #  effCols <- paste0("color:",  as.character(effCols)    ,";display:list-item")
  # effectNames <-lapply(effIds, function(x) p(x,style=paste0("color:",  effCols[x]  ,";display:list-item")) )
 #  effectNames <-lapply(effIds, function(x) div(x,style=paste0("color:beige;display:list-item")) )

   xnum <-  rev(seq_len(length(effIds)))
   effectNames <-lapply(xnum, function(x) div(effIds[x],style=paste0("color:",as.character(effCols[x]),";display:list-item")) )
   } else {
   effectNames <- div("no event")
  }  
 
     } else {
      ##null output in pop sim but still show metadata because it is nice   
   effcols <- c( "modify habitat" = "#FB8861FF", "incorporate records" = "magenta", "translocation"="turquoise")
  effTyps <- ganttdf_dat()$effType 
  effIds  <- ganttdf_dat()$effectId   
  effCols <-   effcols[effTyps]   
  if(length(effIds)>0) {  
    effectNames <-  lapply(effIds, function(x) div(x,style="color:beige;display:inline;"))  
 # effectNames <-lapply(effIds, function(x) p(x,style=paste0("color:",  as.character(effCols[x])    ,";display:list-item")) )  
    }
     }
return(div( effectNames) ) 
 })

ganttdf_yrtxt <- reactive({
  yearViz()
  req(ganttdf_dat())
 # req(yearViz())
  if(is.null(yearViz())) {
    return( as.character(paste0( "metadata added: ") ))
    } else {
  return( as.character(paste0( "metadata added in year ",yearViz(), ": ") ))
      }
})
  
  
output$effTimeLineTxt <- renderUI({ganttdf_txt()})  #baba
output$effTimeLineYrTxt <- renderText({ganttdf_yrtxt()})

 
  
######## output plots ########
 output$effTimeLinePlot_hilights <- renderPlot({ 
 if(!is.null(ganttdf_hili())) return(ganttdf_hili())
 },  bg="transparent", res = 72 , height = function() { 10*(as.numeric(!is.null(ganttdf()))) + 37 * nrow_ganttdf()  })
  
 
 output$effTimeLinePlot <- renderPlot({ 
    ganttdf()  
 },  bg="transparent", res = 72 , height = function() { 10*(as.numeric(!is.null(ganttdf()))) + 37 * nrow_ganttdf() })


 
 
################################################### simulation output plots 
    

recapPlot_N <- reactiveVal(NULL)
recapPlot_N_hili <- reactiveVal(NULL)
annual_Nadt <- reactiveVal(NULL)  
recapPlot_fams <- reactiveVal(NULL)
recapPlot_fams_hili <- reactiveVal(NULL)
annual_Nfams <- reactiveVal(NULL) 

recapSummPlot_fams <- reactiveVal(NULL) 
recapSummPlot_fams_hili <- reactiveVal(NULL) 




output$NTimeLinePlot_N <- renderPlot({
 if(!is.null(recapPlot_N())) {return(recapPlot_N())} else { NULL} 
},  bg="transparent", res = 72, height = "auto",width="auto",execOnResize=FALSE)  
 
output$NTimeLinePlot_N_hilights <- renderPlot({ 
  if(!is.null(recapPlot_N_hili())) {return(recapPlot_N_hili())} else {  NULL} 
},  bg="transparent", res = 72 , height= "auto",width="auto",execOnResize=FALSE)

####

output$NTimeLinePlot_fams <- renderPlot({
 if(!is.null(recapPlot_fams())) {return(recapPlot_fams())} else { NULL} 
},  bg="transparent", res = 72, height="auto")
 
output$NTimeLinePlot_fams_hilights <- renderPlot({ 
  if(!is.null(recapPlot_fams_hili())) {return(recapPlot_fams_hili())} else {  NULL} 
},  bg="transparent", res = 72,  height="auto")


output$NSummTimeLinePlot_fams<- renderPlot({ 
  if(!is.null(recapSummPlot_fams())) {return(recapSummPlot_fams())} else {  NULL} 
},  bg="transparent", res = 72,  height="auto")

output$NSummTimeLinePlot_fams_hilights<- renderPlot({ 
 if(!is.null(recapPlot_fams_hili())) {return(recapPlot_fams_hili())} else {  NULL} 
},  bg="transparent", res = 72,  height="auto")
 
recapSummPlot_N_hili<- reactiveVal()  
recapSummPlot_N <- reactiveVal()  

output$NSummTimeLinePlot_N<- renderPlot({ 
  if(!is.null(recapSummPlot_N())) {return(recapSummPlot_N())} else {  NULL} 
},  bg="transparent", res = 72,  height="auto")
output$NSummTimeLinePlot_N_hilights<- renderPlot({ 
  if(!is.null(recapPlot_N_hili())) {return(recapPlot_N_hili())} else {  NULL} 
},  bg="transparent", res = 72,  height="auto")
 

cols <- reactiveValues()   
#cols$showing <- 1:6    

  
 
        
         
 
outColSelected <- reactiveVal(NULL)  
observeEvent(input$recapTableOut_N_cell_clicked$col,{
  if(input$recapTableOut_N_cell_clicked$col>1 ) {
 input$recapTableOut_N_cell_clicked$col %>% outColSelected()}
},ignoreNULL=TRUE)

observeEvent(input$recapTableOut_fams_cell_clicked$col,{
  if(input$recapTableOut_fams_cell_clicked$col>1 ) {
 input$recapTableOut_fams_cell_clicked$col %>% outColSelected()} 
},ignoreNULL=TRUE)
 

observeEvent(input$recapProximTo_cell_clicked$col,{
  if(input$recapProximTo_cell_clicked$col>1 ) { 
  (input$recapProximTo_cell_clicked$col-2) %>% outColSelected()
 }
},ignoreNULL=TRUE)


observeEvent(input$recapTableOut_headers_cell_clicked$col,{
  cat("clicked headers - ")
  print(input$recapTableOut_headers_cell_clicked)
  print(input$recapTableOut_headers_cell_clicked$col)
  if(input$recapTableOut_headers_cell_clicked$col>0 ) {
    cat("col>0")
    print(input$recapTableOut_headers_cell_clicked$col +1)
    
 input$recapTableOut_headers_cell_clicked$col   %>% outColSelected() }
},ignoreNULL=TRUE)   
  

yearViz <- reactiveVal()

observeEvent(outColSelected(),{  
  req(!is.null(recapTable() ))
  recapPlot_fams_hili(NULL) 
  recapPlot_N_hili(NULL)
  ganttdf_hili(NULL) #recapPlot_N_hili <- recapPlot_fams_hili <- ganttdf_hili <- NULL
  req(!is.null(outColSelected()))
      req(!is.null(recapPlot_N()))
     cat("recapTab column selected  - ")
    colSel <- outColSelected()
    colSel <- as.numeric(colSel)
    print(colSel) 
  if( colSel > 1){   
      print(colSel)
    recapTable <- recapTable() 
   # print(recapTable)
    print( recapTable$year[colSel-1])
   recapTable$year[colSel-1] %>% yearViz()
           annual_Nfams <- annual_Nfams()  
           annual_Nadt <- annual_Nadt()  
           
       #    print(annual_Nadt)
        #   print(annual_Nfams)
           
           
           current_obsYear <- recapTable$year[colSel -1]
           print(current_obsYear) 
     annual_Nadt_hili <- annual_Nadt[annual_Nadt$year == current_obsYear,]
     annual_Nfams_hili <- annual_Nfams[annual_Nfams$year == current_obsYear,]
     maxNobs <- max(annual_Nadt$num.adt)+1
      print(annual_Nadt_hili)
print(annual_Nfams_hili)
yobsStep <-5 
if(maxNobs/yobsStep <4) {yobsStep <- 2} 
#if(maxNobs/yobsStep <2) {yobsStep <- 5}  
 
if(maxNobs>100) {yobsStep <-20}
if(maxNobs>200) {yobsStep <-50}
if(maxNobs>500) {yobsStep <-100}


      recapPlot_N_hili  <-         ggplot()+
        geom_vline(xintercept=annual_Nadt_hili$year[1],  linewidth= 2, color="#ffe62cc2") +
        geom_point(data=annual_Nadt, aes(x=year, y=num.adt ), color="transparent") + 
        geom_point(data=annual_Nadt_hili, aes(x=year, y=num.adt ),  size= 4, color="#ffe62cc2") +
         geom_point(data=annual_Nadt_hili, aes(x=year, y=num.adt ),  size= 2, color="black") +  
         scale_x_continuous("",breaks=seq(min(annual_Nfams$year),max(annual_Nfams$year),1)  )+  
        coord_cartesian(xlim= c(min(annual_Nfams$year),max(annual_Nfams$year))  ) + 
         scale_y_continuous("",breaks=seq( 0,maxNobs,yobsStep) )+   
        coord_cartesian(ylim= c(0, maxNobs)  ) + 
        theme_minimal(base_size = 8)+  
        theme(
        legend.position = "none",  
        axis.text = element_text(colour = "transparent", family="Comfortaa"),
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "transparent"),
                     panel.background = element_rect(fill = "transparent", colour = "transparent"))  +
     ggtitle("ttl")+
     theme(plot.title = element_text(colour = "transparent", family="Comfortaa" ))         
      
      
  maxNobs <- max(annual_Nfams$num.fam)+1
      
      recapPlot_fams_hili  <-         ggplot()+
         geom_vline(xintercept=annual_Nfams_hili$year[1],  linewidth= 2, color="#ffe62cc2") +
         geom_point(data=annual_Nfams, aes(x=year, y=num.fam), color="transparent") +  
         geom_point(data=annual_Nfams_hili, aes(x=year, y=num.fam ),  size= 4, color="#ffe62cc2") +
         geom_point(data=annual_Nfams_hili, aes(x=year, y=num.fam ),  size= 2, color="black") +   
         scale_x_continuous("",breaks=seq(min(recapTable$year),max(recapTable$year),1)  )+  
       coord_cartesian(xlim= c(min(annual_Nfams$year),max(annual_Nfams$year))  ) +  
        coord_cartesian(ylim= c(0, maxNobs)  ) + 
         scale_y_continuous("",breaks=seq(0, maxNobs ))+ 
        theme_minimal(base_size = 8)+  
        theme(   
        legend.position = "none",  
        axis.text = element_text(colour = "transparent", family="Comfortaa"),
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "transparent"),
                     panel.background = element_rect(fill = "transparent", colour = "transparent"))  +
     ggtitle("ttl")+
     theme(plot.title = element_text(colour = "transparent", family="Comfortaa"))       
    effcols <- c( "modify habitat" = "#FB8861FF", "incorporate records" = "magenta", "translocation"="turquoise")
               
   
        cat("gannt")     
      ganttdf_noTxt <- ganttdf_noTxt()
      ganttdf_hili <- ganttdf_noTxt +  
        geom_vline(xintercept= annual_Nfams_hili$year[1],  linewidth= 2, color="#ffe62cc2") +
        scale_color_manual(values = effcols)  +
        geom_point(data=ganttdf_datHili(), aes(x=StartDate, y=effGroup, group=effGroup, color=effType), size=2)  +
        geom_point(data=ganttdf_datHili(), aes(x=EndDate, y=effGroup, group=effGroup,  color=effType), size=2)  
     
         
      ganttdf_hili %>% ganttdf_hili()       
      recapPlot_fams_hili %>% recapPlot_fams_hili()
      recapPlot_N_hili %>% recapPlot_N_hili()
  }  # just dont change if click on first column no effect
   cat("ok")  
}, ignoreInit=FALSE, ignoreNULL=FALSE)
   
  
 
habCells_metaTerrs <- reactiveVal() # terrs ids
habCells_makeTerrs <- reactiveVal() # cell index for terrs
terr_rast_new <- reactiveVal(NULL)


   


houseIcon <-  makeIcon(iconUrl = "house2-pointer.png" , iconAnchorX = 15, iconAnchorY = 30 )

################################################### add metadata - draw on map
clickDraw <- reactive ({return(0+as.numeric(input$metadata_draw))})    
observeEvent( input$metadata_draw, {
    cat("draw on map  - ") 
       ## lock site if any
 if(!is.null(exploSite())){
    cat("lock - ")
  updatePrettyCheckbox(session, "lock_site",  label="exploratory site locked", value=TRUE ) }
       
      
   # to only do this once nt every time (even if it doesnt show..)
 req(clickDraw()==1)       
   cat("first time clicked draw  - ")
     notify_info("use the drawing toolbar to create spatial geometries on the map",timeout=7100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 

           leafletProxy("mapReleaseSite", session) %>%  clearGroup("temp") %>% clearGroup("temp2") %>%
                               addDrawToolbar(targetGroup="temp",
                                                          polylineOptions = drawPolylineOptions(allowIntersection = TRUE,showLength = TRUE, metric = TRUE,
                                                                                                zIndexOffset =20000, 
                                                                                                shapeOptions = drawShapeOptions(clickable=TRUE, weight = 3, smoothFactor = 1,color = "white", fill=FALSE, opacity=1,dashArray = NULL)), #ffe62cc2
                                                          polygonOptions = drawPolygonOptions( showArea = TRUE, metric = TRUE, shapeOptions = drawShapeOptions( stroke = TRUE,  color = "white", weight = 3, fillColor = "white",dashArray = 1, opacity=1,fillOpacity =.4)), 
                                                          circleOptions = FALSE, rectangleOptions = FALSE,  
                                                          markerOptions = #list(markerIcon = houseIcon  ) , 
                                                          drawMarkerOptions(markerIcon = houseIcon, zIndexOffset = 2000),
                                                          circleMarkerOptions = FALSE,  
                                                          editOptions =  editToolbarOptions(edit = FALSE, remove =FALSE, allowIntersection = FALSE), 
                                                          singleFeature = TRUE) 
   
 
################# will ave toupdate with this as leaflet.extras becoming obsolete grrr
})







  

featureName <- reactiveVal(NULL)
featureEffect <- reactiveVal(NULL)  
mapNewFeature <- reactiveVal(NULL)
################################################## plot metadata 

featureSymbols <- c("&#9888", "&#x2318" , paste0("<img src='house3.png' height='16' width='16'></img>")) #"&#127968"   ) # warning + area of interest + house (diff order here because processing forst 2 layers jointly)

# prep for mapping
mapAble <- reactive({
 newFeaturesDat_shp3857()  
  req(newFeaturesDat_shp3857())
  bnd <- newFeaturesDat_shp3857()  
  bnd <- bnd[bnd$layerType != "observation records",] # recs elsewhere
  bnd <- bnd[substring(bnd$LeafletId,1,1) != "w",]      # remove whole area 
  bnd$Lindx <-  as.numeric( factor(bnd$layerType, levels=c("modify habitat","area of interest","observation records")))
  bnd$featureSymb <-  as.character(featureSymbols[bnd$Lindx])  
  bnd$featureCol <- c( "#FB8861FF" ,"#a7ca35", "#decb3d"  ) [bnd$Lindx]
  #bnd$LeafletId <- paste0("leaflet",seq(1:nrow(bnd)))
  bnd$zPane <- c(  "metadataArea","metadataAreaOfInt","metadataPt")[bnd$Lindx]
  bndp <- bnd %>%  st_transform(4326)
return(bndp) # upload feature point - process each poit and only accept sensibles?
})

 
  
######### update map with new feature ########## 
observeEvent(mapNewFeature(),{#newdrawnShape(),{#mapNewFeature(),{  
    req(mapAble())
   req(featureName() != "observation records")  
  cat(paste0("\n----upmap:",featureName(), "  - "))
  featureName <-  c("modify habitat","area of interest")[c("modify habitat","area of interest") %in% cur_layers()]

 leafletProxy("mapReleaseSite", session)  %>% clearGroup("temp") %>% 
              addPolygons (data=mapAble()[mapAble()$layerType %in%  featureName ,], group = ~layerType, layerId= ~LeafletId, 
                           label= ~ sprintf(  "%s %s ",
                                     paste0("<p style='margin-top:0;font-size:16px;color:",featureCol,";'>",featureSymb, "<span style='font-size:11px; text-align:left;color:",featureCol,";'>   ", layerType,": ",  name,"</span></p>"),
                                     paste0("<p style='color:beige;'>(",layerEffect,")") )   %>%  lapply(htmltools::HTML) ,  
                           fillOpacity=~c(.8,.2)[as.numeric(layerType=="area of interest")+1],  
                           weight=~c(2,7)[as.numeric(layerType=="area of interest")+1] ,  
                           opacity=~c(.9,1)[as.numeric(layerType=="area of interest")+1],
                           fillColor= ~featureCol , color=~featureCol ,
                           labelOptions = labelOptions(offset=c(50,0), direction="right",  style = as.list(c( "background-color" = "#0000008f",labstyle_plotLabs))  ) ,
                          options = pathOptions(pane = ~zPane )   ) 
     mapNewFeature(NULL)
 }, ignoreNULL=TRUE, ignoreInit=FALSE) # feb2024
  
# prep for mapping
mapAbleObs <- reactive({
  pointFeature() 
  req(pointFeature())
  req(!is.null(add_labelOnly()))
  bnd <- pointFeature()
  bnd$featureSymb <-  as.character(featureSymbols[3])  
  bnd$featureCol <-   "#decb3d"   
  bnd$zPane <-  "metadataPt"   
  bndp <- cbind( bnd %>% st_coordinates(), bnd %>% st_drop_geometry() ) %>% as.data.frame()
return(bndp)
}) 

 
makeTerrGeoms <- function(hab,metadataFeatCells_perId,listIndx,obsIds) {
  hab_terrs <- terra::unwrap(hab) 
  values(hab_terrs ) <- NA# hab_terrs  <- terra::unwrap(hab_3857_0)
   #names(hab_terrs)[1] <- "LeafletIds" 
                     cells <- as.numeric(as.character(unlist(metadataFeatCells_perId[obsIds])))
                     obsIds <-factor(obsIds, levels=obsIds)
                     hab_terrs[cells] <-   as.numeric( rep(obsIds,  as.numeric(lengths(metadataFeatCells_perId[obsIds]))))  
                     
                     ## if pts too overlapping some terrs can disappear so exclude just in case: (alays happens with eng data!)
                      retainedTerrs <- as.numeric(unique(unlist(hab_terrs[cells] )))
                      
                     #pols <- sf::st_as_sf(terra::as.polygons( hab_terrs, aggregate=TRUE, values=TRUE, na.rm=TRUE, crs="epsg:3857")) %>% sf::st_sf() %>% sf::st_transform(st_crs(4326)) 
                     pols <-sf::st_as_sf(terra::as.polygons( hab_terrs, aggregate=TRUE, values=TRUE, na.rm=TRUE )) %>% sf::st_sf()  %>% sf::st_transform(st_crs(4326)) 
                     pols$LeafletIds <-  obsIds[ retainedTerrs ]#[as.numeric(as.factor(pols$LeafletIds))]
                     sf::st_geometry(pols) <- "geometry"
                     pols$layerType <- "observation records"  
  return(pols)
 }



mapNewObs <- reactiveVal(NULL)
pointFeature_terrPols <- reactiveVal(NULL)
new_pointFeature_terrPols <- reactiveVal(NULL)



observeEvent( mapNewObs(),{
    
    cat("mapNewObs")
#    leafletProxy("mapReleaseSite", session)   %>%  clearGroup("observation records")
 #  mapNewObs(NULL) 
     req(metadataFeatCells_perId()) # null at first if recs are in but not simulated init terrs yet, doesnt replot - but next times?
     req(mapAbleObs())
    
 #    req( "observation records" %in% cur_layers()) #meh

      cat(paste0("----upmap: Obsrecs  - "))
     leafletProxy("mapReleaseSite", session)  %>%  clearGroup("temp") %>% 
              addMarkers(data=mapAbleObs(),lng = ~X, lat = ~Y,
                                icon=makeIcon(iconUrl = "house2-pointer.png", iconWidth = 40, iconHeight = 40, iconAnchorY =  20  ), 
                                group = ~layerType, layerId= ~LeafletId, 
                                label= ~ sprintf(  "%s %s ",
                                     paste0("<p style='margin-top:0;font-size:16px;'>",featureSymb, "<span style='font-size:11px;text-align:left;color:",featureCol,";'>   ", layerType,": ",  name,"</span></p>"),
                                     paste0("<p style='color:beige;'>(",layerEffect,")") )   %>%  lapply(htmltools::HTML) ,  
                           labelOptions = labelOptions(offset=c(-50,0), direction="left",  style = as.list(c( "background-color" = "#0000008f",labstyle_plotLabs))  ) ,
                          options = pathOptions(pane = ~zPane )   ) #%>%
      # showGroup("observation records")
  
  add_labelOnly <- add_labelOnly()
  print(paste0("add_labelOnly :",add_labelOnly))
  
            
  if(base::isFALSE(add_labelOnly)){#  {
                cat("create new  terrs geoms  - ") 
  # browser()
   metadataFeatCells_perId  <- metadataFeatCells_perId() 
  obsIds  <- mapAbleObs()$LeafletId 
  if(!is.null(pointFeature_terrPols())) { obsIds <- obsIds[! obsIds %in% pointFeature_terrPols()$LeafletIds] } # filter to only compute new ones
  listIndx <- names(metadataFeatCells_perId)[which(names(metadataFeatCells_perId) %in% obsIds)]
 
  cat("..prep pt dat  - ")     
      hab_3857w <- hab_3857w()
    cat("terr function  - ")
  # future_promise({ 
#  browser()
     makeTerrGeoms(hab=hab_3857w,
                          metadataFeatCells_perId=metadataFeatCells_perId,
                          listIndx=listIndx,obsIds=obsIds) %>% new_pointFeature_terrPols() # }, seed=TRUE) %...>% pointFeature_terrPols()
                        
  cat(".. polys  - ")       
                     
#pols %>% pointFeature_terrPols()
#print( pols )     # add_labelOnly(NULL) 
 ( obsRecs_new()+1)  %>% obsRecs_new() # retrigger new fam df row for this ne wobs terr  while computing geoms in background adjust with demographics values
}  
  add_labelOnly(FALSE) 
 # add_labelOnly(NULL) # not here, required value to create obsfam df
}, ignoreNULL=TRUE, ignoreInit=FALSE) 
  
  
observeEvent(new_pointFeature_terrPols(),{ 
  cat("add new terr pols  - ")
 rbind(new_pointFeature_terrPols(),pointFeature_terrPols()) %>% pointFeature_terrPols()
 new_pointFeature_terrPols(NULL)
 # remove uncomputed terrs from obs ts
 deleteIds <-  pointFeature()$LeafletIds[!which(pointFeature()$LeafletIds %in%  pointFeature_terrPols()$LeafletIds)]
 if(length(deleteIds)>0) { deleteIds %>% newFeature_deleteId() }
 cat(paste0("\ndeleting ",length(deleteIds)," obs ", deleteIds,"  - "))
},ignoreInit=TRUE, ignoreNULL=TRUE)  

add_labelOnly <- reactiveVal(FALSE) 
add_metadatRas  <- reactiveVal(0) # to trigger rs mapping - not before or else deleted premapping 
drawnshapes     <- reactiveVal() 
deleteDrawnShape<- reactiveVal(NULL)
shapetxt        <- reactiveVal() 


##################################### user deletes drawn features 
observeEvent(deleteDrawnShape(),{
  req(drawnshapes())
      print(drawnshapes())
      drawnshapes <- deleteDrawnShape()
      lapply(drawnshapes, function(todelete) { session$sendCustomMessage("removeleaflet", list(elid="mapReleaseSite", layerid=todelete)) } )
      deleteDrawnShape(NULL)
},ignoreNULL=TRUE,ignoreInit=TRUE) 
  

##################################### new feature drawn on release map 
observeEvent(input$mapReleaseSite_draw_all_features,{    # get feature id
lapply( input$mapReleaseSite_draw_all_features$features, function(ftr) { ftr$properties$`_leaflet_id` } ) %>% drawnshapes()
})
   
 
##################################### select metadata layers 
#disable("metadata_select")
     
    
output$uiMetaSelect_layersGlob <- renderUI({ 
     if(!is.null(intercatch_on_map_sub())) {
    return( tagList(
            strong("global base layers"),
            div(prettyRadioButtons(inputId= "metaselect_listGlob",  label =NULL,   
                                icon=icon("check"), fill=TRUE, inline=FALSE,
                                choices= c("none","river catchment"), 
                                selected = "none",
                                width = "auto"),style="padding: 7px;")#,
           # bsTooltip("metaselect_listGlob","metaselect_listGlob",placement = "top", trigger = "hover")
            ) )  
  } else {
    if(!is.null(rv_temp$pt_buffer_focus)) { 
      if (model_running()==FALSE) {process_background <- br()} else {process_background<- p("background computation in progress: this operation is queued", style="color:darkmagenta;text-align: center;")}
      return(tagList(p("..filtering 10km surrounding catchments..", style="color:magenta;text-align: center;"),
                     process_background,
                   div(img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'), style="text-align:center;") 
                     ))
    }else {
  return( tagList(
            strong("global base layers"),
           div( p("in order to select river catchments, click on the map at the location of interest"),style="padding: 7px;"),
                  ))    
  }}
})
    
 
output$uiMetaSelect_layersCustom <- renderUI({   
  req(mapExists()==TRUE)
          effDf <- newFeaturesDat_display()
          effNames <- effDf$name[substring(effDf$LeafletId,1,1) != "s" & substring(effDf$LeafletId,1,1) != "c" & substring(effDf$LeafletId,1,1) != "w"] # so as to not include the other mapgeoms twice (id starts with s)
          if(!is.null(pointFeature())){effNames <- effNames[!effNames %in% pointFeature()$name]}
          print("effNames ")
          print(effNames )
          if(length(effNames)>0) {
          return( tagList(strong("custom metadata layers"),
                          div(prettyCheckboxGroup(inputId= "metaselect_listCustom",  label = NULL,   
                            icon=icon("check"), fill=TRUE,  choices= effNames,  selected = character(0), width = "auto"), style="padding: 7px;"),
                           bsTooltip ("metaselect_listCustom","metaselect_listCustom",placement = "top", trigger = "hover")))  
            } else {
          return(tagList(strong("custom metadata layers"),
                        div( p("no custom metadata layers created"),style="padding: 7px;"),)  )  
            }   
 #  }
}) 

  
 
show_metaselectModal <- reactiveVal(NULL)
observeEvent( input$metadata_select, {
  cat("isolate catchments in view (...) - ") 
    show_metaselectModal(1) # trigger if no site used no catchments locally
  #  req(!is.null( init_poly()) )# intercatch_on_map())
    
     
   req( any(!is.null(rv_temp$pt_buffer_focus),!is.null(init_poly())) )
    
    if(!is.null(init_poly())){exploGeom <- init_poly() } else { exploGeom <- rv_temp$pt_buffer_focus }
    ## take 10km surrounding of either geom for selecting around sims
   exploGeom <- exploGeom  %>%st_transform(st_crs(27700)) %>%st_buffer(10000)%>%st_transform(st_crs(4326))
   intercatch <- intercatch()  
   future_promise({intercatch[ which(lengths(st_intersects(intercatch , exploGeom)%>%suppressMessages())>0),]},seed=NULL) %...>% intercatch_on_map_sub()
   NULL 
   selectedCatch_metaTxt(NULL)
  #include catchments around for selection may be of interest
}, ignoreNULL=TRUE,ignoreInit=TRUE)



observeEvent( show_metaselectModal(),{ #intercatch_on_map_sub(),{
   cat("select geom on map  - ")  
        showModal(modalDialog( size =  "m",
                  tags$h3("select amongst mapped layers", style="text-align:center;"),  
          fluidRow(column(6, 
                           div(  uiOutput("uiMetaSelect_layersGlob"),style="padding: 17px;padding-top:4vh;") ,
                           div(  uiOutput("selectedCatch_metaTxt"),style="padding: 17px;padding-top:4vh;")),
                   column(6, 
                           div(  uiOutput("uiMetaSelect_layersCustom"),style="padding: 17px;padding-top:4vh;"),
                           div(  uiOutput("uiMetaSelect_intermap"),style="padding: 17px;padding-top:4vh;"))), 
          footer=tagList(
            div(style="display: inline-flex !important; width: fit-content;",
               # hidden(
                  tags$div(id="div_metaselect_btn", actionButton(inputId="metaselect_submit",label="submit"),style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;"),# ),
                div(actionButton(inputId="metaselect_cancel",label="cancel"),style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline!important;")  #  
                     )
 ), 
  bsTooltip("metaselect_cancel","metaselect_cancel",placement = "top", trigger = "hover")   
  ))
    show_metaselectModal(NULL)
}, ignoreNULL=TRUE) # now use the names to display as in for drawing on map or uploading - same ui
   
observeEvent(input$metaselect_cancel,{
  removeModal()
  current_drawnShapeGeom(NULL) # useful?
  selectedCatch_metaTxt(NULL)
  intercatch_on_map_sub(NULL) 
  notify_info("selection cancelled",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
 })    
 



observeEvent(input$metadata_cancel,{
  cat("cancel metadata  - ")   
  notify_info("new feature cancelled",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
  current_metaShape_placeholder(NULL) 
   na.exclude(pointFeature()) %>% pointFeature()
   na.exclude(polyFeature()) %>% polyFeature()
   na.exclude(lineFeature()) %>% lineFeature()
   na.exclude(uploadFeature()) %>% uploadFeature()
  gather_metadata(1)
  removeModal()
  #enable("metadata_submit")
  deleteDrawnShape(drawnshapes()) 
   current_drawnShapeGeom(NULL) # useful? 
},ignoreNULL=TRUE) # reset to NULL if cancelling too
   
  















     
######## compute catchment geometries ##########
intercatch_on_map_sub <- reactiveVal(NULL)  
output$map_catchSelect <- renderLeaflet({  
    req(  input$metaselect_listGlob ) # intercatch_on_map_sub())
    req(intercatch_on_map_sub())
   catchmap  <-  leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE, attributionControl=FALSE )  ) %>%  
                                addProviderTiles("OpenStreetMap")    %>% 
    addMapPane("intercatch_on_map_sub", zIndex = 200)   %>% 
    addPolygons(data=intercatch_on_map_sub(),  label=~WB_NAME, fillOpacity=0,  weight =2,  color="magenta",opacity=1 ,
                              labelOptions = labelOptions( style = as.list(c("color"="darkmagenta", "background-color" =  "#ccccccc4",labstyle_plotLabs)) , direction = "right" ),   
                                        options = pathOptions(pane = "intercatch_on_map_sub"))    
   
  if(!is.null(initValues())){
    catchmap  <-  catchmap %>%  
    addMapPane("intercatch_pts",zIndex = 210) %>% 
                  addMarkers(data= initValues(),  ~lng,~lat , icon=beaverIconWee, options = pathOptions(pane = "intercatch_pts" )) 
  }
   if(!is.null(pointFeature())){
    catchmap  <-  catchmap %>%  
    addMapPane("intercatch_pts",zIndex = 210) %>% 
                  addMarkers(data= pointFeature() , icon=houseIconWee, options = pathOptions(pane = "intercatch_pts" )) 
  } 
  
   return( catchmap )    
 
})
 

  
selectedCatch_meta <- reactiveVal()

selectedCatch_metaTxt <- reactiveVal( NULL ) 
 
observeEvent(input$map_catchSelect_click,{
 req(input$map_catchSelect_click)
  cat("click on catch map  -")
    selectedCatch_meta <- selectedCatch_meta() 
     temp_pt <- st_sfc(st_point(c(  input$map_catchSelect_click$lng, input$map_catchSelect_click$lat) ), crs = st_crs(4326)) 
    new_name <- intercatch_on_map_sub()$WB_NAME[lengths(st_intersects(intercatch_on_map_sub(), temp_pt)%>%suppressMessages())>0]
    cat(paste0("\nclicked on catchment: ",new_name,"  - "))    
    leafletProxy("map_catchSelect", session)  %>% clearGroup("selectedCatch")
                 if(length(new_name)>0){ 
                                 if(new_name  %in% selectedCatch_meta) {
                                 cat("removed  - ")
                                  selectedCatch_meta <- selectedCatch_meta[-which(selectedCatch_meta == new_name)]  
                                  } else { 
                                  cat("added  - ") 
                                  selectedCatch_meta <- c(new_name , selectedCatch_meta)
                                 if("none selected" %in% selectedCatch_meta) { selectedCatch_meta <- selectedCatch_meta[-which(selectedCatch_meta == "none selected")]  }
                                               } 
                           if(length(selectedCatch_meta)>0){
                             cat("map")
                             leafletProxy("map_catchSelect", session)  %>% 
                                           addPolygons(data=intercatch_on_map_sub()[intercatch_on_map_sub()$WB_NAME  %in% selectedCatch_meta,],
                                                                 fillColor="magenta", fillOpacity=0,
                                                       color="black",opacity=1, weight=2,group="selectedCatch" ) 
                           } else {  
                             selectedCatch_meta <- "none selected" }
                          print(selectedCatch_meta)
                          selectedCatch_meta %>% selectedCatch_meta()
            } else { 
   cat("clicked outside selection")
   error_message1 <-  "no selection: click located outside exploratory site"  
   error_message2 <- "define a larger exploratory site to select further catchments" 
  # notify_warning(error_message1) # timeout type = c("success", "failure", "info", "warning"),
   notify_warning(error_message1,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_dbl",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
   notify_info(error_message2,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))
            }
    
     
  selectedCatch_metaTxt <-tagList( div(strong("river catchment selection: "), style="color:#555;top:6vh;position:relative;"),
                                   div(HTML(paste0(  selectedCatch_meta, sep= "<br>")),style="color:#555;top:6.6vh;position:relative;") )
  selectedCatch_metaTxt %>% selectedCatch_metaTxt()  
 }, ignoreNULL=FALSE, ignoreInit=FALSE) 
 
 

observeEvent(input$metaselect_listGlob,{
  if( "river catchment" %in% input$metaselect_listGlob){
    selectedCatch_metaTxt(div(strong("river catchment selection: none"), style="color:#555;top:6vh;position:relative;"))
    } else {
      selectedCatch_metaTxt(NULL)}
})

  
  
output$selectedCatch_metaTxt <- renderUI ({ selectedCatch_metaTxt()  })  


output$uiMetaSelect_intermap <- renderUI({   
  req(input$metaselect_listGlob)
  req("river catchment" %in% input$metaselect_listGlob)# {
   cat("ui catch select") 
    shape <- "a buffer of "
    if(input$shape == "catchment") {shape <- c("catchment geometries within ",shape)}
    txt <- paste0("click to select river catchment(s)")
    ## click on map to select catchment(s) ?
   return(tagList(  leaflet::leafletOutput("map_catchSelect" , height="200px", width="200px") ,
                    tags$style(HTML('#map_catchSelect {border-radius:100px}')),
                    bsTooltip("map_catchSelect", txt,placement = "top", trigger = "hover") ))
 # } else { 
# return(NULL) }
})

 

observeEvent(input$metaselect_submit,{ 
  removeModal()
    "selected map geometry" %>% current_metaShape_placeholder()
    "selected map geometry" %>% newfeature_title() 
     "mapGeom" %>% current_drawnShapeGeom()
intercatch_on_map_sub(NULL)
 
#  selectedCatch_meta(NULL) # reset to reset maps and lsit
})   
         
newFeatureSel <- reactiveVal(0)
 


observeEvent(mapGeomFeature(), {
  cat("new selection of map geoms  - ")
  print(current_metaShape_placeholder())
 # "selected map geometry" %>% newfeature_title()
  #"mapGeom" %>% current_drawnShapeGeom()
}, ignoreNULL=TRUE)
 








 

   
observeEvent(input$mapReleaseSite_draw_new_feature,{ # create features
  cat("new shape") 
   
if(input$mapReleaseSite_draw_new_feature$properties$feature_type != "rectangle"){
   "new drawn geometry" %>% newfeature_title()
  newFeature <- newFeature()
         newFeature <-  as.numeric(newFeature)+1
         featureDat <-  newDrawnFeature( newFeat=input$mapReleaseSite_draw_new_feature, run=newFeature, sim_Nyrs=as.numeric(input$sim_Nyrs )   ) 
          shapeGeom  <-  featureDat$shapeGeom 
           print(shapeGeom)
           featNum <- 1

      if(shapeGeom == "line")   {
        print("line")
          lineFeature <- lineFeature()
           if(!is.null(lineFeature)) { 
               featNum <- nrow(lineFeature)+1 
               while( paste0(featureDat$data$name,paste0(featNum)) %in% lineFeature()$name ){
                 featNum <- featNum+1
               }
          }
           featureDat$data$name <- paste0(featureDat$data$name,featNum)
           rbind(featureDat$data,lineFeature())  %>% lineFeature()  
      } 
      if(shapeGeom == "polygon")   {     
             print("polygon")
          polyFeature <- polyFeature()
             if(!is.null(polyFeature)) {
              
               featNum <- nrow(polyFeature)+1 
               while( paste0(featureDat$data$name,paste0(featNum)) %in% polyFeature()$name ){
                 featNum <- featNum+1
               }
               cat("polyfeat exists  - ")
               print(polyFeature)
                        #deleteDrawnShape(polyFeature$LeafletId) 
                    #    leafletProxy("mapReleaseSite", session)  %>% removeShape(layerId=as.character(polyFeature$LeafletId) )
             }
             featureDat$data$name <- paste0(featureDat$data$name,featNum)
             rbind(featureDat$data,polyFeature())  %>% polyFeature()  
      }
      if(shapeGeom == "point")   {
         print("point")
          pointFeature <- pointFeature()
             if(!is.null(pointFeature)) { 
               cat("pointfeat exists  - ") 
               featNum <- nrow(pointFeature)+1 
               while( paste0(featureDat$data$name,paste0(featNum)) %in% pointFeature()$name ){
                 featNum <- featNum+1
               }
             } 
      #      leafletProxy("mapReleaseSite", session)  %>% removeShape(layerId=as.character(pointFeature$LeafletId) )

          #featureDat$layerType   <- 
          #  featureDat$layerEffect
           featureDat$data$name <- paste0(featureDat$data$name,featNum)
           rbind(featureDat$data,pointFeature())  %>% pointFeature()  
      }
        print("current feature - ")
   print( featureDat$data)    
  #add_labelOnly(FALSE)
   newFeature %>% newFeature()  
   shapeGeom %>% current_drawnShapeGeom()
   featureDat$data$name %>% current_metaShape_placeholder()
  }
}, ignoreNULL=TRUE, ignoreInit=TRUE)
 

newCells3857 <- reactiveVal()

observeEvent(newCells3857(),{
  cat(paste0("\n---- newCells3857: ", length(newCells3857()),"  ",   featureName()," / ",featureEffect()))
  if(!is.null(NdispCellsNew())) {cat(paste0("\ncells = ",NdispCellsNew(),"disp + ",NsuitCellsNew(),"suit + ",NunsCellsNew()," unsuit  - ")) }
}, ignoreNULL=TRUE, ignoreInit=FALSE)  

##################################### user edits drawn features on release map  - replace existing
observeEvent(input$mapReleaseSite_draw_edited_features, { ###editing features
  cat("draw edited feature - ")
  req(!is.null(unlist( input$mapReleaseSite_draw_edited_features$features)))  # if not req, crashes when selecting edit then saving without editing
  
   newFeat <-input$mapReleaseSite_draw_edited_features$features[[1]] 
   cat("replace feature #")
   id <- newFeat$properties$`_leaflet_id`
   print(id)
   newFeature <- newFeature()
   newFeature <-  as.numeric(newFeature)+1
   featureDat <-  newDrawnFeature( newFeat= newFeat, run=newFeature,sim_Nyrs=as.numeric(input$sim_Nyrs )  ) 
   shapeGeom  <-  featureDat$shapeGeom 
   if(shapeGeom == "line")   {
        cat("line")  
     lineFeature <- lineFeature()  
     lineFeature$geometry[lineFeature$LeafletId == id ] <- featureDat$data$geometry
     lineFeature$size[lineFeature$LeafletId == id ] <- featureDat$data$size
 #    lineFeature$name[lineFeature$LeafletId == id ] <- featureDat$data$name
     lineFeature %>% lineFeature() 
   } 
   if(shapeGeom == "polygon")   {
       cat("polygon")
      polyFeature <-  polyFeature() 
      polyFeature$geometry[polyFeature$LeafletId == id ] <- featureDat$data$geometry
      polyFeature$size[polyFeature$LeafletId == id ] <- featureDat$data$size
#      polyFeature$name[polyFeature$LeafletId == id ] <- featureDat$data$name
      polyFeature %>%  polyFeature()  
   }
      if(shapeGeom == "point")   {
       cat("point")
      pointFeature <-  pointFeature() 
      pointFeature$geometry[pointFeature$LeafletId == id ] <- featureDat$data$geometry
      pointFeature$size[pointFeature$LeafletId == id ] <- featureDat$data$size
#      pointFeature$name[pointFeature$LeafletId == id ] <- featureDat$data$name
      pointFeature %>%  pointFeature()  
   }
   newFeature %>% newFeature() 
}, ignoreNULL=TRUE, ignoreInit=TRUE)


############################################### modification by user in metadata recap table - save changes
observeEvent(feature_amendId(),{
  cat("amend geoms metadata  - ")
  feature_amendId <- feature_amendId()
  featureVar_amendId <- featureVar_amendId()
  featureVal_amendId <- featureVal_amendId()
    if(feature_amendId %in% lineFeature()$LeafletId){
    cat("amend line metadata  - ")
    dat <- lineFeature()  }
   if(feature_amendId %in% polyFeature()$LeafletId){
    cat("amend poly  - ")
     dat <-   polyFeature()    }
    if(feature_amendId %in% pointFeature()$LeafletId){
    cat("amend  point  - ")
     dat <-   pointFeature()    } 
   if(feature_amendId %in% uploadFeature()$LeafletId){
    cat("amend upload  - ")
     dat <-  uploadFeature()    } 
   if(feature_amendId %in% mapGeomFeature()$LeafletId){
    cat("amend mapGeom  - ")
     dat <-  mapGeomFeature()    } 
    
     dat[dat$LeafletId == feature_amendId,featureVar_amendId] <- featureVal_amendId 
    
  if(feature_amendId %in% lineFeature()$LeafletId){  dat %>% lineFeature()  }
   if(feature_amendId %in% polyFeature()$LeafletId){  dat  %>%polyFeature()     }
    if(feature_amendId %in% pointFeature()$LeafletId){ dat  %>% pointFeature()  } 
  if(feature_amendId %in% uploadFeature()$LeafletId){  dat %>% uploadFeature()  } 
  if(feature_amendId %in% mapGeomFeature()$LeafletId){  dat %>% mapGeomFeature()  }
   
     
     print (dat[dat$LeafletId == feature_amendId, ])  
  dat[dat$LeafletId == feature_amendId, ] %>% newdrawnShape()
  feature_amendId %>% newFeature_amendId()  
  feature_amendId(NULL)
  gather_metadata(1)
}, ignoreNULL=TRUE)#looloo


newFeature_amendId <- reactiveVal()
featureVal_amendId<- reactiveVal(NULL)
featureVar_amendId<- reactiveVal(NULL)
feature_amendId <- reactiveVal(NULL)
newFeature_deleteId <- reactiveVal(NULL)



observeEvent(input$DTmetaTable_modifHab_cell_click, {
req(base::isTRUE(disable_tableEdits()))
   error_message1 <-  "simulation in progress: protected input"  
   error_message2 <-  "new geometries can be added but metadata used in the simulation should not be amended" 
    notify_warning(error_message1,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_dbl",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
   notify_info(error_message2,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",   
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
})


observeEvent(input$DTmetaTable_modifHab_cell_edit, {
  req(base::isTRUE(disable_tableEdits()) )  
  print("modify habitat Features Table edit   - ")  
       metaTable_modifHab()$LeafletId[input$DTmetaTable_modifHab_cell_edit$row] %>% feature_amendId() 
      (input$DTmetaTable_modifHab_cell_edit$col +1) %>% featureVar_amendId()
      input$DTmetaTable_modifHab_cell_edit$value  %>% featureVal_amendId()
 })
   




observeEvent(input$DTmetaTable_areaOfInt_cell_edit, {
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == FALSE) {  
  print("areaOfInt Features Table edit   - ")  
      metaTable_areaOfInt()$LeafletId[input$DTmetaTable_areaOfInt_cell_edit$row] %>% feature_amendId() 
     (input$DTmetaTable_areaOfInt_cell_edit$col +1) %>% featureVar_amendId()
     input$DTmetaTable_areaOfInt_cell_edit$value  %>% featureVal_amendId()
 } else { 
   error_message1 <-  "simulation in progress: protected input"  
   error_message2 <-  "new geometries can be added but metadata used in the simulation should not be amended" 
    notify_warning(error_message1,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_dbl", 
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
   notify_info(error_message2,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",   
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
   }
  })


  
             



 

##################################### recap table - drawn features (restrict)  
 
  
newFeaturesDat_shp <- reactiveVal(NULL)
newFeaturesDat <- reactiveVal(NULL)
#({
#  req(newFeaturesDat_shp())
# return(newFeaturesDat_shp() %>% st_drop_geometry() %>% as.data.frame() )
#  })
##im computing _shp but not plotting it until locked - that way can use the delete geoms popton in code


 

 

##################################### map all drawn and uploaded features 

newFeaturesDat_display <- reactiveVal(NULL) 
 
                       

observeEvent(newFeaturesDat_display() ,{ 
  req(mapExists()==TRUE)
  cat("update relevant feature type in display metadata df  - ")
     metaTable_modifHab <- metaTable_records <- metaTable_areaOfInt <- NULL    
 
     ## rename default save id only if not used in model
   #  if(base::isFALSE(model_running())) { paste0("_",format(Sys.time(), "%d%b%y"),"_",format(Sys.time(), "%H%M")) %>% saveName_placeholder()}

       newFeaturesDat_display <- newFeaturesDat_display()
  if(length(newFeaturesDat_display )>0){
     cat(paste0("update with new ",featureName(),"  - ")) 
     newFeaturesDat_display <- na.exclude(newFeaturesDat_display) # can be some leftover start geoms when user cancels, safeguards by removing NAs
     

      newFeaturesDat_display %>% newFeaturesDat_display()
   if("modify habitat" %in% newFeaturesDat_display$layerType) {
     cat("dat table  modify habitat - ")
    metaTable_modifHab <- newFeaturesDat_display %>% filter(layerType == "modify habitat") }  
 if("observation records" %in% newFeaturesDat_display$layerType) {
   cat("dat table observation records  - ")
     metaTable_records <- newFeaturesDat_display %>% filter(layerType == "observation records") }  
 if("area of interest" %in% newFeaturesDat_display$layerType) {
   cat("dat table area of interest - ")
     metaTable_areaOfInt <- newFeaturesDat_display %>% filter(layerType == "area of interest") }
   
       
     if(!is.null(outDat_areaOfInterest())){
          outDat_areaOfInterest <- outDat_areaOfInterest()
          outDat_areaOfInterest[outDat_areaOfInterest$LeafletId %in% metaTable_areaOfInt()$LeafletId, ] %>% outDat_areaOfInterest()
          print(outDat_areaOfInterest[outDat_areaOfInterest$LeafletId %in% metaTable_areaOfInt()$LeafletId, ] )
       }  
  }
       
#    if(featureName() == "area of interest")   {
      metaTable_areaOfInt %>% metaTable_areaOfInt()
#      }
#    if(featureName() == "modify habitat")   {
      metaTable_modifHab %>% metaTable_modifHab()
#      }
#    if(featureName() == "observation records")   {
      metaTable_records %>% metaTable_records()
#      }
#    metaTable_areaOfInt %>% metaTable_areaOfInt() 
  
  if(featureName() != "observation records") {
    cat("go map geomfeatures  - ")
    mapNewFeature(1)
  } else { 
     cat("go map obs recs  - ") 
     mapNewObs(1)
  } 
}, ignoreNULL=FALSE,ignoreInit=FALSE)
   
gather_metadata <- reactiveVal(NULL)
###################################################### gather geoms metadata into newFeaturesDat_shp()
#observeEvent({ 
#  pointFeature()
#  lineFeature()
#  polyFeature()
#  uploadFeature()
#  mapGeomFeature()
#  1
#},{


pSc <-FALSE # print script


observeEvent(gather_metadata(),{
  cat("---- gather metadata  - ") 
  pointFeature <- pointFeature()
  if(!is.null(pointFeature)){ 
    if(nrow(pointFeature)==0) {
      pointFeature <- NULL
  pointFeature(NULL)}} 
  mapGeomFeature <-mapGeomFeature()
  if(!is.null(mapGeomFeature)){ 
    if(nrow(mapGeomFeature)==0) {
      mapGeomFeature <- NULL
      mapGeomFeature(NULL)}} 
 
 newFeaturesDat_shp <-  rbind(pointFeature, lineFeature(),polyFeature(),  uploadFeature(),mapGeomFeature )  # assemble constantly but plot separately so geoms are separate lines and polys
 newFeaturesDat_shp <-  newFeaturesDat_shp [!is.na(newFeaturesDat_shp$LeafletId),]  
 newFeaturesDat_shp %>% newFeaturesDat_shp()
 newFeaturesDat <- newFeaturesDat_shp  %>% st_drop_geometry() %>% as.data.frame()  
 if(nrow(newFeaturesDat)==0){newFeaturesDat <- NULL}
 newFeaturesDat %>% newFeaturesDat()
 newFeaturesDat_display <- NULL
   if(!is.null(newFeaturesDat)){ 
   cat("add buttons  - ") 
   newFeaturesDat_display <- addDeleteButtons( trigger=!is.null(newFeaturesDat), condition= (nrow(newFeaturesDat)>0 ),
       suffname="feat", Nbuttons=nrow( newFeaturesDat),  df = newFeaturesDat)   
   }
if(pSc == TRUE) {
  print("=======")
  print(newFeaturesDat_display)}
 
 newFeaturesDat_display %>% newFeaturesDat_display()
   
   gather_metadata(NULL) 
    req(!is.null(featureName()))   
    mapNewObs(NULL)
    mapNewFeature(NULL)
    
     
     #    cat("geomfeatures - ")
#   mapNewFeature(1)
#  } else { 
#     cat("map obs recs  - ")
#    print(mapNewObs())
#     mapNewObs(1)
#  }
 }, ignoreNULL=TRUE, ignoreInit=TRUE) # st_transform elsewhere more efficient obvs
 
   

######################################################## delete row metadata - newFeature_deleteId() and featurename
observeEvent(input$current_id, {
  
  
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == FALSE) {  
     if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete_feat")){
        newFeaturesDat_display <- newFeaturesDat_display()
      dt_row <- which(stringr::str_detect(newFeaturesDat_display()$Buttons, pattern = paste0("\\b", input$current_id, "\\b")))  
      newFeaturesDat_shp<- newFeaturesDat_shp() 
     print( newFeaturesDat_shp)
     newFeaturesDat_shp$layerEffect[dt_row] %>% featureEffect()
     newFeaturesDat_shp$layerType[dt_row] %>% featureName()
     newFeaturesDat_shp$LeafletId [ dt_row ] %>% newFeature_deleteId() 
     newFeaturesDat_shp[dt_row,] %>% newdrawnShape()
     cat(paste0("user deleted ",input$current_id,", df row: ",dt_row,"  - "))
     }
 }
}) 
amendAll_actionType <- reactiveVal(NULL)
newFeature_deleteId <- reactiveVal(NULL)



###########  delete  line or poly feature     ########
observeEvent(newFeature_deleteId(),{ 
cat("deleting : ")
  newFeature_deleteIds <- newFeature_deleteId()
  add_labelOnly(FALSE)
  for (newFeature_deleteId in newFeature_deleteIds) {
     cat( newFeature_deleteId ,": ")

    dat <-  pointFeature()
    if(newFeature_deleteId %in% dat$LeafletId){
    cat("point  - ")
      
       dat[-which(dat$LeafletId == newFeature_deleteId),]   %>%  pointFeature() 
       add_labelOnly(TRUE) # added mar20
  #    dat <-  obsRecs_famDf()
   #   dat <-dat[-which(dat$LeafletId == newFeature_deleteId),]  
    #  dat %>% obsRecs_famDf() # newFeature_deleteId still informed with this point id here so will allow applyeffect  running
     if(!is.null( obsRecs_famDf())){
       obsRecs_famDf()[-which(obsRecs_famDf()$LeafletId == newFeature_deleteId),]  %>% obsRecs_famDf()
       obsRecs_famDf_display()[-which(obsRecs_famDf_display()$LeafletId == newFeature_deleteId),]  %>% obsRecs_famDf_display()
       pointFeature_terrPols()[-which(pointFeature_terrPols()$LeafletId == newFeature_deleteId),] %>% pointFeature_terrPols()
      cat("fam obs tabs updted  - ")
      }}
      
    dat <- lineFeature()   
      if(newFeature_deleteId %in% dat$LeafletId){
    cat("line - ") 
      dat[-which(dat$LeafletId == newFeature_deleteId),] %>% lineFeature()
   #mar15       newFeature_deleteId(NULL) 
          }   
  
      dat <- mapGeomFeature()
    if(newFeature_deleteId %in% dat$LeafletId){
    cat("mapGeom  - ") 
      dat[-which(dat$LeafletId == newFeature_deleteId),]  %>%  mapGeomFeature()
 #mar15         newFeature_deleteId(NULL) 
          }   
    
 
    dat <- uploadFeature()
    if(newFeature_deleteId %in% dat$LeafletId){
    cat("poly  - ") 
      dat[-which(dat$LeafletId == newFeature_deleteId),]  %>%  uploadFeature()
         #newFeature_deleteId(NULL)     # newFeature_deleteId still informed with this point id here so will allow applyeffect  running
         }
    
   
    dat <- polyFeature()
    if(newFeature_deleteId %in% dat$LeafletId){
    cat("poly  - ") 
      dat[-which(dat$LeafletId == newFeature_deleteId),]  %>%  polyFeature()
 #mar15      newFeature_deleteId(NULL) 
      }
  
   # newFeature_deleteId <- NULL # dont retrigger for geoms other than point  
# mar 15 newFeature_deleteId - newdrawnshape update after submit, id newFeature_deleteId still informed it will delete in the DFs THEN the map
  
  } # each feat ID - change into an apply or smthg neater later
   
  ######## tadam
  gather_metadata(1)  
  newdrawnShape(NULL) # so new id is detected as 'to delete' not a new shape to add
#    applyEffect_trigger <- applyEffect_trigger()            ## trigger effect after compiling  gather_metadata(1) maybe? that way no feature no layer from start
#     (applyEffect_trigger+1) %>% applyEffect_trigger() # new did it gather_metadata(1) check!
#      gather_metadata(NULL)     # featureEffect
       #deleteid generated along feature name and effect so can rerun the hab modif code after that
}, ignoreNULL=TRUE, ignoreInit=FALSE)  

   
metaTable_modifHab <- reactiveVal(NULL)  
metaTable_records <- reactiveVal(NULL) 
metaTable_areaOfInt <- reactiveVal(NULL)
  
    
  
  
##################################### display recap table:  drawn and uploaded features (restrict)  
 
 
output$metaTable_modifHab <- renderUI({ 
  if( is.null(metaTable_modifHab())) {
    return(div( "no metadata: draw or upload a spatial feature to modify habitat", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))#jojo
    }  else {
    if( nrow(metaTable_modifHab())==0){return(div("no metadata: draw or upload a spatial feature to modify habitat", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))}
      if( nrow(metaTable_modifHab())>0){ 
    return(div(DTOutput('DTmetaTable_modifHab' ),
      style="line-height: 90%;font-size: 90%;width: 90%; height: auto; left: 7%; bottom: 8px; top: 8px; max-height: 8vh; position: relative; text-align: center; overflow-y: scroll;scrollbar-width: thin;"))
}}
      }) 
output$metaTable_records <- renderUI({ 
  if( is.null(metaTable_records())) {
    return(div("no metadata: draw or upload a spatial feature to add observations", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))
    }  else {
    if( nrow(metaTable_records())==0){return(div("no metadata: draw or upload a spatial feature to add observations", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))}
      if( nrow(metaTable_records())>0){ 
    return(div(DTOutput('DTmetaTable_records' ),
      style="line-height: 90%;font-size: 90%;width: 90%; height: auto; left: 7%; bottom: 8px; top: 8px; max-height: 8vh; position: relative; text-align: center; overflow-y: scroll;scrollbar-width: thin;"))
}}
      }) 


output$metaTable_areaOfInt <- renderUI({ 
  metaTable_a()
      }) 

metaTable_a <- reactive({
  metaTable_areaOfInt()
 if( is.null(metaTable_areaOfInt())) {
    return(div("no metadata: draw or upload a spatial feature to generate a detailed report over an area", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))
    }  else {
    if( nrow(metaTable_areaOfInt())==0){return(div("no metadata: draw or upload a spatial feature to generate a detailed report over an area", style="padding: 0 62px;color:darkmagenta;font-size:90%;"))}
      if( nrow(metaTable_areaOfInt())>0){ 
    return(div(DTOutput('DTmetaTable_areaOfInt' ),
      style="line-height: 90%;font-size: 90%;width: 90%; height: auto; left: 7%; bottom: 8px; top: 8px; max-height: 8vh; position: relative; text-align: center; overflow-y: scroll;scrollbar-width: thin;"))
      }}
}) 
                                                                               

output$DTmetaTable_modifHab <-  DT::renderDT({   
   req(!is.null(metaTable_modifHab()))
   req(nrow(metaTable_modifHab())>0)
   print("metadata modify habitat table  - ")
  dat <- data.frame(metaTable_modifHab() )
  dat <- dat[,-(which(colnames(dat)=="layerEffect"))]
   if( "Buttons" %in% colnames(dat) ) {
   buttons_cols <-  which(colnames(dat)=="Buttons")-1
   invis_cols <- buttons_cols-1
   colNames <- c( "<span style='font-size:17px;'>&#8635;</span>"="Buttons")
   colDisab <- c( 2,3,6)
   } else {
      colNames <- colnames(dat)[-1]
      buttons_cols <- invis_cols <- 6 
      colDisab <- c(1:(ncol(dat)-1) )
    }
   datatable(  dat  , 
      colnames=colNames   ,  ###lolo
      rownames = FALSE, escape=FALSE, selection = "none",
      editable = list(target = "cell",disable = list(columns =  colDisab)) ,
      options = list(dom = 't', ordering=F, 
                      columnDefs = list( list(className = "dt-center", targets = c(1:ncol(dat)-1))    , 
                              list(className = "notselectable", targets = c(1:ncol(dat)-1)),
                              list(className ='dt-grey', targets = c(1,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(1,buttons_cols)), 
                              list(visible = FALSE, targets = c(0,invis_cols)))  )
      )          
 }  , server = FALSE) 
            
   
output$DTmetaTable_records <-  DT::renderDT({   
   req(!is.null(metaTable_records()))
   req(nrow(metaTable_records())>0)
 cat("metadata sightings table  - ")
  dat <- data.frame(metaTable_records() ) 
  dat <- dat[,-(which(colnames(dat)=="layerEffect"))]
  if( "Buttons" %in% colnames(dat) ) {
   buttons_cols <-  which(colnames(dat)=="Buttons")-1
   colNames <- c( "<span style='font-size:17px;'>&#8635;</span>"="Buttons")
    } else {
      colNames <- colnames(dat)
      buttons_cols <- ncol(dat)-1
    }# buttons_cols <-  which(colnames(dat)=="Buttons")-1
   datatable(  dat  ,
      colnames=colNames ,
      rownames = FALSE, escape=FALSE, selection = "none",
      editable = FALSE, 
      options = list(dom = 't', ordering=F, "pageLength" = 200,
                      columnDefs = list( list(className = "dt-center", targets = c(1:ncol(dat)-1))    , 
                              list(className = "notselectable", targets = c(1:6)),
                              list(className ='dt-grey', targets = c(1,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(1,buttons_cols)), 
                              list(visible = FALSE, targets = c(0,buttons_cols-1)))  )
      )          
 }  , server = FALSE) 



output$DTmetaTable_areaOfInt  <-  DT::renderDT({   
   req(!is.null(metaTable_areaOfInt())) 
   req(nrow(metaTable_areaOfInt())>0) 

    print("metadata area of interest table  - ")
  dat <- data.frame(metaTable_areaOfInt() )
  print(dat)
  dat <- dat[,-(which(colnames(dat)=="layerEffect"))]
   if( "Buttons" %in% colnames(dat) ) {
   buttons_cols <-  which(colnames(dat)=="Buttons")-1
   colNames <- c( "<span style='font-size:17px;'>&#8635;</span>"="Buttons")
    } else {
      colNames <- colnames(dat)
      buttons_cols <- ncol(dat)-1
    }
  
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == TRUE) { disbEdits <- c(0:ncol(dat)-1) } else { disbEdits <- c(2,3,6) }
  
  #buttons_cols <-  which(colnames(dat)=="Buttons")-1
   datatable(  dat  ,
      colnames=colNames ,
      rownames = FALSE, escape=FALSE, selection = "none",
      editable = list(target = "cell",disable = list(columns = disbEdits )),
      options = list(dom = 't', ordering=F, 
                      columnDefs = list( list(className = "dt-center", targets = c(1:ncol(dat)-1))    , 
                              list(className = "notselectable", targets = c(1:6)),
                              list(className ='dt-grey', targets = c(1,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(1,buttons_cols)), 
                              list(visible = FALSE, targets = c(0,buttons_cols-1)))  )
      )          
 }  , server = FALSE)# 

 
##################################### upload features (metadata) - shp
 
count_upload <- reactiveVal(0) 
    
 
observeEvent(input_fileUpload_metadata(),{ 
   req(!is.null(input_fileUpload_metadata()))
   cat("clean new upload  - ")     
   shpdf <- input_fileUpload_metadata()
   # input_fileUpload_metadata(NULL)  # formats validated / filtered # now null after processing so conditions whether to proceed with processing
    input_fileUpload_ready <- 1 
      
    
 #   tempdirname <- dirname(shpdf$datapath[1])
#    for (i in 1:nrow(shpdf)) {
#      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]) )
#    }
  #  indx <- grep(pattern = "*.shp$", shpdf$name)
    if("sf" %in% class(shpdf)){ #length(indx)>0){
                cat(".shp  - ")
 #               shpData0 <- st_read(paste(tempdirname,shpdf$name[indx],sep = "/" )) %>% st_make_valid()
                shpData0 <- shpdf%>% st_transform(  crs=st_crs(4326))

       } else { 
                cat(".csv  - ")  
                indx <- grep(pattern = "*.csv$", shpdf$name)
                tempdirname <- dirname(shpdf$datapath[1])
                file.rename(shpdf$datapath , paste0(tempdirname, "/", shpdf$name[1]) )
                 shpData0 <-  shpdf
                 shpData0 <- read.csv(paste(tempdirname,shpdf$name[1],sep = "/" ), stringsAsFactors=FALSE) %>% as.data.frame() 
                 # identify lng/lat cols..
                colnames(shpData0) <- tolower(colnames(shpData0))
                nams <- colnames(shpData0) 
                lngcol <-c(which( nams=="x"), which(stringr::str_detect(nams, pattern = "lng|long|lon|longitude")==TRUE)) [1]
                latcol <- c(which(nams=="y"), which(stringr::str_detect(nams, pattern = "lat|latitude")==TRUE)) [1]
                # note - cant test on part of string with (x,y) because of Year and X.x index default
                  
               
                 
              if(any(is.na(latcol), is.na(latcol))){  
                cat("fail: no coords found in .csv  - ") 
               # input_fileUpload_metadata(NULL) 
                input_fileUpload_ready <- NULL
                notify_failure("can't find coordinates in uploaded file" , timeout=4000,  
                           config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                                         fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl",  
                                         background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
              #  reset('fileUpload_metadata')

                 } else {
                
                colnames(shpData0)[lngcol] <- "lng"
                colnames(shpData0)[latcol] <- "lat"
                shpData0 <- shpData0 [!is.na(shpData0$lng) & !is.na(shpData0$lat),]
                shpData0 <- shpData0 #[1:3,]# for now - takes ages?!  
                   
                if(any(shpData0$lng>500)) {
                  cat("bng coords  - ")
                  shp_crs <- st_crs(27700)
                  shpData0 <- st_as_sf(shpData0,   coords = c("lng","lat"), crs=shp_crs) 
                  shpData0 <- shpData0 %>% st_transform(  crs=st_crs(4326))
                  } else {
                  shp_crs <- st_crs(4326)
                  shpDatatest <- shpData0 
                #  leafletProxy("mapReleaseSite", session) %>% addMarkers(data=shpDatatest, lng=~lng, lat=~lat, group="temp")
                  shpData0 <- st_as_sf(shpData0,   coords = c("lng","lat"), crs=shp_crs) 
               }
          
                 
          #      if(any(shpData0$lng>500)) {shpData0 <- shpData0 %>% st_transform(  crs=st_crs(4326))
          #      }
               # print(shpData0 )#<- cbind(shpData,shpData0)
              #  leafletProxy("mapReleaseSite", session) %>% addCircleMarkers(data=shpData0%>%st_sf(), fillColor="gray44",color="red",radius=4,group="temp", label="...")
                    
                 }
      }
   cat("validate GIS  -") 
    
  if(!is.null(input_fileUpload_ready)){
    
    invalgeom <-  which(st_is_valid(shpData0)==FALSE)
    ninvalgeom <- length(invalgeom)
    invalidGeom_warn <- paste0("found ", nrow(shpData0)," geometries")
    if(ninvalgeom>0) {
    invalidGeom_warn <- paste0( invalidGeom_warn ," (incl. ", ninvalgeom," invalid, removed)")
     shpData0 <- shpData0 [-invalgeom,] #%>% st_make_valid()
    }
     
    invalidGeom_warn  %>% invalidGeom_warn()
    cat(c(invalidGeom_warn, "  - "))
     ## what type of geom is it
    if( any(c("POLYGON","MULTIPOLYGON") %in% st_geometry_type(shpData0) )) {
        cat("poly geoms  - ")
          shpData <- st_union(shpData0 [st_geometry_type(shpData0) %in% c("POLYGON","MULTIPOLYGON"),])%>% st_cast("MULTIPOLYGON") 
          shpData27700 <- st_transform(shpData0, crs=st_crs(27700))
          size <- paste0(round(sum(as.numeric(st_area(shpData27700))*1e-4),-2),"ha") 
           "poly" %>% uploadGeomType()    
    }
    if( any(c("POINT","MULTIPOINT") %in% st_geometry_type(shpData0) )) {
        cat("point geoms  - ")
      #browser()
          shpData <- shpData0[st_geometry_type(shpData0) %in% c("POINT","MULTIPOINT"),]%>% st_cast("POINT")  
          keepFeat <-  which(lengths(st_intersects(shpData ,country_boundaries())%>%suppressMessages())==1)
          size <- paste0(nrow(shpData),"pts")
          shpData <- shpData[keepFeat,]
          leafletProxy("mapReleaseSite", session) %>% addCircleMarkers(data=shpData%>%st_sf(), fillColor="#ffe62cc2",fillOpacity=1,color="gray44",radius=5,group="temp", 
                                                                       label="territory simulation in progress",
                                                                       labelOptions = labelOptions(  style = as.list(c("color"= "gray44", "background-color" =  "#ffe62cc2"  ,labstyle_routeLabs)) , direction = "top" ),)
          shpData <- shpData %>% st_union()
         "point" %>% uploadGeomType()   
 
    } 
      
    cat("template attr - ")  
    
      if(sum(which(lengths(st_intersects(shpData,country_boundaries())%>%suppressMessages())>0))==0) { # upload is not within country boundaries, cancel
       removeModal()
      showModal(modalDialog( size =  "s",       
                     div(img(src="attention.png", width="37px", height="37px"), style="padding:4vh 0 0 0;text-align:center;vertical-align: middle;") ,
                     p(paste0("upload cancelled") , style="padding: 2vh 10px;font-size:20px;vertical-align: middle;text-align:center;") , 
                     div("the feature is not located within the country boundaries"  , style="padding:9px; vertical-align: middle; text-align: center; color: darkmagenta;"),
                        br(),
          footer=tagList(
             div(actionButton('upload_cantdo','ok' ),style="width: fit-content;display: inline-block;") 
          ))    )  
      current_metaShape_placeholder(NULL) # requires changing each time so null after done
      input_fileUpload_ready <- NULL     
     }  else {
 
     count  <- count_upload()+1
     count  %>% count_upload()
         newuploadFeature <- st_geometry(shpData) %>% st_combine()  %>% st_sf()
                        newuploadFeature$LeafletId = paste0("Leaflet",count)
                        newuploadFeature$name <- paste0("upload_",count)   
                        newuploadFeature$type <-paste0("<img src='uploaded.png' height='14' width='14'></img>")  
                        newuploadFeature$size <- size
                        newuploadFeature$start=0
                        newuploadFeature$duration=NA
                        newuploadFeature$layerType=NA
                        newuploadFeature$layerEffect=NA 
  ##here rasterize so always same geometry type? but not as pretty                 
 uploadFeature <- uploadFeature()
 rbind(newuploadFeature,uploadFeature) %>%  uploadFeature()
 cat("ready  - ")
      
      
shpData <- shpData0 %>% st_drop_geometry() %>% as.data.frame()
 nams <- tolower(colnames(shpData))
yearcol <- which(stringr::str_detect(nams, pattern = "year")==TRUE) [1] 
females_col <- which(stringr::str_detect(nams, pattern = "fem|num.f")==TRUE) [1] 
males_col <- which(stringr::str_detect(nams, pattern = "mal|num.m")==TRUE) [1] 

if(!is.na(females_col)){ ## year column found in data, use?
 females_Dat <- as.numeric(as.character(shpData[keepFeat,females_col])) 
 females_Dat[is.na(females_Dat)] <- 1
 females_Dat %>% newuploadFeature_femColumn()
}


if(!is.na(males_col)){ ## year column found in data, use?
 males_Dat <- as.numeric(as.character(shpData[keepFeat,males_col])) 
 males_Dat[is.na(males_Dat)] <- 1
 males_Dat %>% newuploadFeature_malColumn()
}

if(!is.na(yearcol)){ ## year column found in data, use?
 yearDat <- as.numeric(as.character(shpData[keepFeat,yearcol])) 
 yearDat[is.na(yearDat)] <- 2023
 yearDat %>% newuploadFeature_yearColumn()
  }
     }
  }
input_fileUpload_ready %>% input_fileUpload_ready() 
enable("metadata_submit")

}, ignoreInit=FALSE, ignoreNULL=TRUE)  
 
input_fileUpload_ready <- reactiveVal()  
newuploadFeature_yearColumn <- reactiveVal(NULL)
newuploadFeature_femColumn<- reactiveVal(NULL)
newuploadFeature_malColumn<- reactiveVal(NULL)
invalidGeom_warn <- reactiveVal(NULL) 


observeEvent(input$upload_cantdo,{
    removeModal()
},ignoreNULL=TRUE) # reset to NULL if cancelling too
   
 
observeEvent(input$dismiss,{
    removeModal()
},ignoreNULL=TRUE) # reset to NULL 
    
 
fam.start.display_wheaders <- reactiveVal(NULL)

observeEvent(rv_dis$fam.start,{
  cat("fam table ") 
  df <- NULL
  if(!is.null(rv_dis$fam.start)) {
    df <- rv_dis$fam.start
    colnames(df) <- c("group", "females", "males","young") ### rerun this w datatable frmat
      cat("updated  - ")
      df <- as.data.frame(df)
      }
  df %>% fam.start.display_wheaders() 
 
})   
          

### button above tableto +/- Nyoung
output$Nyg_btnText <- renderText({relSite_infotext_Npts_comp()})
output$Nyg_btnpreText <- renderText({
  if( is.null(initValues() ) )  {txt <-"no group created"} else {
   txt <- "groups include"
  }
  return(txt)
   }) 
  

roSelect <- reactive({
rosel <-  ifelse(
   dt_backgroundColours() ==0,
    as.character("<span style='font-size:14px;'>&#9679;</span>"),
    as.character("<span style='font-size:14px;'>&#8857;</span>"))
rosel[which(is.na(rv_dis$pts_bng[,2]))] <- ""
return(rosel)
})
width_buttons_cols <- "5vh"
width_group_col <- "50px"



 



output$famTable <-  DT::renderDT({  
  req(fam.start.display_wheaders())
  dat <- fam.start.display_wheaders()#juju
  buttons_cols <- ncol(dat)+1
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == TRUE) { disbEdits <- c(0:ncol(dat)) } else { disbEdits <- c(0,4:ncol(dat)) }
   
  datatable(data.frame( dat,show=dt_backgroundColours(), roSelect=roSelect()) ,
      rownames = FALSE, escape=FALSE,  colnames=c( "<span style='font-size:17px;'>&#8738;</span>"="roSelect"),
      extensions =c("Select"),  
      selection = "none", 
      editable = list(target = "cell",disable = list(columns = disbEdits )),
      options = list(dom = 't',   ordering=F, 
                      columnDefs = list(
                              list(width = width_buttons_cols, targets = buttons_cols),
                              list(width = width_group_col, targets = 0),
                              list(visible = FALSE, targets = ncol(dat)), 
                              list(className = "notselectable", targets = c(1,2,3)),  
                              list(className ='dt-grey', targets = c(0,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(0,buttons_cols)), 
                              list(className = "dt-center", targets = c(1,2,3)) # 3 is invivible here
                      ),
                       select = list(style = "multi",
                                     selector = "td:not(.notselectable)")
                     )
       ) %>% formatStyle("show",target='row',
                          backgroundColor = styleEqual(c(0,1),c(dt_rowNoshowCol,dt_rowShowCol)))
   }  , server = FALSE)
            
    
 
 
    

#addDeleteButtons <- function ( trigger, condition, suffname, Nbuttons, df) {
 #  df <-  as.data.frame(df)
#   if(base::isTRUE(condition)) {  
#   df <-  as.data.frame(df)  %>% dplyr::bind_cols(tibble("Buttons" =create_btns(paste0(suffname,1:Nbuttons)))) 
#   df$Buttons[which(is.na(df[,2]))] <- "" 
#  }
#  return(df)
#}
 
                                      
observe({       
  req(mapExists()==TRUE ) 
  req(!is.null(initValues()))
 if(base::isFALSE(click_Pts_disabled())){ 
    cat("add buttons  - ")
    addDeleteButtons(     trigger=click_Pts_disabled(), condition= (!is.null(initValues()) & input$relPts_method == "each_pt_on_map"),
       suffname="beav",Nbuttons=input$Nfams_init,  df =  rv_dis$pts_bng )   %>% relPoints_coordsBNG_display() 
 } 
})
 
  
  
  ### delete row
observeEvent(input$current_id, {
      if(!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "delete_beav")){
     cat("user deleted beaver point  - ")
       dt_row <- which(stringr::str_detect(relPoints_coordsBNG_display()[,4], pattern = paste0("\\b", input$current_id, "\\b")))  
     rv_temp$newpts_ll_coords <- rv_temp$pts_ll
     rv_temp$newpts_ll_coords$lng[ dt_row]  <- NA
     rv_temp$newpts_ll_coords$lat[ dt_row]  <- NA
     rv_dis$pts_bng [ dt_row,c(2,3) ] <- NA # this triggers refresh -> relpts becoming newpts_ll
     rv_temp$clickCount_pts <- rv_temp$clickCount_pts-1 
     dt_row %>% dt_row()
     }
}) 
 
 
 

 
output$coordsTable_noedit <-  DT::renderDT({
   click_Pts_disabled()
   req(!is.null(relPoints_coordsBNG_display()))
   req(nrow(relPoints_coordsBNG_display())>0)
      
   notsel <- c(1,2)
   dat <- data.frame(relPoints_coordsBNG_display())
   print(dat)
  colN <- c("group", "x", "y")
    colnames(dat)[ c(1:3)] <-  colN[1:3]
    dat$x <- round(as.numeric(as.character(dat$x)),-2)
    dat$y <- round(as.numeric(as.character(dat$y)),-2)
     if(base::isTRUE(click_Pts_disabled()) ) {dat <- dat[,1:3] }
   print(roSelect())
   print(dt_backgroundColours() )
      buttons_cols <- ncol(dat)+1
   if("Buttons" %in% colnames(dat)) { colN <- c(colN,"<span style='font-size:17px;'>&#8635;</span>")
                                      buttons_cols <- c(buttons_cols,buttons_cols-2)
                                      notsel <- c(1,2,3)}
   colN <- c(colN,"show",  "<span style='font-size:17px;'>&#8738;</span>")
  
   
   datatable(data.frame( dat,show=dt_backgroundColours() , roSelect=roSelect()) , 
            rownames = FALSE, escape=FALSE,  ## for HTML rendering!
            extensions = "Select", selection = "none",
            editable = FALSE, colnames=colN,
            options = list(dom = 't',   ordering=F, 
                           columnDefs = list( 
                              list(width = width_buttons_cols, targets = buttons_cols),
                              list(width = width_group_col, targets = 0),
                              list(visible = FALSE, targets = ncol(dat)), 
                              list(className = "notselectable", targets = notsel),  
                              list(className ='dt-grey', targets = c(0,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(0,buttons_cols)), 
                              list(className = "dt-center", targets = notsel) # 3 is invivible here
                              ),
                             select = list(style = "multi",
                                           selector = "td:not(.notselectable)")
                     )
       ) %>% formatStyle("show", 
                                   target='row',
                                   backgroundColor = styleEqual(c(0,1),c(dt_rowNoshowCol,dt_rowShowCol)))
},                                    server=FALSE)


 
 


output$coordsTable_edit <-  DT::renderDT({ 
   click_Pts_disabled()
   req(!is.null(relPoints_coordsBNG_display())) 
   notsel <- c(1,2)
   dat <- data.frame(relPoints_coordsBNG_display())
   colN <- c("group", "x", "y","show",  "<span style='font-size:17px;'>&#8738;</span>")
   buttons_cols <- ncol(dat)+1
 colnames(dat)[ c(1:3)] <-  colN[1:3]
    dat$x <- round(dat$x,-2)
    dat$y <- round(dat$y,-2)
 
  
   datatable(data.frame( dat,show=dt_backgroundColours() , roSelect=roSelect()) , 
            rownames = FALSE, escape=FALSE,  ## for HTML rendering!
            extensions = "Select", selection = "none",
            editable = list(target = "cell",disable = list(columns = c(0,ncol(dat)+1))), 
            colnames=colN,
            options = list(dom = 't',   ordering=F, 
                           columnDefs = list( 
                              list(width = width_buttons_cols, targets = buttons_cols),
                              list(width = width_group_col, targets = 0),
                              list(visible = FALSE, targets = ncol(dat)), 
                              list(className = "notselectable", targets = notsel),  
                              list(className ='dt-grey', targets = c(0,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(0,buttons_cols)), 
                              list(className = "dt-center", targets = notsel) # 3 is invivible here
                              ),
                             select = list(style = "multi",
                                           selector = "td:not(.notselectable)")
                     )
       ) %>% formatStyle("show", 
                                   target='row',
                                   backgroundColor = styleEqual(c(0,1),c(dt_rowNoshowCol,dt_rowShowCol)))
},                                    server=FALSE)
 
   
   
 
#output$show_relPtsFams <- renderUI({ div(DTOutput('famTable',  width='90%'), style="max-height: 40vh;overflow-y: auto;")  })  

output$show_relPtsCoords <- renderUI({ 
  if(input$relPts_method != "manual_entry") {coordsTable <- "coordsTable_noedit" } else {coordsTable <- "coordsTable_edit"}  
  div(DTOutput(coordsTable, width='90%'), style="max-height:40vh;overflow-y: auto;")
  }) 
   
 
 
 
output$timingTable <-  DT::renderDT({ 
   dat <- rv_dis$timing
   buttons_cols <- ncol(dat)+1
   dat$year <- dat$year #+ startYr_transloc() 
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == TRUE) { disbEdits <- c(0:ncol(dat)) } else { disbEdits <- c(0) }

   
     datatable(data.frame( dat,show=dt_backgroundColours(), roSelect=roSelect()) ,
  rownames = FALSE, escape=FALSE,  colnames=c( "<span style='font-size:17px;'>&#8738;</span>"="roSelect"),
  extensions = "Select", selection = "none",
      editable = list(target = "cell",disable = list(columns = disbEdits )),
      options = list(dom = 't',   ordering=F, 
                         columnDefs = list( 
                              list(width = width_buttons_cols, targets = buttons_cols),
                              list(width = width_group_col, targets = 0),
                              list(visible = FALSE, targets = ncol(dat)), 
                              list(className = "notselectable", targets = c(1,2)),  
                              list(className ='dt-grey', targets = c(0,buttons_cols)),
                              list(className ='dt-head-grey', targets = c(0,buttons_cols)), 
                              list(className = "dt-center", targets = c(1,2)) # 3 is invivible here
                              ),
                       select = list(style = "multi",
                                     selector = "td:not(.notselectable)")
                     )
       ) %>% formatStyle("show", 
                                   target='row',
                                   backgroundColor = styleEqual(c(0,1),c(dt_rowNoshowCol,dt_rowShowCol)))
},                                    server=FALSE)      
            
             
   
output$show_relPtsTiming <- renderUI({ div(DTOutput('timingTable', width='90%'), style="max-height: 40vh;overflow-y: auto;")   })


  
output$initValuesTable <-  DT::renderDT( initValues()  , options = list(dom = 't',autoWidth = TRUE,   
                                             columnDefs = list(list(width = '70px', targets = 0),list(className = 'dt-center', targets = 0)
                                              )), rownames = FALSE , editable = FALSE  )
 
 
infotext_instructions_shape <- reactiveVal(NULL)

output$relSite_instructions_draw <- renderUI({if(input$SimParamsTabs != "landscape") {return(NULL) } else { return(HTML(shapetxt())) } })  
     
 

 output$release_design_infotext <- renderText({rv_mapctry$release_design_infotext }) 
 output$country_infotext <- renderText({rv_mapctry$country_infotext  })
  
 
 ## help text reg site selection - show when site tab is selected
 ## 
  output$infotext_instructions  <- renderUI({if(input$SimParamsTabs != "site") return(NULL) 
                                            HTML(  rv_init$infotext_instructions ) })
 output$infotext_instructions_lockSite <- renderUI({if(input$SimParamsTabs != "site") return(NULL) 
                                            HTML( click_OK_amend()) })
 output$infotext_instructions_siteSel <- renderUI({if(input$SimParamsTabs != "site" | !is.null(rv_init$infotext_init)) return(NULL) 
                                           HTML( rv_init$infotext_instructions_siteSel)  })
 
 
 output$infotext_init<- renderUI({if( is.null(rv_init$infotext_init)) return(NULL) 
                                            HTML( rv_init$infotext_init)  })
 
observeEvent({
  input$SimParamsTabs
  infotext_def_shape()
  1
  },{ 
  if(!is.null( rv_init$infotext_init)) { rv_init$infotext_init <-  NULL } 
},ignoreInit=TRUE)
 
 
 output$infotext_instructions_shape <- renderUI({  if(input$SimParamsTabs == "site" && !is.null(infotext_instructions_shape())) {
   HTML( infotext_instructions_shape())  } else {return(NULL)}
})
 
 
 output$infotext_def_shape  <- renderUI({if(input$SimParamsTabs != "site") return(NULL) 
                                           HTML( infotext_def_shape())  })
 
 
 output$infotext_instructions_habLowqual <- renderUI({ HTML(infotext_instructions_habLowqual())  })
  ## help text reg pts - show when pts tab is selected

output$infotext_instructions_resample  <- renderUI({if(input$SimParamsTabs != "points") return(NULL) 
                                           HTML( infotext_instructions_resample() ) })

output$infotext_instructions_NlocsOK <- renderUI({if(input$SimParamsTabs != "points") return(NULL) 
                                           HTML( pts_N() ) })

output$infotext_instructions_ptmethod<- renderUI({if(input$SimParamsTabs != "points") return(NULL) 
                                           HTML(txt_pt_method() ) })
output$infotext_simError <- renderUI({if(is.null(rv_init$infotext_simError)) return(NULL) 
                                           HTML(rv_init$infotext_simError) })
output$infotext_instructions_pointout<- renderUI({if(input$SimParamsTabs != "points") return(NULL) 
                                           HTML(rv_init$infotext_instructions_pointout) })

output$relSite_infotext_demog  <- renderUI({if(input$SimParamsTabs != "groups") {return(NULL) } else {
                                                  if(is.null(initValues() )) {return(paste0("Group demographics not yet informed.")) } else {
                                               return( HTML(  paste0("Group demographics are informed.")))}}  })

output$operation_ttl <- renderText({paste0("operation: ",input$operation)})  
   
output$relSite_infotext_Npts_comp  <- renderUI({if(input$SimParamsTabs != "groups") return(NULL) 
                                               return( HTML(paste0("The released individuals are " , relSite_infotext_demog_sumtxt())  ))  })
   
output$infotext_instructions_lockPts <- renderUI({if(input$SimParamsTabs != "points") return(NULL) 
                                            HTML( click_lock_pts()) })
       

output$relSite_infotext_timing <- renderUI({if(input$SimParamsTabs != "timing") {return(NULL) } else {
                                                  if(is.null(initValues() )) {return(paste0("The chronology is not yet informed.")) } else {
                                                         HTML( "The chronology is informed.") }}
  })
output$relSite_infotext_timingdet <- renderUI({if(input$SimParamsTabs != "timing") return(NULL) 
                                            HTML(timing_txt()) })

 
 
output$relSite_infotext_loc <- renderUI({ 
  if(is.null(rv_init$infotext_releaseLoc)) return(HTML(as.character(p("exploratory site not defined") )))
   rv_init$infotext_releaseLoc  
})
 

############################################################ help text current metadata          

 

metadataList <- reactive({       
  datlist <- datlist2  <- NULL 
  txt_modifHab <- txt_records <- txt_areaOfInt <- NULL 
  txt_modifHab2 <- txt_records2 <- txt_areaOfInt2 <- NULL 
  if(!is.null(metaTable_modifHab())) {
   if(nrow(metaTable_modifHab())>0) {
     Nfeat <- nrow(metaTable_modifHab())
     txt_rec <- " feature)"
     if(Nfeat>1) {txt_rec <-"x .shp)"}
     txt_modifHab2 <- paste0("  -modify mapped habitat (",Nfeat, txt_rec)
    txt_modifHab <- paste0("  -modified habitat")

   }}
    if(!is.null(metaTable_records())) {
    if(nrow(metaTable_records())>0) {
     Nfeat <- nrow(metaTable_records())
     txt_rec <- " feature)"
     if(Nfeat>1) {txt_rec <-"x .shp)"}
      txt_records2 <- paste0("  -observation records (",Nfeat, txt_rec)
     txt_records <- paste0("  -observation records" )  
    }}
      if(!is.null(metaTable_areaOfInt())) {
    if(nrow(metaTable_areaOfInt())>0) {
     Nfeat <- nrow(metaTable_areaOfInt())
     txt_rec <- " feature)"
     if(Nfeat>1) {txt_rec <-"x .shp)"}
      txt_areaOfInt2 <- paste0("  -area of interest (",Nfeat, txt_rec)
      txt_areaOfInt <- paste0("  -area of interest")
  }}
  datlist <-filterNULL(c(txt_modifHab,txt_records,txt_areaOfInt) )
  datlist2 <-filterNULL(c(txt_modifHab2,txt_records2,txt_areaOfInt2) )
  
  return(list( datlist, datlist2) ) # 2 for download summary txt, for now
})




output$dlText_metadata2 <- renderUI({
  if(input$SimParamsTabs != "landscape" |  is.null(metadataList()[[2]])) {return(NULL) } else {
      datlist <- metadataList()[[2]]
     datlist <-  HTML(paste0(datlist, sep= "<br>"))   
   return(datlist)
   }
 })  

 
 




 
   
output$relSite_infotext_landscapedet<- renderUI({
  if(input$SimParamsTabs != "landscape" |  is.null(metadataList()[[1]])) {return(NULL) } else {
      datlist <- metadataList()[[1]] 
     datlist <-  HTML(paste0(datlist, sep= "<br>"))   
   return(datlist)
   }
 })  

output$relSite_infotext_landscape  <- renderUI({if(input$SimParamsTabs != "landscape") {return(NULL) } else {
                                                  if(is.null(metadataList()[[1]] )) {return(paste0("No additional metadata.")) } else {
                                                                          return(paste0("Additional landscape features:")) }
}
}) 
    
relSite_infotext_demog_sumtxt <- reactive({
 txt <- NULL
  if(!is.null(relSite_infotext_Npts_comp())) {
    demog_group <- demog_group()
    if(demog_group == "combination") {demog_group <- paste0("a combination of adult pairs and families ")}
    txt <- paste0( demog_group, " (",relSite_infotext_Npts_comp(),")")
  }
  return(txt)
})
    
  
timing_txt <- reactive({
  txt <-NULL
  if(!is.null(initValues())){
      if(input$timing != "several_years") {txt <- paste0( "all on the same year")} else{txt <- paste0( " over ", length(seq(min(initValues()$year),max(initValues()$year))), " years")}
      txt <- paste0("releases ",txt)
      }
  return(txt)
  })

#output$relSite_infotext_timing_summary <- renderUI({ if(is.null(Nfams_init_actual())) return(HTML(as.character(p("translocation timeline not informed") )))
#  HTML(as.character(p(timing_txt())))  })



output$relSite_infotext_timing_summary <- renderText({   if(is.null(Nfams_init_actual()))  return( "translocation timeline not informed" )
  timing_txt()    })
 
 
output$relSite_infotext_demog_summary <- renderText({  if(is.null(Nfams_init_actual())) return( "demographics not informed")   
   relSite_infotext_demog_sumtxt()   })
 
output$relSite_infotext_Npts_summary  <- renderText({ if(is.null(Nfams_init_actual())) return ("number of translocated groups not informed") 
 return(  paste0(Nfams_init_actual()," points located"))   })
   
     





disable_tableEdits <- reactiveVal(FALSE)
mgmt.reps <- reactiveVal()
mgmt.years <- reactiveVal()
mgmt.startYear<- reactiveVal()
start_sim <- reactiveVal(NULL)
trigger_sim <- reactiveVal(NULL)

 

operation <- reactive({  return(input$operation ) })

show_simOutputNULL <- reactiveVal(NULL)
show_simOutput<- reactive({ 
    return(as.numeric(any(!is.null(datSave_sim_pop() ),!is.null(show_simOutputNULL()))) ) # == 1 when output   
}) 
 
sTestOutput <-  reactive({ 
    return(as.numeric(!is.null(datSave_settleTest() )) ) # == 1 when output   
}) 
  
  
    
#  note on: show_simCanStart()  show_trySettle()
#  input$operation # resets points only not site when toggling - seems convenient
#  input$operation alters site_poly() which triggers reset points and reqs unlocked pts
observeEvent(input$operation,{  
    cat("trigger undo_settling")
    trigger_undo_settling(trigger_undo_settling()+1)
#   trigger_undo_sim(trigger_undo_sim()+1)#} 
    updatePrettyCheckbox(session, "lock_pts",  label="release locations unlocked", value=FALSE ) 
    
    if(input$operation == "translocation") {
      cat("replot translocas if any  - ")
     counter_initPopChange(counter_initPopChange()+1) 
    }
}, ignoreInit=TRUE, ignoreNULL=TRUE) 
  



   
show_simCanStart <- reactive({ 
   req( input$operation)
   initValues()
   pointFeature_terrPols()
   if( input$operation == "translocation"){ return(  !is.null(initValues()))   }  
   if( input$operation == "observation") { return(  !is.null(pointFeature_terrPols()))  }   # link to obsrecsdf instead?may be issues if nrow=0 may hae to foolproof
}) 
     
show_trySettle <- reactive({ 
  req(input$operation) 
  if( input$operation == "translocation"){ return( base::isTRUE(!is.null(initValues())))}
  if( input$operation == "observation"){ return( 0 )}
}) 
 
   
observeEvent( input$try_settling ,{ ## start both the same
  "settlement_test" %>% sim_type()  
  # click_Pts_disabled(TRUE) 
    
 # updatePrettyCheckbox(session, "lock_pts",  label="release locations locked", value=TRUE  )
      modelPrep(0) 
   disable("try_settling")
   disable("undo_sim")
   disable("start_sim")
   disable("startYr_trySettl") 
   initValues <- initValues()
   paste0("running..") %>% notSettling() 
   paste0(nrow(initValues) ," groups released"  ) %>% notSettling_pars()
   Ntranslocs <- length(which(initValues$year == input$startYr_trySettl )) 
   gp <- " groups "
   if(Ntranslocs==1) {gp <- " group "}
   sim_in_prog2 <- paste0(Ntranslocs,gp,"translocated in " ,input$startYr_trySettl ) 
   sim_in_prog2 %>%  sim_in_progress2()
   sim_in_prog2 %>%  sim_in_progress2_inputTab()
   sim_in_prog2 %>% sim_in_progress2_outputTab() 
   sim_in_prog2 %>% sim_in_progress2_aboutTab() 
   
   sim_in_prog <- "initial settlement test"
   sim_in_prog %>% sim_in_progress_inputTab()
   sim_in_prog %>% sim_in_progress() 
   sim_in_prog %>% sim_in_progress_outputTab() 
   sim_in_prog %>% sim_in_progress_aboutTab() 
   if(input$show_alerts == "warn when output changes" & !is.null(datSave_sim_pop() )) {
   dl_message <- "output will be replaced when the simulation is complete"   
   notify_info(dl_message,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
   }
   
   trigger_sim(1) 
}, ignoreNULL=TRUE, ignoreInit=TRUE)
 
 

observeEvent( input$start_sim ,{ ## start both the same
  "global_pop" %>% sim_type() 
   sim_out$fam_sim  <- NULL # has to occur before slider value updated
   click_Pts_disabled(TRUE)
   disable("try_settling")
   disable("undo_settling")
   disable("undo_sim")
   disable("start_sim") 
   paste0("running..") %>%  startGrowing()   
   
   sim_in_prog <- "population growth simulation "
   sim_in_prog %>% sim_in_progress_inputTab()
   sim_in_prog %>% sim_in_progress()
   sim_in_prog %>% sim_in_progress_outputTab()
   sim_in_prog %>% sim_in_progress_aboutTab()
   
   
 if(input$show_alerts == "warn when output changes" & !is.null(datSave_sim_pop() )) {
    dl_message <- "output will be replaced when the simulation is complete"   
   notify_info(dl_message,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
    }
  
   trigger_sim(1)  
       modelPrep(0) 
}, ignoreNULL=TRUE, ignoreInit=TRUE)  



## trigger sin and undo at same time to do!
observeEvent(trigger_sim(),{#input$start_sim,{ # reset 
   sim.time(Sys.time())  
   cat("\n---- user triggered sim  - ")
   operation <- operation()
   rv_init$infotext_simError <- NULL
 if( operation  == "translocation") {   
   }
  
if( operation  == "observation") {
  init_poly <- init_poly()  
  fakeClick <-  init_poly %>% st_centroid()  %>% st_coordinates() %>% as.data.frame()
   cat("fakeClick  - ")
   colnames(fakeClick) <- c("lng","lat")
           fakeClick %>% input_mapReleaseSite_click()
            if (input$shape != "none"){
              notify_info("creating exploratory site from metadata",timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) }
            "river catchments all" %>% input_mapReleaseSite_click_group()
            updateRadioGroupButtons(session,"buffer_sel",selected=5000) 
    }
   
   
   
    disable_tableEdits(TRUE) # disables edits in transloc and site inputs, leaves landscape free for additions
    disable("timing")
    disable( "demog")  
    simModelOut(0)
     
    model_running_UItxt(paste0("no starting population"))
    model_running_UItxtout(NULL)
    #model_sim_UItxtout(NULL)
    model_running_UItxt(paste0("compiling GIS layers")) 
    disable("sim_Nreps")
    disable("sim_Nyrs")
    disable("operation") 

    simEffects <- NULL
    if(input$addMetadata_records =="incorporate records" &  !is.null(pointFeature()))  { # terrs are mapped aready, from pt drawn
      cat("add observed terrs - ")
      simEffects <- c(simEffects, "obsRecords") 
     }  
    metaTable_modifHab <- metaTable_modifHab()    
    if(input$addMetadata_modifHab == "modify mapped habitat" & !is.null(metaTable_modifHab)){ 
      if(nrow(metaTable_modifHab)>0){
      cat("add modified hab - ")
      simEffects <- c(simEffects, "make0")
        } }
     if(input$addMetadata_areaOfInt !="report overall growth" & !is.null(metaTable_areaOfInt()) )  { # terrs are mapped aready, from pt drawn
      cat("add areaOfInt - ")
      simEffects <- c(simEffects, "areaOfInt") 
       }    
    if(operation  == "translocation" & !is.null(initValues()) )  { 
      cat("add release points - ")
    disable("lock_pts")
    disable("lock_site")
    if(operation == "translocation"){disable("operation")} # prevents removeing released gorups during simulation but still can add obs
      }  
     
    simEffects %>% simEffects()   
    print(as.character("simulation includes: ",simEffects, "effects")) 
    model_running(TRUE)
    model_running_UItxtout(NULL) 
#   modelPrep(0) 
    start_sim(1)
    cat("params val - ") 
   local_habw(NULL)
}, ignoreNULL=TRUE)
 
simEffects <- reactiveVal(NULL) # list of effects to inc in sim, just effect names as featureName / effectType or NULL
# efects amongst: "obsRecords" "make0" 





 
 ###https://oceanhealthindex.org/news/raster_to_terra/
# minYear <- reactive({ format(Sys.time()-9*365*60*60*24,"%Y") })
# maxYear <- reactive({ format(Sys.time()-3*365*60*60*24,"%Y") })
   
  
 
init_poly <- reactiveVal(NULL) 

observe({  
   req(any(!is.null(pointFeature()),Nfams_actual_display()>0))
   req(model_running()==FALSE)
   cat("\ninit_poly  - ") 
   initValues   <- initValues()
   pointFeature <-  newFeaturesDat_shp()# not using pointFeature() to void recomputing when a point observation is badly located and the deleted - would run this twice
  
  cat(paste0("Nfams:",Nfams_actual_display(),"  - "))
  
  allx <- ally <- allpts <- NULL
  
# convex hull around all beaver points  
if(Nfams_actual_display()>0){
  allx <-na.exclude(initValues$lng)
  ally <-na.exclude(initValues$lat)   
 }  
  
 if(!is.null(pointFeature) & input$addMetadata_records =="incorporate records" ){
   pointFeature <- na.exclude(pointFeature[pointFeature$size == "1terr",])
   if(nrow(pointFeature)>0) {
  #   print(pointFeature)
   pointFeature <- pointFeature %>% st_coordinates()  
    allx <-c(allx, pointFeature[,1])
    ally <-c(ally, pointFeature[,2])  
 }
}   
  if(!is.null(allx)) {
   if(length(allx)==1) {
      allpts <- st_multipoint(matrix(c(allx,ally),length(allx),2)) %>% st_sfc(crs=st_crs(4326)) %>% st_transform(st_crs(27700)) %>% st_buffer(1000) %>% st_transform(st_crs(4326))
   } else {
       allpts <- st_multipoint(matrix(c(allx,ally),length(allx),2))  
   allpts <- allpts%>%  st_convex_hull()%>% st_sfc(crs=st_crs(4326))%>% st_transform(st_crs(27700)) %>% st_buffer(1000) %>% st_transform(st_crs(4326))
 #if(length(allx)>1) { 
#   allpts  <-  allpts  %>% st_union() %>% st_transform(st_crs(27700)) %>% lwgeom::st_minimum_bounding_circle(nQuadSegs =10) %>% st_sfc(crs=st_crs(27700))%>% st_transform(st_crs(4326))
  } 
  allpts %>% init_poly() # only change if non null? june2024
} 
})  ## indpdt from site sel

  
observeEvent(init_poly(),{
  init_poly <- init_poly()
  req(!is.null(init_poly))
#leafletProxy("mapReleaseSite", session) %>% clearGroup("init_poly_test") %>% 
 # addPolylines(data=init_poly, group="init_poly_test",   color="red", opacity=1, weight=1)
})
 
### rasterising metadata features - buffer and transform to 3857
newFeaturesDat_shp3857 <- reactive({ 
         req(newFeaturesDat_shp())
         polyFeature <- newFeaturesDat_shp()
        
         ##note POINTS for terrs are not in there, this is for plotting, already have cells for points at this stage
    #  whole area is apoint geom
    pols  <- polyFeature[st_geometry_type(polyFeature) %in% c("MULTIPOLYGON","POLYGON") ,]  %>% st_transform(st_crs(3857))
    polys <- polyFeature[st_geometry_type(polyFeature) %in% c("POINT", "LINESTRING"),] %>% st_transform(st_crs(27700)) %>%  st_buffer(25) %>% st_transform(st_crs(3857)) ## not sure of buffer here - just 25f rdisaply or?? was 2
    pols  <- rbind(pols,polys)  
    pols  <- pols[! st_geometry_type(pols) %in% c("POINT", "LINESTRING"),] 
    pols  <- na.exclude(pols)
    # remove linestrings 
    # note: in this df the lines are now polygons, for common function retrieving cells, faster
   
return(pols) 
})

computeSimExtentWrapped <- function(h3857w,init_poly,travelDis){  
         h3857     <- terra::unwrap(h3857w)  
         temp_bbox0 <-  st_buffer( init_poly ,travelDis) %>% st_union() %>% st_bbox()  %>% st_as_sfc() 
         st_crs(temp_bbox0) <- st_crs(27700)  
         sf_extent <- temp_bbox0 %>% st_transform(st_crs(3857)) %>% st_bbox()
         bbext3857 <- ext( c(sf_extent$xmin,sf_extent$xmax, sf_extent$ymin, sf_extent$ymax)) 
         bbext3857 <- terra::align(bbext3857, h3857, snap="near")        
         bbext3857 <- vect(bbext3857)
         crs(bbext3857)  <- "EPSG:3857"
         bbtemplate3857  <-  terra::crop(h3857 ,bbext3857,mask=TRUE, snap="out",ext=TRUE)
         bbext3857       <-  ext(bbtemplate3857)
         local_hab <- bbtemplate3857
         local_hab <- subst(local_hab , NA, 0)  # base unchanged habitat
        # local_hab %>% local_hab()
         local_hab <- terra::wrap(local_hab, proxy=FALSE)
         return(local_hab)
}


 


 
simStartYearParams <- reactiveValues(minyear="",startYearText="not informed")
output$simStartYear_txt <- renderText({  simStartYearParams$startYearText })
observe({ 
  req(show_simCanStart()==TRUE)
  req(input$operation) 
minyearTrans <- minyearObs <- minyear <- NA 
if(input$operation == "translocation" & !is.null(initValues()))  {
  if(nrow(rv_dis$timing)>0) {minyearTrans <- min(rv_dis$timing$year)}}
  
if(input$addMetadata_records =="incorporate records" & !is.null(pointFeature())){
if( nrow(pointFeature()%>%na.exclude())>0)  {minyearObs <- min(pointFeature()$start) }}
 cat("(normal if warning:)")
minyear <- min(minyearTrans,minyearObs,na.rm=TRUE)
 req(minyear!= Inf)
                    if ( !is.na(minyearObs) & !is.na(minyearTrans)){
                            if (minyearObs == minyearTrans) { startYearText <- "year of first beaver translocation and first beaver territory observation record" }
                            if (minyearObs > minyearTrans)  { startYearText <- "year of first beaver translocation" }
                            if (minyearObs < minyearTrans)  { startYearText <- "year of first beaver territory observation record" }
                   } else {
                     if ( !is.na(minyearObs))  { startYearText <- "year of first beaver territory observation record" }
                      if ( !is.na(minyearTrans))  { startYearText <- "year of first beaver translocation" } 
                   }     
isolate({
  simStartYearParams$minyear <- minyear
  simStartYearParams$startYearText <- startYearText   
})
cat(paste0("start year:  ",minyear,"  - "))
})




observeEvent(start_sim(),{  
  local_hab  <- local_hab()
  local_habw <- local_habw()
  if(is.null(local_habw)){
         cat("compute extent  - ")  
         init_poly <- init_poly() 
         init_poly <- init_poly %>% st_transform(crs=st_crs(27700)) # include all relevant features in extent for global popsim later
         if(sim_type() == "settlement_test") { travelDis <- 5000 } else {  
                      travelDis <- 3000*as.numeric(as.character( input$sim_Nyrs ))}      ####!! make sliding window on raster instead?  
     
         h3857<- hab_3857w() 
         local_habw  <-computeSimExtentWrapped(h3857w=h3857,init_poly=init_poly,travelDis=travelDis) 
         local_habw %>% local_habw()
         local_hab  <- terra::unwrap(local_habw)
         local_hab %>% local_hab()  
 cat("wrapped local hab  - ")  
}
mgmt.years  <- as.numeric(as.character( input$sim_Nyrs ))  # after starting sim 
mgmt.years %>% mgmt.years()
  
   initValues <- initValues()
   startYr_transloc <- startYr_transloc()
   operation <- operation()
   simEffects <- simEffects() 
   error_message <- NULL 
   depCells <- NULL 
   cells_make0  <- NULL
   cells_make50  <- NULL
   cells_make75 <- NULL
  
country <- country()
sim_type <- sim_type()    
validated_input <- TRUE 
all_incFeatures <- NULL  # collect included features id for reporting output - filter main table by leafletIds 
 

 
 cat("\n---- prep simulation data\n")  
 cat(paste0("operation: ",operation, " - effects: ",simEffects, " - sim_type: ",sim_type, " - mgmt.years: ",mgmt.years()) ) 
 
 
 
 
  cat("\nmetadata: ")
  if(length(simEffects)>0) { ## all the metadata
  cat("aye  - ")
        metadataFeatType_annual <- metadataFeatType_annual() 
       # print(metadataFeatType_annual)
        metadataFeatCells_perId <- metadataFeatCells_perId()
      #  print( metadataFeatCells_perId ) 
  }else {
    cat("nae  -")
  }
  fam.start <- NULL ## have to merge fam.start from release and from added terr
  departings <- NA
  
  
  
#         print(paste0("\n-------------------------------------------- determine extent -") )
#         init_poly <- init_poly()
#         print(init_poly)
#         init_poly <- init_poly %>% st_transform(crs=st_crs(27700)) # include all relevant features in extent for global popsim later
#         if(sim_type == "settlement_test") { travelDis <- 5000 } else {  
#                      travelDis <- 3000*as.numeric(mgmt.years) }      ####!! make sliding window on raster instead? 
# 
#         temp_bbox0 <-  st_buffer( init_poly ,travelDis) %>% st_union() %>% st_bbox()  %>% st_as_sfc() 
#         st_crs(temp_bbox0) <- st_crs(27700) 
#         print(temp_bbox0)
 #       
#         h3857     <- hab_3857() 
#         sf_extent <- temp_bbox0 %>% st_transform(st_crs(3857)) %>% st_bbox()
#         bbext3857 <- ext( c(sf_extent$xmin,sf_extent$xmax, sf_extent$ymin, sf_extent$ymax)) 
#         bbext3857 <- terra::align(bbext3857, h3857, snap="near")        
#         bbext3857 <- vect(bbext3857)
#         crs(bbext3857)  <- "EPSG:3857"
#         bbtemplate3857  <-  terra::crop(h3857 ,bbext3857,mask=TRUE, snap="out",ext=TRUE)
#         bbext3857       <-  ext(bbtemplate3857)
#         local_hab <- bbtemplate3857
#         local_hab <- subst(local_hab , NA, 0)  # base unchanged habitat
      #   local_hab <- local_hab()
startYr_trySettl <- input$startYr_trySettl         
startYr_trySettl %>% startYr_trySettl()
StartYear <- simStartYearParams$minyear  

 

  cat(paste0( "\nstartYear: ",StartYear, " - startYr_trySettl: ",startYr_trySettl) )  


 if(operation == "translocation" &   !is.null(initValues))  {  # ==sim_type == "settlement_test"?
cat(" translocation  - ")  
        # print(initValues)
          xy <- cbind(x=initValues$lng, y=initValues$lat) 
          
          if(sim_type == "settlement_test") { # one year    
             trans_thatYr <- which(initValues$year ==  startYr_trySettl)
             cat(c("\ntrans_thatYr : ", trans_thatYr ,"  - "))
             
             if(length(trans_thatYr)>0) { xy <- xy [trans_thatYr,] } else {
               cat("\nno translocation on selected year: cancel simulation  - ")
               validated_input <- FALSE
               error_message <-   paste0("no translocation planned on selected test year (",startYr_trySettl,")")
             } 
          }
   if( validated_input == TRUE ) {      
          pts <- st_multipoint(matrix(c(xy), ncol = 2, byrow = FALSE), dim = "XY")   %>% st_sfc()  
          st_crs(pts) <- st_crs(4326)
          pts <- pts %>% st_transform(st_crs(3857)) %>% st_cast("POINT")
           extract3857 <-  terra::extract(local_hab, vect(pts), ID=TRUE, cells=TRUE, xy=TRUE, method="simple", touches=TRUE)
          #watch HERE only takes points released on relevant year and reattributes correctIDs!!! trans_thatYr
           
             
      if(length(which(extract3857[,2]==0))>0) {           ### if some locations within unsuitable hab - stop  
         cat("\nlocated in unsuitable hab: cancel simultation  - ")
         validated_input <- FALSE
         updatePrettyCheckbox(session, "lock_pts",  label="release locations unlocked", value=FALSE )    # unlock pts just so can relocate easily 
         error_message <- "local habitat is unsuitable"  
         } else {
         cat("departure cells  - ")
         depCells <- extract3857$cell
          if(sim_type == "settlement_test") { #
          departings <- trans_thatYr 
         
  }}   ##simulation cancelled after poits lcoated by clicking in suit hab but not extracted ot suit==2 throughout AAARGH
            
   
             
             
             
             
                   if(sim_type == "settlement_test" & validated_input==TRUE) { # one year - compute cell index and make hab and ter rasters      
             cat("input guid  - ") 
               #      ter.start <-  local_hab
               #       values(ter.start) <- 0    
                         
                #   ter.start[depCells] <- departings# !important but relying on maintained order between pts and extract3857$ID
        
        
         }
  } # if !is.na habitat , got depells      (or sim is cancelled)
  } # intervention ( ie model includes translocations )    
 cells_make0 <- cells_make50 <- cells_make75 <- NULL
 cells_obs <- vals_obs <- famDf_obs <- NULL 
 
 
  if( validated_input == TRUE & length(simEffects)>0 ) {
   if("make0" %in% simEffects){ 
      newFeaturesDat_shp <- newFeaturesDat_shp3857()
     # print(newFeaturesDat_shp)
      cat("\n-- modify habitat: ")
     
       cat("retrieve modified cells  - ") 
        make0Ids <-  newFeaturesDat_shp$LeafletId[newFeaturesDat_shp$layerEffect == "make unsuitable"] 
        make50Ids <-  newFeaturesDat_shp$LeafletId[newFeaturesDat_shp$layerEffect == "50% mortality"] 
        make75Ids <-  newFeaturesDat_shp$LeafletId[newFeaturesDat_shp$layerEffect == "75% mortality"]  
                
     if(length(make0Ids)>0) { 
        cat("100%  - ") 
        effdf <- metaEff_make0_SumTab()  %>% as.data.frame() 
         make0df  <- effdf[effdf$effectId %in% make0Ids,  ]  
         make0df <-  make0df[,-1] # Ids   
         effdf_make0 <- effdf
     }
     if(length(make50Ids)>0) { 
       cat("50%  - ")
        effdf <- metaEff_make50surv_SumTab()  %>% as.data.frame() 
         make50df  <- effdf[effdf$effectId %in% make50Ids,  ]  
         make50df <-  make50df[, -1] # Ids   
         effdf_make50 <- effdf

     }
      if(length(make75Ids)>0) { 
       cat("75%  - ")
        effdf <- metaEff_make75surv_SumTab()  %>% as.data.frame() 
         make75df  <- effdf[effdf$effectId %in% make75Ids,  ]  
         make75df <-  make75df[, -1] # Ids   
         effdf_make75 <- effdf
      }
         
    ############   
    if(length(make50Ids)>0 & sim_type == "global_pop") { # otherwise doesnt apply.. 
       cat("\nglobal pop + 50% survival patches   - ")    
              effYears <- colnames(make50df)[which(colSums(make50df) > 0)] 
              effCells <- ll    
              effdf <- effdf_make50
      for (yr in effYears) {
       cells <- NULL
       feats_that_year <- which(effdf [,which(colnames(effdf)==yr)] ==1)
       feats_that_year <- effdf$effectId[feats_that_year]
       print(paste0("features included  ",yr, ": ",feats_that_year))
       all_incFeatures <- unique(c(all_incFeatures,feats_that_year))
       cat("retrieve geoms  - ")
       feats_geoms  <- newFeaturesDat_shp[newFeaturesDat_shp$LeafletId %in% feats_that_year,]  # in 3857
       for(feat_geom in 1:length(feats_geoms$LeafletId)){
         feat <- feats_geoms[feat_geom,] 
       extract3857 <-  terra::extract(local_hab, vect(feat), ID=TRUE, cells=TRUE, xy=TRUE,  method="simple", touches=TRUE)
       cat("extract3857  - ")  
        cells0 <-  extract3857$cell  
       if(length(cells0)>0){  cells <-unique( c(cells, cells0))   }
       } 
       effCells[[yr]] <- as.numeric(cells)     # effCells is list per year with numeric (cells indx) for each year to be applied 
       } # each year   
      cells_make50 <- effCells 
      } 
            
    if(length(make75Ids)>0 & sim_type == "global_pop") { # otherwise doesnt apply.. 
       cat("\nglobal pop + 75% survival patches   - ")    
              effYears <- colnames(make75df)[which(colSums(make75df) > 0)] 
              effCells <- ll 
              effdf <- effdf_make75 
      for (yr in effYears) {
       cells <- NULL
       feats_that_year <- which(effdf [,which(colnames(effdf)==yr)] ==1)
       feats_that_year <- effdf$effectId[feats_that_year]
       print(paste0("features included  ",yr, ": ",feats_that_year))
       all_incFeatures <- unique(c(all_incFeatures,feats_that_year))
       cat("retrieve geoms  - ")
       feats_geoms  <- newFeaturesDat_shp[newFeaturesDat_shp$LeafletId %in% feats_that_year,]  # in 3857
       for(feat_geom in 1:length(feats_geoms$LeafletId)){
         feat <- feats_geoms[feat_geom,] 
       extract3857 <-  terra::extract(local_hab, vect(feat), ID=TRUE, cells=TRUE, xy=TRUE,  method="simple", touches=TRUE)
       cat(" extract3857  - ")  
        cells0 <-  extract3857$cell  
       if(length(cells0)>0){  cells <-unique( c(cells, cells0))   }
       } 
       effCells[[yr]] <- as.numeric(cells)     # effCells is list per year with numeric (cells indx) for each year to be applied 
       } # each year   
      cells_make75 <- effCells 
      }       
         
        
     if(length(make0Ids)>0) {   # only landscape modif that is applied also for settlement test - others affect growth
                   effdf <- effdf_make0 
       if(sim_type == "settlement_test") {  
       cat("\nsettlement test: retrieve effect on test year   - ")
       effYears <- yr <- paste0("y",  startYr_trySettl) 
       
       feats_that_year <- which(effdf [,which(colnames(effdf)==yr)] ==1)
       feats_that_year <- effdf$effectId[feats_that_year]
       cat(paste0(yr,": retrieve geoms for ",length(feats_that_year), "features  - ")) 
        effCells <-NULL
       metadataFeatCells_perId  <- metadataFeatCells_perId() 
       for(feat_that_year in feats_that_year) {
       feat_geoms  <- newFeaturesDat_shp[newFeaturesDat_shp$LeafletId == feat_that_year,]    
         
       extract3857 <-  terra::extract(local_hab, vect(feat_geoms), ID=TRUE, cells=TRUE, xy=TRUE,method="simple", touches=TRUE)
       cat(" extract3857  - ") 
       effCells <- unique(c(effCells , extract3857$cell))  # effCells is numeric (cells indx) or na if out of range  
       }
         if(any(depCells %in% effCells)) {  
           cat("no dispersal possible from selected release locations  -\n")
          if(length(departings [-which(depCells %in% effCells)])==0) {
           validated_input <- FALSE
            error_message <- "no dispersal possible from selected release locations"
            }
         }
      }  #sim_type == "settlement_test" - 
    
     if(sim_type == "global_pop") { 
       cat("global pop simulation: retrieve effect on all simulation years   - ")    
              effYears <- colnames(make0df)[which(colSums(make0df) > 0)] 
              effCells <- ll   
      for (yr in effYears) {
        cells <- NULL
       feats_that_year <- which(effdf [,which(colnames(effdf)==yr)] ==1)
       feats_that_year <- effdf$effectId[feats_that_year]
       cat( c(yr,"-"))
       all_incFeatures <- unique(c(all_incFeatures,feats_that_year))
       feats_geoms  <- newFeaturesDat_shp[newFeaturesDat_shp$LeafletId %in% feats_that_year,]  # in 3857
       for(feat_geom in 1:length(feats_geoms$LeafletId)){
         feat <- feats_geoms[feat_geom,] 
       extract3857 <-  terra::extract(local_hab, vect(feat), ID=TRUE, cells=TRUE, xy=TRUE,  method="simple", touches=TRUE)
       cat(" extract3857  - ") ## could keep IDs here to identify effect per gom IN FUTURE DEVS?
       # cells0 <-  na.exclude(extract3857$cell) 
        cells0 <-  extract3857$cell#[!is.na(extract3857$cell)] 
         
      # print(cells0)
       if(length(cells0)>0){ #nrow(extract3857) >0) { # in case   feature doesnt overlap with the local area
         cells <-unique( c(cells, cells0))
        }
       } 
       effCells[[yr]] <- as.numeric(cells)     # effCells is list per year with numeric (cells indx) for each year to be applied 
       } # each year   
     } #sim_type == "global_pop"
       cells_make0 <- effCells 
  }#length(make0Ids)>0  
         
} # if mode==make0       
  
   
    
 
  
 areaOfInt_cells <- NULL  
 if(sim_type == "global_pop" &  "areaOfInt"  %in% simEffects){ 
        effdf<- metaEff_areaOfInt_SumTab()
        effdf <- effdf %>% as.data.frame() 
        metadataFeatCells_perId  <- metadataFeatCells_perId() 
        feature_shp              <- newFeaturesDat_shp() #fetch LeafletIds then cells from effect summ df
        obsIds <- feature_shp$LeafletId[feature_shp$layerType == "area of interest"]
     }
    
      
  if("obsRecords" %in% simEffects){ 
          cat("\ninclude obsRecs_famDf  - ")
 h3857     <- hab_3857()
 hab0 <- h3857 
 
        effdf <- metaEff_obsRecs_SumTab()
        effdf <- effdf %>% as.data.frame() 
        
    if(sim_type == "global_pop"){
      cat("each year  - ")
        obsRecs_famDf <- obsRecs_famDf() # kiki# user=informed or default vals for demographics, needs formatting
        obsRecs_famDf <- subset(obsRecs_famDf, select=c(year,LeafletId,fam.id,num.m, num.f, young, qual))
       
        obsRecs_famDf$obsRecsYears <- paste0("y",obsRecs_famDf$year )
        obsRecs_famDf <- subset(obsRecs_famDf, select=-year)
        obsRecs_famDf$fam.id <- as.numeric(as.factor(obsRecs_famDf$fam.id))
        
        famDf_obs <- obsRecs_famDf # fam df with years and demog and leafletIds
         
        metadataFeatCells_perId  <- metadataFeatCells_perId()  
        obsIds <- famDf_obs$LeafletId
        
        all_incFeatures <- unique(c(all_incFeatures,obsIds))
        
         cells_obs0 <-metadataFeatCells_perId[famDf_obs$LeafletId]
         cat("coords from cells  - ") 
        cells_obs  <-vector("list", length=length(cells_obs0))
        names(cells_obs) <-  names(cells_obs0)
         cat("cells from coords   - ")
        for(fam in names(cells_obs)) { 
        extract3857_h <-terra::extract(h3857, cells_obs0[[fam]], xy=TRUE)
        #print(extract3857_h)
        xy <- cbind(x=extract3857_h$x, y=extract3857_h$y) 
        extract3857 <-  terra::extract(local_hab, xy, cells=TRUE, method="simple" )
         cells_obs[[fam]] <- extract3857$cell
        }
       
      cat(paste0(length(cells_obs), " obs fams ready  -\n"))
    } 
    
  } 
   
  } # if any metadata

 if(sim_type == "global_pop") {  
 cat(" global pop -")     
   
   if(!is.null(depCells)) {
     cat("translocs, annually  - ")
     depCells0 <- ll     
     initValues$transYears <- paste0("y",initValues$year)# + input$startYr_transloc)
     for (ind in initValues$group) {  
        depCells0[[initValues$transYears[ind]]] <-c(depCells0[[initValues$transYears[ind]]], depCells[ind] )
     }
     depCells <- depCells0
     
   } # includes translocation pts
   
    ter.start <-  local_hab
    values(ter.start) <- 0        # set to 0 here only to dble check hab vals in script 
    mgmt.reps   <- as.numeric(as.character(input$sim_Nreps)) # 
    mgmt.reps %>% mgmt.reps() 
    StartYear %>% mgmt.startYear()    
     mgmt.years <- mgmt.years()

   reportFreq_vals <-seq(1,(mgmt.years+1),1) 
   cat(paste0(mgmt.years," mgmt years  -  ",mgmt.reps," reps" )) 
   
   paste0( mgmt.years," years,  ",mgmt.reps," reps" ) %>%  startGrowing_pars()
  
   sim_in_prog2 <- paste0( mgmt.years," years,  ",mgmt.reps," reps" ) 
   sim_in_prog2 %>%  sim_in_progress2()
   sim_in_prog2 %>%  sim_in_progress2_inputTab() 
   sim_in_prog2 %>% sim_in_progress2_outputTab()
   sim_in_prog2 %>% sim_in_progress2_aboutTab()
   div(p("simulation in progress, output will refresh soon"),style="background-color: #777;width: fit-content;  padding: 0 24px;color:#ffe62cc2;border-radius: 20px; text-align:center;")   %>% outputReadYN_txt()
 
}      
  

metaEff_in_sim <- NULL
if(!is.null(all_incFeatures)){
cat(paste0("\nincluding features: ",all_incFeatures))
       all_avail_features <- metaEff_all_SumTab()
      indx <- which(all_avail_features[,1] %in% all_incFeatures) 
      metaEff_in_sim <-all_avail_features[indx,]  
}
metaEff_in_sim %>% metaEff_in_sim()
   cat(paste0("\n~~~~~~~~~ .\n"))
   
 if(validated_input==TRUE) {
    ############### start sim in async
if(sim_type == "settlement_test") {  
 
  cat("\nasync start  (...) ")
       if(length(unique(cells_make0))==1){  if(is.na(cells_make0)) {cells_make0<- NULL }}
      #  ter.start <- terra::wrap(ter.start)  
        local_hab  <- local_habw() 
  future_promise({  simpop_function_transloc(hab=local_hab, initValues=initValues,
                                          #   ter.start=ter.start, 
                                             depCells=depCells,    
                                             cells_make0=cells_make0,departings=departings,
                                             country=country )   }, seed=TRUE) %...>% sim_pop_transloc() 
NULL
  
     }  
   
    
   
    
   
   
if(sim_type == "global_pop") {  
   cat("\nasync start  (...) ")
  # depCells is list of cells per year, or null
#  ter.start <- terra::wrap(ter.start)  
 local_hab  <- local_habw() # local_hab <- terra::wrap(local_hab)   
    cat("go  - ") 
  #  seq.reps <- seq_len(mgmt.reps)
    simpop_asyncFunction <-  function(seq.reps) {
    rep <- seq.reps
    
    
    future_promise({
    simpop_function(hab=local_hab, StartYear=StartYear, fam.start=fam.start, 
                              mgmt.years=mgmt.years, mgmt.reps=rep,
                              country=country,
                              depCells=depCells,initValues=initValues,
                              cells_make0=cells_make0,
                              cells_make50=cells_make50, cells_make75=cells_make75,
                              cells_obs= cells_obs, famDf_obs=famDf_obs, 
                              reportYears = reportFreq_vals )
               }, seed=TRUE)  %>% then(onFulfilled =   notify_success( paste0("init ",rep) ,timeout=1500,   
                  config_notify(width="100px ", height = "24px",borderRadius="50px", pauseOnHover =FALSE ,clickToClose=FALSE,showOnlyTheLastOne=TRUE,
                                 fontFamily ="Comfortaa", fontSize ="11px",className="lower_notifs_sgl",  
                                 background ="#555555D1" ,textColor="#c7f443",notiflixIconColor="#c7f443"))) 
   
    }   
  
   seq.reps <- seq_len(mgmt.reps) 
    
   
  prelim_output <-  purrr::map(seq.reps, simpop_asyncFunction) %>%
  promise_all(.list = .) 
  
   prelim_output   %...>% prelim_output()
    
#   promise_all( 
#     for(rep in seq_len(mgmt.reps)){ # sweet can create unnamed lists!
#    #  print(paste0("rep ",rep))
#          future_promise({ simpop_function(hab=local_hab, StartYear=StartYear, fam.start=fam.start, #ter.start=ter.start, 
#                              mgmt.years=mgmt.years, mgmt.reps=rep,country=country,
#                              depCells=depCells,initValues=initValues,
#                              cells_make0=cells_make0,
#                              cells_make50=cells_make50, cells_make75=cells_make75,
#                              cells_obs= cells_obs, famDf_obs=famDf_obs, 
#                              reportYears = reportFreq_vals )
#               }, seed=TRUE)
#          }) %...>%  prelim_output()
   
   NULL
}
   
  } else { #if(validated_input==TRUE)  
      notify_failure("simulation cancelled" , timeout=4000,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl",  
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
      notify_warning(error_message ,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
      
     model_running(FALSE)  
     enable("start_sim") 
     enable("try_settling") 
     mgmt.years(NULL) # KEEP! required to display the slider, wont disaply for init settlement test that way
     start_sim(NULL)
     enable("startYr_trySettl")  
    enable("timing")
    enable( "demog") 
     if(base::isTRUE(playSound())){
     insertUI(selector = "#placeholder_timing",  # beep.wav should be in /www of the shiny app
                 where = "afterEnd", 
                 ui = tags$audio(src = "beep-problem.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")  ) }
     disable_tableEdits(FALSE)
     rv_init$infotext_simError <- paste0(hr(),error_message,hr()) 
      }
   
   NULL  
   sim_type(NULL)
   trigger_sim(NULL)
 
 
 }, ignoreInit=TRUE, ignoreNULL=TRUE)
 

playSound <- reactiveVal(TRUE)
observeEvent(input$playSound,{
  cat(paste0("playSound is", input$playSound, "  - "))
   if(input$playSound == TRUE) {  playSound(TRUE)   }
   if(input$playSound ==FALSE) {  playSound(FALSE)  }
}, ignoreInit=TRUE, ignoreNULL=FALSE)

 
prelim_output<- reactiveVal()

observeEvent(prelim_output(),{ 
     req(is.promise(prelim_output())==FALSE) 
   cat("prelim output not promise - ") 
     req(length(prelim_output())>0)          
     cat("prelim output  non null- ") 
   
     famIndx <- 2
     terIndx <- 1
     outIndx <- 3 
     
      prelim_output <- prelim_output()
      fam.all <- do.call("rbind", lapply(prelim_output, `[[`, famIndx))
      ter.all <- do.call("rbind", lapply(prelim_output, `[[`, terIndx))
      out.all <- do.call("rbind", lapply(prelim_output, `[[`, outIndx))
      polys_simulatedTerrs <- NULL 
 
    if(!is.null(fam.all)){
         famSumm <- fam.all  %>% group_by(rep.id,fam.id,year) %>% summarise(num.adt=num.m + num.f) %>% ungroup()
         famSumm <- famSumm[famSumm$num.adt>0,]
            cat("out1")   

         ter.all$count <- 1 
         cat("----------")
         ter_occSumm <- NULL
         if(nrow(famSumm)>0) { 
                     for (repnum in seq_len(max(famSumm$rep.id))){
                          occfam <- unique(famSumm$fam.id[famSumm$rep.id == repnum])
                          ter_occSumm <- rbind(ter_occSumm,ter.all[ ter.all$rep.id == repnum & ter.all$ter %in% occfam,])
                     }  
  #browser()
  cat("out2")   
   ter_occSumm <- ter_occSumm %>% group_by(cell,year) %>% reframe(dens=sum(count)) #summarise(dens=sum(count)) %>% ungroup()
                 r_terSumm0 <- local_hab()
                 values(r_terSumm0) <- NA 
                 if (nrow(ter_occSumm)>0) {   
                     for(yr in  seq_len(mgmt.years()+1)) {
                            print(paste0("year ",yr))   

                           if(yr %in% ter_occSumm$year ){
                           r_terSumm <- r_terSumm0      
                           terra::values(r_terSumm)[ter_occSumm$cell[ter_occSumm$year == yr]] <-  ter_occSumm$dens[ter_occSumm$year == yr] 
                           pol_terSumm0 <- sf::st_as_sf(terra::as.polygons(r_terSumm, dissolve = F)) # was T -to id the families but not relevant when several reps
                           pol_terSumm0$year <- yr
                           polys_simulatedTerrs <- rbind(polys_simulatedTerrs,pol_terSumm0)
                } }
                  names(polys_simulatedTerrs)[1] <- "dens" 
        }  }  
    } ## if some output  
     cat("out")   
    laggedFam <- out.all %>%  group_by(rep.id) %>% reframe(year=year, num.fam=num.fam, value1=num.fam/lag(num.fam), value2= lag(num.fam))    
    laggedFam <- out.all %>%  group_by(rep.id) %>% reframe(year=year, num.fam=num.fam, value2= lag(num.fam)) 
    laggedFam$value2[is.na(laggedFam$value2)] <- out.all$num.fam.obs[1] + out.all$num.fam.trans[1] 
    laggedFam <- laggedFam$value2 # year 0 before
          cat("out4")   

    out.all$laggedFam <-  as.numeric(laggedFam)
    out.all$growthFactorFam <- 0
    out.all$growthFactorFam[out.all$laggedFam>0] <-  out.all$num.fam [out.all$laggedFam>0]/out.all$laggedFam[out.all$laggedFam>0]
                  cat("out5")   

    out.table  <- out.all %>% group_by(year) %>% reframe(
                        num.fam = paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(num.fam),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>", "(",min(num.fam),"-",max(num.fam),")") ,
    
                        num.fam.obs = paste (  "<span style='font-weight:bold;line-height:2em;'>", min(num.fam.obs)  ) , 
                       
                        num.trans = paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(num.fam.sttld),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>", "(",min(num.fam.sttld),"-",max(num.fam.sttld),") / ", min(num.fam.trans) ),
                       
                        growthFactorFam=   paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(growthFactorFam)*100),"%</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>",  "(",round(min(growthFactorFam)*100),"-",round(max(growthFactorFam)*100),"%)" ),
                        
                        num.adt.pairs= paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(num.adt.pairs),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>", "(", min(num.adt.pairs),"-", max(num.adt.pairs),")" ), 
                         
                        num.adt = paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(num.adt),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>", "(",min(num.adt),"-",max(num.adt),")"),
                  
                        num.adt.obs = paste ( "<span style='font-weight:bold;line-height:2em;'>", min(num.adt.obs)," &",min(num.young.obs) ) ,      
                        
                        num.adt.settled = paste ( "<span style='font-weight:bold;line-height:2em;'>", round(mean(num.adt.settled),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>", "(",min(num.adt.settled),"-",max(num.adt.settled),") / ",min(num.adt.translocs)),
                        
                        num.adt.rmved = paste ( "<span style='font-weight:bold;line-height:2em;'>",  round(mean(num.adt.rm),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>",  "(",min(num.adt.rm),"-",max(num.adt.rm),")"),
                  
                        num.yng.rmved = paste ( "<span style='font-weight:bold;line-height:2em;'>",  round(mean(num.yng.rm),1),"</span>",
                        "<br><span style='font-weight:normal;font-size:90%;'>",  "(",min(num.yng.rm),"-",max(num.yng.rm),")") 
      )
                         
    out.table$growthFactorFam[1] <- as.numeric(NA) # factor =no sense on year 1           
    ## clean null values
    out.table$num.trans[which(substring(out.table$num.trans,nchar(out.table$num.trans)-1,nchar(out.table$num.trans))  == " 0") ] <- 0 
    out.table$num.adt.settled[which(substring(out.table$num.adt.settled,nchar(out.table$num.adt.settled)-1,nchar(out.table$num.adt.settled)) == " 0") ] <- 0
    out.table$num.yng.rmved[which(substring(out.table$num.yng.rmved,nchar(out.table$num.yng.rmved)-1,nchar(out.table$num.yng.rmved)) == " 0") ] <- 0
    out.table$num.adt.rmved[which(substring(out.table$num.adt.rmved,nchar(out.table$num.adt.rmved)-1,nchar(out.table$num.adt.rmved)) == " 0") ] <- 0
           cat("out9")   
   
    ### for plotting       
    ters <- out.all %>% group_by(year) %>%  reframe(mins = min(num.fam), means=mean(num.fam), maxs=max(num.fam)) %>% as.data.frame()
    ters$popType <- "families"
    trans <- out.all %>% group_by(year) %>%  reframe(mins = min(num.fam.sttld), means=mean(num.fam.sttld), maxs=max(num.fam.sttld)) %>% as.data.frame()
    trans$popType <- "translocated" 
    ads <- out.all %>% group_by(year) %>%  reframe(mins = min(num.adt), means=mean(num.adt), maxs=max(num.adt)) %>% as.data.frame()
    ads$popType <- "adults"
    adsSettled <- out.all %>% group_by(year) %>%  reframe(mins = min(num.adt.settled), means=mean(num.adt.settled), maxs=max(num.adt.settled)) %>% as.data.frame()
    adsSettled$popType <- "adsSettled" 
     cat("out6")   

    out.plot <- rbind(ters, trans, ads,adsSettled)
 
    if(!is.null(polys_simulatedTerrs)){
           cat("out7")   

        simpop <- list(out.table, out.plot,polys_simulatedTerrs, out.all)
    } else {
      simpop <-list( NA, NA, NA,out.all)
      cat("null output")
    } 
  
    simpop %>% sim_pop()     
    prelim_output(NULL)   
}, ignoreNULL=TRUE)

# stores start year value at start of sim to match in output
startYr_transloc <- reactive({
 if(is.null(rv_dis$timing)) {return(thisYear())} else {return(min(rv_dis$timing$year))}
})   
   
startYr_trySettl  <- reactiveVal() 
startYr_obsRecs  <- reactiveVal() 
## mimi     
 
       

## sim time estimation collect data
## observeEvent(start_sim(),{
## Sys.time() %>%  simTime()

##  <- Sys.time() - ()
##  %>%  sim.time()
## perfDat <- read.csv("perfDat.csv")
## thisPerf <- cbind(dat=Sys.date(), mgmt.years=mgmt.years(), mgmt.reps=mgmt.reps(), sim.time=sim.time())
## perfDat <- rbind(perfDat, thisPerf)

 
sim_type <- reactiveVal("settlement_test")
 
sim_pop_transloc <- reactiveVal(NULL)
sim_pop <- reactiveVal(NULL)
#, animate = animationOptions(200)
 
allow_update_sim <- reactiveVal(NULL)

observeEvent(sim_pop(),{ 
  #  trigger_undo_settling(trigger_undo_settling()+1)
   # trigger_undo_sim(trigger_undo_sim()+1)
    allow_update_sim(1)
},ignoreNULL=TRUE) # null when sim in progress

observeEvent(allow_update_sim(),{   #sim_pop(),{  
  ### note: sim_pop() is never NULL unless triggered by input$undo_sim or during sim
  
  req(!is.null(allow_update_sim()))  
  allow_update_sim(NULL)
  
     outColSelected(NULL) 
     cols$showing <- 1:6 
     sliderParams$min <- 0
     sliderParams$max <- 1
     sliderParams$value <- 0
#    updateSliderInput(session, "show_simYrs" ,  min = 0, max=1, value= 0)  # dummy for change with update
# recapTable(NULL) 
 outDat_areaOfInterest(NULL)
 recapTableOut_fams(NULL) 
 recapTableOut_N(NULL) 
 recapPlot_N(NULL)
 annual_Nadt(NULL) 
 annual_Nfams(NULL)
 recapPlot_fams(NULL) 
 
recapSummPlot_fams(NULL) 
recapSummPlot_fams_hili(NULL)
recapSummPlot_N_hili(NULL) 
recapSummPlot_N(NULL) 
  
 
req(!is.null(sim_pop())) # that is out.plot which is non null even when no family in output - only on start or when no output
cat("some output")
#req(sim_pop())   
 "viewing simulated population<br> " %>% model_sim_UItxtoutTtl()
   model_sim_UItxtout(NULL) # only here or wont show when running model when simpop disappears but savedat still has vals to dl or display
 simOutput(1)  # show output plots/tables
 sim_pop <- sim_pop()   
 mgmt.startYear <- mgmt.startYear() # mgmt.startYear()
 mgmt.reps <- as.numeric( mgmt.reps() )
  
  print(length(sim_pop[[1]]))
 if(length(sim_pop[[1]])>1){ # when output is null simpop[[1]] is null, but using simpop[[3]] really here first
        fam_sim  <- st_transform(sim_pop[[3]],  crs = st_crs(4326))  
        fam_sim$dens <- as.numeric(as.character(fam_sim$dens))
        fam_sim$Nruns <- ">0-39%"
        fam_sim$Nruns[fam_sim$dens/mgmt.reps >.39] <- "40-59%" 
        fam_sim$Nruns[fam_sim$dens/mgmt.reps >.59] <- "60-79%"
        fam_sim$Nruns[fam_sim$dens/mgmt.reps >.79] <- "80-100%"
        fam_sim$layer <- "probabilities over all reps"
                              #domain=c( "1 to 3 runs", "3 to 5 runs","5 to 10 runs","10 to 15 runs" )) # ?
       
       fam_sim$Nruns <- factor(fam_sim$Nruns, levels=c(">0-39%","40-59%","60-79%","80-100%"))#c( "1 to 3 runs", "3 to 5 runs","5 to 10 runs","10 to 15 runs" ))
       fam_sim$year <-  fam_sim$year +mgmt.startYear-1

        sim_out$fam_sim <- fam_sim # set to null at start of sim
          recap_table <- sim_pop[[1]] %>% as.data.frame()
   recap_plot  <- sim_pop[[2]] %>% as.data.frame()
   print("recap_table") 
   
   recap_table$year <- as.numeric(as.character(recap_table$year )) +  mgmt.startYear  -1 #starts year 1
   recap_plot$year <- as.numeric(as.character(recap_plot$year )) +  mgmt.startYear  -1 #starts year 1
cat("ok")
    
 show_simOutputNULL(NULL)
 
 } else { 
  as.character("<br><span style='color:#e77e68;font-weight: bold;'>no simulated territories in output!")  %>% model_sim_UItxtout() 
  recapTableOut_fams(NULL) 
   sim_out$fam_sim <- NULL
   recap_table <- recap_plot <- NULL
   recapSummPlot_N(NULL) ## rendering plot of null values for 1 rep, no table no summary plot
   recapSummPlot_fams(NULL) 
   outputReadYN_txt( p("no output") ) 
   datSave_sim_pop(NULL) ##no downloadable output
   show_simOutputNULL(1) # to show NULL output instead of year slider
 } 
 
 recap_table %>% recapTable()
   cat("\n--------------------------------------------------- sim_pop ready! - ")
    simModelOut(1)    
   start_sim(NULL) 
   mgmt.years <- mgmt.years()
   startGrowing(NULL)
   paste0("output ready!") %>%  startGrowing_pars()
     
      
    
 #####  reset map display 
 reset_simTriggers <- reset_simTriggers()
 (reset_simTriggers+1) %>% reset_simTriggers()
        
   
   
annual_stats <- sim_pop[[4]] %>% as.data.frame()
translocEff <- NULL
 ganttdf <- NULL
 recEff <- NULL
#rep.id year num.fam num.fam.obs num.fam.sttld num.fam.trans num.adt.settled num.adt_translocs num.adt deaths.adt num.juv    
annual_stats$year <-   annual_stats$year   +  mgmt.startYear -1
 translocEffect <- annual_stats %>% group_by(year) %>% summarise (N=min(num.fam.trans))
translocEffect <- translocEffect[translocEffect$N>0,] 
#print(translocEffect)
if(nrow(translocEffect)>0){ 
        for(ro in 1:nrow(translocEffect)){
        translocEff0 <- data.frame(StartDate= translocEffect$year[ro], EndDate= translocEffect$year[ro], 
                                  effectId= paste0("translocation (",   translocEffect$N[ro],"gp)"))
        translocEff <- rbind(translocEff,translocEff0) 
        }   
        cat("translocations  - ")
         translocEff$effType <- "translocation"
} 
         
   
    # metaEff_in_sim() updated as soon as model triggered but only show when other outputs are ready, here - means download wouldnt be synced!
    
    effdf <- metaEff_in_sim()  
  if( !is.null(effdf)) { # update at start of sim includes only effects in current sim - filtered metaEff_all_SumTab()
       cat("added effects  - ") 
       effdf <- effdf [!effdf$effectId %in% pointFeature()$LeafletId,]
          if(nrow(effdf)>0) { 
            newFeaturesDat_display <- newFeaturesDat_display()
             ganttdf <- data.frame(effectId=effdf$effectId,  StartDate= 0,  EndDate=0)
              tdf <- t(effdf[,2:ncol(effdf)])
              colnames(tdf) <- effdf$effectId
              tdf <- as.data.frame(cbind(year=as.numeric(gsub("\\D", "",  row.names(tdf))),tdf))
            
             newFeaturesDat_shp3857 <- newFeaturesDat_shp3857()  
                        
              for(eff in ganttdf$effectId) {
                effdat <- tdf[,colnames(tdf) == eff]
                yrs <- tdf$year[which(effdat==1)]
                
                ganttdf$StartDate[ganttdf$effectId==eff] <- as.numeric(gsub("\\D", "",   min(yrs)))
                ganttdf$EndDate[ganttdf$effectId==eff]   <- as.numeric(gsub("\\D", "",  max(yrs)))
                if(ganttdf$StartDate[ganttdf$effectId==eff] != ganttdf$EndDate[ganttdf$effectId==eff]){
                ganttdf$effectId[ganttdf$effectId==eff]  <- paste0(newFeaturesDat_display$name[newFeaturesDat_display$LeafletId == eff],
                 " active ",ganttdf$StartDate[ganttdf$effectId==eff]," - ",ganttdf$EndDate[ganttdf$effectId==eff])
                } else {
                ganttdf$effectId[ganttdf$effectId==eff]  <- paste0(newFeaturesDat_display$name[newFeaturesDat_display$LeafletId == eff]) 
                  } 
                ganttdf$effType <- newFeaturesDat_shp3857$layerType[newFeaturesDat_shp3857$LeafletId ==eff] 
              } 
             
             
              ganttdf$StartDate [ganttdf$StartDate<min(annual_stats$year) ] <- min(annual_stats$year)
              ganttdf$EndDate [ganttdf$EndDate> max(annual_stats$year) ]  <- max(annual_stats$year)
              
      }}
     
    effdf <- metaEff_in_sim() 
  if( !is.null(effdf)) {   
      effdf <- effdf [ effdf$effectId %in% pointFeature()$LeafletId,]

  recEff  <- data.frame(pointFeature() %>% st_drop_geometry() %>% group_by(start) %>% summarise(StartDate=mean(start),EndDate=mean(start),effectId=paste0("observation (",length(unique((LeafletId))),"terr)"),effType="incorporate records") ) 
  recEff  <-subset(recEff,select=-start)
  cat("obs recs - ") 
  }
             
               
    
    
     
        ganttdf <- rbind(translocEff, recEff, ganttdf) # }
        ganttdf <- ganttdf[order(ganttdf$effType,ganttdf$StartDate),]
        
        
        
         
    effcols <- c( "modify habitat" = "#FB8861FF", "incorporate records" = "magenta", "translocation"="turquoise")
   ganttdf$effGroup <- ganttdf$effType
   ganttdf$effGroup[ganttdf$effGroup=="modify habitat"] <- ganttdf$effectId[ganttdf$effType=="modify habitat"]
     
     
      if(!is.null(ganttdf)) {        
  cat("ganttdf   - ")   
   #     ganttdf$effectId_axis <- letters[1:length(ganttdf$effectId)]
       # ganttdf$effectId_axis <- letters[1:length(ganttdf$effGroup)]
         #length(ganttdf$effectId) %>% nrow_ganttdf()  
        length(unique(ganttdf$effGroup)) %>% nrow_ganttdf() 
        ganttdf$effType <- factor(ganttdf$effType, levels=c( "incorporate records", "translocation","modify habitat" ))

 ganttdf$effGroup <- factor( ganttdf$effGroup, levels=c( "incorporate records", "translocation",ganttdf$effGroup[ganttdf$effType == "modify habitat"]))
  ganttdf <- ganttdf[order(ganttdf$effType,ganttdf$StartDate),]
 ganttdf$effectId_axis <-letters[as.numeric( ganttdf$effGroup )]
        
        
        
        
         ganttdf_dat <- ganttdf 
        ganttdf_dat %>% ganttdf_dat()
        
 ganttdf_dat$effectId  <- str_replace(ganttdf_dat$effectId,"observation","obs") # remove id past _ symbol for different selection sets
 ganttdf_dat$effectId  <- str_replace(ganttdf_dat$effectId,"translocation","transl")
 ganttdf_dat$effectId  <- str_replace(ganttdf_dat$effectId,"terr","") # remove id past _ symbol for different selection sets
 ganttdf_dat$effectId  <- str_replace(ganttdf_dat$effectId,"gp","") # remove id past _ symbol for different selection sets
 ganttdf_dat$effectId  <- str_replace(ganttdf_dat$effectId,"\\(","(x") # remove id past _ symbol for different selection sets
 
         
 
  ganttdf <-  ganttdf_dat
  
       ganttdf <-
        ggplot() +
          geom_segment(data = ganttdf ,aes(x = StartDate, y = effGroup, xend = EndDate, yend = effGroup), color=1, linewidth=.5)+  
        geom_point(data=ganttdf, aes(x=StartDate, y=effGroup, group=effGroup, color=effType), size=2)     +
        geom_point(data=ganttdf, aes(x=EndDate, y=effGroup, group=effGroup, color=effType), size=2) +  
        scale_color_manual(values = effcols) +
        geom_text(data=ganttdf, aes(x= StartDate+.1 , y=effGroup, group=effGroup, color=effectId, label=effectId) , 
                  vjust =-.4, hjust = 0,  family="Comfortaa", size = 8/.pt)     +  
       
     #   geom_segment(data = ganttdf ,aes(x = StartDate, y = effectId, xend = EndDate, yend = effectId), color=1, linewidth=.5)+  
    #    geom_point(data=ganttdf, aes(x=StartDate, y=effectId, group=effectId, color=effType), size=2)     +
    #    geom_point(data=ganttdf, aes(x=EndDate, y=effectId, group=effectId, color=effType), size=2) +  
    #    scale_color_manual(values = effcols) +
     #   geom_text(data=ganttdf, aes(x= StartDate+.1 , y=effectId, group=effectId, color=effectId, label=effectId) , 
    #              vjust =-.4, hjust = 0,  family="Comfortaa", size = 8/.pt)     + # font-size per pt, handy in-built function
         theme_minimal(base_size = 8)+ 
         theme( axis.text.x = element_text(colour = "#555555cf", family="Comfortaa"),
                        axis.text.y = element_text(colour = "transparent"), 
                    legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "white"),
                     panel.background = element_rect(fill = "#e1e1d0", colour = NA))+
      scale_y_discrete("", labels=ganttdf$effectId_axis ) +
      scale_x_continuous("",breaks=seq(min(annual_stats$year),max(annual_stats$year),1))+ 
      coord_cartesian(xlim= c(min(annual_stats$year),max(annual_stats$year))  )  
       
          
      ganttdf_dat %>% ganttdf_datHili()
      ganttdf %>% ganttdf()       
      
    
     
      ganttdf_noTxt <-  ggplot() +
            geom_segment(data = ganttdf_dat ,aes(x = StartDate, y = effGroup, xend = EndDate, yend = effGroup), color="transparent", linewidth=.5)+ 
      #         geom_segment(data = ganttdf_dat, aes(x = StartDate, y = effectId, xend = EndDate, yend = effectId), color="transparent", linewidth=.5)+   
         theme_minimal(base_size = 8)+ 
        scale_y_discrete("", labels=ganttdf_dat$effectId_axis ) +
        scale_x_continuous("",breaks=seq(min(annual_stats$year),max(annual_stats$year),1))+
        coord_cartesian(xlim= c(min(annual_stats$year),max(annual_stats$year))  )  +
         theme( axis.text.x = element_text(colour = "transparent", family="Comfortaa"),
                     axis.text.y = element_text(colour = "transparent"), 
                     legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(), 
                     plot.background = element_blank(),
                     panel.grid =  element_blank(),
                     panel.background = element_blank()) 
        
        ganttdf_noTxt %>% ganttdf_noTxt()
      } 
   
    
       
cat("annual_stats  - ")
  
 
annual_Nadt <- subset(annual_stats, select=c(rep.id, year, num.adt, num.adt.settled, num.adt.obs))
annual_Nadt$popType <- "adults"
annual_Nadt %>% annual_Nadt()
 annual_Nfams <- subset(annual_stats, select=c(rep.id, year, num.fam, num.fam.sttld, num.fam.obs))
annual_Nfams$popType <- "families"                      
 annual_Nfams %>% annual_Nfams()
                                      
   Ncols <- c(  "adults" =   "#008080" ,  "families" = "darkmagenta" )
   Ncols_ave <- c( "adults" =  "turquoise" , "families" ="magenta"  )
 
maxNobs <- max(annual_Nadt$num.adt)+1
maxNterr <- max(annual_Nfams$num.fam)+1
yearSeq <- seq(min(annual_stats$year),max(annual_stats$year),1)
yearLims <- c(min(yearSeq),max(yearSeq))
 
#print(seq(min(recap_table$year),max(recap_table$year),1))
 yobsStep <-tobsStep <-2 
if(maxNobs>26) {yobsStep <- 5}
if(maxNobs>65) {yobsStep <- 10} 
if(maxNobs>100) {yobsStep <-20}
if(maxNobs>200) {yobsStep <-50}
if(maxNobs>500) {yobsStep <-100}
if(yobsStep>6) {tobsStep <-yobsStep/2}

   recap_plot_N <-   ggplot()+ 
         geom_line(data=annual_Nadt, aes(x=year, y=num.adt, group= rep.id, color=popType), linewidth=.5) +
      scale_x_continuous("",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  + 
     coord_cartesian(ylim= c(0, maxNobs)  ) + 
         scale_y_continuous("",breaks=seq( 0,maxNobs,yobsStep) )+
         scale_colour_manual(values = Ncols)+
         ggtitle("simulated total adult abundance (annual count)") + 
         theme_minimal(base_size = 8)+  
         theme(  
               axis.text = element_text(colour = "#555555cf", family="Comfortaa"), 
          legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "white"),
                     panel.background = element_rect(fill = "#e1e1d0", colour = NA),
          plot.title = element_text(hjust=0, vjust=1, family="Comfortaa"))  
  
 recap_plot_N %>% recapPlot_N()
cat("adts vs time plot  - ") 

 
  recap_plot_fams <-   ggplot()+    
         geom_line(data=annual_Nfams, aes(x=year, y=num.fam, group= rep.id, color=popType), linewidth=.5) + 
           scale_x_continuous("",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  + 
          coord_cartesian(ylim= c(0, maxNterr)  ) + 
         scale_y_continuous("",breaks=seq(0,maxNterr,yobsStep/2))+
         scale_colour_manual(values = Ncols)+ 
         theme_minimal(base_size = 8)+  
         theme( 
               axis.text = element_text(colour = "#555555cf", family="Comfortaa"), 
         legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "white"),
                     panel.background = element_rect(fill = "#e1e1d0", colour = NA)) +
     ggtitle("simulated total number of territories (annual count)")+
     theme(plot.title = element_text(hjust=0, vjust=1, family="Comfortaa"))
  
 recap_plot_fams %>% recapPlot_fams() 
   
 req(!is.null(recap_plot))
recapSummPlot_fams  <-   ggplot(data=recap_plot[recap_plot$popType == "families",])+  
         geom_ribbon( aes(x=year, ymin=mins , ymax=maxs, fill=popType), linewidth=0, alpha=.6) +
         geom_line(  aes(x=year, y=means, color=popType), linewidth=.5) +
            scale_x_continuous("",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  + 
          coord_cartesian(ylim= c(0, maxNterr)  ) + 
         scale_y_continuous("",breaks=seq( 0,maxNterr,tobsStep) )+
         scale_colour_manual(values = Ncols)+ 
         scale_fill_manual(values = Ncols_ave)  + 
          ggtitle("simulated total number of territories (annual summary)") +
         theme_minimal(base_size = 8)+  
         theme(axis.text = element_text(colour = "#555555cf", family="Comfortaa"), 
               legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "white"),
                     panel.background = element_rect(fill = "#e1e1d0", colour = NA),
                    plot.title = element_text(hjust=0, vjust=1, family="Comfortaa"))
 recapSummPlot_fams %>% recapSummPlot_fams()

library(ggtext) 
## downloadable plot
output_subtitle <- paste0("annual summary based on ",mgmt.reps()," simulation runs")
 
recapfams <- ggplot(data=recap_plot[recap_plot$popType == "families",])+  
         geom_ribbon( aes(x=year, ymin=mins , ymax=maxs, fill=popType), linewidth=0, alpha=.6) +
         geom_line(  aes(x=year, y=means, color=popType) ) +
            scale_x_continuous("year of simulation",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  + 
          coord_cartesian(ylim= c(0, maxNterr)  ) + 
         scale_y_continuous("number of territories",breaks=seq( 0,maxNterr,tobsStep) )+
         scale_colour_manual(values = Ncols)+ 
         scale_fill_manual(values = Ncols_ave)  + 
                    labs(title="simulated number of beaver territories", subtitle = output_subtitle,  
                         caption ="<p>generated in <span style='color:darkmagenta;'>shinyapps.io/beavApp_testBranch_apr24</span></p>" )+
                    theme_bw(base_size =7)+  
                    theme( text = element_text(colour = "#555555cf", family="Comfortaa"),
                           axis.text = element_text(colour = "#555555cf", family="Comfortaa"), 
                           axis.title.y = element_text(colour = "#555555cf", family="Comfortaa"), 
                           axis.title.x = element_text(colour = "#555555cf", family="Comfortaa"),  
                           legend.position = "none",  plot.caption = element_markdown(size = 6, family="Comfortaa"),
                           plot.title = element_text(colour = "black",hjust=0, vjust=1, family="Comfortaa"))   
   
    
 
##### summary stats plots 
   
 #recap_plot <- recap_plot[recap_plot$maxs>0,]
 
  
  recap_plot$mins[recap_plot$maxs==0 ] <- NA
  recap_plot$means[recap_plot$maxs==0 ] <- NA
  recap_plot$maxs[recap_plot$maxs==0 ] <- NA

 
 recap_plot_Summ  <-   ggplot(data=recap_plot[recap_plot$popType == "adults",]) + 
         geom_ribbon( aes(x=year, ymin=mins , ymax=maxs, fill=popType), linewidth=0, alpha=.6) +
         geom_line(  aes(x=year, y=means, color=popType), linewidth=.5) +
          scale_x_continuous("",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  + 
          coord_cartesian(ylim= c(0, maxNobs)  ) +   
         scale_y_continuous("",breaks=seq( 0,maxNobs,yobsStep) )+  
         scale_colour_manual(values = Ncols)+ 
         scale_fill_manual(values = Ncols_ave)   +   
     ggtitle("simulated total adult abundance (annual summary)") +
     theme_minimal(base_size = 8)+  
         theme(axis.text = element_text(colour = "#555555cf", family="Comfortaa"), 
               legend.position = "none",  
                     axis.title.x = element_blank(), 
                     axis.title.y = element_blank(),   
                     plot.background = element_blank(),
                     panel.grid =  element_line( colour = "white"),
                     panel.background = element_rect(fill = "#e1e1d0", colour = NA),
                    plot.title = element_text(hjust=0, vjust=1, family="Comfortaa"))
  
 recap_plot_Summ %>% recapSummPlot_N() 
# outColSelected(0) 
cat("fams vs time plot  - ")

## prepare data for download
 ##  which(as.numeric(sapply(cells_make0, is.null))==0)
   #if(!is.null(sim_pop[[3]])){  
      outputReadYN_txt(NULL)
       
  
## downloadable plot
 recapN <-  ggplot(data=recap_plot[recap_plot$popType == "adults",]) + 
                      geom_ribbon( aes(x=year, ymin=mins , ymax=maxs, fill=popType), linewidth=0, alpha=.6) +
                      geom_line(  aes(x=year, y=means, color=popType) ) +
                      scale_x_continuous("year of simulation",breaks=yearSeq)+ coord_cartesian(xlim= yearLims  )  +  coord_cartesian(ylim= c(0, maxNobs)  ) +   
                      scale_y_continuous("number of adults")+  
                      scale_colour_manual(values = Ncols)+ 
                      scale_fill_manual(values = Ncols_ave)+   
                      labs(title="simulated adult beaver abundance", subtitle = output_subtitle, 
                           caption = "<p>generated in <span style='color:#008080;'>shinyapps.io/beavApp_testBranch_apr24</span></p>")+
                      theme_bw(base_size =7)+  
                      theme(text = element_text(color = "#555555cf", family="Comfortaa"),
                            axis.text = element_text(), 
                            axis.title.y = element_text(colour = "#555555cf", family="Comfortaa"), 
                            axis.title.x = element_text(colour = "#555555cf", family="Comfortaa"),  
                            legend.position = "none",  plot.caption = element_markdown(size = 6, family="Comfortaa"),
                            plot.title = element_text(color = "black",hjust=0, vjust=1, family="Comfortaa"))   
 mgmt.reps <- mgmt.reps()
     labs_sim  <-  as.character( paste0("<span style='float:left;font-weight:bold;padding:0;padding-right:4px;margin:0;font-size:100%;color:magenta'><i class='fa-solid fa-spinner'></i></span>  
                                <span style='float:left;color:magenta;padding:0;'>simulation output  </span><br>
                                <span style='font-weight:bold;color:darkmagenta;float:none;'>year ",
                                                sim_out$fam_sim$year,
                                                 ", ",sim_out$fam_sim$Nruns,
                                "</span><br><p>cell occupied in ",sim_out$fam_sim$dens," / ",mgmt.reps," reps.</p>")) 
 
  sim_out$fam_sim$labs_sim <- sprintf(  "%s",labs_sim  )   %>%  lapply(htmltools::HTML)  
 
outputDat <-  list( simpop_shp=sim_out$fam_sim,
                    recPlot_summN_gg=recapN,
                    recPlot_summFams_gg=recapfams,
                    recTable_annual_stats = annual_stats) 
 outputDat <- outputDat [ which(as.numeric(sapply(outputDat, is.null))==0)]
#  req(!is.null(outputDat))
 outputDat %>% datSave_sim_pop() 
   #}  
  #  sim_pop(NULL)##may20
}, ignoreInit=FALSE, ignoreNULL=FALSE)

 
recapTable <- reactiveVal(NULL)
recapTableOut_fams <- reactiveVal(NULL)
recapTableOut_N <- reactiveVal(NULL)
recapTableOut_headers<- reactiveVal(NULL)
datSave_sim_pop <- reactiveVal(NULL)
datSave_settleTest <- reactiveVal(NULL)

 

### !!note : These extensions do/may not work in Shiny apps: Scroller does not work with column selection (#283).!! 
### selection diabled, working it out with cell clicks
observeEvent(recapTable(),{
  cat( "recap  -  ")
  dat <- recapTable() 
  mgmt.reps <- mgmt.reps()
  dat <- t(dat) %>% as.data.frame()
  vars <- c( "",#"output per simulation year" ,
              "simulated territories",
              "territories observed",
              "translocated groups settled/total",
              "annual growth factor",
              "families including pair of adults", 
              "simulated adult abundance",
              "observed abundance",
              "translocated adults settled/total", 
              "removed adults",
              "removed young")
  
  units <- c(  "",#"annual summary over ",mgmt.reps," repetitions"), 
             "mean (min-max)",
             "",
             "mean (min-max)/total",
             "mean (min-max)",
             "mean (min-max)",
             "mean (min-max)",
             "number of adults & young",
             "mean (min-max)/total",
             "mean (min-max)",
             "mean (min-max)" ) 
  vars <-  paste( "<span style='font-weight:bold;line-height:2em;text-wrap: wrap;'>", vars,"</span>")
  units <-paste("<span style='text-align:center;font-weight:normal;text-wrap: wrap;'>",units)
  lab <-  paste ( vars , units , sep="<br>") 
  dummyvars <- letters[seq(1:length(vars))]
  dat <- cbind(dummyvars,lab, dat) %>% as.data.frame()
   
  
   ##condition for printing or not tables?
  
  
#   dumRo <-dat[1,]
#  dumRo[1:length(dumRo)] <- rep(paste( "<span style='font-weight:bold;line-height:.1em;color:transparent;padding:0;'>.</span>"),length(dumRo))
#  dat <- rbind(dat[1,],dumRo, dat[c(2:3),],dumRo, dat[c(4:5),])
   cat("render  -")
   totW <- 42
   firstcolW <- 12
   anycolW <- 7 # note - config for years 3-5-10
   mgmt.years <- mgmt.years()
   mgmt.years <- as.numeric(mgmt.years)
   
   if(mgmt.years ==5) {
     firstcolW <- 12
     anycolW <- 5}
   
   if(mgmt.years ==10) {
     firstcolW <- 12
     anycolW <- 4}
   
   
  #  anycolW <- (40-firstcolW)/(ncol(dat)-2) #round(50/(ncol(dat)+2))
   firstcolWidth <- paste0(firstcolW ,"vw")

 
firstcolW %>% firstcolW()
anycolW %>% anycolW()

   firstcolWidth %>% colWidthRecapYrs()
   year_dat <- t(data.frame(year=c("z",".", seq(1,mgmt.years+1)-1+simStartYearParams$minyear)))
   datatable( year_dat,    
       colnames = rep("", ncol(year_dat)),
       selection = "none", 
       rownames = FALSE, escape=FALSE,  
       extensions = c('KeyTable', 'FixedColumns'),
        options = list(dom = 't',autowidth = TRUE, ordering=F, #scrollX = TRUE,  
                       fixedColumns = list(leftColumns = 1),
                          columnDefs = list(  
                          list(className ='dt-grey', targets =c( 2 :( ncol(year_dat)-1))), 
                          list(className = "dt-center", targets = c(2:(ncol(year_dat)-1) )),
                          list(width = paste0(anycolW,"vw"),  targets =c( 2 :( ncol(year_dat)-1))),
                          list(width =  firstcolWidth ,  targets =c(1)) ,
                         list(visible = FALSE, targets = c(0 )),
                         list(className ='dt-oval',targets =c( 2 :( ncol(year_dat)-1))  )  ,
                         list(className = "dt-transparent",  targets =c(1)), 
                         list(className ='dt-first', targets = c(1))  
                          )))  %>% recapTableOut_headers()
   
 ttlCellStyle <- as.character("font-size:80%; margin: 8px 0 0 0 !important;")
    
   cat("hdrs tabl  - ")
 #print(rbind(dat[2,], rep("", ncol(dat)),dat[c(3,4),], rep("", ncol(dat)),dat[c(5,6),] ))
 #  datatable( rbind(dat[2,], rep("", ncol(dat)),dat[c(3,4),], rep("", ncol(dat)),dat[c(5,6),] ),#dat[c(2:6),] ,  

      terrdat <- rbind(dat[2,], 
                      c("",as.character(p("INPUT",style=c("color:#d4d4d4;",ttlCellStyle))),rep("", ncol(dat)-2)),
                    dat[c(3,4),], 
                      c("",as.character(p("ANALYSIS",style=c("color:#d0a6d8;",ttlCellStyle))),rep("", ncol(dat)-2)),
                    dat[c(5,6),] )
      terrdat[6,3] <- "NA"
        datatable(terrdat  , 
                    colnames = c("","OUTPUT",rep("", ncol(dat)-2)), 
      # selection =list(target = 'column',mode = "single" ,disable = list(columns =1)), 
        selection = "none", rownames = FALSE, escape=FALSE,
       extensions = c('KeyTable', 'FixedColumns'),
       options = list(dom = 't',autowidth = TRUE, ordering=F, keys = TRUE,pageLength=-1,
                       fixedColumns = list(leftColumns = 1),
                       columnDefs = list( 
                     #   list(className ='dt-grey', targets = c(1)), 
                         list(width = paste0(anycolW,"vw"),  targets =c(2:(ncol(dat)-1))) ,#
                         list(width =  firstcolWidth ,  targets =c(1)) ,
                         list(className ='dt-first', targets = c(1)) ,
                         list(visible = FALSE, targets = c(0)))  )   )    %>% recapTableOut_fams()
   
   cat("fams tabl  - ")
   datatable(# rbind(dat[7,], rep("", ncol(dat)),dat[c(8,9),], rep("", ncol(dat)),dat[c(10,11),] ),#dat[c(2:6),] ,  
     rbind(dat[7,], 
              c("",as.character(p("INPUT",style=c("color:#d4d4d4;",ttlCellStyle))),rep("", ncol(dat)-2)),
              dat[c(8,9),], 
              c("",as.character(p("ANALYSIS",style=c("color:#68b6c6;",ttlCellStyle))),rep("", ncol(dat)-2)),
             dat[c(10,11),] ),  
     
              colnames = c("","OUTPUT",rep("", ncol(dat)-2)), 
       selection = "none", 
       #selection =list(target = 'column',mode = "single" ,disable = list(columns =1)), 
       rownames = FALSE, escape=FALSE, 
       extensions = "FixedColumns",
       options = list(dom = 't',autowidth = TRUE, ordering=F, #scroller = TRUE,
                       fixedColumns = list(leftColumns = 1),pageLength=-1,
                       columnDefs = list( 
                      #  list(className ='dt-grey', targets = c(1)), 
                        list(width = paste0(anycolW,"vw"),  targets =c(2:(ncol(dat)-1 ))) ,#
                         list(width =  firstcolWidth ,  targets =c(1)) ,
                         list(className ='dt-first', targets = c(1)) , 
                         list(visible = FALSE, targets = c(0)))  )  )  %>% recapTableOut_N()   
    
   
  
   cat("inds tabl  - ")
}, ignoreNULL=TRUE)


output$uirecapTableOut_headers <-  renderUI ({ 
 req(colWidthRecapYrs())
  wdth <- colWidthRecapYrs()
 print(paste0("width is ",wdth))
 
 styleTab <-   as.character(paste0( 
      "line-height: 90%;font-size: 90%;background-color:transparent;color:#555555cf!important;",
      "border-radius: 12px; width: fit-content;")) #margin-left: ",wdth,";"))  
  styleTab2 <-   as.character(paste0( "margin-left: ",wdth,";width: fit-content;"))
  
  return(  tagList(
       # div(p("_"),style=styleTab2),
        div(DTOutput("recapTableOut_headers") ,  style=styleTab),
        bsTooltip("recapTableOut_headers" ,"year of simulated output" ,placement = "top", trigger = "hover" )
))
  
})
      


dt_rowShowCol <- "#ffe62cc2c2" # alpha

output$recapTableOut_headers <-  DT::renderDT({  
  req(recapTableOut_headers())
  req(currentOutput_year())# should be null when no data
  tbl <- recapTableOut_headers()   
  tbl %>% formatStyle(columns=outColSelected()+1, backgroundColor ="#d6c335")# dt_rowShowCol)  # BEAuT! 
},  server=FALSE)
 

colWidthRecapYrs <- reactiveVal(NULL)


colCol <- "#f5f5dc" # hi alpha
colCol1 <- "#ffffffc4" # hi alpha
colCol2 <- "#f8cdff80" # hi alpha
 colCol3 <- "#6c99ab9e" # hi alpha ##b66e56?

currentOutput_year <-  reactiveVal(NULL)  
 
output$recapTableOut_fams <-  DT::renderDT({   
  req(recapTableOut_fams())
  req(currentOutput_year())# should be null when no data
     tbl <- recapTableOut_fams()  # should be null when no data so tbl should be just null when nothing going on
     tbl  <- tbl %>% formatStyle(0, target = "row",   
                                 color = styleRow(c(1,2,3,4,5,6,7),c("darkmagenta","beige", "black", "black","beige", "black", "black")) ) 
     
     tbl  <- tbl %>% formatStyle(columns=2 , target = "cell",  backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c("beige","#444444","#d1d1d1","#d1d1d1","#444444" ,"#99849d","#99849d"))) 
     tbl  <- tbl %>% formatStyle(columns=c(3:(currentOutput_year() +2)) , target = "cell",   
                                 backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c(colCol,"#444444",colCol1,colCol1,"#444444" ,colCol2,colCol2))) 
     tbl %>% formatStyle(columns=outColSelected()+1,target = "cell", borderRadius = "50px", 
                       backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c("darkmagenta","#444444", dt_rowShowCol, dt_rowShowCol,"#444444" , dt_rowShowCol, dt_rowShowCol)), 
                       color =  styleRow(1,"magenta", "black"))  
   },  server=FALSE) 


output$recapTableOut_N <-  DT::renderDT({  
  req(recapTableOut_N())
 req(currentOutput_year())
  tbl <- recapTableOut_N() 
  tbl <- tbl %>% formatStyle(0, target = "row",  color = styleRow(1,"#008080", "black"))  
  tbl  <- tbl %>% formatStyle(columns=2 , target = "cell",  backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c("beige","#444444","#d1d1d1","#d1d1d1","#444444" ,"#57727d","#57727d"))) 
  tbl  <- tbl %>% formatStyle(columns=c(3:(currentOutput_year()+2)) , target = "cell",   
                                 backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c(colCol,"#444444",colCol1,colCol1,"#444444" ,colCol3,colCol3))) 
  tbl %>% formatStyle(columns=outColSelected()+1,target = "cell", borderRadius = "50px", 
                       backgroundColor =  styleRow(c(1,2,3,4,5,6,7),c("#008080","#444444", dt_rowShowCol, dt_rowShowCol,"#444444" , dt_rowShowCol, dt_rowShowCol)), 
                       color =  styleRow(1,"turquoise", "black"))   
},  server=FALSE)



  
Nstranded <- reactiveVal(0)
Ngoing <- reactiveVal(0)
Nsettling <- reactiveVal(0)

observeEvent(sim_pop_transloc(),{ 
  print("--------------------------------------------------- sim_pop_transloc ready! - ")
 paste0(format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")) %>% saveName_placeholder()
   sim_pop_transloc <- sim_pop_transloc() 
  req(initValues())
  initValues <- initValues()
  fam_sim <- routes0 <- NULL
  if(!is.null(sim_pop_transloc[[1]])){ 
       fam_sim <- terra::unwrap(sim_pop_transloc[[1]])  
       simModelOut(1)    
       start_sim(NULL)   
   }
  
      leafletProxy("mapReleaseSite", session) %>%  clearGroup("initial settlement test") %>%  clearGroup("routes")   %>%  clearGroup("sTest_pols")   
      greenz <- c("#C0CA33","#777700", "#8AB833","#AFB42B")
      greenzAlpha <- alpha(greenz, .4)
      greenz <-"#005959"  # c("#0086b3","#8066b2","#0a919c", "#9926bf" ,"#148a9e","#00668c" )  
      greenzAlpha <- alpha(greenz, .9)
 

      palette_fams <- colorFactor(palette =rep(greenz, length=nrow(initValues)) ,domain= as.factor(seq(1, nrow(initValues)))) # ?
      palette_famsAlpha <- colorFactor(palette =rep(greenzAlpha, length=nrow(initValues)) ,domain= as.factor(seq(1, nrow(initValues)))) # ?
      initValues$group <- as.factor(initValues$group)
       startYr_trySettl <- startYr_trySettl()

      ## including time effect for lagged releases
      initValues <- initValues[initValues$year == startYr_trySettl,]
     
      
      
     if(!is.null(sim_pop_transloc[[2]])){
         cat("routes data  - ")
         routes0 <-  sim_pop_transloc[[2]] 
     } 
     
 routes <- NULL  
 routesWander <- NULL
 Nsettling <- 0 # not settling actually just dispersing!
 pointz0 <- NULL
 
    if(!is.null(routes0)){
      routes0$id <- as.numeric(as.character(routes0$id))
      if(nrow(routes0)>1) { 
      cat("sort  - ")
      
  
     routes0 <- na.exclude(as.data.frame(routes0))
 ## remove doubles
 
 rr <- routes0 

   for (pt in 2:nrow(rr)){
    if(rr$lng[pt]==rr$lng[pt-1] & rr$lat[pt] == rr$lat[pt-1]){
     if( rr$stage[pt] %in% c("step", "next", "disperse") & rr$stage[pt-1] %in% c("step", "next", "disperse")) {rr$stage[pt] <- "delete"} #else {rr$stage[pt-1] <- "delete"} 
    } 
   }
 
 ##########################################   routes0 <- rr[rr$stage != "delete",]
#   print(head(routes0))
 
 #  routes0<-rr
  routes0 <- rr[rr$stage != "delete",]
  for(id in sort(unique(routes0$id))){
       
     route  <-  routes0 [routes0$id == id,] # & routes0$stage != "unsuitable",]#& routes0$path<10,]
    print(id)
   #  print(route)
      
      if(route$lng[nrow(route)] %in% route$lng[c(1,2)]  & route$lat[nrow(route)] %in% route$lat[c(1,2)] ){
        if(  "no depart"  %in% route$stage )  {
          
          } else {
           
             if( ! "settling"  %in% route$stage ) {
            
              if( nrow(route)>0){
                
                i=0
          while(route$lng[nrow(route)] %in% route$lng[c(1,2)]  & route$lat[nrow(route)] %in% route$lat[c(1,2)] & i<10 ){
         cat("reduce")
            i=i+1
            route <- route[1:(nrow(route)-1),] # avoid line back to init pt
          }}
                      }}}
#     print(route)
      
#  print(table(route$stage))
  if("settling" %in% route$stage) {
     route <- route[ 1:which(route$stage=="settling")[1],]
  }  
     if( nrow(route)>0){
        route$path <- 1:nrow(route)  
     if(nrow(unique(cbind(route$lng,route$lat)))>1){
        cat(" routes lines  - ")
       # Create points data
 if("settling" %in% route$stage) {settling <- "settling"} else {settling<-"notsettling"} 
        
 multipoints <- st_multipoint(matrix(c(initValues$lng[initValues$group == id],route$lng,
                                       initValues$lat[initValues$group ==id],route$lat), ncol = 2, byrow = FALSE), dim = "XY") # lat lng switch jan2
 
 points <- st_cast(st_geometry(multipoints), "POINT") 
 Nsettling <- Nsettling+1
 
# for mapping, simplify repeated visits - points  
pointz <- subset(route,select=c("lng", "lat","stage")) %>% st_as_sf(coords = c("lng", "lat"), crs = 4326, dim = "XYZ")
if(nrow(pointz)>50){
  cat("simplify  npts - ")
  
  pointz <- pointz[!duplicated(pointz$geometry),] #lala mar11
  
}
pointz0 <- rbind(pointz0,pointz ) 

 

# Number of total linestrings to be created
n <- length(points) - 1 # was -1

# Build linestrings
linestrings <- lapply(X = 1:n, FUN = function(x) { 
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line) 
}) 
# One MULTILINESTRING object with all the LINESTRINGS  
multiline  <- st_multilinestring(do.call("rbind", linestrings) ) 
multiline  <- st_sfc(multiline, crs=st_crs(4326) ) %>% st_as_sf() 
multiline$group <-  id
multiline$settling <-  settling 
routes <- rbind(routes, multiline)


linewander <- lapply(X = 1:2, FUN = function(x) { 
  pair <- st_combine(c(points[x], points[x + 1]))
  line <- st_cast(pair, "LINESTRING")
  return(line) 
}) 
# One MULTILINESTRING object with all the LINESTRINGS  
multilinewander  <- st_multilinestring(do.call("rbind", linewander) ) 
multilinewander  <- st_sfc(multilinewander, crs=st_crs(4326) ) %>% st_as_sf() 
multilinewander$group <-  id
multilinewander$settling <-  settling 
routesWander <- rbind(routesWander, multilinewander)
      }}
 }
 cat("routes ok")  


   
#   routes <- rmapshaper::ms_simplify(routes)
   if(!is.null(routes) ){
     st_geometry(routes) <- "geometry"
     names(routes)[names(routes) == "x"] <- "geometry" 
     routes$group    <-  factor(as.numeric(as.character(routes$group)), levels=initValues$group)
     routes$settling <-  factor( routes$settling, levels= c( "settling","notsettling"))
     palette_settlingYN <- colorFactor(palette =c("turquoise","turquoise" ), domain=levels(routes$settling))
    
     
     
    poplabs<- as.character( paste0( "<span style='strong'>initial settlement test</span><br>
                                 <span style='line-height:0.5em!important;font-weight:normal;margin-left:12px;'>simulated dispersal route, group ", 
                                 routes$group,"</p>")) 
    poplabs <- sprintf(  "%s", poplabs )   %>%  lapply(htmltools::HTML)  
     
            
    leafletProxy("mapReleaseSite", session) %>% 
              addPolylines(data=routes , group= "initial settlement test", color="transparent", opacity=.8, weight=6, 
                            highlight = highlightOptions( color="#ffe62cc2", bringToFront = FALSE ),
                            popup =   paste0("initial settlement test, group #",routes$group), 
                            popupOptions= popupOptions(  closeButton = FALSE ) ,
                            options = pathOptions(pane = "routes") ) %>%
             addPolylines(data=routes  , group= "initial settlement test", color=~palette_settlingYN(settling),  opacity=1, weight=2,   
                          popup =  poplabs,#  paste0("initial settlement test, simulated dispersal route group #",routes$group), 
                          labelOptions = labelOptions(  style = as.list(c("color"= "#a7ca35", "background-color" = "#777"  ,labstyle_routeLabs)) , direction = "top" ),   
                          popupOptions=popupOptions(  closeButton = FALSE ) ,
                          options = pathOptions(pane = "routes") ) %>%
             addPolylines(data=routesWander, group= "initial settlement test", color="red",  opacity=1, weight=1, dashArray=c(5,5),  
                            labelOptions = labelOptions(  style = as.list(c("color"= "wander", "background-color" = "#777"  ,labstyle_routeLabs)) , direction = "top" ), 
                            options = pathOptions(pane = "routes") ) 
    
    
    

        
    } } 

    # for download
    routes %>% datSave_settleTest() 
    removeTooltip(session, "save_settleTest")
    addTooltip(session, "save_settleTest","save_settleTest (?incl metadata?x .shp)" ,placement = "right", trigger = "hover" )
    }
 
 
  #  paste0( Nsettling," / ", length(unique(routes$group)), " groups settled!")  %>% notSettling_pars() 
    notSettling(NULL)
    fam_sim0 <- fam_sim  
    fam_sim  <- NULL 
    plotpoints_wLabs <- multipoints_noLabs <- NULL 
    

#######################  map route points if any
if(length(pointz0) >0){     
cat("stages  - ")
 markedEvents <- as.factor(c("depart","unsuitable","insufficient", "no depart","settling"))
 palette_settling <- colorFactor(palette = c("turquoise", "orangered"  ,"magenta","darkmagenta", "#a7ca35"),domain=markedEvents)  
    
#pointz0$stage <-  factor(pointz0$stage, levels= markedEvents )
    
###  map route stage points 
multipoints_noLabs <- pointz0$geometry[!pointz0$stage %in% markedEvents]  
multipoints_wLabs  <- pointz0[pointz0$stage %in% markedEvents,]
multipoints_wLabs$stage <-  factor(multipoints_wLabs$stage, levels=c( "depart","unsuitable","insufficient", "no depart","settling"))
 
     

if(length(multipoints_noLabs)>0){
  multipoints_noLabs <- multipoints_noLabs %>% st_sf() 
  st_geometry(multipoints_noLabs) <- "geometry"
  names(multipoints_noLabs)[names(multipoints_noLabs) == "x"] <- "geometry" 
  print(multipoints_noLabs) 
  
  leafletProxy("mapReleaseSite", session) %>%  
    addCircleMarkers(data=multipoints_noLabs, group= "initial settlement test", color="#d8e470", radius=4, opacity=.5  )%>%  
    addCircleMarkers(data=multipoints_noLabs, group= "initial settlement test", 
                                 color="#d8e470", radius=2, opacity=1,
                                 options = c(pathOptions(pane = "initial settlement test"), markerOptions(riseOnHover = TRUE)) ) 
}
     
pointz0$stage <-  factor(pointz0$stage, levels= markedEvents )
 
for (stage in markedEvents[markedEvents %in% pointz0$stage]){
  pts  <- pointz0[pointz0$stage == stage,]  
  indx <- which(c( "depart","unsuitable","insufficient", "no depart","settling") == stage)
  colz <- c("turquoise", "orangered"  ,"magenta","darkmagenta", "#a7ca35")[indx]
  backColz <- "#d8e470"

  leafletProxy("mapReleaseSite", session) %>% 
      addCircleMarkers(data=pts, group= "initial settlement test", color="#d8e470", radius=4, opacity=.5  )%>%  
      addCircleMarkers(data=pts, group= "initial settlement test",
                              label=~stage,
                              color= ~palette_settling(stage), radius=2, opacity=.9,
                              options =  pathOptions(pane = "initial settlement test"),  
                              labelOptions = labelOptions( style = as.list(c("color"= "#555555", "background-color" = backColz ,labstyle_routeLabs)) ,
                                                         direction = "top" ) )  
}
 
}  
            

fam_sim0pols <- NULL
#######################  map init terrs if any
if(!is.null(fam_sim0)){
  cat("add init settlmnt territories: ")
 
      fam_sim0pols       <- st_as_sf(as.polygons(fam_sim0, aggregate=TRUE, values=TRUE, na.rm=TRUE, crs="epsg:3857")) %>% st_sf() %>% st_transform(st_crs(4326)) 
      colnames(fam_sim0pols)[1] <- "group"  
      print(nrow(fam_sim0pols))
leafletProxy("mapReleaseSite", session) %>%       
                          addPolygons(data=fam_sim0pols,  group="initial settlement test",color= "darkmagenta" ,opacity=.5,
                          fillColor= "turquoise",#palette_famsAlpha(fam_sim0pols$group) ,
                          fillOpacity=.8,
                          stroke=2,
                          options = pathOptions(pane = "sTest_pols"))#  %>%  
      #        addRasterImage ( fam_sim0  , project =FALSE,  group="initial settlement test" ) %>% original dat to test proj issues 
 
}   
 

#######################  map stranded if any
 strandedId <- initValues$group
  
 if(!is.null(fam_sim0pols)){
  strandedId <- strandedId[-which( strandedId %in% fam_sim0pols$group)]
 } 
if(length(strandedId)>0) {
   cat( "stranded releases  -")
   initValues <- initValues()
   released_labs <- released_labs()
   leafletProxy("mapReleaseSite", session) %>%    
          addMarkers(data=initValues[strandedId,], ~lng,~lat ,icon=skull,   group= "initial settlement test",
              label= released_labs[strandedId], options = pathOptions(pane = "stranded" ),
              labelOptions = labelOptions( offset=c(-50,0), direction="left",                           
                                           style = as.list(c( "background-color"="black", "color" =  "#ffe62cc2" ,labstyle_plotLabs))  ) ) 
              
 
}        
Ngoing <- length(initValues$group[initValues$year ==startYr_trySettl] )
Nstranded <- length(strandedId)
Nsettling <- Ngoing - Nstranded
   txt <- as.character(paste0(  "year ", startYr_trySettl,", ",Nsettling,"/",Ngoing, " settled!" ) )
   if(Nsettling >0){
   as.character(paste0("<span style='color:yellow!important;'>",txt))  %>% model_sTest_UItxtout() 
     
   }else {
     as.character(paste0("<span style='color:#e77e68!important;'>",txt))  %>% model_sTest_UItxtout()
}
   
   
Nstranded %>% Nstranded()
Ngoing %>% Ngoing()
Nsettling %>% Nsettling()

   if(input$addMetadata_modifHab == "use current landscape") { txt1 <- "(unmodified habitat, " } else { txt1 <- "(modified habitat, "}
   if(input$addMetadata_records =="incorporate records" ) { txt2 <- "added observation records)" } else { txt2 <- "no observation record)"}
    txt <- as.character( paste0( txt1,txt2))
       # p("some output") %>% dlText_settleTest() 
   print(paste0( Nsettling,"/", length(initValues$group[initValues$year ==startYr_trySettl] ), " settled in year ", startYr_trySettl ,
                 br(),"scenario: ",br(),
                 txt1))
    

Nset <- as.character( paste0( Nsettling,"/", length(initValues$group[initValues$year ==startYr_trySettl] ), " settled in year ", startYr_trySettl)) 

       HTML(as.character( paste0(
           div(p(Nset),style="text-align:left;"),
           div(p( txt ),style="text-wrap:wrap;text-align:left;") ) )) %>% dlText_settleTest() 
                
   
 #   paste0( Nsettling," / ", length(unique(routes$group)), " settled!")  %>% notSettling_pars() 
   
#######################  reset map display 
 reset_simTriggers <- reset_simTriggers()
 (reset_simTriggers+1) %>% reset_simTriggers()
     mgmt.years(NULL) # KEEP! required to display the slider, wont disaply for init settlement test that way
     ## specific to transloc so not included in  reset_simTriggers that is combined with sim_pop output too
    
}, ignoreInit=TRUE, ignoreNULL=TRUE)
 

 


 
 
saveName_final <- reactive({ 
  if(input$save_name =="") {return(saveName_placeholder())} else {return(input$save_name)}
  })  
  
saveName_placeholder <-  reactiveVal(paste0( format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")))
 
observeEvent(saveName_placeholder(),{ # placehoder nae changes with main events eading to change save_name, unlink each time so no mess up with path when saving - nice
 #req(input$save_name =="")
  #unlink(tempdir(), recursive = TRUE)
 updateTextInput(session, 'save_name', value =saveName_placeholder()) #will take this as val only if userdoesnt amend
})



output$ui_save_settleTest <- renderUI({
  req(input$operation == "translocation")  
         return( tagList(  
               fluidRow(style="text-align:left;color:turquoise;", 
                   div('Settlement Test',style="display:inline;" ), 
                   div(downloadButton("save_settleTest", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;"),
                   fluidRow( class="input_summary_row",style="border-color:turquoise;width: fit-content;min-height:4vh;text-wrap: wrap;padding: 12px 6px",
                                div(uiOutput("dlText_settleTest"), class="hdr_simParams", style="color:turquoise;padding: min(7px, 0.9vh);")   )  ) 
         ))  
})
 
observe({
   if(input$operation == "translocation" & is.null(datSave_settleTest() )){disable("save_settleTest")
   }else{
     enable("save_settleTest")
   #  paste0("_",format(Sys.time(), "%d%b%y"),"_",format(Sys.time(), "%H%M")) %>% saveName_placeholder()
     }
})

save_metadata <- reactiveVal(0)
save_settleTest<- reactiveVal(0)
save_layout<- reactiveVal(0)
save_simpop<- reactiveVal(0)
save_all<- reactiveVal(0)

observeEvent(input$save_settleTest,{
  cat("save stest")
(save_settleTest()+1) %>% save_settleTest()
}) 
observeEvent(input$save_metadata,{ 
(save_metadata()+1) %>% save_metadata()
}) 
observeEvent(input$save_layout,{
 
(save_layout()+1) %>% save_layout()
   
}) 
observeEvent(input$save_simpop,{
(save_simpop()+1) %>% save_simpop()
})

 

  
   
 
  
 
 
output$DTobsRecs_famDf <-  DT::renderDT({ #fifi
   
   req(obsRecs_famDf_display()) 
  cat("obsRecs demog data table  - ")
 # print(obsRecs_famDf_display()) #init null
  dat <- as.data.frame(obsRecs_famDf_display() )
   cat(paste0(nrow(dat), " obs  - "))
   req(nrow(dat)>0)  
  dat <- dat[,-(which(colnames(dat) %in% c("LeafletId","qual") ))]
      colNames <- c("last seen","group", "females", "males", "young")
      colDisab <- 0
   
   datatable(  dat  , 
      colnames=colNames   ,   
      rownames = FALSE, escape=FALSE, selection = "none",
      editable = TRUE,#list(target = "cell"),#disable = list(columns =  colDisab)) ,
      options = list(dom = 't', ordering=F, pageLength=-1,
                      columnDefs = list( list(className = "dt-center", targets = c(1:(ncol(dat)-1) ))    , 
                              list(className = "notselectable", targets = colDisab),
                              list(className ='dt-grey', targets =  colDisab ),
                              list(className ='dt-head-grey', targets =  colDisab )   )
      ))          
 }  , server = FALSE)
 

  # bsTooltip("DTobsRecs_famDf","DTobsRecs_famDf",placement = "top", trigger = "hover")      
   
output$obsPt_famui <- renderUI({  
  return(  tagList(
            div("observed territories input", style="text-align:center;padding-top: 27px 0;font-size: 155%;"),  
            div(p("associated demographics can be informed in the table")  ,style="padding-bottom: 27px ;height:17px;text-align:center;color:magenta;font-size:90%;"),

                  div(DTOutput("DTobsRecs_famDf"),
      style="line-height: 90%;font-size: 90%;width: 80%; height: auto; left: 7%; bottom: 8px; top: 8px; max-height: 14vh; position: relative; text-align: center; overflow-y: auto;") 
      )
) 
})
obsRecs_new <- reactiveVal(0) 
#add_labelOnly
#obsRecs_new


observeEvent(obsRecs_new(),{ # trigger post plotting
  
#  disable_tableEdits <- disable_tableEdits()
 #if(  disable_tableEdits == FALSE) {  
   req(pointFeature())
  cat("\n---- update obsRecs_famDf   - ")
   obsRecs_famDf <- obsRecs_famDf()  
     add_labelOnly <- add_labelOnly() 
          pointFeature <- pointFeature()
   #       print(pointFeature)
          # if triggered by deletig pt in obs df, just remove row, add_labelOnly will be null
          # but also - obsRecs_famDf may be non null at first when pts uploading but vals for add_labelOnly() are ready for processing so need conditioning
          # otherwise runs twice -families duplicated
     if(!is.null(obsRecs_famDf)) {  
      if(!is.null(pointFeature)){   
       if(nrow(pointFeature)>0){
         cat("clean  - ")
         obsRecs_famDf <- unique(obsRecs_famDf[obsRecs_famDf$LeafletId %in% pointFeature$LeafletId,])
       }else {
         obsRecs_famDf<-NULL}
         }else {
         obsRecs_famDf<-NULL
         }
       # obsRecs_famDf %>% obsRecs_famDf()
        } # clean 
   #         obsRecs_famDf %>% obsRecs_famDf()

   if(base::isFALSE(add_labelOnly)) { # new point
     cat("new point  - ")
   #  print(obsRecs_famDf)
       numFeat <-  num.f <-   num.m <- 1
      if(is.list(newCells3857())) { numFeat <- length(newCells3857())} # if uploaded points, a list
      if(input$use_demoginDat==TRUE & !is.null(newuploadFeature_femColumn())){
               num.f <-  as.numeric(as.character(newuploadFeature_femColumn()))[1:numFeat]
               num.m <-  as.numeric(as.character(newuploadFeature_malColumn()))[1:numFeat] } # demog from shp
     
      newFams <- data.frame(LeafletId=pointFeature$LeafletId[1:numFeat], # by using length(newCells3857()) making new row for each currently processed points(may be many)
                          year=pointFeature$start[1:numFeat], 
                          fam.id=pointFeature$name[1:numFeat],
                          num.m= num.m,
                          num.f= num.f,
                           young=3 ,qual=20)
      newFams$young <- rpois(nrow(newFams), litter.size)+rpois(nrow(newFams), litter.size)
      obsRecs_famDf <-  rbind( newFams,obsRecs_famDf)  
      obsRecs_famDf   %>% obsRecs_famDf()
      }
     
   if(base::isTRUE(add_labelOnly)) { # new labs existing point
     cat("same points, new labs  - ")
      obsRecs_famDf$LeafletId   <- pointFeature$LeafletId 
      obsRecs_famDf$year   <- pointFeature$start 
      obsRecs_famDf$fam.id <- pointFeature$name 
      obsRecs_famDf  %>% obsRecs_famDf() 
   # add_labelOnly(FALSE)
     }
 #    add_labelOnly(NULL) # to rest if consecutives FF or TT # add_labelOnly(NULL) already if deleting points
}, ignoreInit=TRUE)

#observe({
 #  print("disable_tableEdits:")
#   print(disable_tableEdits())
#    print(unlist(input$DTmetaTable_records_cell_clicked))
#   print("---------------fam disp")
#    print(obsRecs_famDf_display())
#   print("---------------fam df")
 #  print(obsRecs_famDf()) 
#})
 

observeEvent(input$DTmetaTable_records_cell_clicked, { #fifi triggered also hwne table becomes null
  cat("\nrec table click  - ")
#  print(obsRecs_famDf_display())
 # print(obsRecs_famDf()) 
#  obsRecs_famDf_display <- obsRecs_famDf_display()
#  unique(obsRecs_famDf_display) %>% obsRecs_famDf_display()
  disable_tableEdits <- disable_tableEdits()
 if(  disable_tableEdits == FALSE) {  
  req(!is.null(unlist(input$DTmetaTable_records_cell_clicked)))
  click <-  input$DTmetaTable_records_cell_clicked 
  req(click$col!=7) # delete button column
            req(obsRecs_famDf())
            obsRecs_famDf <- obsRecs_famDf()
            obsRecs_famDf %>% obsRecs_famDf_display()
            cat("obs table click  - ") 
               showModal(modalDialog( size = "m", 
                     style="display: flex;flex-direction: column;align-content: center;flex-wrap:wrap;",
               uiOutput("obsPt_famui"), 
               br(),
                    footer=tagList(
                      div(actionButton(inputId="obsRecs_submit",label="submit"),style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;")
                      ) 
             
    ))
 } else { 
  error_message1 <-  "simulation in progress: protected input"  
   error_message2 <-  "new geometries can be added but metadata used in the simulation should not be amended" 
    notify_warning(error_message1,timeout=4000,   
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)", className="lower_notifs_dbl",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif )) 
   notify_info(error_message2,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",   
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
   
   }
}, ignoreInit=TRUE, ignoreNULL=TRUE) 

#   bsTooltip("obsRecs_submit","obsRecs_submit",placement = "bottom", trigger = "hover")

observeEvent(input$obsRecs_submit,{ #booboo
cat("submit obsRecs - ")   
   removeModal()  
   obsRecs_famDf  <- obsRecs_famDf()  
   obsRecs_famDf  %>% obsRecs_famDf_display()  
  ######## to do here - input this info into df for simulation.. getting very detailed in options here..
}, ignoreNULL=TRUE)  
  

observeEvent(input$DTobsRecs_famDf_cell_edit, {
 cat("edit obsRecs  - ")  
      ro <-  input$DTobsRecs_famDf_cell_edit$row 
      col <- input$DTobsRecs_famDf_cell_edit$col
      val <- input$DTobsRecs_famDf_cell_edit$value
      
      if(col<2){
              pointFeature <- pointFeature()
              if(col==0) {pointFeature$start[ro] <- val} 
              if(col==1) {pointFeature$name[ro] <- val}  
              pointFeature %>% pointFeature()
              gather_metadata(1) 
            #  add_labelOnly(TRUE) # i.e just update geom labs, dont replot raster
              "observation records" %>% featureName() # could do by layer for replotting smaller subset
      }  else { # update demog fam tab
       obsRecs_famDf <- obsRecs_famDf()
       if(val <0) {val <- 0}
           if(col==2) {obsRecs_famDf$num.m[ro] <- val}
           if(col==3) {obsRecs_famDf$num.f[ro] <- val}
           if(col==4) {obsRecs_famDf$young[ro] <- val}
      obsRecs_famDf %>%  obsRecs_famDf() }
      add_labelOnly(TRUE)
})
     
   

fam_sim_filtered <- reactiveVal() ## note save a version of sim_out$fam_sim for download as becomes null when new sim triggered and prev output still visible


 # list(simpop_shp=sim_out$fam_sim, #datSave_sim_pop$simpop_shp use to map  ? # set to null at start of sim
  #    recPlot_summN_gg=recap_plot_Summ,
   #     recPlot_summFams_gg=recapSummPlot_fams,
    #  recTable_annual_stats = annual_stats) %>% datSave_sim_pop()
        

simNonNull <- reactiveVal(0)
observeEvent(datSave_sim_pop(),{ #sim_out$fam_sim,{ 
 simNonNull(0)
  leafletProxy("mapReleaseSite", session)  %>% removeControl("simulated occupancy legend") %>% clearGroup("simulated occupancy")
  req(datSave_sim_pop())
    req(sim_out$fam_sim) # not null at first and only runs once - prevents running when no output for territories as will be NULL but other outputs are not (tabs full of zeros)
  simNonNull(1)
  cat("new pop to map!  - ")   
   mgmt.reps <- mgmt.reps() 
   mgmt.reps <- as.numeric(as.character(mgmt.reps))  
   datSave_sim_pop <- datSave_sim_pop() 
     minyear <- min(datSave_sim_pop$recTable_annual_stats$year)
     maxyear <- max(datSave_sim_pop$recTable_annual_stats$year)
     length(unique(datSave_sim_pop$recTable_annual_stats$year)) %>% currentOutput_year() #stores value for mgmt.years that is also used as trigger for undo_settl

     sliderParams$min <- minyear 
     sliderParams$max <- maxyear
     sliderParams$value <- maxyear
     
     outColSelected(2) # triggers hilights
     
    req(input$show_simYrs)
    proximToTable(NULL)
       if(input$show_simYrs == sliderParams$value) { 
       sliderParams$value <- minyear }  # vary to trigger if same year as currently visualising? bu req() as no input to start with
       refresh_legendTxt(refresh_legendTxt()+1)
 
 }, ignoreNULL=FALSE)


sliderParams <- reactiveValues(min=0, max = 2, value = 1)
 
output$uishow_simYrs <- renderUI({
  req(simNonNull()==1)
  req(sliderParams)
  return(  tagList(  div(  sliderInput ( "show_simYrs" , label = NULL,min = sliderParams$min, max = sliderParams$max, value = sliderParams$value, step = 1,
                                 sep=""), 
                                       class="outputSlider",
                                       style="position: absolute;text-align:center;color: #ffe62cc2;margin:0; width: -webkit-fill-available;top:1.4vh;") ,
                    bsTooltip("show_simYrs" ,"display simulation output for selected year",placement = "bottom", trigger = "hover" ) 
  ))
}) 
   
output$uishow_simYrsFigs <- renderUI({
  req(colWidthRecapYrs())
  wdth <- colWidthRecapYrs()
 print(paste0("width is ",wdth))
 
 styleTab <-   as.character(paste0( 
      "color: #ffe62cc2;margin:0;top: 2vh; margin-left: ", wdth,";margin-right:2vw;")) 
  
   return(    div(  sliderInput ( "show_simYrsFigs" , label = NULL,min = sliderParams$min, max = sliderParams$max, value = sliderParams$value, step = 1,
                                  sep="", width = '100%'), 
                                        class="outputSlider", class="outputSlider2", 
                                       style= styleTab  ) 
   )
}) 

 
filteredData <- reactive({
    req(mapExists()==TRUE )
    req(!is.null(datSave_sim_pop())) 
    req(input$show_simYrs>10)
req(input$show_simYrs %in% datSave_sim_pop()$simpop_shp$year) 
  return(datSave_sim_pop()$simpop_shp %>% filter(year == input$show_simYrs))
 })

observe({ 
#    req(mapExists()==TRUE )
#    req(!is.null(datSave_sim_pop()))
#    cat("map pop  - ") 
#    req(input$show_simYrs>10)
    palette_Nruns <- palette_Nruns()
#    palette_dns <- palette_dens() 

#   leafletProxy("mapReleaseSite", session) %>% clearGroup("simulated occupancy") 
# req(input$show_simYrs %in% datSave_sim_pop()$simpop_shp$year) 
# req(filteredData())
  leafletProxy("mapReleaseSite", session) %>% clearGroup("simulated occupancy") %>%
                 addPolygons(data=filteredData(), #datSave_sim_pop()$simpop_shp %>% filter(year == input$show_simYrs), 
                          fillColor= ~palette_Nruns(Nruns),  color= "black",
                          fillOpacity=.8, weight=1,opacity=1,  
                          popup= ~labs_sim,
                          popupOptions = popupOptions(  style = as.list(c("color"="#555", labstyle_plotLabs)) , direction = "top" ), 
                          group="simulated occupancy",
                          options = pathOptions(pane = "simulated occupancy"))   
})

observeEvent(filteredData(),{
  filteredData <- filteredData()
  req(is.null(filteredData))
    leafletProxy("mapReleaseSite", session) %>% clearGroup("simulated occupancy") 
},ignoreNULL=FALSE)
 

##################################### undos - erase current output for  sim
observeEvent(undo_sim(),{
   if(is.null(sim_pop())){ # dont allow click when null otherwise will remove new output as soon as produced
   cat("no output to remove")
  } else {
    cat("undo_sim - ")
     proximToTable(NULL)
   show_simOutputNULL(NULL)
    sim_pop(NULL)       # null when sim is running
   # allow_update_sim(1) # null when sim is running
  sim_out$fam_sim <- NULL 
  leafletProxy("mapReleaseSite", session) %>%  clearGroup("simulated occupancy") %>% removeControl("simulated occupancy legend") 
  fam_sim_filtered(NULL)
  simOutput(0) # allows showing plots 
  recapTable(NULL)
  ganttdf_hili(NULL)
  ganttdf(NULL)
  simModelOut(0) # to remove slider in top navbar ui progress box
  datSave_sim_pop(NULL) # remove panel slider output
  outDat_areaOfInterest(NULL)
  }
}, ignoreInit=TRUE)  

trigger_undo_sim <- reactiveVal(0)
trigger_undo_settling <- reactiveVal(0)
 undo_settling <- reactive({c(input$undo_settling, trigger_undo_settling())})
 undo_sim <- reactive({c(input$undo_sim,trigger_undo_sim()) })
 
##################################### undos - erase current output for settltmt 
observeEvent(undo_settling(),{ #input$undo_settling,{
 cat("\nundo_settling: ")
  if(is.null(sim_pop_transloc())){ # dont allow click when null otherwise will remove new output as soon as produced
  cat("no output to remove  - ")
  } else {
     notify_success("settlement test output removed",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  

 #   layers_list <- addLayersCont_list (cur_layers=cur_layers(),new_layer=c( "initial settlement test"),type="clear")  
  #  layers_list %>% cur_layers()
    leafletProxy("mapReleaseSite", session) %>%  clearGroup("initial settlement test") %>% removeControl("initial settlement test")  %>% clearGroup("routes") 
     Nsettled_txt(NULL)
    notSettling(NULL)
    notSettling_pars(NULL)
    sim_pop_transloc(NULL)
    proximToTable(NULL)
    datSave_settleTest(NULL)
    req(!is.null(initValues()) & input$operation=="translocation") # only replot translocs if operation is translocation - could be was triggered when switching operation
    counter_initPopChange(counter_initPopChange()+1)   # trigger replot to get original beaver icons at release locs..
      cat("output removed  - ")
} 
  # note: no similar process with sim_pop as not keeping visualisation while runing test - no point
}, ignoreInit=TRUE)  




##################################### reset map display post sim

observeEvent(reset_simTriggers(),{
  req(reset_simTriggers()>0)
  
     if(base::isTRUE(playSound())) {
     insertUI(selector = "#placeholder_timing",
                 where = "afterEnd",
                # beep.wav should be in /www of the shiny app
                 ui = tags$audio(src = "beep-done1.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")  )
   updateTabsetPanel(session,"navBar",selected = "spatial layout")  
     }
   notify_success("new output ready!" ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
 cat("----  reset map display  - ")
     model_running(FALSE) 
     enable("start_sim") 
          if(!is.null(sim_pop())) {enable("undo_sim") } 
     enable("try_settling") 
     if(!is.null(sim_pop_transloc())) {enable("undo_settling") } else {disable("undo_settling") }
     enable("lock_pts")
     enable("lock_site")
     enable("startYr_trySettl") 
     enable("timing")
     enable("demog")   
     enable("sim_Nyrs") 
     enable("sim_Nreps") 
     enable("operation") 
     disable_tableEdits(FALSE)
     simModelOut(1)    
     start_sim(NULL)
})  

 
 
   


##################################### show current values for reatives in app UI - remove for deploy
output$inputValues_text <-  renderPrint({reactiveValuesToList(input)})
inputValues_text <- reactive(input)
output$text <-  renderPrint({reactiveValuesToList(input)})
  
  
 


##################################### info text - point sampling methods
txt_pt_method <- reactive({  
    if(is.null(exploSite())){return(paste0("No release point located yet - the exploratory site must be defined first."))
      } else {
      if(base::isTRUE(click_Pts_disabled())) { return(NULL)
        } else{
              if(input$relPts_method == "manual_entry")           {txt <-   paste0("manual entry.",br(),"Point locations are informed manually: amend the coordinates in the table to specify each release location.") }
              if(input$relPts_method == "random_location_across") {txt <-   paste0("automated.",br(),"Coordinates are generated automatically: click on the button to generate a set of potential release points within habitat suitable for settlement.")  }
              if(input$relPts_method == "each_pt_on_map")         {txt <-   paste0("click on map.",br(),"Point locations are determined visually: click on the map at each release point location.")  }
    
          return( paste0(br(), "Selected method: ",txt))
          }}
})
 
   
         

##################################### user edits translocation data tables
 
observeEvent(input$timingTable_cell_edit, {
  model_running <- model_running()
  cat("timing table edit   - ") 
  if(model_running== FALSE) {
     row  <- input$timingTable_cell_edit$row
     rv_dis$timing$year[ which(rv_dis$timing$group == row) ] <- input$timingTable_cell_edit$value  
 } else { 
   initValues <- initValues()
   print(initValues)
   rv_dis$timing$year <- initValues$year 
   cat("no amending now model running  - ")
notify_warning("parameters should not be amended while the simulation is running" ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  } 
})
   



 
observeEvent(input$coordsTable_edit_cell_edit, { 
  cat("coords table user edit  - ")  
  if(base::isFALSE(click_Pts_disabled()) & input$relPts_method == "manual_entry"){
    click <- input$coordsTable_edit_cell_edit
     row  <- click$row
     clmn <- click$col +1 # as omitted first col rownames
     x <- round(rv_dis$pts_bng$X[row],1)
     y <- round(rv_dis$pts_bng$Y[row],1)
     cat("update value in display table  - ")
            if(clmn==2) {
              x <- as.numeric(click$value)
              rv_dis$pts_bng$X[row] <- x} 
            if(clmn==3) {
              y <- as.numeric(click$value)
              rv_dis$pts_bng$Y[row] <- y} 
    newpt_bng <- st_as_sf(data.frame(lng=x , lat=y),   coords = c("lng","lat"), crs=st_crs(27700)) 
    newpt_ll_coords <- st_coordinates(st_transform(newpt_bng , crs = st_crs(4326)))  
    newpts_ll_coords <- rv_temp$pts_ll  # incorporate change to release pts df
        if(clmn==2) {newpts_ll_coords$lng[row] <-   as.numeric(newpt_ll_coords[,1])  }  
        if(clmn==3) {newpts_ll_coords$lat[row] <-   as.numeric(newpt_ll_coords[,2])  } 
    rv_temp$newpts_ll_coords <- newpts_ll_coords 
 }
})

 
## clean up issue: when deleting all obs recs sim output should e cleared too!

   
 
 
observeEvent(input$mapReleaseSite_marker_click$id,{  ### unselect in table and plot if highlighted marker is clicked
   print(paste0("click on released group # ",input$mapReleaseSite_marker_click$id))
   markerClick_id  <- input$mapReleaseSite_marker_click$id
   if(is.numeric(markerClick_id )){
             markerClick_id  %>% markerClick_id_hilight()
             dfrows_selected <-   dfrows_selected()
             dfrows_selected[-which(dfrows_selected == markerClick_id)] %>% dfrows_selected() 
  }
}) 
 
  
  

 
################################################### assess new mapReleaseSite_shape_click
 
 
observeEvent(input$mapReleaseSite_click,{
   cat("clicked  - ") 
 # req(base::isFALSE(model_running()))
  req(input$mapReleaseSite_click$lat == input$mapReleaseSite_shape_click$lat)      ## because: _click anywhere out country doesnt update the _shape click - it just keeps the last value + last coords
  cat("listen  - ")
  markerClick_id_hilight <- markerClick_id_hilight() 
  if(base::isFALSE(click_Site_disabled()) ) { # resrict area to selected site
           cat("new site location - ") 
           exploSite(NULL)  
           input$mapReleaseSite_click %>% input_mapReleaseSite_click()
  }  
  if(base::isTRUE(click_Site_disabled()) & base::isFALSE(click_Pts_disabled()) & is.null(markerClick_id_hilight) )  {  # so wont register click if on highlighted transloc group on map..
           cat("pts unlocked - ") 
           input$mapReleaseSite_click %>% input_mapReleaseSite_click()
  }
 markerClick_id_hilight(NULL) # reset bcse the group id remains same until changed by click - never null    
}, ignoreNULL=TRUE) 
  
input_mapReleaseSite_click_group <- reactiveVal(NULL)

observeEvent(input$mapReleaseSite_shape_click$group,{
  cat("clicked group  - ")
 input$mapReleaseSite_shape_click$group %>% input_mapReleaseSite_click_group()
 
if(input$shape == "none"){
  sitedat <- sitedat()
  sitedat$click_lat <- input$mapReleaseSite_click$lat
  sitedat$click_lng <- input$mapReleaseSite_click$lng
  sitedat$shape <- input$shape  
  sitedat$buffer <- as.numeric(input$buffer_sel) 
  sitedat %>% sitedat_temp()
 #req(intercatch_on_map()) 
}
})             
sitedat_temp <- reactiveVal(NULL)

observe({
  req(mapExists()==TRUE)
  cat(paste0("\ndisabled clicks: catch=", click_catch_disabled(),", site=",click_Site_disabled(),", points=", click_Pts_disabled(),"  - "))
})
 


 observeEvent(input_mapReleaseSite_click_group(),{
 cat(paste0("\nclicked on :", input_mapReleaseSite_click_group(),"  layer - "))
  req(input_mapReleaseSite_click_group()) 
  req(input_mapReleaseSite_click()) # june10
  req(intercatch_on_map())
  cat("check  pt- ")
    if(base::isFALSE(click_catch_disabled())) {
  if(input$shape == "catchment") {
     click_group <- input_mapReleaseSite_click_group()
  if(click_group %in% c("selected site", "catchments features","river catchments all")){ 
                  click_Pts_disabled(TRUE)
                  selected_catchments_names <- selected_catchments_names()
               #  browser() 
                  temp_pt <- st_sfc(st_point(c(  input_mapReleaseSite_click()$lng, input_mapReleaseSite_click()$lat) ), crs = st_crs(4326)) 
                  new_name <- intercatch_on_map()$WB_NAME[lengths(st_intersects(intercatch_on_map(), temp_pt)%>%suppressMessages())>0]
                  cat(paste0("catchment: ",new_name,"  - "))  
                  if(length(new_name)==0) { # clicked outside
                         rv_init$infotext_instructions_pointout <- paste0(hr(),"Click located outside exploratory site boundaries - not registered.") 
                         cat("not registered  - ")
             notify_warning("click whithin the exploratory site boundaries" ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
                 } else {
                         
                         rv_init$infotext_instructions_pointout <- NULL
                         if(new_name  %in% selected_catchments_names) { 
                            cat("removed  - ") 
                           selected_catchments_names <- selected_catchments_names[-which(selected_catchments_names == new_name)]  
                         } else { 
                         cat("added  - ") 
                         selected_catchments_names <- c(new_name , selected_catchments_names)
                         }
                 } 
   selected_catchments_names %>% selected_catchments_names()
   input_mapReleaseSite_click(NULL) ###? needed to allow catch selection now? wtf - removed apr16 # and again may27 as prevents updating display of btns
  } else {   # if click outside of catch geoms
         click_Pts_disabled(FALSE)
         cat("out of range  - ")
    ## move rel site here  
          }
  }}
  input_mapReleaseSite_click_group(NULL)
}, ignoreNULL=TRUE, ignoreInit=FALSE)
 

txt_site_selected <- reactiveVal(NULL)
observe({#Event(input$mapReleaseSite_shape_click$lat,{
    req(mapExists()==TRUE)
    cat("\nhelpBox txt  - ")
    rv_init$infotext_instructions_siteSel <- txt_site_selected()
})#, ignoreNULL=TRUE,ignoreInit=FALSE )   
 


############################################################ map highlights
   
observeEvent({
   dfrows_selected()
   rv_temp$addMapHilights # re add after replot?
  1
  },{
      req(!is.null(rv_temp$pts_ll))  
     leafletProxy("mapReleaseSite", session) %>%  clearGroup("table_hilight") 
      rv_temp$addMapHilights <- NULL      
     ptdf <- NULL 
      if(length(dfrows_selected())>0){
         cat(c("\n highlight ",dfrows_selected(), "  - ")) 
          ptdf <- na.exclude(as.data.frame(rv_temp$pts_ll)[dfrows_selected(),])    
     }  
     ptdf %>% ptdf()  
 }, ignoreNULL=FALSE, ignoreInit=TRUE)

observeEvent(ptdf(),{      
     cat("add  - ") 
     leafletProxy("mapReleaseSite", session) %>%   
               addCircleMarkers(data=ptdf(),~lng,~lat, group="table_hilight",   fillColor="#ffe62cc2", fillOpacity=.8, opacity=0,  radius=40,
                                 layerId = ptdf()$group,
                                 options = pathOptions(pane = "highlights") )   
      ptdf(NULL)
}, ignoreNULL=TRUE, ignoreInit=TRUE)
 
observeEvent(input$timingTable_rows_selected ,{ # hilight points time table 
  cat("hili timing table  - ")
  ro <- input$timingTable_rows_selected
  dfrows_selected <- c(ro, dfrows_selected() )
   dfrows_selected[!dfrows_selected %in% dfrows_selected[duplicated(dfrows_selected)]] %>% dfrows_selected()
  },ignoreInit=TRUE, ignoreNULL=TRUE) 

observeEvent(input$famTable_rows_selected ,{
  cat("hili fam table  - ")
  ro <- input$famTable_rows_selected
  dfrows_selected <- c(ro, dfrows_selected() )
  dfrows_selected[!dfrows_selected %in% dfrows_selected[duplicated(dfrows_selected)]] %>% dfrows_selected()
  },ignoreInit=TRUE, ignoreNULL=TRUE) 
 
observeEvent(input$coordsTable_edit_rows_selected ,{ 
  cat("hili coords table  e- ")
  ro <- input$coordsTable_edit_rows_selected
  dfrows_selected <- c(ro, dfrows_selected() )
  dfrows_selected[!dfrows_selected %in% dfrows_selected[duplicated(dfrows_selected)]] %>% dfrows_selected()
  },ignoreInit=TRUE, ignoreNULL=TRUE) 

  
observeEvent(input$coordsTable_noedit_rows_selected ,{ 
  print("hili coords table  noe- ")
   ro <- input$coordsTable_noedit_rows_selected
   dfrows_selected <- c(ro, dfrows_selected() )
   dfrows_selected[!dfrows_selected %in% dfrows_selected[duplicated(dfrows_selected)]] %>% dfrows_selected()
   print("highlights coords table  noe2- ")
  },ignoreInit=TRUE, ignoreNULL=TRUE) 


      
    
    

observeEvent(input$buffer_sel,{   #  not a pure observer so that I can partition the app in stages in use init values here for those params
    cat(c(" buffer: ",input$buffer_sel,"  - "))
    sitedat<- sitedat()
    sitedat$buffer_val <- as.numeric(input$buffer_sel) 
    sitedat %>% sitedat() 
}, ignoreInit=TRUE, ignoreNULL=TRUE)


 
               

 
observeEvent(input$shape,{  
  req(mapExists()==TRUE)
  cat(c("\n---- changing site shape to ",input$shape))
  if(input$shape == "none") {
    infotext_instructions_shape(NULL)
    disable("buffer_sel")
    sitedat<- sitedat()
    sitedat$shape  <- input$shape
    sitedat$click_lat <- NA # resets site and maps accordingly - not NULL sa removes value from reactVlas entirely..
    sitedat %>% sitedat()
     leafletProxy("mapReleaseSite", session) %>%clearGroup("river catchments all") %>%  clearGroup("table_hilight")  %>% removeControl("river catchments all")  %>%  clearGroup("catchments features")

    } else {
      sitedat<- sitedat()
    sitedat$shape  <- input$shape
    sitedat %>% sitedat()
    enable("buffer_sel")
     rv_init$infotext_releaseLoc <- NULL # resets description
     click_Site_disabled(FALSE) # ??
    }
}, ignoreInit=FALSE, ignoreNULL=TRUE)
 
  
observeEvent(input_mapReleaseSite_click(),{ # change ocation of candidate site when clicking OR locate release points depending on release pts  mode
    cat("new point registered  - ")
    rv_init$infotext_instructions_pointout <- NULL
     rv_init$infotext_simError <- NULL 
         temp_pt <- st_sfc(st_point(c(  input_mapReleaseSite_click()$lng, input_mapReleaseSite_click()$lat) ), crs = st_crs(4326))   
        temp_coords <- st_coordinates(temp_pt)
    
        
         
        
         ###### if exploratory site was not locked:  look into what it means reg site location and shape:  
         if( base::isFALSE(click_Site_disabled()) ) { 
                        # if no poly  - click is new site, generate rel pts
                            if( is.null(exploSite())   ) {  
                                             cat("no poly: new site  - ")
                                              selected_catchments_names(NULL)
                                              list(  click_lat=  input_mapReleaseSite_click()$lat , click_lng=  input_mapReleaseSite_click()$lng ,
                                                                            buffer_val =  as.numeric(input$buffer_sel), shape= input$shape ) %>% sitedat() 
                              }
                             
                            
                        # if poly exists + manual + outside poly - new site
                          if( !is.null(exploSite()) ) {
                                            cat("poly exists  - ")  
                                                            if(lengths(st_intersects(temp_pt,exploSite())%>%suppressMessages())==0) { # if clicked outside existing shape in manual- click is new site
                                                                 cat("out - ")
                                                               if( base::isTRUE(click_Site_disabled()) ) { 
                                                                 cat("site locked no new pt  - ")
                                                                 rv_init$infotext_instructions_pointout <- paste0(hr(),"Location is outside site boundaries - click not registered.") 
                                                                 notify_warning("locate click whithin the exploratory site boundaries" ,timeout=5100,  
                                                                         config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                                                                                       fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", # note: not pretty but cant add those css to css class, has to be inline..
                                                                                       background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
                                                                 
                                                               }
                                                                 if(base::isFALSE(click_Site_disabled())) { # resrict area to selected site
                                                                 cat(" new site  - ") 
                                               if(input$relPts_method != "random_location_across" ) {
                                                                cat("manual or click  - ") 
                                                                selected_catchments_names(NULL)
                                                                 list(  click_lat=  input_mapReleaseSite_click()$lat , click_lng=  input_mapReleaseSite_click()$lng ,
                                                                buffer_val = as.numeric(input$buffer_sel), shape= input$shape ) %>% sitedat() }
                                                   
                                            # if  in random mode - click is new site, generate rel pts
                                              if( input$shape == "buffer" & input$relPts_method == "random_location_across"   ) {  
                                                               cat("random / new site  - ")
                                                                list(  click_lat=  input_mapReleaseSite_click()$lat , click_lng=  input_mapReleaseSite_click()$lng ,
                                                                                          buffer_val = as.numeric(input$buffer_sel), shape= input$shape ) %>% sitedat()  } 
                                                                   }}
                          }
                                   
                                 }  # run if site not locked only

         if( base::isTRUE(click_Site_disabled()) ) {   
           ###### if exploratory site was locked:  look into what it means reg points: 
           # if poly exists + manual + inside poly - keep site, new pts via clicking:
            if( !is.null(exploSite()) ) { 
                  cat("poly exists  + site locked - ")
                if(input$relPts_method == "each_pt_on_map" ) {
                      cat("manual or click   - ")     
                      if(lengths(st_intersects(temp_pt,exploSite())%>%suppressMessages())>0) { 
                      cat("within site  - ")  
                         
                              #  if(input$relPts_method == "each_pt_on_map" ) {
                                cat("pt click - ") 
                                      counter_clickPts <- rv_temp$clickCount_pts +1 
                                       if(counter_clickPts < (input$Nfams_init+1) ) {
                                         cat("fewer clicks than locs wanted  - ")
                                         if(is.null(initValues())){
                                           cat("no initVals: build df  - ")
                                                    rv_temp$validatedPts <- seq(1:counter_clickPts)
                                                    rv_temp$clickCount_pts <-  counter_clickPts} 
                                         if(!is.null(initValues())){
                                           cat("init val df exists - select which group coords to replace")
                                                    rv_temp$validatedPts <- which(!is.na(initValues()[,2]))
                                                    rv_temp$clickCount_pts <-  counter_clickPts
                                                    cat("keep incrementing trigger  - ")
                                                                    if( length(rv_temp$validatedPts)==0) {
                                                                      cat("no group left - make init vals NULL and trigger =1")
                                                                              initValues(NULL)
                                                                              rv_temp$clickCount_pts <-  1
                                                                              }
                                                                    }
                                                    } else {
                                                      cat("start new coords df ")
                                                    rv_temp$validatedPts <- 1#roo
                                                    rv_temp$clickCount_pts <- 1
                                                    initValues(NULL)
                                                    cat("restart counter  - ") 
                                        } #restart counter 
                                 
                                    cat("count clicks  - ")
                                #}   
                                       p2x <-  as.numeric(temp_coords[2]) # input$mapReleaseSite_shape_click$lat)  
                                       p2y <-  as.numeric(temp_coords[1]) # input$mapReleaseSite_shape_click$lng)  
                                       
                                         
                                       
                                     if(is.null(initValues() )){
                                       cat("no init vals") 
                                          
                                  #     isolate({
                                         cat("isolate  - ")
                                              val$clickx =    c( p2x,  val$clickx  )  [1:rv_temp$clickCount_pts] 
                                              val$clicky =    c( p2y,  val$clicky )   [1:rv_temp$clickCount_pts]    
                                              if(rv_temp$clickCount_pts>input$Nfams_init) { cat("wrong /  more pts than fams  - ")  } 
                                      #       })     
                                     #jan11#  # rv_sim$newpts_ll   <-   newpts_ll  <- st_as_sf(data.frame(  lng= val$clicky[!is.na(val$clickx)]  , lat=val$clickx[!is.na(val$clickx)] ),   coords = c("lng","lat"), crs=st_crs(4326)) 
                                   
                                  #    temp_pt <- st_sfc(st_point(c(adjusted_coords[1], adjusted_coords[2]) ), crs = st_crs(4326))   
             
                                   newpts_ll  <-   st_as_sf(data.frame(  lng= val$clicky[!is.na(val$clickx)]  , lat=val$clickx[!is.na(val$clickx)] ),   coords = c("lng","lat"), crs=st_crs(4326)) 
                                   newpts_BNG   <- st_transform( newpts_ll  , crs=st_crs(27700))  
                                   newpts_BNG_coords   <- round(as.data.frame( st_coordinates(  newpts_BNG  )))
                                    rv_dis$pts_bng <-  cbind(group=seq(1,nrow(newpts_BNG_coords)),newpts_BNG_coords )
                                   rv_sim$newpts_ll   <-    st_as_sf(data.frame(  lng= newpts_BNG_coords[,1] , lat=newpts_BNG_coords[,2] ),   coords = c("lng","lat"), crs=st_crs(27700)) %>% st_transform(  crs=st_crs(4326))
                                   
                                 
                                 
                                     rv_temp$newpts_ll_coords  <-  as.numeric( st_coordinates(rv_sim$newpts_ll ))  # this replaces rel pts after 

                                  }
                                    if(!is.null(initValues() )){
                                      cat("initvals exist   -")
                                             replacerow <- which(is.na(initValues()[,2]))[1]
                                               replacerow %>% replacerow() 
                                             val$clickx[replacerow] <- p2x
                                             val$clicky[replacerow] <- p2y  
                                             
                                          rv_sim$newpts_ll [replacerow,] <- newpt_ll <- st_as_sf(data.frame(  lng= p2y  , lat=p2x ),   coords = c("lng","lat"), crs=st_crs(4326)) 
                                          rv_temp$newpts_ll_coords  [replacerow,c(2,3)] <-  as.numeric( st_coordinates( newpt_ll ))  # this replaces rel pts after 
                                          newpt_BNG   <- st_transform( newpt_ll  , crs=st_crs(27700))       
                                        #   newpt_BNG_coords   <- round(as.data.frame( st_coordinates(  newpt_BNG  ))-50, -2) +50          
                                          newpt_BNG_coords   <-  trunc(as.data.frame( st_coordinates(  newpt_BNG  )) /100)*100+50
                                   #         p2x<- trunc(p2x/100)*100+50
                                   #    p2y<- trunc(p2y/100)*100+50
                                    
                                          rv_dis$pts_bng[replacerow,c(2,3)] <-    newpt_BNG_coords 
                                     }
                        ##gonna be a mess here in indexing as removes NAs? reduces nrows and trigegrs Nfams actual..
                         
                                         cat("pts_ll  - ")
                                         rv_temp$ptstp_nro <- nrow(!is.na(rv_dis$pts_bng[,2]))
                                    
                      } else { # if click is not within site
                        if(input$operation == "translocation"){
                        rv_init$infotext_instructions_pointout <- paste0(hr(),"Click located outside exploratory site boundaries - not registered.")
                        notify_warning("locate click whithin the exploratory site boundaries" ,timeout=5100,  
                               config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
                              }}
                  } else { #if method is not click on map
                     if(input$operation == "translocation"){
                       cat("not click on map method  - ")
                    if(input$relPts_method == "manual_entry") {
                      notify_warning("manual coordinates input - clicks on the map are not registered" ,timeout=5100,  
                               config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
                      rv_init$infotext_instructions_pointout <- paste0(hr(), "Manual entry - clicks on the map are not registered.") }
                     } 
                  if(input$operation == "translocation"){
                    if(input$relPts_method == "random_location_across") { 
                    notify_info("automated point locations - clicks on the map are not registered" ,timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2 ,notiflixIconColor=txtCol_notif2  ))  
                      rv_init$infotext_instructions_pointout <- paste0(hr(), "Automated point locations - clicks on the map are not registered.") }
                    }      
                             }
              }# if site poly exists
         } # if site click disabled (ca only be pts clocks)
 
   }, ignoreInit=FALSE, ignoreNULL=TRUE)
replacerow <- reactiveVal(NULL)         
  

# saving the prepared leaflet plot as a html file
#htmltools::save_html(lplot, "leaflet.html")

 
   



click_lock_pts  <- reactive({
   NULL  
   if(!is.null(exploSite()) && !is.null(click_Pts_disabled()))  {  
        if(base::isTRUE(click_Pts_disabled())) {return (paste0(hr(), "Release locations are locked:",
                                                              br(),
                                                              "unlock to select alternative coordinates, or proceed with informing the demographics of the groups released.")  )
                } else {
                       if(Nfams_actual_display() == input$Nfams_init) {
                       return(paste0(hr(),"Lock the current coordinates to validate as release locations.") ) 
                         } else {
                         return(NULL )  }
             }   
   }  
})


click_OK_amend <- reactive({
   req(exploSite())
   if(!is.null(exploSite()))  {  
        if(base::isTRUE(click_Site_disabled())) {return (paste0(hr(), 
                                                              "The exploratory site is locked:",br(),"unlock the current selection to build an alternative candidate site, or proceed with locating release points within the site.")  )
                } else {
                       return(paste0(hr(), "Lock the current selection to validate it as a exploratory site.") ) 
            }   
   }  
  } ) 

pts_N <- reactive({
txt <- NULL
if(base::isTRUE(click_Pts_disabled())) { txt <-paste0("The simulation will include ", input$Nfams_init, " release locations.",br()) }
return(txt)
})



infotext_instructions_resample <-  reactive({
     txt <- paste0( "Coordinates are defined for ", Nfams_actual_display(),"/",input$Nfams_init, " potential release points.")
     if(base::isTRUE(click_Pts_disabled())) { txt <- paste0("Release locations are defined.") }
     return(txt)
  }) 
 
  

 
observeEvent({
  input$shape
  intercatch_on_map()
  1
  },{  
     
 leafletProxy("mapReleaseSite", session) %>%clearGroup("river catchments all") %>% removeControl("river catchments all") 
 req(input$shape !="none")
 req(intercatch_on_map() )
 intercatch_on_map <- intercatch_on_map() 
  req(!is.null(intercatch_on_map)) 
   cat("map local catch boundaries - ")  
  req(!is.na(sitedat()$click_lat))
  req( sitedat()$click_lat!=50)
  leafletProxy("mapReleaseSite", session) %>%  
           addPolygons(data=intercatch_on_map()   , group="river catchments all", 
                                    fillColor ="transparent", fillOpacity=0,  weight =1,  color="magenta",opacity=.5,
                                    options= pathOptions(pane = "intercatch_on_map")  )  

      update_catch_plot <- update_catch_plot()+1
      update_catch_plot %>% update_catch_plot()
 
  }, ignoreInit=FALSE, ignoreNULL=FALSE)
  
 
 
##################################### add local catchments layer to release map 
observeEvent( update_catch_plot() ,{
   
    leafletProxy("mapReleaseSite", session) %>%  clearGroup("catchments features") 

  req(input$shape !="none")
   
  req(intercatch_on_map_plot()) 
  req(intercatchLabs_plot())
      intercatchLabs_plot <- intercatchLabs_plot() 
  cat("map catch polys  -  ")


        ### here associate hments on map ------- with other layer of catchments - intercatch on map general that is tranparent, make top layer
       leafletProxy("mapReleaseSite", session) %>%   
         addPolygons(data=intercatch_on_map_plot(), group="catchments features" , fillColor = "magenta", 
                                    fillOpacity=~prop_suit*.5  ,  weight =3,  color="transparent",  opacity=1, dashArray=c(5,5), 
                                    highlight = highlightOptions(color="darkmagenta", bringToFront = FALSE , sendToBack = TRUE  ), 
                                     label =intercatchLabs_plot,  
                                    options = pathOptions(pane = "intercatch_on_map_plot"),
                                    labelOptions = labelOptions( style = as.list(c("color"="darkmagenta", "background-color" =  "#ccccccc4",labstyle_plotLabs)) , direction = "right" ))   
 
       #intercatch_on_map_plot() is not updating always - labs suit local
    update_suitras_catch <- update_suitras_catch()+1
    update_suitras_catch %>% update_suitras_catch() 
  }, ignoreInit=FALSE, ignoreNULL=FALSE)
 
#candsite_labdirection = "right" , textOnly = FALSE,  offset=c(75,-50) )



candsite_lab <- reactive({ 
 req(siteK())
   if(!is.null(rv_sim$candsite_lab )) {
     return( sprintf( "<b>%s </b><br>approx. %s",  "exploratory site", siteK() )  %>%  lapply(htmltools::HTML))
  } else {return(NULL)} 
})
 
 
  
 

observeEvent(txt_site_selected(),{
  req(input$shape)
  if(input$shape =="none") {
   "Only existing populations included in the simulation." %>% infotext_def_shape()
    } else {
  cat("txt site - ") 
  req(siteK())
  if(txt_site_selected() == paste0("Exploratory site boundaries are defined.")){ ## m2 to km2 = *1e-6   - m2 to ha = *1e-4
    if ( input$shape == "catchment") {
     Nctch <- length(selected_catchments_names())
     if(Nctch<2) {ctxt <- paste0(Nctch," catchment ")} else {ctxt <- paste0(Nctch," catchments ")}
        paste0( "An area covering ", ctxt ,", containing approx. ", siteK()," ha suitable for settlement") %>% infotext_def_shape() 
           } else {
        paste0( "A buffer of ",as.numeric(input$buffer_sel)/1000,"km radius containing approx. ", siteK()," ha suitable for settlement") %>%  infotext_def_shape() 
     } 
  }
     }
 }, ignoreNULL=FALSE, ignoreInit=FALSE)
 


  
observeEvent({
  init_poly() 
  exploSite()         ##
 # focusReady()#  click_OK_amend()#click_Site_disabled()
  sitedat()$click_lat 
  1
  },{#popo
  req(mapExists()==TRUE )
    if( model_running()==FALSE){
      local_hab(NULL) #dont reset unless model running because can create a site while model running and that ruins the output sync
      local_habw(NULL)
    }
 #browser() # discrep between sitedat shape and input?
  cat(paste0("\nchange site shape: ",input$shape, "  - "))
             if ( sitedat()$shape=="none"){
                Nfeat_records =ifelse(!is.null(Nfeat_records()),Nfeat_records(),0) 
               if(Nfeat_records==0){
                 paste0( "To simulate the observation of an existing population, locate observed territories on the map.") %>% infotext_instructions_shape()
               } else {
                 paste0( "Observed territories registered. To view local catchment features, create an exploratory site.") %>% infotext_instructions_shape()
               }
                    
                # paste0( "To start a simulation, add observation records or define an exploratory site to set-up a translocation.") %>% infotext_instructions_shape()
                 infotext_def_shape("Only existing populations included in the simulation.") 
                 click_Site_disabled(TRUE)
                 click_Pts_disabled(TRUE)
                 click_catch_disabled(TRUE)
                 paste0("No exploratory site used.") %>% txt_site_selected()
                 } else {

  if(base::isTRUE(click_Site_disabled()) ) {  #disable input$mapReleaseSite_shape_click 
          infotext_instructions_shape(NULL)
           #click_catch_disabled(TRUE) 
           if(!is.null(exploSite())) { paste0( "Exploratory site boundaries are defined.") %>% txt_site_selected()    } # rv_temp$pt_buffer is larger than sitepoly
           } 
          if(base::isFALSE(click_Site_disabled())) {  
                   cat("unlocked site  - ")
            
                 if ( sitedat()$shape == "catchment"){  
                          Ncatch <- length(selected_catchments_names())
                         if(Ncatch==0){ 
                                  if(any(is.na(sitedat()$click_lat),sitedat()$click_lat==50)){ paste0("Site location unknown.") %>% txt_site_selected() 
                                     } else { 
                                   paste0("General area located; no catchment selected.") %>% txt_site_selected() }
                          } else { paste0("Exploratory site located.") %>% txt_site_selected()}
                          
                            if(Ncatch<2){Ncatch <- paste0(Ncatch," catchment.")} else {Ncatch <- paste0(Ncatch," catchments.")}
                          paste0( "Spatial geometry built using ", Ncatch) %>% infotext_def_shape()  
                          paste0(br(), "Build the exploratory site geometry by selecting amongst the catchments highlighted on the map.", br() ) %>% infotext_instructions_shape()
                   }
                  if ( sitedat()$shape == "buffer"){    
                    
                            if(any(is.na(sitedat()$click_lat),sitedat()$click_lat==50)){ paste0("Site location unknown.") %>% txt_site_selected() } else { paste0("Exploratory site located.") %>% txt_site_selected()}
                             paste0(br(), "Build the exploratory site geometry by clicking at its location on the map, and change its size using the zone radius.",
                                    br() ) %>% infotext_instructions_shape()
                                    paste0( "Spatial geometry built using a buffer of ",sitedat()$buffer_val/1000 ,"km radius.") %>% infotext_def_shape() 
                   }  
                    click_Pts_disabled(FALSE)  
                    click_catch_disabled(FALSE)
          }  
}
    

}, ignoreInit=FALSE, ignoreNULL=FALSE)
  

##################################### add new exploratory site polygon to release map    
observeEvent({
  rv_sim$candsite_lab
  exploSite()
  1
  },{  
  req(mapExists()==TRUE )
    cat("\nnew poly!  - ") 
   leafletProxy("mapReleaseSite", session) %>% clearGroup("selected site")  %>% clearGroup("selected catchments")  %>% clearGroup("selected catchments labs")        
    paste0( format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")) %>% saveName_placeholder()   

   if(!is.null(exploSite()) & input$shape !="none"){
     req(candsite_lab()) # NULL once POINTS locked so doesnt show when no longer useful bEAuT!
             leafletProxy("mapReleaseSite", session) %>%     #390) %>% 
                addPolylines(data=exploSite(),  color = "black" , dashArray=c(5,5),  group = "selected site", weight=rv_sim$candsite_weight,  
                              label =  candsite_lab(),   opacity=1, 
                              labelOptions = labelOptions(direction = "right", style = as.list(c("background-color" =  "#ffe62cc2" ,"color"="#555", labstyle_plotLabs),
                                                                textOnly = FALSE,  offset=c(75,-50) )),
                            options = pathOptions(pane = "relSite_poly") ) %>%
                            addPolylines(data=exploSite(),  color = "#ffe62cc2" , group = "selected site", weight=rv_sim$candsite_weight,
                                         opacity=rv_sim$candsite_yellOp,options = pathOptions(pane = "relSite_polyYellow"))  
 
   }
    
       if(!is.null(exploSite_catchments())){
           cat("add catchments names  - ") 
         
            leafletProxy("mapReleaseSite", session) %>%  clearGroup("selected catchments labs") %>% 
                 addPolylines( data = exploSite_catchments(), weight =1, opacity =1, color = rv_sim$candcatch_col, #dashArray = c(5,5),
                               group="selected catchments",
                               options = pathOptions( clickable = FALSE,pane = "relSite_polycatch")) %>% 
                 addLabelOnlyMarkers (data=exploSite_catchments() %>% st_centroid(), 
                               label = ~WB_NAME, group="selected catchments labs",
                               labelOptions = lapply(seq_len(nrow(exploSite_catchments())), function(x) {
                               labelOptions(noHide = TRUE, textOnly = TRUE, opacity = 0.8 , offset=c(0,0),style = as.list(c(  "color"="beige","opacity"=".8", "text-warp"="wrap","font-weight"="bold","font-size"="90%",labstyle_plotLabs)))
                              })) %>%
                  groupOptions("selected catchments labs", zoomLevels = 12:20)  
            
       }   
    
}, ignoreNULL=FALSE, ignoreInit=FALSE) # ignoreNULL because clearing groups and layers upstream, on start (new site) - ignoreInit=F because updating with starting values when testing in filtered mod mode

 
        # ignoreInit added ap28  test init
    
##################################### add suitable hab layer per catchment to release map
 observeEvent (update_suitras_catch(),{ 
  
   leafletProxy("mapReleaseSite", session) %>%   clearGroup("within accessible catchments") %>% 
                                                 clearGroup("local suitable") %>%  removeControl("local suitable")  
  
   req(!is.null(local_suitras_catch()))  #   added ap28  test init
   cat("map suitable patches  - ")
   req(input$shape !="none") 
       local_suitras_catch <- local_suitras_catch()
       suitLabs <-  merge( local_suitras_catch(), intercatch() %>% st_drop_geometry(), by="WB_NAME") %>% subset(select=c(WB_NAME, habLabs))
       suitLabs <- as.character( paste0( "<span style=' color:darkmagenta;line-height:0.5em!important;font-weight:bold;margin-left:7px;;text-align:left;'>",
                           suitLabs$WB_NAME,"</span><br>
                            <span style=' color:#008080;line-height:0.5em!important;font-weight:normal;margin-left:7px;'>approx. ",
                           suitLabs$habLabs)) 
       suitLabs <-  sprintf( "%s",suitLabs) %>%  lapply(htmltools::HTML)   
# suitLabs <-   intercatchLabs_plot() 
## catch labels on layers:     "intercatch_on_map_plot", "suit_in_catch"
  
      
    req( local_suitras_catch()) # has 0 rows when all NULL
    leafletProxy("mapReleaseSite", session)  %>%  
                  addPolygons(data=local_suitras_catch(), color="transparent",  opacity=1, weight=1,  
                              fillColor="darkmagenta", fillOpacity=.7,
                              #   highlight = highlightOptions(  fillColor="darkmagenta", fillOpacity=.7, bringToFront = FALSE),
                                 group= "catchments features", 
                                 options = pathOptions(pane = "suit_in_catch" ),
                                 label = suitLabs,  
                                 labelOptions = labelOptions( style = as.list(c(  "background-color" =  "#ccccccc4",labstyle_plotLabs,"text-align"="left!important")) , direction = "right" )) #      %>%

  
# if(length(selected_catchments_names())>0) { 
#   cat("catch names ")
#     leafletProxy("mapReleaseSite", session)  %>%  
#                  addPolygons(data=local_suitras_catch()[local_suitras_catch()$WB_NAME %in% selected_catchments_names() ,],   color="transparent",  opacity=1, weight=1, fillColor="darkmagenta", fillOpacity=.7,
#                                 group= "catchments features", 
#                                 options = pathOptions(pane = "suit_in_catch" ),
#                                 label = suitLabs,  
#                                 labelOptions = labelOptions( style = as.list(c(  "background-color" =  "#ccccccc4",labstyle_plotLabs,"text-align"="left!important")) , direction = "right" )) #      %>%#
#
# }
 }, ignoreNULL=FALSE, ignoreInit=FALSE) 


  
 
 
 
 
 
##################################### lock site when switching pt sampling method if site exists     
observeEvent(input$relPts_method,{ 
req( !is.null(exploSite()) )
   cat("lock site  - ")
   updatePrettyCheckbox(session, "lock_site",  label="exploratory site locked", value=TRUE )   
}, ignoreInit=TRUE, ignoreNULL=TRUE)                     
 
  
##################################### automated point sampling ####
observe({ 
  if(input$relPts_method != "random_location_across") {shinyjs::disable("simRandPts")} else {shinyjs::enable("simRandPts")}
})
 
observeEvent (input$simRandPts,{  
  if(is.null(exploSite())) { 
      notify_info("to generate release points, first define an exploratory site" , timeout=4000,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl",  
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
        
}
  req(exploSite())
  warning_noSuitHab(0)
  rv_dis$fam.start <- rv_temp$pts_ll <- NULL  # so it changes at end of computation and triggers plotting 
  rv_init$infotext_instructions_pointout <- NULL
  rv_init$infotext_simError <- NULL
  infotext_instructions_habLowqual(NULL)
     sampleArea <- st_intersection(local_suitras_catch(),  exploSite() )%>%suppressMessages() #%>% st_transform(st_crs(4326)) 
      if(nrow(sampleArea)>0){
                  Nfams <- input$Nfams_init 
                  newpts_BNG <- st_sample(sampleArea ,Nfams,by_polygon = FALSE)  %>% st_transform(st_crs(27700))  
                  rv_temp$ptstp_nro <- 0  
                  newpts_BNG_coords   <- round(as.data.frame( st_coordinates(  newpts_BNG  )), 0)    #round(as.data.frame( st_coordinates(  newpts_BNG  )),  -2)#  + 50
                  colnames(newpts_BNG_coords) <- c("x", "y") 
                  rv_sim$newpts_BNG  <-    st_as_sf(data.frame( x= newpts_BNG_coords$x , y=newpts_BNG_coords$y ),   coords = c("x","y"), crs=st_crs(27700)) 
                  rv_dis$pts_bng <-   as.data.frame(cbind (group=seq(1, nrow(newpts_BNG_coords)), newpts_BNG_coords)) # added rv_sim$newpts_BNG jan8
                  rv_temp$ptstp_nro <- nrow(newpts_BNG_coords)
     } else {  
            cat(" not enought suitable habitat  -")
            warning_noSuitHab <- warning_noSuitHab()
           (warning_noSuitHab+1) %>% warning_noSuitHab()
            rv_temp$ptstp_nro <- 0 ##??? added feb2024 not sure
     }   
       cat(paste0("generated ", rv_temp$ptstp_nro ," random pt coords  - ")) 
}) 
 
 
observeEvent(warning_noSuitHab() ,{ ### message if release points were not computed:  
        infotext_instructions_habLowqual(NULL)
       rv_temp$ptstp_nro <- 0
       if(warning_noSuitHab()>0) { 
         paste0(hr(),"Habitat suitable for beaver settlement is insufficient; select an alternative site to simulate a translocation.") %>% infotext_instructions_habLowqual()
      notify_failure("release cancelled!",timeout=4000,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl",  
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
      notify_warning("insufficient suitable habitat within site",timeout=5100,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))    
          } 
}, ignoreInit = TRUE)

observeEvent(Nsettled_txt(),{
  if(is.null(Nsettled_txt())) {model_running_UItxtout(NULL)}
}, ignoreNULL=FALSE, ignoreInit=FALSE)  


##################################### change in release points coords - create matching dfs  ####
observeEvent(rv_dis$pts_bng,{
  cat("reset: ")
   init_terrsPolys(NULL)
   Nsettled_txt(NULL)
  # superDispcells(NULL)
    
  if(nrow( rv_dis$pts_bng) == input$Nfams_init) {
    cat("new fams and timing dfs - ")
       Nyg_options <- func_Nyng(demog_gp=input$demog,Nfams=input$Nfams_init, Nmin=Nyoungs_perfam_min(), Nmax=Nyoungs_perfam_max())   
       rv_dis$fam.start <- data.frame(group= seq(1:input$Nfams_init),  fem=1, mal=1,young=Nyg_options)
       rv_dis$timing <- data.frame(group=seq(1:input$Nfams_init), year=thisYear()) 
       
       input$demog %>% demog_group() 
  } else {
      cat(" delete fams and timing dfs  -")
       Nyg_options <- NULL
       rv_dis$fam.start <- NULL
       rv_dis$timing <- NULL
  }
}, ignoreNULL=TRUE)
 
thisYear <- reactiveVal( as.numeric(format(Sys.Date(),"%Y")) )

##################################### change in release points coords - compile all coords ####
observeEvent(rv_dis$pts_bng,{     
   cat("\ninit pts change  - method:  ") 
  
 leafletProxy("mapReleaseSite", session) %>%  clearGroup("initial settlement test") %>% removeControl("initial settlement test") %>%
    clearGroup("initial settlement lines") 
  
            if(input$relPts_method == "random_location_across") {
              cat("automated - ") # take values in BNG produced frrom sampling algo, convert
       #        newpts_ll_coords  <-  as.data.frame(st_coordinates( rv_sim$newpts_BNG )  )  
               newpts_ll_coords  <-  as.data.frame(st_coordinates(st_transform(rv_sim$newpts_BNG, crs =st_crs(4326))))# st_crs(4326))  )) 
       colnames(newpts_ll_coords) <- c("lng", "lat")
               rv_temp$pts_ll<- as.data.frame(cbind (group=seq(1, nrow(newpts_ll_coords)), newpts_ll_coords))
               rv_temp$newpts_ll_coords <- newpts_ll_coords
            }
 
            if(input$relPts_method == "manual_entry") {
              cat("manual entry  - ") # take values in lalng produced from conversion at time of change in table cell
               newpts_ll_coords <- as.data.frame(rv_temp$newpts_ll_coords)
               rv_temp$pts_ll <- as.data.frame(cbind( group=seq(1, nrow(newpts_ll_coords)), newpts_ll_coords)) 
             }
  
            if(input$relPts_method == "each_pt_on_map") { 
              cat("each pt as click  - ") #  
              if(!is.null(initValues())){   # modified by "-" button
                cat("remove or add pts")
                                 rv_temp$pts_ll <- as.data.frame( rv_temp$newpts_ll_coords) 
                                 rv_temp$pts_ll$group <- seq(1, nrow(rv_temp$pts_ll)) 
              } else {
                cat("new pt")
                                 newpts_ll_coords <-  as.data.frame(st_coordinates( rv_sim$newpts_ll  )  )
                                 colnames(newpts_ll_coords) <- c("lng", "lat")
                                 rv_temp$pts_ll<- as.data.frame(cbind (group=seq(1, nrow(newpts_ll_coords)), newpts_ll_coords))
              }
            }
     # up to now for each method Nfams_init_actual() <- NULL was set, so this change will trigger plotting:
     length(!is.na(rv_temp$pts_ll[,2])) %>% Nfams_init_actual() 
 
 }, ignoreInit=TRUE)

   
  
    
########  reset release points  ########
observeEvent({
  exploSite()  
  input$relPts_method
  input$Nfams_init
   1
  },{ 
  req(base::isFALSE(click_Pts_disabled()))  
    cat("\nreset release pts   - ")
    init_terrsPolys(NULL)
    Nsettled_txt(NULL)
    dfrows_selected(NULL)
    notSettling(NULL)
    removeUI('#text', immediate = T)       
   
   rv_temp$clickCount_pts <- 0 #  new click stock 
   rv_init$infotext_instructions_pointout <- NULL
   rv_init$infotext_simError <- NULL
   infotext_instructions_habLowqual(NULL)
   cat("clear  map -  ") 
    
   leafletProxy("mapReleaseSite", session) %>%   clearGroup("initial settlement test") %>% removeControl("initial settlement test")  %>% 
    clearGroup("initial settlement lines")   %>%  clearGroup("routes") %>%  clearGroup("selected catchments labs")%>%    
    clearGroup("simulated occupancy") %>% removeControl("simulated occupancy legend") 
    
  
     
   #####  if no simulation in progress  
   # if model-Running=TRUE dont deletesite etc just reinitialized with the above
     req(base::isFALSE(model_running()))  
     cat("pts reset  - ") 
     initValues(NULL)
     rv_temp$pts_ll <- rv_dis$fam.start <- rv_sim$timing  <- rv_dis$pts_bng <- NULL
     if(input$lock_pts==TRUE) {updatePrettyCheckbox(session, "lock_pts",  label="release locations unlocked" , value =FALSE )}
     rv_temp$validatedPts <-   rv_temp$ptstp_nro <- 0 
     Nfams_init_actual(NULL)   
 #    layers_list <- addLayersCont_list (cur_layers=layers_list,new_layer=c( "translocation set-up" ),type="clear")  
 #    layers_list %>% cur_layers()    
     leafletProxy("mapReleaseSite", session) %>%   clearGroup("table_hilight")  %>% clearGroup("translocation set-up") # %>%   removeControl("translocation set-up")  
      
        
    if(input$relPts_method == "each_pt_on_map") {  #resrict area to selected site
      cat("click  -")
              val <- reactiveValues(clickx =NULL, clicky =NULL ) 
              rv_dis$fam.start <-  NULL
               Nfams_init_actual(NULL)   ## to reset coords table disaplyed
#rv_temp$pts_ll<-  NULL
              } 
   
    if(input$relPts_method == "manual_entry") {
        if(!is.null(exploSite())){
          cat("manual  -")
          if(input$shape == "buffer") {
                    sample_line <- st_cast( st_buffer(st_transform(exploSite() , st_crs(27700)),  -sitedat()$buffer_val/2)  , "LINESTRING") 
                    temp_pts_ll <- st_transform(st_sample(sample_line, input$Nfams_init, type="regular" ),st_crs(4326) )
                    } else {
                    temp_pts_ll <- st_sample(exploSite(), input$Nfams_init, bypolygon=TRUE)
                    }# to be modififed by user
          st_crs(temp_pts_ll) <-  st_crs(4326) 
          temp_pts_ll_coords <-  as.data.frame(st_coordinates( temp_pts_ll )) [,c(1,2)]
          colnames(temp_pts_ll_coords) <- c("lng","lat")
          rv_temp$newpts_ll_coords <- temp_pts_ll_coords
          temp_ptsBNG_coords <-matrix(round(st_coordinates(st_transform(temp_pts_ll, st_crs(27700)))[,1:2],-1), ncol=2) %>% as.data.frame()# as.data.frame(round(st_coordinates(st_transform(temp_pts_ll, st_crs(27700)))[,1:2],-1))
          colnames(temp_ptsBNG_coords) <- c("X","Y")
          rv_dis$pts_bng<-cbind(group=seq(1:nrow(temp_ptsBNG_coords)), temp_ptsBNG_coords) 
        } 
      counter_initPopChange(counter_initPopChange()+1)
      }
   
}, ignoreInit=TRUE)

  
 
 
observeEvent(input$Nfams_init,{   # change nfams while already simulated some pts #SHOULD REMOVE THIS??!
    cat("change Nfams_init  - ")
    if (input$relPts_method !=  "random_location_across") { # click to run pts for that one 
      if(!is.null(rv_temp$pts_ll)){
                rv_temp$pts_ll <- as.data.frame(rv_temp$pts_ll)[1:input$Nfams_init,c("lng","lat")]
                rv_temp$pts_ll$group <- seq(1,nrow(rv_temp$pts_ll)) 
            }       }
}, ignoreInit=TRUE) 
    
   


##################################### demog - inform via table edit  ####
observeEvent(input$famTable_cell_edit ,{
    cat("\nedit fam table")
    row  <- input$famTable_cell_edit$row
    clmn <- input$famTable_cell_edit$col +1 # as omitted first col rownames
    val <-input$famTable_cell_edit$value
    if(val<0) {val<-0}
    rv_dis$fam.start[ row,clmn] <- val   
    young <-  rv_dis$fam.start$young
         if(sum( young)==0) { cat("all adts  - ")
                                    shinyWidgets::updateRadioGroupButtons(inputId="demog", session, selected = "all adults" )
                                    "all adults" %>% demog_group()
         } else {
         if(length(which( young==0))>0) { cat("combo  - ")
                                   shinyWidgets::updateRadioGroupButtons(inputId="demog", session, selected = "combination" )
                                    "combination" %>% demog_group()
             } else {
                                    cat("all fams  - ")
                                    shinyWidgets::updateRadioGroupButtons(inputId="demog", session, selected = "all families" )
                                   "all families" %>% demog_group()
         }         }  
  rerun_demog(FALSE) 
  cat("rerun demog=FALSE  - ")
 }, ignoreNULL=TRUE, ignoreInit=TRUE) 
 
##################################### N young - UI/buttons and numbers
Nyoungs_perfam_min <- reactiveVal(1)
Nyoungs_perfam_max <- reactiveVal(3) 

output$downNyg_btns <- renderUI({
   if(input$demog == "all adults") {return(NULL)} else { 
             return(actionLink("down", NULL, icon(name = NULL, style = "background: url('arrow-down_yel.png');background-size: contain;
                                                                        background-position: center;background-repeat: no-repeat;height:24px;width:24px;display: inline-flex;" ))   )  } 
})

output$upNyg_btns <- renderUI({
   if(input$demog == "all adults") {return(NULL)} else { 
             return(actionLink("up", NULL, icon(name = NULL, style = "background: url('arrow-up_yel.png');background-size: contain;
                                                                      background-position: center;background-repeat: no-repeat;height:24px;width:24px;display: inline-flex;")) )   }
})  

observeEvent(input$up,{
  cat("up  - ")
  Nyoungs_perfam_min <- Nyoungs_perfam_min()
  (Nyoungs_perfam_min+1) %>% Nyoungs_perfam_min() 
  Nyoungs_perfam_max <- Nyoungs_perfam_max()
  (Nyoungs_perfam_max+2) %>% Nyoungs_perfam_max() 
  Nyg_options <- func_Nyng(demog_gp=input$demog,Nfams=input$Nfams_init, Nmin=Nyoungs_perfam_min(), Nmax=Nyoungs_perfam_max())  
  rv_dis$fam.start$young <-Nyg_options 
})
  
observeEvent(input$down,{
  cat("down - ")
      Nyoungs_perfam_min <- Nyoungs_perfam_min()
      Nyoungs_perfam_min <- Nyoungs_perfam_min-1
      if(Nyoungs_perfam_min<1) {Nyoungs_perfam_min <- 1}
      Nyoungs_perfam_min %>% Nyoungs_perfam_min() 
      Nyoungs_perfam_max <- Nyoungs_perfam_max()
      Nyoungs_perfam_max <- Nyoungs_perfam_max-1 
      if(Nyoungs_perfam_max<1) {Nyoungs_perfam_max <- 1}
      Nyoungs_perfam_max %>% Nyoungs_perfam_max()
      Nyg_options <- func_Nyng(demog_gp=input$demog,Nfams=input$Nfams_init, Nmin=Nyoungs_perfam_min(), Nmax=Nyoungs_perfam_max())  
      rv_dis$fam.start$young <-Nyg_options 
})
    

observeEvent(input$demog,{    # only triggered by user changing demog - not by update of selection when editing table
     if (base::isTRUE(rerun_demog())) {
  cat("rerun demog  - ")
  #   Nyoungs_perfam_min(1)
  #   Nyoungs_perfam_max(3)
        Nyg_options <- func_Nyng(demog_gp=input$demog,Nfams=input$Nfams_init, Nmin=Nyoungs_perfam_min(), Nmax=Nyoungs_perfam_max())  
       rv_dis$fam.start$young <- Nyg_options 
        input$demog %>% demog_group()
     } else {
       cat("dont rerun demog - reset to TRUE  - ")
       rerun_demog(TRUE)} 
    }, ignoreInit=TRUE )  
 
 
             
##################################### gather release params values into initial df ####
observeEvent({ 
  rv_temp$pts_ll
  rv_dis$pts_bng
  rv_dis$fam.start
  rv_dis$timing # add a trigger there for avoiding modif params when sim in progress 
  1
}, {  
  initValues(NULL) 
  req( rv_dis$fam.start)  
    cat("\ninit vals update   - ")
    # build display table gathering all info = coords + demog + timing, eorder by group id
    print(rv_dis$timing)
    print(rv_temp$pts_ll)
    print(rv_dis$fam.start)
     data.frame(  lng=rv_temp$pts_ll$lng, lat=rv_temp$pts_ll$lat,
                                        group=rv_dis$fam.start$group,  females=rv_dis$fam.start$fem, males=rv_dis$fam.start$mal, young=rv_dis$fam.start$young,
                                         year=rv_dis$timing$year) %>% initValues()
   
    rv_temp$validatedPts <- nrow(rv_temp$pts_ll)
    rv_temp$ptstp_nro <- nrow(rv_temp$pts_ll) # review this may be not ligned with !is.na()
    nrow(rv_temp$pts_ll) %>% Nfams_init_actual() 
    notSettling(NULL) # summ text
    startGrowing(NULL)
    trigger_undo_settling(trigger_undo_settling()+1) # just reset all for ow - in future could trigger only whe params like demog or time are changed
 }, ignoreNULL=FALSE, ignoreInit=TRUE) # to create a df of sort from start



  
    
     
##################################### generate new released groups labels ####
observeEvent({
  rv_temp$pts_ll
  initValues()
  1
  }, { 
  cat("new labs  - ")
    if(!is.null(rv_temp$pts_ll)) {
             counter_initPopChange <- counter_initPopChange()+1 
                  counter_initPopChange <- counter_initPopChange()+1 
             counter_initPopChange %>% counter_initPopChange()    }
             released_labs <-released_labs_stranded <-  NULL # so that it triggers replot every time there is a new pt even if not labelled
       if(!is.null(initValues()) ) {
          initValues <- initValues()
          if(  input$Nfams_init == length(which(!is.na(initValues$lng)))){   
            gp_code <- groupLabs_stranded <- NULL
            for(gp in  initValues$group) {
              hexc <- as.character(paste0("&#x",2779+gp))
              gp_code0 <- HTML(as.character(paste0("<p>released group <span style='font-size:17px;'>",hexc,";</span></p>")))
              gp_code <- c(gp_code,gp_code0)
              groupLabs_stranded0 <- HTML(as.character(paste0("<p>not settling: group <span style='font-size:17px;'>",hexc,";</span></p>")))
              groupLabs_stranded<- c(groupLabs_stranded,groupLabs_stranded0)
            }
 
       groupLabs <- gp_code  
             
                  demogLabs<- paste0(initValues$fem, " females +", initValues$mal," males +", initValues$young, " young")   
                  timingLabs <- paste0( "<p>release year ", initValues$year,"</p>")
                  
                  if(length(unique(rv_dis$timing$year))>1)  { 
            shinyWidgets::updateRadioGroupButtons(inputId = "timing", session, selected= "several_years")
            released_labs <-    sprintf(  "%s %s %s",  groupLabs,  demogLabs,  timingLabs)  %>%  lapply(htmltools::HTML) 
      }  else {
                        released_labs <-    sprintf(  "%s %s",  groupLabs,  demogLabs)  %>%  lapply(htmltools::HTML)  
                        released_labs_stranded <-    sprintf(  "%s %s",  groupLabs_stranded,  demogLabs )  %>%  lapply(htmltools::HTML)  

                         }
            } ## only show labs at the end - otherwise confusing as group#1 is always the last point plotted 
       }
     released_labs_stranded %>% released_labs_stranded() 
     released_labs %>% released_labs()
          
   }, ignoreNULL=FALSE, ignoreInit=TRUE) # so not full legend on init
    
    
 
 
observeEvent(input$timing,{  ## reset all years to all 1 when clicking on 'same year
   req(rv_dis$timing)
   if(input$timing == "same_year") { rv_dis$timing$year <- thisYear() }   
}, ignoreInit=FALSE)  

 
      

##################################### replot initial population ####
observeEvent( counter_initPopChange(),{ 
    cat("\nmap released groups  - ") 
 #   layers_list <- addLayersCont_list(cur_layers= cur_layers(),new_layer= "translocation set-up", type="add"  )
     
          leafletProxy("mapReleaseSite", session) %>% 
                        clearGroup("translocation set-up") %>%# removeControl("translocation set-up") %>% 
                        clearGroup("routes") %>% clearGroup("initial settlement test")  
  
            
           req(!is.null(rv_dis$pts_bng))
    
#if(is.null(initValues() ))  
   df_complete <- FALSE
         if(nrow(rv_dis$pts_bng)==input$Nfams_init) { df_complete <- TRUE }

        if(  nrow(rv_dis$pts_bng)>0) {
         cat("replot  - ") 
            
        if(base::isFALSE(df_complete)  ) { 
              cat("df incomplete - ")
              if(!is.null(rv_temp$pts_ll)){  # temp points
                       
                      dat <- na.exclude( rv_temp$pts_ll)
                      leafletProxy("mapReleaseSite", session) %>%  
                                   addMarkers(data=dat,  ~lng,~lat ,icon= beaverIcon_init, options = pathOptions(pane = "point"),
                                               label= NULL,  group="translocation set-up" )  # no labs here yet
                       init_terrsPolys(NULL)
                       Nsettled_txt(NULL)
                       removeUI('#text', immediate = T)       
               } 
          }  
       
        if(base::isTRUE(df_complete)  ) { 
              cat("df complete  - ") ###check if can plot with NA to keep demog display when removing pts in click on map mode
              
              leafletProxy("mapReleaseSite", session) %>%   
                                   addMarkers(data= initValues(),  ~lng,~lat ,icon= beaverIcon_init  , options = pathOptions(pane = "point"),
                                            label= released_labs(),  group="translocation set-up",  
                                            labelOptions = labelOptions( offset=c(-50,0), direction="left", 
                                                                         style = as.list(c( "background-color"="#0000008f", "color" =  "turquoise" ,labstyle_plotLabs)) )) %>%
                                  addMarkers(data= initValues()[initValues()$young>0,],  ~lng,~lat ,icon= beaverIconSmall_init ,   label= released_labs(),  group="translocation set-up",  
                                            labelOptions = labelOptions( offset=c(-50,0), direction="left", 
                                                                         style = as.list(c( "background-color"="#0000008f", "color" =  "turquoise" ,labstyle_plotLabs))),
                                               options = pathOptions(pane = "point") )%>%
                                  addMarkers(data= initValues()[initValues()$year>min(initValues()$year),],  ~lng,~lat ,icon= timeIcon_init , label=NULL,  options = pathOptions(pane = "point"), group="translocation set-up"  ) 
                paste0(format(Sys.time(), "%d%b%y"),"_",format(Sys.time()+60*60, "%H%M")) %>% saveName_placeholder()   
                        
        }
           
   
       rv_temp$addMapHilights <- 1   
      refresh_legendTxt(refresh_legendTxt()+1)
      modelPrep(0)
}
}, ignoreInit=TRUE, ignoreNULL=FALSE)
       
  
          
 

##################################### text demographics   
relSite_infotext_Npts_comp <- reactive({   
  Nyoung_text <- NULL
  req(!is.null(initValues())) 
    initValues <- initValues()
    Nyoung_text <- paste0(min(initValues$young), " to ",max(initValues$young), " young")
    if(min(initValues$young) == max(initValues$young)) {
          if(min(initValues$young)==0) {Nyoung_text <- paste0("no young")} else {Nyoung_text <- paste0(max(initValues$young), "young each") }
     } 
    return( Nyoung_text )  
})
    


    
##################################### N fams actual 
Nfams_actual_display <- reactive({  
  if(is.null(Nfams_init_actual())) {return(0)} else { return(Nfams_init_actual())}   })
        
output$Nfams_init_actual<- renderText({ Nfams_actual_display()   }) # fams actually located within candiate exploratory site

                                                    
       yaycol <- "#a7ca35"
       naycol <- "#e6e6e6"           
        
       
famNcolor <- reactive({ 
   input$Nfams_init 
  if(is.null(initValues())){return(naycol)}else {
  if(length(!is.na(initValues()[,2])) == input$Nfams_init) { return(yaycol)} else {return(naycol)}}
})
       

output$Nfams_init_actual_UI <- renderUI({ 
  #return(tagList(
    div( Nfams_actual_display(),class="form-control",style=paste0("width: 5vw;background-color:",as.character(famNcolor()),"!important;vertical-align:middle!important;margin-top:4px;min-height: 20px;") )
 #)) 
  }) # ]0#Nfams_init_actual_UI()})    
       
       
#Nfams_init_actual_UI <- reactive({  
#            return(tagList(
#              div(textOutput( "Nfams_init_actual"  ),class="form-control",style="width: 5vw;background-color:",famNcolor(),"!important;vertical-align:middle!important;margin-top:4px;")
#              ))  
#}) 
   
 


output$mapReleaseSite <- renderLeaflet({   
    mapReleaseSite()  })  
 
  
 # group "river catchments all" == pane "intercatch_on_map"
# group -"selected site"  = pane "relSite_poly"
# group "catchments features" = "intercatch_on_map_plot"
##################################### map local site #### 
 # output$mapReleaseSite <- renderLeaflet({  mapReleaseSite()  })  
mapReleaseSite <- reactiveVal()   
launchMapLayers <- reactiveVal(FALSE)

observeEvent(launchTab2(),{    
  req(launchTab2()==TRUE ) # Only display if tab is 'Map Tab'
  req(country_boundaries()) # executable since habtif3857_path() is in
  dat <- cdat [country_indx(),] 
  cat("\n--RENDER COUNTRY MAP*")
  mapReleaseSite <-  
    leaflet(options = leafletOptions(zoomControl = FALSE, doubleClickZoom = FALSE )  )%>% 
        addProviderTiles("OpenStreetMap") %>%  
        addMapPane("country", zIndex = 201) %>%
        addMapPane("habitat_loZoom", zIndex = 203) %>%
        addMapPane("customPoly", zIndex = 204) %>%
        addMapPane("suit_in_catch", zIndex = 205) %>% # catchnames labs 

        addMapPane( "intercatch_on_map", zIndex=208) %>%
        addMapPane( "intercatch_on_map_plot", zIndex=209) %>%  # catchnames labs
    
        addMapPane("relSite_polycatchYellow", zIndex=229) %>% 
        addMapPane("relSite_polyYellow", zIndex=229) %>% 
        addMapPane("relSite_polycatch", zIndex = 230) %>%  
        addMapPane("relSite_poly", zIndex = 230)  %>%     
    
     #   addMapPane("suit_in_catch", zIndex = 410) %>% # catchnames labs 
    
        addMapPane("rivCatch", zIndex = 550) %>% 
        addMapPane("opeSurf", zIndex = 560) %>%  
        addMapPane("mgmtSurf", zIndex = 570) %>%   
    
        addMapPane("metadataAreaOfInt", zIndex =510)  %>% 
        addMapPane("simulated occupancy", zIndex = 515) %>% 

        addMapPane("metadataArea", zIndex =520)  %>% 
   #      addMapPane("metadataLine", zIndex = 530)  %>%
        addMapPane("sTest_pols", zIndex = 620)     %>% 
        addMapPane("routes", zIndex = 650)  %>%
        addMapPane("initial settlement test", zIndex = 700) %>%   
        addMapPane("metadataPt2", zIndex = 760)   %>% # simulated observed territories pts
        addMapPane("highlights", zIndex = 755) %>%    # transloc pts
        addMapPane("metadataPt", zIndex = 800)    %>% 
         addMapPane("metadataLine", zIndex = 800)  %>%
        addMapPane("point", zIndex =9998)    %>%
        addMapPane("stranded", zIndex = 9999)    %>%   
        addMiniMap (position = "bottomright") %>%  
        addScaleBar( position =  "bottomright")   #%>%
    #    flyTo( lng=dat$lng_countries, lat=dat$lat_countries, zoom=dat$zoom_countries)
    
  habtif3857_path  <- habtif3857_path() 
 # dat <- cdat [country_indx(),] 
  
   mapReleaseSite <- mapReleaseSite %>%  
        addPolygons(data=country_boundaries() ,  fillColor =  "transparent", color = "transparent", fillOpacity = 0, group="nondefined",
                    options = pathOptions(pane ="country" ) )  %>%
           leafem::addGeotiff(file=habtif3857_path,   project = FALSE,
                               colorOptions=leafem::colorOptions(palette = c(   alpha("grey20",.35),alpha("grey20",.7)) ,  breaks=c(1,2), na.color = "transparent")  ,
                                stroke=NULL,  group="habitat hiZoom" ) %>%  
            hideGroup("habitat hiZoom")%>%    #or else will fly to bounds each time new layer in 
           addPolygons(data=intercatch(),fillColor="gray34",opacity=.5,color="#BEBEBE33", weight=4,fillOpacity=~trunc (floor(prop_suit*10))/10,
                      group="habitat loZoom",  options = pathOptions(pane ="habitat_loZoom")) %>% 
           addMeasure( position = "bottomright", primaryLengthUnit = "kilometers",
                          primaryAreaUnit = "hectares",  activeColor = "black", completedColor = "black",
                          popupOptions = list(className = "leaflet-measure-resultpopup", autoPanPadding = c(10,10)),
                          captureZIndex = 10001, localization = "en" )  %>%      
           addLayersControl( overlayGroups  =  "beaver habitat"  ,
                            position = "bottomleft", options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)   )
  
     mapReleaseSite$dependencies <- c(mapReleaseSite$dependencies, leafpm::pmDependencies()) ##  needed for toolbar package
     mapReleaseSite %>% mapReleaseSite()
         mapExists(TRUE)    
       cat("\nmap exists!  - ")
       cur_layers(NULL)
       hiZoom_prev(0)
       outputOptions(output, "mapReleaseSite", suspendWhenHidden = FALSE)
}, ignoreNULL=TRUE, ignoreInit=TRUE)
 

 





 
 
 
 
focusReady <- reactive({ ## allows buttons panel at all
  val <- 0
  if( any(!is.null(input$mapReleaseSite_click),!is.null(pointFeature_terrPols()))) {val<-1}
  return(val)
})

 



 ##################################### fly to extent ####
observeEvent (input$adjust_view,{ 
 cat("go to extent  - ")
  focus_area <- NULL
  if(input$shape=="none"& is.null(exploSite())) { # i.e. when shape==none still go to click on map without plotting or registering a site
             cat("no site  - ")  
    if(!is.null(input$mapReleaseSite_click)){
               pt_buffer_focus <-  st_buffer(st_transform(st_sfc(st_point(c(input$mapReleaseSite_click$lng -.01, input$mapReleaseSite_click$lat) ), crs = st_crs(4326))  , st_crs(27700)), 2000) 
               focus_area <- st_as_sfc(st_bbox(pt_buffer_focus))
               cat("temp buffer  - ")  } else {
          if(!is.null(pointFeature_terrPols())){  
            if(nrow(pointFeature_terrPols())>0){
              focus_area <- st_as_sfc(st_bbox(st_buffer(st_transform(pointFeature_terrPols() , st_crs(27700)), 2000))) } }
          }   
     } else {
  if( !is.null(exploSite())) {
    cat("exploSite()  - ")
    focus_area <- st_as_sfc(st_bbox( st_buffer(st_transform(exploSite(), st_crs(27700)), 2000))) 
     } else if (!is.null(rv_temp$pt_buffer_focus)) { 
       cat("rv_temp$pt_buffer_focus")
   focus_area <- st_as_sfc(st_bbox(rv_temp$pt_buffer_focus))
     }  
     } 
  if(!is.null(focus_area) ) {
          bbox_coords <- as.numeric(st_bbox(st_transform( focus_area, st_crs(4326) )))
          leafletProxy("mapReleaseSite", session) %>%  
                     flyToBounds( bbox_coords[1] -.005,bbox_coords[2],bbox_coords[3]-.005,bbox_coords[4])
  } else { 
          init_poly <- init_poly() 
          if(!is.null(init_poly)){ 
                   init_poly <- init_poly %>% st_centroid() %>% st_coordinates() %>% as.data.frame()
                    leafletProxy("mapReleaseSite", session) %>%   flyTo( lng=init_poly$X -.01, lat=init_poly$Y, zoom=10)  
                     
             } 
   }
}, ignoreNULL=TRUE)
   


##################################### compute local carrying capacity ####
siteK <- reactive({
  exploSite()
  exploSite_catchments()
  req(!is.null(suitablePatches()))
  req(any(!is.null(exploSite()),!is.null( exploSite_catchments())))
   siteK <-0
     if ( input$shape == "catchment"& !is.null(exploSite_catchments())) {siteK <- as.character(paste0(sum(exploSite_catchments()$suit_ha1),"ha suitable for settlement within selection") ) }
     if ( input$shape == "buffer" & !is.null(exploSite()))    { ### compute coverage within  buffer
    #   cat("compute siteK  - ") 
      # exploSite() #4326   
            local_suitras <- st_intersection(suitablePatches(), exploSite())%>%suppressMessages() %>% st_union() %>% st_area()  %>%as.numeric()
            siteK <- as.character(paste0(round(as.numeric(local_suitras)*1e-4),"ha suitable for settlement within the buffer"))   
      }   
 return(siteK)  
})

  
  
###################################################################### visualise layers -triggers ####
   
customLayer_display <- reactiveVal(0) 
LCM_display <- reactiveVal(0)
NBNAtlas_display <- reactiveVal(0)
opeSurf_display <- reactiveVal(0)
mgmtSurf_display <- reactiveVal(0)
rivCatch_display <- reactiveVal(0)

 
observeEvent(input$landCoverMap ,{  
  req(mapExists()==TRUE )
  cat("LCM display  - ")
 LCM_display <- LCM_display() 
  if (LCM_display ==0){  LCM_display(1) }
  if (LCM_display ==1){  LCM_display(0) }
 refresh_legendTxt(refresh_legendTxt()+1)
})  
    

observeEvent(input$NBNAtlas ,{  
  req(mapExists()==TRUE )
  cat("NBNAtlas display  - ")
 NBNAtlas_display <- NBNAtlas_display() 
  if (NBNAtlas_display ==0){  NBNAtlas_display(1) }
  if (NBNAtlas_display ==1){  NBNAtlas_display(0) }
 refresh_legendTxt(refresh_legendTxt()+1)

})      
    
mgmtSurf <- reactiveVal(NULL)
opeSurf  <- reactiveVal(NULL)
customLayer <- reactiveVal(NULL)

observeEvent(input$deleteCustomLayer,{
  cat("\nremove custom layer  -")
   updateMaterialSwitch(session, "view_custom", value = FALSE) # also when upload cancelled!
   customLayer(NULL)
   customLayerGeomType(NULL)
   notify_info("layer removed",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
},ignoreInit=TRUE)


output$fileInput_customLayer <- renderUI ({
  input$view_custom # dependency, will rest whe clicked
  fileInput("fileUpload_customLayer", label='spatial information upload2', buttonLabel=  div( img(src="upload.png", width="16px", height="16px")),placeholder ="custom layer",
                      accept= c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj",".csv"), multiple=TRUE,width ="100%")
})

observeEvent(input$view_custom,{
req(mapExists()==TRUE )
  cat("custom display  - ")
  customLayer_display <- customLayer_display() 
  if (input$view_custom==FALSE){  customLayer_display(0) }
  if (input$view_custom==TRUE){ 
    customLayer <- customLayer()
    if(is.null(customLayer)){ 
      showModal(modalDialog( size =  "m",
               tags$h3("view a custom spatial layer", style="text-align:center;"),   
                p("upload a custom spatial layer to view on the current map" , style="color:darkmagenta;vertical-align: middle;padding-bottom: 7px;text-align:center;"),
               fluidRow(
                 column(5,offset=1 , 
                        div( textInput('customLayer_name', 'spatial layer name', placeholder=  "input_shapefiles"), style="padding:0!important;"),  
                      div(style="padding-top: 32px;",
                          uiOutput("fileInput_customLayer"),
                          ),
                      strong("fill colour", style="display:inline;padding:1vw;"),
                      div(style="width:20px;display: -webkit-inline-box;",colourInput("customLayerCol",NULL,showColour ="background", returnName = TRUE,value = "#d8e470",palette = "limited",allowedCols=c("#ffe62cc2","magenta","#d8e470","#FB8861FF","turquoise"))) 
                      ),
                 column(5 , style="padding: 17px 0;font-size: 90%;", 
                         p("geographical information data can be uploaded either a table (.csv) or shapefiles (.shp/.shx, .dbf, .sbn/sbx, .prj)"),
                         p("accepted CRS: British National Grid (epsg:27700) and latitude-longitude (epsg:4326)"))),  
                       
                
               footer=tagList( div(actionButton("customLayer_submit",label="submit"), style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;"), #  
                               div(actionButton('customLayer_cancel','cancel' ),style="margin: 2px 2px 0 4px!important;white-space: nowrap;display: inline-block !important; width: fit-content; position: relative;") 
                        ) 
            ))   
  } else {
    customLayer_display(1) # if layer already uploaded just show it with toggle - can reupload a new one with button
  }
  }
},ignoreInit=FALSE,ignoreNULL=TRUE)

 
input_fileUpload_customLayer <- reactiveVal(NULL)
customLayerGeomType<- reactiveVal(NULL)

observeEvent(input$fileUpload_customLayer ,{
  req(mapExists()==TRUE)
error_message1 <- NULL
req(!is.null(input$fileUpload_customLayer))  
    shpdf <-input$fileUpload_customLayer
    cat( "\n new custom layer input file: " ) 
    if(any(file_ext(shpdf$name) %in% c( "shp", "dbf", "sbn", "sbx", "shx", "prj"))) {
      cat(" shapefile   - ")  
     NmandatoryGISFiles <- sum(as.numeric(c( "shp", "dbf", "shx") %in% file_ext(shpdf$name))) 
      
     if( NmandatoryGISFiles<3) {
       cat("shapefile not complete -  ")
        error_message1 <- "shapefiles not complete - check file formats and try again!"
        
        input_fileUpload_customLayer <- NULL  
          }  
          cat("input ok  - ")      
          input_fileUpload_customLayer <- shpdf   
       }
       
   if(is.null(error_message1) & any(file_ext(shpdf$name) %in%  "csv" & length(shpdf$name)<4))  { # if was bad shapefile just give up, only try if no weird other files
     if(length(shpdf$name)>1) {
        error_message1 <- "more files than required - subsetting single .csv" 
        cat("more files than required - subsetting .csv  - ")
        shpdf <- shpdf[which(file_ext(shpdf$name) %in%  "csv")[1],]
     }
        input_fileUpload_customLayer <- shpdf   
         cat("csv file ok  - ") 
        }      
        
if(!is.null(error_message1)) {
       notify_warning("upload cancelled" , timeout=4000,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_dbl",  
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))    
      notify_warning(error_message1 ,timeout=4000,   
                 config_notify(width="14vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=FALSE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",  
                               background =col_warn,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))    
      reset('fileUpload_customLayer')
      disable("customLayer_submit") 
     if(base::isTRUE(playSound())){
     insertUI(selector = "#placeholder_timing",  # beep.wav should be in /www of the shiny app
                 where = "afterEnd", 
                 ui = tags$audio(src = "beep-problem.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")  ) }
      
} else {  
  enable("customLayer_submit")   }
  input_fileUpload_customLayer %>% input_fileUpload_customLayer()
},ignoreInit=FALSE,ignoreNULL=FALSE)





observeEvent(input$customLayer_submit,{ 
  cat("submit custom layer  - ")
    req(!is.null(input_fileUpload_customLayer()))
        
   shpdf <-input_fileUpload_customLayer()
   input_fileUpload_customLayer(NULL)  # formats validated / filtered
     
    tempdirname <- dirname(shpdf$datapath[1])
    for (i in 1:nrow(shpdf)) {
      file.rename(shpdf$datapath[i], paste0(tempdirname, "/", shpdf$name[i]) )
    }
    indx <- grep(pattern = "*.shp$", shpdf$name)
    if(length(indx)>0){
      cat(".shp  - ")
      shpData0 <- st_read(paste(tempdirname,shpdf$name[indx],sep = "/" )) %>% st_make_valid() 
      } else { 
      cat(".csv  - ")  
      indx <- grep(pattern = "*.csv$", shpdf$name)
      shpData0 <- read.csv(paste(tempdirname,shpdf$name[indx],sep = "/" ), stringsAsFactors=FALSE) %>% as.data.frame() 
       # identify lng/lat cols..
      colnames(shpData0) <- tolower(colnames(shpData0))
      nams <- colnames(shpData0)
       lngcol <-c(which( nams=="x"), which(stringr::str_detect(nams, pattern = "lng|long|lon|longitude")==TRUE)) [1]
      latcol <- c(which(nams=="y"), which(stringr::str_detect(nams, pattern = "lat|latitude")==TRUE)) [1]
      # note - cant test on part of string with (x,y) because of Year and X.x index default
        
      colnames(shpData0)[lngcol] <- "lng"
      colnames(shpData0)[latcol] <- "lat"               
      shpData0 <- shpData0 [!is.na(shpData0$lng) & !is.na(shpData0$lat),]
      shpData0 <- shpData0 #[1:3,]# for now - takes ages?!  
 
      
       shp_crs <- st_crs(27700)
      if(any(shpData0$lng<0)) { shp_crs <- st_crs(4326) }

      shpData0 <- st_as_sf(shpData0,   coords = c("lng","lat"), crs=shp_crs)  
        }
      cat("uploaded   -") 
      shpData  <-  shpData0 %>% st_transform(crs=st_crs(4326))
 
    ## what type of geom is it
    if( any(c("POLYGON","MULTIPOLYGON") %in% st_geometry_type(shpData) )) {
        cat("poly geoms - ")
          shpData <- st_union(shpData [st_geometry_type(shpData) %in% c("POLYGON","MULTIPOLYGON"),])
          shpData <- st_crop(shpData,country_boundaries())
          "poly" %>% customLayerGeomType()  
    }
    if( any(c("POINT","MULTIPOINT") %in% st_geometry_type(shpData) )) {
        cat("point geoms - ")
          shpData <- shpData[st_geometry_type(shpData) %in% c("POINT","MULTIPOINT"),]%>% st_cast("POINT")  
           keepFeat <-  which(lengths(st_intersects(shpData ,country_boundaries())%>%suppressMessages())==1)
           shpData <- shpData[keepFeat,]
           shpData <- shpData %>% st_union()
          "point" %>% customLayerGeomType() 
    } 
      
      if(sum(which(lengths(st_intersects(shpData,country_boundaries())%>%suppressMessages())>0))==0) { # upload is not within country boundaries, cancel
        removeModal() 
       showModal(modalDialog( size =  "s",       
                     div(img(src="attention.png", width="37px", height="37px"), style="padding:4vh 0 0 0;text-align:center;vertical-align: middle;") ,
                     p(paste0("upload cancelled") , style="padding: 2vh 10px;font-size:20px;vertical-align: middle;text-align:center;") , 
                     div("the feature is not located within the country boundaries"  , style="padding:9px; vertical-align: middle; text-align: center; color: darkmagenta;"),
                        br(),
          footer=tagList(
             div(actionButton('uploadcustom_cantdo','ok' ),style="width: fit-content;display: inline-block;") 
          )))
    customLayer<-NULL  # requires changing each time so null after done
     } else {
    customLayer <- st_geometry(shpData) %>% st_combine()  %>% st_sf()
      }
  customLayer %>% customLayer()  
},ignoreInit=FALSE,ignoreNULL=TRUE)  
      
observeEvent(input$uploadcustom_cantdo,{
  removeModal() 
   cat("cant do custom layer  -") 
   updateMaterialSwitch(session, "view_custom", value = FALSE)  
   customLayer(NULL)
   customLayerGeomType(NULL)
   notify_failure("upload cancelled!",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_fail,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 ))  
     if(base::isTRUE(playSound())){
     insertUI(selector = "#placeholder_timing",  # beep.wav should be in /www of the shiny app
                 where = "afterEnd", 
                 ui = tags$audio(src = "beep-problem.wav", type = "audio/wav", autoplay = T, controls = NA, style="display:none;")  ) }
})


observeEvent(input$customLayer_cancel,{
removeModal()
  cat("cancel custom layer")
updateMaterialSwitch(session, "view_custom", value = FALSE)
customLayer(NULL)
customLayerGeomType(NULL)
   notify_info("upload cancelled",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                               background =col_info,textColor=txtCol_notif2,notiflixIconColor=txtCol_notif2 )) 
})



observeEvent(input$opeSurf,{ 
   req(mapExists()==TRUE )
   cat("opeSurf  - ")
 opeSurf_display <- opeSurf_display()  
 if (opeSurf_display ==1){  opeSurf_display(0) } 
 if (opeSurf_display ==0){
    opeSurf <- opeSurf()
    if(is.null(opeSurf)){ load_opeSurf_4326(country()) %>% opeSurf()  } 
       opeSurf_display(1) }
  
})   
             
observeEvent(input$mgmtSurf ,{ 
   req(mapExists()==TRUE )
  cat("mgmtSurf  - ")
 mgmtSurf_display <- mgmtSurf_display()  
  if (mgmtSurf_display ==1){  mgmtSurf_display(0) } 
  if (mgmtSurf_display ==0){   
      mgmtSurf <- mgmtSurf()
     if(is.null(mgmtSurf)){    load_mgmtSurf_4326(country())   %>% mgmtSurf()  }
     mgmtSurf_display(1) }
 
})   



 observeEvent(input$rivCatch,{ # display intercatch() - simplify?
   req(mapExists()==TRUE )
  req(rivCatch_display())
    rivCatch_display <- rivCatch_display() 
 cat("rivCatch display  - ") 
  
    if (rivCatch_display ==0){  rivCatch_display(1) }
  if (rivCatch_display ==1){  rivCatch_display(0) } 
 })   

   
region <- reactiveVal(NULL)
observeEvent(intercatch_on_map(),{
   unique(intercatch_on_map()$OPCAT_NAME)[1] %>% region()
# print(paste0("ope catchments :",opcats) )
# if(length(opcats)>1) {
#   region <- sample (opcats ,2)
#   region  <- paste0(region[1], "/",region[2])
# }
#  region %>% region()
cat(paste0("\nregion: ",unique(intercatch_on_map()$OPCAT_NAME)[1],"  - "))
}) 
 
 



##################################### new exploratory site ####
# any site change - compute pt buffer (final if shape=buffer, overlapping catchments if not)
observeEvent (sitedat() , {  
  req(mapExists()==TRUE )
  cat("\nnew site  - ")
   
  req(country_boundaries())
         country_boundaries <- country_boundaries()
         rv_sim$candsite_lab <- "exploratory site"
         rv_sim$candcatch_col <- "transparent" # or else changing from catch to buff after generating a site makes all catch boundaries black

         val <- reactiveValues(clickx =NULL, clicky =NULL ) 
         rv_dis$pts_bng <- rv_temp$pts_ll <-   rv_dis$fam.start <- NULL  
         rv_temp$clickCount_pts <- 0 # new site new click stock
         initValues(NULL)
         rv_temp$validatedPts <-   rv_temp$ptstp_nro <-0 # 
         Nfams_init_actual(NULL)    # this and up allows resetting Nfams actual disaplyed on UI 
               selected_catchments_names(NULL)   
               rv_temp$pt_buffer <-  NULL
               exploSite(NULL) 
               exploSite_catchments(NULL) 
               local_suitras(NULL)  
         req(!is.na(sitedat()$click_lat))
               cat(" effective click  - ")
               click_lng <- sitedat()$click_lng
               click_lat <- sitedat()$click_lat
               pt_buffer <-  st_buffer(st_transform(st_sfc(st_point(c(  click_lng, click_lat) ), crs = st_crs(4326))  , st_crs(27700)), sitedat()$buffer_val) 
               pt_buffer_focus <- st_buffer(pt_buffer, 2000)  # focus is larger (for MAP fosuc)
               rv_temp$pt_buffer_merc <- pt_buffer_focus # visualise surrounding suit to allow comparison when chossing site
               sf_use_s2(FALSE)
               rv_temp$pt_buffer <-  st_intersection(st_transform(pt_buffer,crs=st_crs(4326)),country_boundaries)%>%suppressMessages()
               rv_temp$pt_buffer_focus <-  st_intersection(st_transform(pt_buffer_focus,crs=st_crs(4326)),country_boundaries)%>%suppressMessages()
               cat("site boundaries  - ")  
                      if(input$shape == "buffer") {## current map of country with point and buffer
                                  if(!is.null(pt_buffer)){
                                        cat("site is a buffer  - ")
                                         region <- region() 
                                         rv_temp$pt_buffer %>% exploSite()
                                         paste0("total area is approx. ",round(as.numeric(st_area(rv_temp$pt_buffer)*1e-4)),  "ha") %>% siteArea_txt()
                              
                                   }
                        }
                      if(input$shape == "catchment") {  
                                  cat("site is built with catchments  - ")
                                  exploSite(NULL) 
                                  exploSite_catchments(NULL) 
                        }
      cat("reset map  - ")                   ## clear all to do with setup but leave dranw shapes? in case.. - legend not matching with that idea here !!!!!!!!!!!
      leafletProxy("mapReleaseSite", session) %>%  
                             clearGroup("selected site") %>%  clearGroup("buffer_pt") %>%  clearGroup("selected catchments") %>%  clearGroup("table_hilight")   
       click_Site_disabled(FALSE)
       rv_mapctry$release_design_infotext <- paste0("release parameters")
        
}, ignoreInit=FALSE, ignoreNULL=TRUE ) 
   
Ncatch_txt <- reactiveVal(NULL)     
siteArea_txt <- reactiveVal(NULL)
observe({
  suff <- NULL
   if (country() == "Scotland"){suff <- "main catchment #"}
   if (input$shape == "catchment" & !is.null(region())){
rv_init$infotext_releaseLoc <-   p(HTML(paste0(Ncatch_txt(), " selected in the region of ",suff , region(), 
                                              br(),"total area approx. ",siteArea_txt(), br(),  siteK())))
}
  if (input$shape == "buffer" & !is.null(region())){
rv_init$infotext_releaseLoc <-  p(HTML(paste0("buffer of ", as.numeric(input$buffer_sel)/1000, "km radius in the region of ",suff , region(),  
                                              br(),siteArea_txt(),  br(), siteK() ))) 
                                                
  }
  if (input$shape == "none"){  
  rv_init$infotext_releaseLoc <-  p(HTML(paste0("no exploratory site used"))) 
  } 
   
})

 

#####################################  catchments options - display selected names  #### 444
observeEvent(selected_catchments_names(),{ 
  req(sitedat()$shape)
  exploSite_catchments <- exploSite <- NULL
  if(sitedat()$shape == "catchment") {
      cat("show selected catchments    - ")    
      if(length(selected_catchments_names())>0) { 
         
        exploSite_catchments <- intercatch_on_map()[ intercatch_on_map()$WB_NAME %in%  selected_catchments_names() ,]  
        exploSite <-  exploSite_catchments %>% summarize(do_union=TRUE)%>%st_as_sfc()%>%suppressMessages() ## %>% st_union()   #avoids warning       # rv_temp$pt_buffer_focus <- rv_temp$pt_buffer_focus used for filtering catchments 
        Ncatch <- length(selected_catchments_names())
            if(Ncatch==1) {Ncatch_txt <- paste0(Ncatch, " catchment")}
            if(Ncatch>1) {Ncatch_txt <- paste0(Ncatch, " catchments")} 
        Ncatch_txt %>% Ncatch_txt()
        paste0( round(as.numeric(st_area(exploSite )*1e-4)),"ha.") %>% siteArea_txt()
        
 #          rv_init$infotext_releaseLoc <-  p(HTML(paste0(Ncatch_txt, " selected in the ", region(), " region",br(),"total area approx. ",round(as.numeric(st_area(exploSite )*1e-4)),"ha.")))
              
      
          } else { # none selected yet
         paste0("No potential exploratory site currently selected.") %>% txt_site_selected()
         rv_init$infotext_releaseLoc <-  "not defined" 
         cat("none  - ")
          } 
        st_agr(exploSite_catchments)<- "constant" 
        exploSite_catchments %>% exploSite_catchments() 
        exploSite %>% exploSite() 
  }
  
    if(sitedat()$shape == "buffer") {
    intercatch_on_map()[ which(lengths(st_intersects(intercatch_on_map(),rv_temp$pt_buffer_focus)%>%suppressMessages())>0),]  %>% exploSite_catchments()
      # meant to not changing exploSite()
      }
  
  
}, ignoreNULL=FALSE, ignoreInit=FALSE)

selected_catchments_names <- reactiveVal() 
nonselected_catchments_names <- reactive ({intercatch_on_map()$WB_NAME[ !intercatch_on_map()$WB_NAME %in%  selected_catchments_names()]    } )
  
output$selected_catchments_names<- renderUI ({
  if(length(selected_catchments_names())==0) return( div(HTML(paste0("( none selected )")),style="font-size:95%;color:#555;"))
  div(HTML(paste0(selected_catchments_names(), sep= "<br>")),style="color:#555;")  
   })

output$nonselected_catchments_names<- renderUI({ 
    if(length(nonselected_catchments_names())==0) return( div(HTML(paste0("( all catchments selected )")),style="font-size:95%;color:#555;"))

        div(HTML(paste0(nonselected_catchments_names(), sep= "<br>")),style="color:#555;")  
})
 
output$catchSelection_title <- renderUI({ 
    if(base::isTRUE(click_Site_disabled()))  return(
      column(10,offset=1,div("Catchments contained in exploratory site",style="font-weight:bold;text-align:left;font-size:90%;"))   )
      column(10,offset=2,div("current catchment selection",style="font-weight:bold;text-align:center;font-size:90%;"))  
   })

 


intercatch_on_map_plot <- reactive({
  req(intercatch_on_map())
return(intercatch_on_map()[intercatch_on_map()$WB_NAME %in% nonselected_catchments_names(),])
})

intercatchLabs_plot    <- reactive({ 
      suitLabs <- as.character( paste0( "<span style=' color:darkmagenta;line-height:0.5em!important;font-weight:bold;margin-left:7px;;text-align:left;'>",
                          intercatch_on_map_plot()$WB_NAME,"</span><br>
                           <span style=' color:#008080;line-height:0.5em!important;font-weight:normal;margin-left:7px;'>approx.",
                          intercatch_on_map_plot()$habLabs)) 
      suitLabs <-  sprintf( "%s",suitLabs) %>%  lapply(htmltools::HTML)   
    return(suitLabs)
  
}) 
 
 


##################################### filter suitable hab within selected catchments  - simplified geoms, for mapping
local_suitras_catch <- reactive({
        req(intercatch_on_map())
      ca <- NULL
        if(!is.null(intercatch_on_map())){
       ca<-suitablePatches()[suitablePatches()$WB_NAME %in% unique(intercatch_on_map()$WB_NAME),] # filter by catch id not coverage =faster
        }
       return(ca)
}) 
##################################### selected catchments ####
intercatch_on_map <- reactive({  
  req(intercatch_filter())
 # cat("filter river catchments  - ")
  return(intercatch()[intercatch_filter(),] )
})
  
 
intercatch_filter <- reactive({ 
  req(intercatch())
  rv_temp$pt_buffer_focus
  req(!is.null(rv_temp$pt_buffer_focus))
   fil <- as.numeric(which(lengths(st_intersects(intercatch(), rv_temp$pt_buffer_focus )%>%suppressMessages())>0) )
  return(fil)
 }) # buff otherwise extends catch selection area with each click.. # & input$shape == "buffer"?
   
      

################################################# link buttons with help/download/diff tab action     
observeEvent({
  input$ask_siteGeometry
  input$ask_relPoints 
  input$ask_groupChars 
  input$ask_timing 
  input$ask_landscape 
   1
  },{    
         updateTabsetPanel(session,"navBar",selected = "about") 
         updateNavlistPanel(session,"aboutTabs", selected = "Initial Set-up")    
}, ignoreInit=TRUE, ignoreNULL=TRUE)
   
observeEvent(input$ask_siteGeometry,{ updateTabsetPanel(session,"setup_ask", selected = "site")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 

observeEvent(input$ask_landscape,{    updateTabsetPanel(session,"setup_ask", selected = "landscape")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 

observeEvent(input$ask_groupChars,{   updateTabsetPanel(session,"setup_ask", selected = "groups")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 

observeEvent(input$ask_timing,{       updateTabsetPanel(session,"setup_ask", selected = "timing")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 

observeEvent(input$ask_relPoints,{    updateTabsetPanel(session,"setup_ask", selected = "points")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 

observeEvent(input$ask_app,{  
  updateTabsetPanel(session,"navBar",selected = "about") 
  updateNavlistPanel(session,"aboutTabs", selected = "Acknowledgements")  
},ignoreNULL=TRUE, ignoreInit=TRUE) 


observeEvent(input$ask_app0,{  
 cat("Acknowledgements?")  
    shinyjs::show(selector ="#show_author")   
},ignoreNULL=TRUE, ignoreInit=TRUE) 
 

 
observeEvent(input$ask_resOutput,{ 
         updateTabsetPanel(session,"navBar",selected =  "about" )  
         updateNavlistPanel(session,"aboutTabs", selected = "Simulation Outputs")    
}, ignoreInit=TRUE, ignoreNULL=TRUE)
 



observeEvent(input$ask_simBtns,{ 
         updateTabsetPanel(session,"navBar",selected = "about") 
         updateTabsetPanel(session,"aboutTabs", selected = "Action Buttons")  
}, ignoreInit=TRUE, ignoreNULL=TRUE)

 
observeEvent(input$ask_legend ,{ 
         updateTabsetPanel(session,"navBar",selected = "about")  
         updateNavlistPanel(session,"aboutTabs", selected = "Map Legend")    
}, ignoreInit=TRUE, ignoreNULL=TRUE)

 
## switch panel tab to check data input          
observeEvent(input$view_input,{         
   updateTabsetPanel(session, "navBar", selected = "input summary")  
}, ignoreInit=TRUE, ignoreNULL=TRUE)
 
  
   
################################################# download output ###########
filelist <- NULL
output$save_layout = downloadHandler(
           filename = function(){
             saveName_final <- saveName_final()
            return( paste0("SimLayout",saveName_final,".zip")) },
           content = function(file){
                simName <-  saveName_final() #saveName_placeholder()
                initValues <- NULL
            if(!is.null(initValues())) { initValues <- as.data.frame(initValues()) %>% st_as_sf(coords = c("lng", "lat"), crs= st_crs(4326))}   
                sitePoly <- exploSite()
                file=file
                FUN_save_layout(simName, filename, file, initValues, sitePoly, "only")
           },
 contentType = "application/zip"
  )   
 

 
output$save_metadata= downloadHandler(
           filename = function(){
             saveName_final <- saveName_final()
            return( paste0("SimMetadata",saveName_final,".zip")) },
           content = function(file){
                 simName <- saveName_final()# saveName_placeholder() 
                 newFeaturesDat_shp <- newFeaturesDat_shp() 
                 modify_landscape   <- newFeaturesDat_shp %>% filter(layerType ==  "modify habitat"  )  
                 obs_records        <- newFeaturesDat_shp %>% filter(layerType ==  "observation records" )  
                 area_of_interest   <- newFeaturesDat_shp %>% filter(layerType ==  "area of interest"  )   
                 if(nrow(modify_landscape)==0) {modify_landscape <- NULL}
                 if(nrow(obs_records)==0) {obs_records<- NULL}
                 if(nrow(area_of_interest)==0) {area_of_interest <- NULL} 
                 file=file
                 FUN_save_metadata(simName, filename, file, modify_landscape, obs_records, area_of_interest, "only")
           },
 contentType = "application/zip"
  )   
   

output$save_simpop = downloadHandler( 
           filename = function(){
             saveName_final <- saveName_final()
                return( paste0("SimulatedBeaverPop",saveName_final,".zip")) },
            content = function(file){
                 simName <-  saveName_final()#saveName_placeholder()
                 simParams <-  sim_data() 
                 simpop <- datSave_sim_pop()
          
                simParams[[1]][1] <- simName
                if(!is.null(simpop)) {
                 simpop_shp <- subset(simpop$simpop_shp  , select=-labs_sim) %>% st_transform(crs= st_crs(4326))
                 recapPlot_summAdt <- simpop$recPlot_summN_gg 
                 recapPlot_summFam <- simpop$recPlot_summFams_gg 
                 recTable_annual_stats <- simpop$recTable_annual_stats
                 simParams <- simParams
                 }   
              file=file
                FUN_save_simpop(simName,
                                filename, file, 
                                simpop_shp,
                                recapPlot_summAdt,recapPlot_summFam, recTable_annual_stats,
                                simParams,
                                "only")
           },
 contentType = "application/zip"
)
 
   



output$save_settleTest = downloadHandler(
           filename = function(){
             saveName_final <- saveName_final()
            return( paste0("SettlmntTest",saveName_final,".zip")) },
            content = function(file){
                simName <- saveName_final()#saveName_placeholder()
                settleTest <- datSave_settleTest() # routes   
                file=file 
                FUN_save_settleTest (simName, filename, file, settleTest, "only" ) 
           },
 contentType = "application/zip"
  ) 

 
     

 
############## ABOUT tab text      ####
output$refs         <- renderText({paste0( ref_SNHReport2020,ref_SNHReport2015, ref_SNHReport1997,ref_SNHReport_fromRep1,ref_SNHReport_fromRep2 )})
  
  
output$AboutDownloading <- renderText({ AboutDownloading})
output$AboutOutput_f    <- renderText({ AboutOutput_f  })
output$AboutOutput0     <- renderText({ AboutOutput0  })
output$AboutOutput1a    <- renderText({ AboutOutput1a })
output$AboutOutput1b    <- renderText({AboutOutput1b } )
output$AboutOutput2a    <- renderText({ AboutOutput2a  })
output$AboutOutput2b    <- renderText({ AboutOutput2b })

## description demog
output$AboutPars_fams0  <- renderText({ AboutPars_fams0  }) 
output$AboutPars_fams1  <- renderText({ AboutPars_fams1  }) 
output$AboutPars_fams2  <- renderText({ AboutPars_fams2  })
output$AboutPars_fams3  <- renderText({ AboutPars_fams3  })
output$AboutPars_fams4  <- renderText({ AboutPars_fams4  })
        
output$AboutPars_suit <- renderText({ AboutPars_suit }) 
output$AboutPars_disp <- renderText({ AboutPars_disp })
output$AboutPars_surv <- renderText({ AboutPars_surv })
output$AboutTheModel  <- renderText({ AboutTheModel  })


NBNAtlas <- reactiveVal(NULL)
provider_ID<- reactiveVal(NULL)
##################################### visualise - NBN Atlas  ####
observeEvent(input$NBNAtlas,{    ### this doesnt work well! create indiv reactives for datasets then add independently
    cat("NBNAtlas layer  - ")
   NBNAtlas_display <- NBNAtlas_display()
   if (NBNAtlas_display ==1){  
        cat("add")  
        pts         <- NBNAtlas()
        provider_ID <- provider_ID()    
    if(is.null(NBNAtlas())) {
               pts <-  read.csv(here::here("metadata/clean_data/NBNAtlas_UK_cleanDat_Jul23.csv"), stringsAsFactors=FALSE)
               pts <- pts[pts$cntry == rv_mapctry$bnd_cntry$country,] 
               provider_ID <- read.csv(here::here("metadata/clean_data/NBNAtlas_UK_provider_ID.csv"), stringsAsFactors=FALSE ) 
               provider_ID <-provider_ID[provider_ID$cntry == rv_mapctry$bnd_cntry$country,] 
               pts$provider <- as.factor(pts$provider)
                pts %>% NBNAtlas()
                provider_ID %>% provider_ID()
              }
              palette_providers <- colorFactor(provider_ID$provider_col,domain=   pts$provider) 
              leafletProxy("mapReleaseSite", session) %>% #removeControl("NBN Atlas records") %>%
                            addCircleMarkers(   lng =   pts$X,  lat =   pts$Y,
                                               fillColor =  palette_providers(pts$provider), fillOpacity =  pts$recent,  opacity =  pts$recent,   stroke = 1, color="white",
                                               radius = round(pts$npts_cex/2),
                                               popup = paste0("Source: ", pts$provider, ", ", pts$year, "<br>",  "Dataset: ",pts$dataset,  "<br>",
                                                            pts$recs ), group="NBN Atlas records")                                         
   } ### original NBNAtlas dataset is >39k rows - agrgegated in script

   if (NBNAtlas_display ==0){  
      leafletProxy("mapReleaseSite", session) %>% clearGroup( "NBN Atlas records") %>% removeControl(layerId="NBN Atlas records")  %>% removeTiles("NBN Atlas records")    
   }
},ignoreInit=TRUE, ignoreNULL=FALSE)  
 

#landCoverMap        <-   "https://digimap.edina.ac.uk/mapproxy/wms/environment-licensed/d185c4c2d88ff2ee8b311cfce5873de390673242f1b6d83af2d8abba05c2fcdc?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities"

 #landCoverMap        <-"https://catalogue.ceh.ac.uk/maps/e4b36270-8246-4afa-8eff-6781210a0477?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=WMS&format=image/png&STYLE=inspire_common:DEFAULT"


#landCoverMap        <-"https://catalogue.ceh.ac.uk/maps/e4b36270-8246-4afa-8eff-6781210a0477?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=LC.WoodedRiparianZones&format=image/png&STYLE=default"
  landCoverMapLegend <-  "https://catalogue.ceh.ac.uk/maps/e4b36270-8246-4afa-8eff-6781210a0477?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=WMS&format=image/png&STYLE=inspire_common:DEFAULT"
 
 #andCoverMap        <-"https://catalogue.ceh.ac.uk/history/39157337ff076ed12a1ac2dbfc4a01f2779e9d3e/e4b36270-8246-4afa-8eff-6781210a0477"
 
 #landCoverMap        <-"https://catalogue.ceh.ac.uk/maps/e4b36270-8246-4afa-8eff-6781210a0477?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=LC.WoodedRiparianZones&format=image/png&STYLE=default"
 #https://catalogue.ceh.ac.uk/maps/e4b36270-8246-4afa-8eff-6781210a0477?request=GetCapabilities&service=WMS&cache=false
 #landCoverMap       <- "https://catalogue.ceh.ac.uk/maps/315e9cbb-efdc-4726-bf97-bd9176af3c9d?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=LC.25m.GB&format=image/png&STYLE=default"
 #landCoverMapLegend <- "https://catalogue.ceh.ac.uk/maps/315e9cbb-efdc-4726-bf97-bd9176af3c9d?language=eng&version=1.3.0&service=WMS&request=GetLegendGraphic&sld_version=1.1.0&layer=WMS&format=image/png&STYLE=inspire_common:DEFAULT"


##################################### visualise - LCM  #### 
    landCoverMap <- "https://digimap.edina.ac.uk/mapproxy/wms/environment-licensed/d185c4c2d88ff2ee8b311cfce5873de390673242f1b6d83af2d8abba05c2fcdc?SERVICE=WMS&VERSION=1.3.0&REQUEST=GetCapabilities"
   observeEvent(input$landCoverMap,{    ### this doesnt work well! create indiv reactives for datasets then add independently
  req(mapExists()==TRUE ) # see about here or not, dont load at first for now
 
 LCM_display <- LCM_display()
  cat("LCM ")
  if (LCM_display ==1){  
    cat("map  -")
leafletProxy("mapReleaseSite", session) %>%   
  addWMSTiles(landCoverMap,
    layers ="LCM2019_vector_GB",group="land cover map", 
    options = WMSTileOptions(version="1.3.0",format = "image/png", transparent = TRUE),
    attribution = "Land cover map | Based upon LCM2020 © UKCEH 2021"  ) 
} else { 
    cat("removed  -")
    leafletProxy("mapReleaseSite", session) %>%   clearGroup("land cover map") %>%  removeControl("land cover map") 
   }
})
  
   
 
##################################### GIS attribution catchments boundaries  ####

# country <- c("Scotland","Wales","England") [country_indx] 
cntry_attribution <- reactive({
  req(country_indx())
  return(
  c(  as.character(paste0(" River catchments | © SEPA. Some features of this information are based on digital spatial data licensed from the Centre for Ecology and Hydrology © NERC (CEH). Contains OS data © Crown copyright [and database right]."))
      ,
      as.character(paste0(" River catchments | Contains Natural Resources Wales information © Natural Resources Wales and Database Right. All rights Reserved. Contains Ordnance Survey Data. Ordnance Survey Licence number AC0000849444. Crown Copyright and Database Right. Derived in part from 1:50,000 and 1:250,000 scale digital data under permission from British Geological Survey. ©NERC."))
      ,
      as.character(paste0(" River catchments | © Environment Agency copyright and/or database right 2015. All rights reserved."))
  )[country_indx()]
)
})
  

observe({
  if(any(c(rivCatch_display(),opeSurf_display(),mgmtSurf_display())==1)) {
cat("catch attr  - ")
leafletProxy("mapReleaseSite", session) %>% addTiles(urlTemplate = "", attribution = cntry_attribution(), group="river catchments attribution")  
} else {
  leafletProxy("mapReleaseSite", session) %>% clearGroup("river catchments attribution")
}
})
      
##################################### visualise - catchments boundaries  ####

#rivCatch_display
#mgmtSurf_display
#opeSurf_display

  
observeEvent(rivCatch_display(),{     
  req(mapExists()==TRUE ) # see about here or not, dont load at first for now
  rivCatch_display <- rivCatch_display()
  cat("rivCatch display  - ")
  if (rivCatch_display ==1){   
    if(country() == "Scotland") {
       cat_typ2 <- "main catchment "
       cat_typ1 <- "sub-bassin "
      }else {
       cat_typ2 <- "operational catchment "
       cat_typ1 <- "management catchment "
      }
    
  leafletProxy("mapReleaseSite", session) %>%   
  addPolylines(options = pathOptions(pane ="rivCatch" ),
               data=  intercatch(),  group="river catchments boundaries", color="magenta", weight=2,opacity=1,
               popup= ~ sprintf(  "%s %s %s %s", 
                        paste0("<p style='margin:4px;font-size:11px;color:magenta;text-align: center;'>river catchment <span style='text-align:left;color:black;'>", 
                               WB_NAME,"</span></p>"),
                        paste0("<p style='margin:4px;font-size:11px;color:#93a94c;text-align: center;'>",cat_typ2,"<span style='text-align:left;color:black;'>", 
                               OPCAT_NAME,"</span></p>"),                                     
                        paste0("<p style='margin:4px;font-size:11px;color:orange;text-align: center;'>",cat_typ1,"<span style='text-align:left;color:black;'>", 
                               MNCAT_NAME,"</span></p>"),                                     
                        paste0("<p style='margin:4px;font-size:11px;color:#008080;text-align: center;'>hydromorphology pressure <span style='text-align:left;color:black;'>", 
                               hydromorph,"</span></p>"))   %>%  lapply(htmltools::HTML) 
  ) 
  
  } else {  
     cat("clear rivCatch  - ")
    leafletProxy("mapReleaseSite", session) %>%   clearGroup("river catchments boundaries")   
  }
}) 
   


observeEvent(customLayer(),{     
  req(mapExists()==TRUE) # see about here or not, dont load at first for now
  removeModal() 
  leafletProxy("mapReleaseSite", session) %>% clearGroup("custom layer")  
   
  req(!is.null(customLayer()))
  req(!is.null(customLayerGeomType()))
       notify_success("new layer added",timeout=5100,  
                 config_notify(width="12vw",borderRadius="50px", cssAnimationStyle="from-right",pauseOnHover = TRUE ,clickToClose=TRUE,showOnlyTheLastOne=TRUE,
                               fontFamily ="Comfortaa", fontSize ="clamp(10px,.7vh,.8vh)",className="lower_notifs_sgl", 
                                background =col_succ,textColor=txtCol_notif,notiflixIconColor=txtCol_notif ))  
 customLayer_display <- customLayer_display()
    layername <- "custom layer"
  if(input$customLayer_name!=""){layername <- input$customLayer_name}
  
  cat("customLayer display  - ")  
  
    if(customLayerGeomType()=="poly"){  
  leafletProxy("mapReleaseSite", session) %>%   
  addPolygons( options = pathOptions(pane ="customPoly"),
               data=  customLayer(),  group="custom layer", color="white",opacity=1, fillColor=input$customLayerCol,weight=1, fillOpacity=.6,
              label=layername , 
              labelOptions = labelOptions(direction = "top" ,offset=c(0,-30), style = as.list(c("background-color"="white","color"="#777" ,labstyle_plotLabs))  ))     }
   
    if(customLayerGeomType()=="point"){ 
      if(layername == "custom layer") {layername <- "custom point"}
      cat("map custom pts  - ") 
      customLayer <-  st_cast(st_geometry(customLayer()), "POINT")   
      
      leafletProxy("mapReleaseSite", session) %>%   
     addCircleMarkers( data= customLayer ,  group="custom layer",color="white",opacity=1, fillColor=input$customLayerCol,radius=7, fillOpacity=1,
              label=layername,   options = pathOptions(pane = "stranded" ),
              labelOptions = labelOptions(direction = "top" ,offset=c(0,-30), style = as.list(c("background-color"="white","color"="#777" ,labstyle_plotLabs))  ))
      }  
 customLayer_display(1) # first time laoding the custom layer, display trigger  
},ignoreNULL=FALSE) # so does removeModal() if null too  



observeEvent(customLayer_display(),{
req(mapExists()==TRUE) 
  customLayer_display <- customLayer_display()
  if(customLayer_display ==1 ){
     leafletProxy("mapReleaseSite", session) %>%   showGroup("custom layer")   
    } else {  
     cat("clear customLayer  - ")
    leafletProxy("mapReleaseSite", session) %>%   hideGroup("custom layer")
  }
})  



observeEvent(opeSurf_display(),{     
  req(mapExists()==TRUE) # see about here or not, dont load at first for now
  opeSurf_display <- opeSurf_display()
  cat("opeSurf display  - ")
  if (opeSurf_display ==1){   
  leafletProxy("mapReleaseSite", session) %>%   
  addPolylines(options = pathOptions(pane ="opeSurf"),
               data=  opeSurf(),  group="opeSurf catchments boundaries", color="#0b9f9f",   weight=3, opacity=1, 
               popup= ~ sprintf(  "%s %s", 
                        paste0("<p style='margin:4px;font-size:11px;color:#0b9f9f;text-align: center;'>operational catchment <span style='text-align:left;color:black;'>", 
                               OPCAT_NAME,"</span></p>"),                                     
                        paste0("<p style='margin:4px;font-size:11px;color:orange;text-align: center;'>management catchment <span style='text-align:left;color:black;'>", 
                               MNCAT_NAME,"</span></p>") )   %>%  lapply(htmltools::HTML) 
  )    
  } else {  
     cat("clear opeSurf  - ")
    leafletProxy("mapReleaseSite", session) %>%   clearGroup("opeSurf catchments boundaries")   
  }
})  
 
 

observeEvent(mgmtSurf_display(),{     
  req(mapExists()==TRUE) # see about here or not, dont load at first for now
  mgmtSurf_display <- mgmtSurf_display()
  cat("mgmtSurf display  - ")
  if (mgmtSurf_display ==1){   
  leafletProxy("mapReleaseSite", session) %>%   
  addPolylines(options = pathOptions(pane ="mgmtSurf"),
               data=  mgmtSurf(),  group="mgmtSurf catchments boundaries", color="#ffb75a", opacity=1,   weight=4,
               popup= ~ sprintf(  "%s", 
                        paste0("<p style='margin:4px;font-size:11px;color:#ffb75a;text-align: center;'>management catchment <span style='text-align:left;color:black;'>", 
                               MNCAT_NAME,"</span></p>") )   %>%  lapply(htmltools::HTML) 
  ) 
  } else {  
     cat("clear mgmtSurf  - ")
    leafletProxy("mapReleaseSite", session) %>%   clearGroup("mgmtSurf catchments boundaries")   
  }
})  
  
 
  
 
  
################ handy for dev but remove for deploy
 #   session$onSessionEnded(stopApp)  
 
######### disconnect onClose #########  
 session$onSessionEnded(function() {
   plan(sequential)
    suspendInterrupts({
        cat(sprintf("Closing session %s\n", session$token))
       session$close() 
     })
 })
 

### https://rdrr.io/cran/shinydisconnect/man/disconnectMessage.html#google_vignette

#options(shiny.launch.browser=FALSE)

#session$onSessionEnded(function() { unlink(tempdir(), recursive = TRUE) }) # remove temp files used for dl

session$onFlushed(function() {
  session$sendCustomMessage(type = "activate_tooltips", "")
  cat("\n--FLUSH* ")
  }, once = TRUE )  
     
observe({
  input$show_help
  req(mapExists()==TRUE)
  cat(" toggle tooltips display  - ")
 if(input$show_help=="hide") {source(here::here("toolTips/ui_removeTooltips.R"), local=TRUE)}  
 if(input$show_help=="show") {source(here::here("toolTips/ui_addTooltips.R"), local=TRUE)}   
})  
 
sim_data <- reactiveVal(NULL)
sim.time <- reactiveVal(NULL)

 observeEvent(sim_pop(),{
    req(!is.null(sim_pop()))
    country =country() 
    Nfeat_modifhab =0#ifelse(!is.null(Nfeat_modifHab()),Nfeat_modifHab(),0)
    Nfeat_records =ifelse(!is.null(Nfeat_records()),Nfeat_records(),0) 
    Nfeat_areaOfInt =ifelse(!is.null(Nfeat_areaOfInt()), Nfeat_areaOfInt(),0) 

simTime_m <- round(as.numeric(difftime(Sys.time()+60*60, sim.time(),  units = c( "mins" ))),1)
saveName_final =saveName_final()
 sim_data <- data.frame(name=saveName_final,
                           country=country,
                           date = Sys.Date(),
                           Nth_sim=session$input$start_sim,

                           simTime_m=simTime_m, 
                           sim_Nreps=session$input$sim_Nreps,
                           sim_Nyrs=session$input$sim_Nyrs,
                           operation=session$input$operation, 

                           shape= session$input$shape,
                           buffer_sel=session$input$buffer_sel,
 
                           Nfams_init= session$input$Nfams_init, 
                           demog=session$input$demog,
                           timing=session$input$timing,

                           relPts_method=session$input$relPts_method,
 
                           draw=as.numeric(session$input$metadata_draw),
                           upload=as.numeric(session$input$metadata_upload),
                           select=as.numeric(session$input$metadata_select),
                           obs=Nfeat_records,
                           Nmodif=  Nfeat_modifhab,
                           NareaOfInt =Nfeat_areaOfInt, 
                           try_settling =session$input$try_settling,
                           dl_meta =length(session$input$save_metadata)  
                             )

 if(sim_data$operation == "observation"){ sim_data[,c(11,12,13,14,21)] <- NA}
 if(sim_data$shape == "none"){ sim_data[,c(9,10)] <- NA}

 colnames(sim_data) <-  c( 
  "simulation ID",   "country","date", "Nth sim","simulation duration (mins)",
  "sim Nreps", "sim Nyrs",  "operation", 
  "exploratory site","explo site buffer (m)",  
  "trans N groups",  "trans demog" ,"trans timing" ,  "trans loc method",    
  "metadata draw" , "metadata upload","metadata select","Nfeat obs recs" , "Nfeat modified hab" , "Nfeat areaOfInt",   
   "settlmt test" , "dl metadata"   )     
  sim_data <- as.data.frame(sapply(sim_data, function(x) gsub("\"", "", x)))
  colnames(sim_data) <-NULL 
  sim_data  %>% sim_data()
 
 })
 
#observe({
#outs <- outputOptions(output)
#lapply(names(outs), function(name) {
#  outputOptions(output, name, suspendWhenHidden = FALSE) 
#})
#print(length(names(outs)))
##193
#print(length(unique(names(outs))))
#print(paste0("mapExists ==",mapExists())) 
#})
 
 shinyjs::show(selector ="#div_txt_start",    anim=TRUE, animType="fade"  ) 
 shinyjs::hide(selector ="#div_startW",    anim=TRUE, animType="fade", time=1   ) 
    delay(2500,   shinyjs::show(selector ="#launchP_selcntry", anim=TRUE, animType="fade" ))

 
 
 delay(4000, shinyjs::show(selector ="#div_txt_prog",    anim=TRUE, animType="fade", time=1 )) #progress will be displ.. 
  
  
delay(2000,
      shinyjs::show(selector ="#div_howto1",    anim=TRUE, animType="fade", time=1 )) 
delay(3000,
     shinyjs::show(selector ="#div_howto2",    anim=TRUE, animType="fade", time=1 )) 
delay(4000,
      shinyjs::show(selector ="#div_howto3",    anim=TRUE, animType="fade", time=1 )) 
delay(5000,
      shinyjs::show(selector ="#div_howto4",    anim=TRUE, animType="fade", time=1 )) 
  
  
  
  
  
   
  
 
}



#### Run App ####
 shinyApp(ui = ui, server = server )
 