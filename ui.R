                                       
                                                    
####  
# UI ####
####                                        
ui <- fluidPage(    
             shinyjs::useShinyjs(),
             title ="BEAVER POP SIMULATION",
             shinytitle::use_shiny_title(),
             busy_window_title("BEAVER POP SIMULATION.."),
             tags$head(tags$script(paste0("Shiny.addCustomMessageHandler('activate_tooltips', function(x) {","$('[data-toggle=\"tooltip\"]').tooltip();})"))  ), 
             includeCSS(here("www/aesthetics.css")),
             tags$head(tags$link(rel="stylesheet",href="https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css")),
             tags$style(HTML('#password {max-width: max-content !important;font-family: monospace;padding: 0 15px;min-width: 247px;margin: 4px 0 0 0;}')), 
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
     
    
             
##### launch page #####           
   tags$div(id="show_launchPage",  style="z-index:20;position:absolute;background-color:#ffe62c52!important;",
   navbarPage(title=div( img(src="beaver-facing-right-small_sh_green.png", width="auto", height="auto",id="beav0", class="animate__animated animate__bounce animate__delay-2s",
                               style="margin-left:6vw;width:clamp(.7*50px,.8vw,1vw) ;height:clamp(.7*30px,.8vw,1vw);"),
                            appTitle ),  position = "fixed-top", 
     tabPanel(div(style="font-weight: 100; font-size: 81%;", "v1-june2024" , 
                   actionButton("ask_app0", "?", inline=TRUE, class="btn_Ask" , class="btn_AskApp",style="margin-top: 0 !important;") ),
                   id="tab0", 
            
                 hidden(  
                  tags$div(id="show_author", div(class="auth",style="font-size: 11px;position:absolute;color:magenta;top:10px;z-index:40;margin-left:66vw;font-weight: bolder;", "an app developed by .zelda.", span( "github.com/zeldavanderwaal", style="font-weight:normal!important;margin-left:17px;color:yellow;") ))
                  ), 
              
                br(),
                tags$div(id="prelaunchPage_ui", 
                         p( "initialising data..", style="padding-top:46vh;margin-left: 47vw; width: 300px;color: magenta;position: absolute;z-index:0;"),
                         div(img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'),  
                             style="padding-top:52vh;padding-left:48vw;z-index:0;position:absolute")  ), 
                launchPage_ui_txt 
               )
     )) 
     ,  
   
  

##### first tab - app starting page #####             
    
  hidden(
  tags$div(id="show_panels",    
  navbarPage(title=div(img(src="beaver-facing-right-magenta.png", width="auto", height="auto",id="beav", class="animate__animated animate__bounce animate__delay-5s", 
                    style="margin-left:6vw;width:clamp(.7*50px,.8vw,1vw) ;height:clamp(.7*30px,.8vw,1vw);"),
                    appTitle),
                    id="navBar",position = "fixed-top",
  
  tabPanel("spatial layout",   
           div(id = 'placeholder_timing'), # sound effects
   
           hidden( tags$div(id="pretab2_ui",
                     p(id="initLoadingTxt2","loading country data..", style="padding-top:47vh;padding-left:47vw;color:#ffe62cc2;z-index:0;position: absolute;"),
                     div(img(src="https://media.tenor.com/POZpgMB2XzgAAAAi/neon-yellow-neon-pink.gif", height='40px', width='40px'),  style="padding-top:52vh;padding-left:49vw;position:absolute") 
                     )), 

          tags$div(id="leafmap", leaflet::leafletOutput("mapReleaseSite" , height="100vh")  ),  

##### model running panel tab #1 #####
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
 
##### buttons panel ####
  conditionalPanel( condition = "output.SiteSelected0 == 1",     
               absolutePanel(id = "buttons_panel", class="buttons_panel",
                             class = "panel panel-default", fixed = TRUE,  #class="animate__animated animate__fadeIn animate__delay-1s", 
                             style = 'display:flex;background:transparent;border: 0px;margin-left: max(480px, 22vw);top:max(59px,8vh);',
                             draggable = FALSE, top = 0, left ="auto", right = "auto",bottom="auto", width="auto", height = "auto",
                            
                              actionButton("ask_simBtns", "?", inline=TRUE, class="btn_Ask", style="margin:10px 0 !important;"  ), 
                                
                              div( style="display:inline-flex;background:#555555c4;border-radius: 50px;",
                                 actionButton("adjust_view",  div(style="text-align:center;padding:0px;","adjust", br(),icon("binoculars" ),br(),"view" )   ,
                                              class="btn_adj",class="btn_Simulate" )    ,  
                                 div(actionButton("view_input",  div(style="text-align:center;padding:0px;","check",br(),icon("list-check"),br(),"input"),  
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
                                                       div(uiOutput("modelsTestOutput_panel"),  style="text-align:center;padding-top:4px;" )  )          
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
                                                          div(uiOutput("uishow_simYrs"))  )   
                               )     
    )) , # condPanel

        
  conditionalPanel( condition ="output.countryOK == 1", 
          hidden(tags$div(id="div_map_titles",
                 absolutePanel(id = "leaflegend_panel", class = "panel panel-default", fixed = TRUE,    
                              style = 'background-color:transparent;border-color:transparent;position:absolute',
                              draggable = FALSE, top = 0, left ="auto", right = "1vw",bottom="auto", width="auto", height = "auto", 
                                div(id="legendTitle",
                                      actionButton("ask_legend", "?", inline=TRUE, class="btn_Ask" , class="btn_AskOut"), 
                                      p("Mapped layers legend",style=" margin-bottom: 0;"), class= "leafLegendTitle", class= "leafLegendTitletop", 
                                  style="text-wrap: nowrap;padding: 3px 1vw; border-radius: 4vh;display: flex;align-items: center;") 
                 ), 
                absolutePanel(id = "simParams_panel", class = "panel panel-default", fixed = TRUE,   
                              style = 'background-color:transparent;border-color:transparent;', #position:absolute;
                              draggable = FALSE, top =0, left ="1vw", right = "auto",  width="auto", height = "auto",
                                 div(id="parsPanelTitle",  p("Simulation parameters",style=" margin-bottom: 0;"), 
                                        class= "leafLegendTitle",  class= "leafLegendTitletop1", 
                                        style="padding: 4px 1vw; border-radius: 4vh;") 
                 ),
           absolutePanel(id = "leafcontrol_panel", class = "panel panel-default", fixed = TRUE, #class="animate__animated animate__fadeIn animate__delay-2s", 
                               style = 'background-color:transparent;border-color:transparent;position:absolute',
                               draggable = FALSE, top ="auto", left ="1vw", right = "auto",bottom="51px", width="auto", height = "auto",
                                  div(id="contrPanelTitle", p("Mapped layers controls",style=" margin-bottom: 0;"), class= "leafLegendTitle", style="padding: 4px 1vw; border-radius: 4vh;") 
                 )
  ))), #condPanel
 
 tags$style(HTML('#SimParamsTabs {font-size:90%;display:flex;align-content:center;align-items:center;flex-wrap:wrap;justify-content:space-between;
                   background-color:#999!important;border-radius: 0 7vh 0 0;}')),                  
 
   
####  params panel (topleft) ####
  conditionalPanel(condition = "output.countryOK == 1", 
      hidden(tags$div(id="div_paramsPanel",             
          absolutePanel( id = "buffersize", 
                      style = "min-width: 390px;  width:20vw; background:transparent;border: 0!important; color:black;opacity:.82;", 
                      class = "panel panel-default",  class= "leafLegendTitletop1a", fixed = TRUE, draggable = FALSE, top =0  , left ="2vw", right = "auto", bottom = "auto",  
          
           tabsetPanel(id="SimParamsTabs",    
                 tabPanel("site",  id="candSiteTab",  align="center", style="padding-bottom:6px;background:white;opacity:.96;border-radius:0 0 20px 20px;",
                             fluidRow(class="hdr_darkBack", 
                                                column(10,div("Exploratory site geometry",class="hdr_darkText")),
                                                column(2, div(actionButton("ask_siteGeometry", "?", inline=TRUE, class="btn_Ask" ), style="float:left"))
                             ),  
                             br(),
                             fluidRow(column(4,  syle="top: 0.8vh;", div("Operation",class="hdr_params", style="margin-left:12px;top:0.8vh;" )), 
                                      column(7,  div(shinyWidgets::radioGroupButtons(inputId ='operation', label=NULL, choices=c("observation","translocation"), 
                                                                        selected="observation", width="100%"), style="margin-bottom:17px;"))),
                             
                              br(),
                              fluidRow(column(4, syle="top: 0.8vh;", div("Base geometry",class="hdr_params", style="margin-left:12px;" )), 
                                      column(7, div(shinyWidgets::radioGroupButtons(inputId ='shape', label=NULL, choiceNames=c("none","buffer","catchment"), choiceValues=c("none","buffer","catchment"),  individual=FALSE, 
                                                     selected="none", width="100%"), style="margin-bottom:17px;"))),
                             
                               br(),   
                               hidden(tags$div(id="div_buffer_sel",
                               fluidRow(column(4,  syle="top: 0.8vh;",
                                                       div("Zone radius",class="hdr_params", style="margin-left:12px;margin-right:0px; " )), 
                                        column(7,  sim_buff_div )
                               ))),  
                                      conditionalPanel("output.catchment_selection==1" , style="margin-top:12px",
                                                 fluidRow( uiOutput("catchSelection_title")  )),
                                       
                               fluidRow( column(10, offset=1, 
                                      conditionalPanel("output.catchment_selection==1", 
                                               fluidRow( 
                                                  column(2,  div( img(src="check.png", width="auto", height="auto", style="margin-top: 8px;width:1.7vh;height:1.7vh;" ))), 
                                                  column(10,  style="margin:0;padding:6px;background-color:#ffe62cc2ba;border-radius: 20px 20px 20px 20px;",
                                                         div(uiOutput( "selected_catchments_names" ) ,style="font-size:90%;max-height:20vh;overflow-y:auto;"))
                                      ))   ,
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
                             
                tabPanel("points", id="relPointsTab",  align="center", style="background:white;opacity:.96;padding-bottom:6px;border-radius:0 0 20px 20px;",
                                      fluidRow(class="hdr_darkBack", 
                                                  column(10,div("Release points coordinates",class="hdr_darkText")),
                                                  column(2,actionButton("ask_relPoints", "?", inline=TRUE, class="btn_Ask" ), style="float:left") 
                                        ),
                                        br(),   
                                        fluidRow(  column(3,  
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
                                       hidden(tags$div(id="div_coordsTable", uiOutput("show_relPtsCoords") )),
                                       conditionalPanel("output.releasePointsOK_tolock == 1" , 
                                                          div(shinyWidgets::prettyCheckbox(inputId = "lock_pts" ,  label = "release locations unlocked", 
                                                                                             shape = "round",  value = FALSE, fill=TRUE, 
                                                                                             icon=icon("lock")), style="margin: 18px 12px 12px 12px" ) 
                                       ) 
                    ), 
                    
                tabPanel("groups", id="beaversTab",align="center",style="background:white;border-radius:0 0 20px 20px;",
                             fluidRow(class="hdr_darkBack", 
                                               column(10, div("Demographics",class="hdr_darkText")),
                                               column(2,actionButton("ask_groupChars", "?", inline=TRUE, class="btn_Ask" ), style="float:left")  
                                               ), 
                             br(),
                             fluidRow(  column(7,  shinyWidgets::radioGroupButtons(inputId = "demog",label=NULL, 
                                                                        choices = c("all adults","all families","combination"), individual=TRUE,
                                                                        selected = "all adults", width="100%", direction = "horizontal") )  , 
                                        column(5,  div(textOutput("Nyg_btnpreText"), style="font-size:90%;margin:0;margin-top:7px;text-align:center;"),
                                                   div(textOutput("Nyg_btnText"), style="font-size:90%;text-align:center;"),
                                                   div( uiOutput("downNyg_btns"), style="display: inline-flex;"),   
                                                   div( uiOutput("upNyg_btns"), style="display: inline-flex;")    
                             )), 
                             br(), 
                             conditionalPanel("output.releasePointsOK==1" ,  
                                                       div(DTOutput('famTable',  width='90%'), style="max-height: 40vh;overflow-y: auto;") 
                                                ),#condpanel
                                      br()
                 ),  
                                       
                tabPanel("timing", id="timingTab",align="center",style="background:white;border-radius:0 0 20px 20px;",
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
                          ),   
                                      br(),
                                      conditionalPanel("output.releasePointsOK==1" ,  
                                                             uiOutput("show_relPtsTiming")),
                                      br() 
                 ),  
              
                 tabPanel("landscape", id="metadataTab",style="background:white;text-align:center;border-radius:0 0 20px 20px;",
                           fluidRow(class="hdr_darkBack", 
                                      column(10, div("Simulation landscape and scenarios",class="hdr_darkText")),
                                      column(2, actionButton("ask_landscape", "?", inline=TRUE, class="btn_Ask" ), style="float:left")  
                                  ),   
                           fluidRow(style="margin-top:12px;",
                                   column(9, 
                                           div("view layers", style="font-size: 90%;margin-bottom: -3px!important;"),
                                           div(class="btns-panel-metadat", style="font-size: 90%;",
                                                   fluidRow(column(3,  
                                                              p(id="div_lcm","land cover", style="text-align: center;padding-left: 17px;"),
                                                              shinyWidgets::materialSwitch(inputId = "landCoverMap", status = "danger")), 
                                                           column(3, 
                                                                  p(id ="div_NBNAtlas","NBN records", style="text-align: center;padding-left: 17px;"),
                                                                  shinyWidgets::materialSwitch(inputId = "NBNAtlas", label = NULL, status = "danger")) , 
                                                           column(3,  
                                                                 div(id ="div_customLayer", p("custom", style="margin: 9px 4px 15px 15px;"),actionButton("deleteCustomLayer","↻", inline=TRUE, class="buttontab", style="height:1.4vh;min-width:1.4vh;width: 1.4vh;"), style="display: flex;text-align: center;"),
                                                                 shinyWidgets::materialSwitch(inputId = "view_custom", label = NULL, status = "danger") 
                                                                 )) 
                                               ),
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
                                                       )) 
                                     ),  
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
                                               ))
                                      ),
          
                           br(),
                           div(style="max-height: 43vh;overflow-y:auto;scrollbar-width: thin; height: -webkit-fill-available;",
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
                                              conditionalPanel("output.show_metaAreaOfInt==1" , style="text-align:center;",
                                                                             uiOutput('metaTable_areaOfInt' ) 
                                              ),
                                              br() 
                           ) #div
        )#tabpanel  
      )#tabset
    )#abspanel
  ))), # condPanel
   
 
#####  help panel  #####
   hidden(tags$div(id="div_helpPanel",
     absolutePanel(id = "Release_site_info",   # class="animate__animated animate__bounce animate__delay-3s",
                  class = "panel panel-default", fixed = FALSE, style = 'opacity:0.82;z-index: 9999;padding: 14px 25px;border-radius: 5vh 0vh 5vh 5vh;max-width: max(370px, 18vw);overflow-wrap:normal; text-wrap: wrap;',
                  draggable = FALSE, top = "auto", right = "22vw", left = "auto", bottom =0, width = "auto", height = "fit-content" ,
        
          div(id = 'placeholder', style = 'background-color:black;border-color:transparent;border-radius:7px;' ),
          tags$head(tags$style("#text{padding:0;margin-bottom:7px;font-size:12px;color:#ffe62cc2;font-family:monospace;text-align:center;}"))  ,
          div( div(uiOutput("infotext_init"),style="color:darkmagenta;"),
               hidden(tags$div(id="div_help", img(src="arrow-left_teal.png", width="auto", height="auto", style="width:3vh;height:3vh;"),  
                               class="animate__animated animate__bounceOut animate__delay-2s animate__repeat-2", style="animation-fill-mode: none;" )), 
               style="display: flex;align-items: center;"),   
         
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
   
    
 
#####  second main panel (input summary) #####
 tabPanel( "input summary", style="background-color:gray20;margin:0px;height:90vh",  
          ####  model running panel tab #2
           conditionalPanel( condition ="output.modelRunning == 1",
                absolutePanel( id = "prog_panel_model2",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
                    uiOutput("modelRunning_panel_inputTab")  
            )),
           conditionalPanel( condition ="output.modelPrep == 1",
                absolutePanel( id = "prog_panel_prep2",top ="8px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
                   div(textOutput("modelPreping_panel_inputTab"),style="padding: 12px 27px;")
            )), 
            
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
                             )
              )),
              column(3, offset=1,
                      fluidRow(style="color:yellow;", 
                             div(id="siteRow",  
                                  div('Layout' ),  
                                  fluidRow(class="input_summary_row",style="text-align:center;border-color:yellow;",
                                                div("exploratory site",class="hdr_simParams", style="color:yellow;text-align:left!important;"), 
                                                div(uiOutput("relSite_infotext_loc"), style="color:beige;margin-left:17px;"))
                                 ),   
                              absolutePanel( id ="input_summary_transloc", style ="width: -webkit-fill-available;",
                                  fluidRow( class="input_summary_row",style="border-color:yellow;",
                                                           div(textOutput("operation_ttl"),class="hdr_simParams", style="color:yellow;text-align:left!important;") , 
                                                           tags$div(id ="inputSum1",textOutput("relSite_infotext_Npts_summary"), style="color:beige;margin-left:17px;")   , 
                                                           tags$div(id ="inputSum2",textOutput("relSite_infotext_demog_summary"), style="color:beige;margin-left:17px;")   , 
                                                           tags$div(id ="inputSum3",textOutput("relSite_infotext_timing_summary"), style="color:beige;margin-left:17px;padding-bottom:10px;")  ,
                                                           hidden(tags$div(id ="inputSum4",p("only existing beaver populations are included in the simulation"), style="color:beige;margin-left:17px;")))
                      )) 
              ),  
                     
              column(3,    ## metadata
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
              column(3,## simulation params
                     fluidRow(style="text-align:left;color:#d8e470",  
                                             div('Population growth simulation' ,style="color:#d8e470;display:inline;" ), 
                                             fluidRow(  class="input_summary_row",style="border-color:#d8e470",
                                                           div(textOutput("sim_assumption_txtStartYr"),class="hdr_simParams"  ,style="text-align:left;float:left;color:#d8e470;"), 
                                                           div(textOutput("simStartYear_txt") ,style="display:inline-flex;padding:7px 10px 12px 12px;") 
                                              ),  
                                              fluidRow(  class="input_summary_row",style="border-color:#d8e470",
                                                         div(textOutput("sim_assumption_txtNyr"),class="hdr_simParams"  ,style="display:inline-block;text-align:left;float: left;color:#d8e470;"), 
                                                         sim_Nyrs_div
                                              ),
                                              fluidRow( class="input_summary_row",style="border-color:#d8e470",
                                                        div(textOutput("sim_assumption_txtNrep"),class="hdr_simParams"  ,style="display:inline-block;text-align:left;float: left;color:#d8e470;"), 
                                                        sim_Nreps_div
                       )) 
              )
)),
  

#####  third main panel (output) ####
 tabPanel( "simulation output", 
      ####  model running panel tab #3 ####
      conditionalPanel( condition ="output.modelRunning == 1",
           absolutePanel( id = "prog_panel_model3",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
           uiOutput("modelRunning_panel_outputTab")  
       )),
      conditionalPanel( condition ="output.modelPrep == 1",
          absolutePanel( id = "prog_panel_prep3",top ="8px", class = "shiny-notification-warning", style = 'display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
          div( textOutput("modelPreping_panel_outputTab"),style="padding: 12px 27px;")
       )),  
             
  fluidRow(style="position: absolute; top: 8vh; left: 3vw; right: 3vw;", 
           column(2,    div(id="siteRow_out",
                        fluidRow(style="text-align:left;color:yellow;", 
                                 div('Layout',style="display:inline;" ), 
                                 div(downloadButton("save_layout", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;"),
                                 fluidRow( class="input_summary_row",style="width: fit-content;min-height:4vh;text-wrap: wrap;padding: 12px 6px",
                                              div(uiOutput("dlText_layout"), class="hdr_simParams", style="color:yellow;"), 
                                  )                
                        )), 
                       fluidRow(style="text-align:left;color:orange;", 
                                div('Metadata',style="display:inline;" ), 
                                div(downloadButton("save_metadata", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;"), 
                                fluidRow(class="input_summary_row",style="border-color:orange;width: fit-content;min-height:4vh;text-wrap: wrap;padding: 12px 6px",
                                      div(uiOutput("dlText_metadata"), class="hdr_simParams", style="color:orange;")
                       )),                        
                       conditionalPanel("output.but_sTestOutput == 1" ,
                                uiOutput("ui_save_settleTest") ## init settlement params 
                       )
          ),  
          
          ##### simulation params #####
          column(10,   
            fluidRow(style="text-align:left;left:2vw;top: 8vh; position: relative;", 
                       div(uiOutput("popGrowth_title"),style="display:inline-flex;"), 
                       div(downloadButton("save_simpop", "" ,class = "btn-success",style = "height:50px;"),style="display:inline;padding-left: 12px;"),
                       div(actionButton("ask_resOutput", "?", inline=TRUE, class="btn_Ask" ), style="display:inline-flex;float:none;margin-left:4px;   ")  , 
                       fluidRow( class="input_summary_row",style="border-color:#d8e470;margin-right:2vw ;min-height: 7vh;border-radius: 0 7vh 6vh 8vh!important;",
                                  div(uiOutput("outputReadYN_txt"),
                                      style="position:absolute;padding:1vh 0 0 6vh;left: 25vw;"),
      
                     
                     
                     
                    conditionalPanel( condition ="output.simOutput == 1",
                            absolutePanel(id = "simOutput_panel", class = "panel panel-default", fixed = TRUE, 
                                          style = 'position: relative;background-color: transparent;border-color: transparent;',
                                          draggable = FALSE, top =0, left =0, right = 0,bottom= 0,
                                          
                                           
                               
                                 
                                          
                                 fluidRow(style="display:block;padding-top: 2vh",  
                                         
                                       ##### output tables #####
                                      column(7, 
                                          div(style="overflow:hidden;border-radius:30px;overflow-y: clip; height: fit-content;",# 76vh;max-height: 800px;",      
                                          div(style="overflow-x:auto;display: grid;overflow-y: clip;scrollbar-width: thin;",  
                                                     tags$div(id="nonNULLheaders_Output",uiOutput("uirecapTableOut_headers"), style="background:transparent!important;"), 
                                                     
                                                      div(p("OCCUPANCY: BEAVER TERRITORIES", style="margin: 0"), style="margin: 37px 0 0 0;color: #e7d236;line-height:.2em;"),
                                                      hidden(tags$div(id="noTerrs_inOutput",
                                                            p("no simulated territory in output!" , style="padding: 2vh;") )), 
                                                     tags$div(id="nonNULLTerrs_inOutput",  DTOutput("recapTableOut_fams" , height="31em"),   
                                                           style ="line-height: 90%; font-size: 90%; color:#555555cf!important; 
                                                           border-radius:30px;text-wrap: nowrap;width: fit-content;position:relative;"), 
                                      
                                                     absolutePanel(id ="abundPanel",  style="text-align:center;position:relative;top:3em;", #bottom: 355px
                                                              div(p("ABUNDANCE: INDIVIDUAL BEAVERS"), style="color: #e7d236;height:30px;")
                                                     ),  
                                                     hidden(tags$div(id="noAdlt_inOutput",
                                                             p("no simulated abundance in output!" , style="padding: 5vh;") )),    
                                                     tags$div(id="nonNULLAdlt_inOutput",style="border-radius:12px;top:1em;height: 32em;",  
                                                              DTOutput("recapTableOut_N", height="31em"),
                                                              style ="line-height: 90%; font-size: 90%; color:#555555cf!important; 
                                                              border-radius:30px;text-wrap: nowrap;width: fit-content;position:relative;") 
                                       ))),      
                                                
                                      ##### abundance plots #####
                                      column(5,  style="height:-webkit-fill-available;",# height:76vh;max-height:800px;  
                                          hidden(tags$div(id="noPlotOutput",
                                                     img(src="beav-nulltoo.png", width="auto", height="auto",id="beavnull", class="animate__animated animate__hinge animate__delay-1s",
                                                                     style="width:60px;margin:2vh"),
                                                     img(src="beav-null.png", width="auto", height="auto",id="beavnull2", class="animate__animated animate__hinge animate__delay-2s",
                                                                     style="width:50px;padding-top: 13vh;"),
                                                     img(src="beav-nullhey.png", width="auto", height="auto",id="beavnull3", style="width:40px;")
                                               )),
                                          tags$div(id="nonNULL_plotOutput",
                                                   style="display:grid;display:flex;flex-direction: column;align-items: stretch;justify-content: space-between;padding-top:2em;",      
                                                     div(radioGroupButtons(inputId = "show_outputType", label=NULL, choices= c("each repetition", "summary values") , 
                                                                     selected="each repetition", individual = FALSE ),
                                                                     style="text-align:center;padding-bottom:5em;"),
                                                                        
                                                     conditionalPanel( condition ="output.showEachRep == 1",
                                                            absolutePanel(id = "showEachRep_fams", class = "panel panel-default", fixed = TRUE, 
                                                                      style = 'position: relative;background-color: transparent;border-color: transparent;height:24em;',
                                                                      draggable = FALSE, top =0, left =0, right = 0,bottom= 0,   
                                                                       div(class = "NTimeLinePlots_fams",
                                                                         div(plotOutput("NTimeLinePlot_fams", height="25em" )   ),
                                                                         div(plotOutput("NTimeLinePlot_fams_hilights" ,  height="25em")  ))
                                                            ),
                                                            tags$style("
                                                              .NTimeLinePlots_fams { position: relative; }
                                                                #NTimeLinePlot_fams { position: absolute;border-radius: 12px;background-color:beige; }        
                                                                #NTimeLinePlot_fams_hilights { position: relative;border-radius: 12px;background-color: transparent;} ")
                                                      ),
                                                     conditionalPanel( condition ="output.showEachRep == 0",
                                                            absolutePanel(id = "showSumm_fams", class = "panel panel-default", fixed = TRUE, 
                                                                        style = 'position: relative;background-color: transparent;border-color: transparent; height:24em;',
                                                                        draggable = FALSE, top =0, left =0, right = 0,bottom= 0, 
                                                                         div( class = "NSummTimeLinePlots_fams",
                                                                           div( plotOutput("NSummTimeLinePlot_fams", height="25em") ),
                                                                           div( plotOutput("NSummTimeLinePlot_fams_hilights", height="25em"))
                                                           ),
                                                            tags$style("
                                                                .NSummTimeLinePlots_fams { position: relative; }
                                                                  #NSummTimeLinePlot_fams { position: absolute;border-radius: 12px;background-color:beige; }        
                                                                  #NSummTimeLinePlot_fams_hilights { position: relative;border-radius: 12px;background-color: transparent; } ")
                                                      )),
                                                    
                                                     absolutePanel(id = "show_N",style="position:relative;max-height:300px;height:30vh;",
                                                            conditionalPanel( condition ="output.showEachRep == 1",
                                                                  absolutePanel(id = "showEachRep_N", class = "panel panel-default", fixed = TRUE, 
                                                                        style = 'position:relative;background-color: transparent;border-color: transparent; padding: 0;', 
                                                                        draggable = FALSE, top ="5em", left =0, right =0,bottom= 0,
                                                                        div( class = "NTimeLinePlots_N",
                                                                           div( plotOutput("NTimeLinePlot_N", height="25em" ) ),
                                                                           div( plotOutput("NTimeLinePlot_N_hilights", height="25em" ))
                                                                         ),
                                                                tags$style( "
                                                                    .NTimeLinePlots_N {  position: relative; }
                                                                      #NTimeLinePlot_N { position: absolute;border-radius: 12px;background-color:beige; }        
                                                                      #NTimeLinePlot_N_hilights { position: relative;border-radius: 12px;background-color: transparent; } "
                                                            ) )),
                                                            conditionalPanel( condition ="output.showEachRep == 0",
                                                                   absolutePanel(id = "showSumm_N", class = "panel panel-default", fixed = TRUE,
                                                                         style = 'position:relative;background-color: transparent;border-color: transparent; padding: 0; ', 
                                                                        draggable = FALSE, top ="5em", left =0, right = 0,bottom= 0,
                                                                        div( class = "NSummTimeLinePlots_N",
                                                                           div( plotOutput("NSummTimeLinePlot_N", height="25em" ) ),
                                                                           div( plotOutput("NSummTimeLinePlot_N_hilights", height="25em" ))
                                                                          ),
                                                                        tags$style(" 
                                                                            .NSummTimeLinePlots_N { position: relative;  }
                                                                            #NSummTimeLinePlot_N { position: absolute;border-radius: 12px;background-color:beige; }        
                                                                            #NSummTimeLinePlot_N_hilights { position: relative;border-radius: 12px;background-color: transparent; } "
                                                                 )),  
                                                            br()
                                                            )
                                                   )# absPanel
                            )
                         )#col5         
                  ) #panel
                                        # row
              )) #condPanel
            )# fluidRow 
          ),# fluidRow
          
          
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
                          tags$style( "
                              .effTimeLinePlot { position: relative; }
                                  #effTimeLinePlot { position: absolute;border-radius: 12px;background-color:beige; }        
                                  #effTimeLinePlot_hilights { position: relative;border-radius: 12px;background-color: transparent; } ")  )  
           ))) ,
           conditionalPanel( condition ="output.areaOfInterestTline == 1",            
           fluidRow(style="text-align:left;color:#b6b612;top: 8vh; position: relative;left: 2vw;margin-left:-15vw;",  
                   div('Area of interest Observations',style="display:inline;" ),
                   div(actionButton("proximTo", label="compute", inline=TRUE, class="btn_aOfInt"), 
                                style="display:inline-flex;margin-left:32px;"),
                   fluidRow(class="input_summary_row",style="min-height:4vh;text-wrap: wrap;padding: 12px 6px;border-radius: 0 7vh 15vh 42px;border-color: #b2b212;",
                          conditionalPanel( condition ="output.recapProximToNonNuLL==1 ",
                                        fluidRow(column(width=7,  div(DTOutput("recapProximTo", height="100%") ,
                                                                        style ="overflow:auto;top:1em;height:fit-content;max-height:100vh;line-height:90%;font-size:90%;color:#555555cf!important;
                                                                        text-wrap:wrap;width:fit-content;position: relative;padding-bottom: 7px;visibility: visible;border-radius:30px;" )   
                   ))))
           )) 
    )#col10  
   )#fluidRow
  ),#tabPanel
 
####  fourth main panel (refs & info) ####
#tableOutput('foo'),
    ui_aboutTab    
    ),
    bsTooltip(id ="save_name", "name the project to attribute its ID to downloaded data",placement = "right", trigger = "hover")
  ))
)
 
