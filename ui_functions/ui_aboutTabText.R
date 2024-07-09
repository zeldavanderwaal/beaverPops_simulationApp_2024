# Module UI function

    #class="animate__animated animate__bounceOut animate__delay-3s"
cat("load 'about' tab  - ")

   
ui_aboutTab <-   tabPanel(title=div(style="display:inline;","about",    actionButton("ask_app", "?", inline=TRUE, class="btn_Ask",style="margin-top: 0 !important;",
                                                                           class="btn_AskApp",class="animate__animated animate__fadeIn"))  ,
                          value="about",
                              icon = hidden( tags$div(id="div_about", img(src="arrow-left_yel.png", width="auto", height="auto", style="padding-top:0;width:3vh;height:3vh;margin-top:-4px!important;"),  class="animate__animated animate__bounceOut animate__delay-1s animate__repeat-2",style="display:inline;float:right;margin:0 4px;animation-fill-mode: none;" )),
 
                          
                           conditionalPanel( condition ="output.modelRunning == 1",
      absolutePanel( id = "prog_panel_model4",top ="11px", class = "shiny-notification-warning", style = 'position: fixed;display:flex;border:1px solid #ff00ff;cursor:wait;width: fit-content;',
          uiOutput("modelRunning_panel_aboutTab")  
)),
          p("", style="margin-top:15vh;"),
          fluidRow(  column(10, offset=1,  
      navlistPanel(    id="aboutTabs",   well=FALSE, widths = c(3, 9),
       
                       
  tabPanel( 'General' , 
                  fluidRow(  div('Steps of simulation set-up',style='color:yellow;text-align:center;'),  
                              hr(style='border-color:yellow;width:80%') ),
                           
                        absolutePanel(id = "about_Howto1", class = "panel panel-default",   
                                           fixed = TRUE, style = 'position:absolute;z-index:1;align-items: center;padding:14px;border-radius:15vh;background-color:#dccb39;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = 96, height = 96,
                                 div("1.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
                                 div("design an operation")), 
                         
                        absolutePanel(id = "about_Howto1b", class = "panel panel-default", 
                                           fixed = TRUE, style = 'min-height:96px;width:-webkit-fill-available;align-items: center;padding:14px;background-color:#efee4c0f;color:lightgrey;border-color:#dccb39d4;border-radius:25vh;position:relative',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height ="auto",
                                div( style="padding-left: 132px;",
                           
                                 p(style="display: list-item;", "the first step is to select between two types of operations in",
                                     span(class="aboutBtnTxt","spatial layout",style="color:orange;border-color:orange;"),">",
                                     span(class="aboutBtnTxt","SIMULATION PARAMETERS",style="color:#ffe62cc2;"),">",
                                     span(class="aboutBtnTxt","operation",style="color:lightgrey;border-color:lightgrey;"),
                                      ), 
                                  
                                     
                                  fluidRow(style="background-color: #fff8c036; border-radius: 20px;margin: 2px 12vw 2px 3vw; ",
                                            column(width=7,  div(style="padding-left:7px","select ",span(class="aboutBtnTxt","translocation",style="color:#ffe62cc2;")," to design an operation that includes the translocation of beavers" )), 
                                            column(width=5,   div(style="padding-left:7px","or, select ",span(class="aboutBtnTxt","observation",style="color:#ffe62cc2;")," if no translocation is planned and you want to observe a known population" )),
                                 ), 
                                  br(),        
                                p(style="display: list-item;", "then, an exploratory site can be defined in order to visualise local catchments features (optional) or set-up a translocation (mandatory)" ),
                                 fluidRow(column(width=6,offset=1,  div("1. select a ",span(class="aboutBtnTxt","base geometry",style="color:lightgrey;border-color:lightgrey;")," that will contain the release locations (if applicable) and will highlight the habitat patches suitable for beaver settlement that are contained in all the river catchments that overlap with the exploratory site" )), 
                                          column(width=2, div("2. click on the map at the location of interest"  )),
                                          column(width=3, div("3. adjust the ",span(class="aboutBtnTxt","zone radius",style="color:lightgrey;border-color:lightgrey;")," to vary the site coverage" ))
                                ),  
                                br(),
                                p(style="display: list-item;", "if the simulation includes a translocation, the operation is then described using three parameters"),
                               fluidRow( column(width=3,offset=1,  div( span(class="aboutBtnTxt","points",style="color:lightgrey;border-color:lightgrey;")," locate the translocation release points on suitable habitat, click on the map or enter exact release location coordinates" )),
                                        column(width=5, div( span(class="aboutBtnTxt","groups",style="color:lightgrey;border-color:lightgrey;")," inform the number of adults and young; note that in the model, a group unit is considered a family that will establish a territory for all the individuals of the group; to add individuals that are young but will disperse to find a territory on their own, enter the individual as a single adult" )),
                                        column(width=3, div( span(class="aboutBtnTxt","timing",style="color:lightgrey;border-color:lightgrey;")," the default year of release is the current year but releases can be staggered through time" ) ))
                        ))   , 
                             
                       absolutePanel(id = "about_Howto2", class = "panel panel-default", 
                                           fixed = TRUE, style = 'position:absolute;z-index:4;align-items: center;padding:14px;border-radius:15vh;background-color:#dc9f39;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = 96, height = 96,
                                div("2.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
                                div("describe the landscape")),
                         
                         absolutePanel(id = "about_Howto2b", class = "panel panel-default",  
                                           fixed = TRUE, style = 'column-gap:2vw;min-height:96px;position:relative;align-items: center;padding:14px;background-color:#efee4c0f;color:lightgrey;border-color:#dc9f39d4;border-radius:25vh;',  
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto", 
                              
                               div( style="padding-left: 132px;",
                                   
                                  p(style="display:list-item;","refer to ",
                                      span(class="aboutBtnTxt","spatial layout tab",style="color:orange;border-color:orange;"),">",
                                      span(class="aboutBtnTxt","SIMULATION PARAMETERS",style="color:#ffe62cc2;"),">",
                                      span(class="aboutBtnTxt","landscape",style="color:lightgrey;border-color:lightgrey;"),
                                      " to visualise and change existing landscape conditions"
                                      ), 
                              
                              fluidRow(column(width=3,offset=1,div( span(class="aboutBtnTxt","view", style="color:lightgrey;border-color:lightgrey;"),
                                                           " NBN records, land cover map, or a custom layer as well as river, operational and management catchments for the whole country" ),
                                             br(),
                                             div( span(class="aboutBtnTxt","summary tables",style="color:lightgrey;border-color:lightgrey;")," will display the current metadata available; 
                                                           toggle betwen the two options above each table to include/exclude features in the simulation; features deleted using ",
                                                           span(class="aboutBtnTxt","-",style="color:magenta;border-color:magenta;")
                                                           ," in the table are not be recoverable", style="display:block")  
                                              ), 
                                       
                                       
                                       
                                       
                                       column(width=7,p( span(class="aboutBtnTxt","add",style="color:lightgrey;border-color:lightgrey;"),
                                                           " spatial features to the map in order to create alternative landscape conditions that will affect habitat suitability for beaver dispersal and settlement:"),
                                              
                                              
                                              div(  style="margin: 0 1vw;padding: 4px;background-color:#746845bf;border-radius: 20px;", 
                                                     
     
                                               div(span(class="aboutBtnTxt","modify landscape",style="color:#FB8861FF;border-color:#FB8861FF;")," allows defining managed zones or barriers"),
                                               div("the surface covered by the added feature can be modified in two ways:"),
                                               fluidRow(column(width=5, offset=1,
                                                       div("made ",span(class="aboutBtnTxt","unsuitable",style="color:#FB8861FF;border-color:#FB8861FF;"),
                                                           " so that existing populations are removed and new populations can not cross or settle; use this option to create barriers to dispersal")),
                                                       column(width=6,
                                                       p("associated with a survival rate lowered by", span(class="aboutBtnTxt","50-75%",style="color:#FB8861FF;border-color:#FB8861FF;"),
                                                           " which can represent either poor conditions or the removal of individuals from the simulated population at each respective rate") 
                                               )),
                                              
                                               p(span(class="aboutBtnTxt","incorporate records" ,style="color:#ffe62cc2;")," allows known territories to be created on the map on the most recent year of observation"),
                                               div(span(class="aboutBtnTxt","area of interest",style="color:#b6b612;border-color:#b6b612;")," allows assessment of potential occupancy over time in relation to a sptial feature"), 
                                              )
                                              
                                              
                                              
                                              
                                              
 
                                              
                                              ) )
                        ,
                        br(),
                         fluidRow(     column(width=11,  
                        div(style="display:list-item;","spatial features can be drawn on the map, uploaded, or selected amongst river catchments or existing geometries"),
                         div(style="display:list-item;","note that territory observation records must be included if the selected operation is an ",
                        span(class="aboutBtnTxt", "observation",style="color:#ffe62cc2;"),
                        ", otherwise describing the landscape is optional"),
                        div(style="display:list-item;","if no change is included, the simulation will run in existing conditions, as mapped")  
                              
                              )))) ,
                          
                        
                        absolutePanel(id = "about_Howto3", class = "panel panel-default", 
                                           fixed = TRUE, style = 'position:absolute;z-index:4;align-items: center;padding:14px;border-radius:15vh;background-color:#d430cb;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = 96, height = 96,
                                  div("3.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
                                  div("let the population grow")) ,
                         
                        absolutePanel(id = "about_Howto3b", class = "panel panel-default",  
                                           fixed = TRUE, style = 'min-height:96px;position:relative;align-items: center;padding:14px;background-color:#efee4c0f;color:lightgrey;border-color:#dc39d3a3;display:flex;border-radius:25vh;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto", 
                                  div( style="padding-left: 132px;",
                                     p(style="display:list-item;","once all conditions are set, check the ",
                                         span(class="aboutBtnTxt","input summary",style="color:orange;border-color:orange;")," and adjust the simulation settings in ", span(class="aboutBtnTxt","Population growth simulation ",style="color:#d8e470;border-color:#d8e470;") ),
                                       
                                   fluidRow(style="background-color: #f36bff30;border-radius: 20px;margin: 2px 4vw;",
                                           column(width=5, div(span(class="aboutBtnTxt","simulation duration",style="color:#d8e470;")," how many years to run the simulation for; the simulation uses an annual timestep and effects are applied at the start of each year (except for the first year to avoid potential contradictions between observed territories and unsuitable conditions)", style="display:block")),
                                           column(width=4, div(span(class="aboutBtnTxt","number of simulation runs",style="color:#d8e470;")," the number of times the simulation will run in similar starting conditions; output summaries will be based on frequencies derived from the number of runs", style="display:block")) ,
                                           column(width=3, div("when all parameters are ready, click on ", span(class="aboutBtnTxt","spatial layout ",style="color:orange;border-color:orange;"),">",span(class="aboutBtnTxt","start growing",style="color:magenta;border-color:magenta;")  ) )
                                           ),
                                fluidRow(  style="padding-top: 7px;",   column(width=11,   
                                   p(style="display:list-item;","the model uses a high definition grid and depicts stochastic movement for each step of the dispersal stage for each dispersing sub-adult;
                                       computation time will increase with the global surface covered (due to GIS data handling), habitat patchiness (if longer dispersal routes are required to reach settlement), the number of families (and associated dispersing sub-adults), the simulation duration (dispersing sub-adults increasing exponentially), and the number of simulation runs (since repeating the process as many times)"),
                                   p(style="display:list-item;","click ", span(class="aboutBtnTxt","try settling", style="color:turquoise;border-color:turquoise;") ," to generate potential dispersal routes from each translocation departure located on the map, as incorporated in the model (for illustration purposes)"),
                                   p(style="display:list-item;","each simulation run involves the simulation of each individual beavers dispersal across the landscape on 100mx100m raster cells, as well as population dynamic generating the population growth in abundance") 
                                ))
                                   
                                   
                                   
                           )),
            
            
            
                        
                        absolutePanel(id = "about_Howto4", class = "panel panel-default", 
                                           fixed = TRUE, style = 'position:absolute;z-index:4;display:inline-flex;align-items: center;padding:14px;border-radius:15vh;background-color:#a8c61d;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = 96, height = 96,
                                  div("4.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
                                  div("assess potential outcomes")) ,
                         
                        absolutePanel(id = "about_Howto4b", class = "panel panel-default", 
                                           fixed = TRUE, style = 'position:relative;display:inline-flex;align-items: center;padding:14px;background-color:#efee4c0f;color:lightgrey;border-color:#a8c61da3;border-radius:25vh;height:fit-content;',
                                           draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width ="auto", height = "auto", 
                        
                            div( style="padding-left: 132px;",
                                  div(style="display:list-item;","when all the simulation runs are completed the output can be assessed:"),
                                  
                                 fluidRow(style="background-color: #98cf3d2e;border-radius: 20px;margin: 2px 4vw;padding-left: 24px;",
                                    
                                 div(style="display:list-item;","landscape occupancy outputs are displayed on the main map in ",span(class="aboutBtnTxt","spatial layout",style="color:orange;border-color:orange;") ),
                                 p("describe freq mapped outputs................ ") ,
                                 
                                 div(style="display:list-item;","beaver population outputs are displayed in ",
                                       span(class="aboutBtnTxt","simulation output",style="color:orange;border-color:orange;") ,">",
                                       span(class="aboutBtnTxt","Population growth simulation ",style="color:#d8e470;border-color:#d8e470;") ),
                                div("describe plots & tabs outputs................ ") 
                                 ),
                                      
                             
                        fluidRow(   style="padding-top: 7px;",    column(width=11,     
                           p( style="display:list-item;","if effects were included in the simulation, the ", 
                                span(class="aboutBtnTxt","Added metadata timeline",style="color:#FB8861FF;border-color:#FB8861FF;"), " will be displayed under the simulation output"),
                         
                           p(style="display:list-item;","to obtain metrics on the simulated population over an area of interest, 
                                      add spatial features on the map using the available options in ",
                                      span(class="aboutBtnTxt","spatial layout",style="color:orange;border-color:orange;"),">",
                                      span(class="aboutBtnTxt","SIMULATION PARAMETERS",style="color:#ffe62cc2;"),">",
                                      span(class="aboutBtnTxt","landscape",style="color:lightgrey;border-color:lightgrey;"),">",
                                      span(class="aboutBtnTxt","add",style="color:lightgrey;border-color:lightgrey;") 
                               ," and click on ",
                               
                              span(class="aboutBtnTxt","simulation output",style="color:orange;border-color:orange;"),">",
                              span(class="aboutBtnTxt","Area of interest observations",style="color:#b6b612;border-color:#b6b612;"),">",
                              span(class="aboutBtnTxt","compute",style="color:#d8e470;border-color:#d8e470;"), 
                              "displayed under the summary tables; a table of annual summaries will be available for each feature (re-click when adding new features)"),   
                           p(style="display:list-item;","simulation outputs including any metadata included in the simulation can be downloaded in common formats for export and analysis into any alternative software" )

                         )) ))
            ),             
                                   
                                 
                    
                     tabPanel( 'Assumptions and Parameters' , 
                               
                              fluidRow( 
                                   div('Assumptions and Parameters in the Simulation Model',style='color:turquoise;text-align:center;'),  
                                   hr(style='border-color:turquoise;width:80%') ,
                                   
                                   div('The App largely uses the spatially-explicit process-based model detailed in Shirley',
                                   span(' et al. ', style='font-style:italic'),
                                   '2015, initially developed and later validated using beaver population data from the Tay and Earn catchments. The model used for simulated range expansion of translocated beaver populations includes two main components: a Geographical Information System storing habitat suitability values, and an individual-based population dynamics module simulating life histories and individual dispersal within the GIS-held landscape.
                                       Important nodes, processes and parameter values included in the model are described below.', 
                                     style='color:beige;margin-bottom:20px;text-align:justify;') , 
                              
                                   div('Overview of the Simulation Model', style='color:turquoise;padding-top:11px;') ,
                                   div(textOutput('AboutTheModel'),class="aboutTabs2", style="max-height:20vh;column-width: 17vw;column-gap: 2vw;padding: 4px 2vw!important;"), 
                                   br(),
                                   div('Habitat Quality Values', style='color:turquoise;padding-top:11px;' ),
                                   div(textOutput('AboutPars_suit') , 
                                                 br(),
                                                 div('Beaver families', style='padding-top:11px;') ,
                                                 div(textOutput('AboutPars_fams0'),class="aboutTabs2"),  
                                                 fluidRow( column(10,offset=1,
                                                                 column(6,
                                                                        textOutput('AboutPars_fams1'), 
                                                                        br(),
                                                                        textOutput('AboutPars_fams2'), 
                                                                      ),
                                                                column(6,
                                                                       textOutput('AboutPars_fams3'), 
                                                                       br(),
                                                                       textOutput('AboutPars_fams4'), 
                                                                     )
                                                       )) ,
                                        class="aboutTabs2", style="max-height:20vh;"),
                                   div('Dispersal', style='color:turquoise;padding-top:30px;') ,
                                   div(textOutput('AboutPars_disp'),class="aboutTabs2"), 
                                   br(),
                                   div('Survival & Reproduction', style='color:turquoise;padding-top:11px;') ,
                                   div(textOutput('AboutPars_surv'),class="aboutTabs2"), 
                        )),
                      
                    tabPanel( "Initial Set-up", 
                              fluidRow(div('Initial Set-up', style='color:#edf794;text-align:center;'),  
                                       hr(style='border-color:#edf794;width:80%'),
                                       div('Each translocation comes with characteristics that are informed during the Initial set-up.
                                            Most parameters can be either generated automatically within a user-specified range, or provided precisely by the user. 
                                            Automated modes are useful for initial exploration and constrasting scenarios. 
                                            Precise data entry will generate output that can be tailored to a specific translocation scenario.
                                            The set-up process is partitioned into a few steps: site selection, point location, demographics, timing, metadata.
                                            Information regarding the each step is detailed below. 
                                           ', 
                                     style='color:beige;margin-bottom:20px;text-align:justify;') ),
                              
                                 
                                       
                                                               
                        tabsetPanel( id="setup_ask", 
                                tabPanel(class="aboutTabs2",
                                  title ="site",  
                                     uiOutput("ask_siteSelection"),
                                     div(style="subTextTitleTop", "the exploratory site serves as a guidance when setting up the spatial layout of any operation as it allows visualising local habitat features per catchments,
                                       it is mandatory to design translocations as the area will contain the initial release locations (as a visual help, and without limiting any further aspect of the population simulation)"),
                            
                                                        
                                   div(style="display:inline-flex;align-items: center;",
                                     h5("operation", style="color:black;"),span("- define the objective of the simulation", class="subTextTitle")),
                                   fluidRow(style="background-color:#8080803b;margin: 0;color: black;",
                                   fluidRow(column(width=6,  
                                                   p("observation",class="subTextTitle2"),
                                                    p("select observation if the simulation does not include a translocation"),
                                                   p("existing territory observation records will be required to start a population growth simulation"),
                                                   p("these can be incorporated by clicking on the map at each territory location or uploading coordinates for territory centroids")),
                                            column( width=6,
                                                   p("translocation",class="subTextTitle2"),
                                                    p("select translocation if the simulation includes a translocation; existing territory observation records can also be included"),
                                                    p("designing a translocation will first require creating an exploratory site, which is built by selecting a base geometry shape and a buffer before clicking on the map at the desired location") )          
                                   )),
                                  
                                   br(),                     
                                   div(style="display:inline-flex;align-items: center;",
                                     h5("base geometry", style="color:black;"),span("- use an exploratory site to build the spatial layout (optional)", class="subTextTitle")),
                                   fluidRow(style="background-color:#8080803b;margin: 0;color: black;",
                                   fluidRow(column(width=2,  
                                                   p("none",class="subTextTitle2"),
                                                    p("select if no exploratory site is required (allows observation only)") ),
                                            column( width=5,
                                                   p("buffer",class="subTextTitle2"),
                                                    p("select buffer to create a round exploratory site"),
                                                    p("any catchment that intersects with the buffer will be highlighted on the map and show the suitable habitat patches contained") )   ,
                                           column( width=5,
                                                   p("catchment",class="subTextTitle2"),
                                                    p("select catchment to build an exploratory site using river catchment geometries"),
                                                   p("any catchment that intersects with the buffer will be highlighted on the map and show the suitable habitat patches contained"),
                                                   p("one or several catchments may be selected by clicking on each catchment to add on the map") 
                                                   )
                                   )),
                                  
                                   br(),                     
                                   div(style="display:inline-flex;align-items: center;",
                                     h5("zone radius", style="color:black;"),span("- change the size of the exploratory site (optional)", class="subTextTitle")), 
                                   fluidRow(style="background-color:#8080803b;margin: 0;color: black;",
                                   fluidRow(column(width=2, p("2-25km") ,class="subTextTitle2" ),
                                            column( width=9,
                                                   p("the zone radius allows changing the size of the exploratory site") )
                                   )),
                                  
                                   br(),                     
                                  div(style="display:inline-flex;align-items: center;",
                                     h5("selected catchments", style="color:black;"),span("- when the exploratory site is built with river catchments geometries", class="subTextTitle")), 
                                     fluidRow(style="background-color:#8080803b;margin: 0;color: black;",
                                      p("a ist of the currently selected catchments will be displayed as the user selects each geometry") ),
                                  
                                   br(),                     
                                  div(style="display:inline-flex;align-items: center;",
                                     h5("lock exploratory site", style="color:black;"),span("- avoid triggering events by clicking on the map", class="subTextTitle")), 
                                    fluidRow(style="background-color:#8080803b;margin: 0;color: black;",
                                             p("if using buffer as geometry, the selection mode is halted once the exploratory site is locked, allowing future clicks not to relocate the buffer on the map"),
                                             p("if using catchments as geometry, the selection mode is halted once the exploratory site is locked, allowing future clicks not to intervene in the catchment selection") ) 
                                  
                                ),
                                
                                
                                
                                
                                
                                
                                
                        tabPanel(class="aboutTabs2",
                                  title ="points",
                                  uiOutput("ask_ptsLocation"),
                                  
                                 div(style="subTextTitleTop","releases will be located within the exploratory site; release point locations can be informed by the user in three different ways"),
                            
                                                       
                                   h5("automated point locations"),
                                   fluidRow(style="background-color:#8080803b;margin: 0;",
                                   fluidRow(column(width=3,  
                                                   p("click to generate a target number of release locations"),
                                                   div(img(src="beaver-yellow.png", height='14px', width='14px')  ,style="margin:4px!important;" ))  ,
                                            column(width=8,
                                            p("points will be located within the exploratory site in a cell randomly selected amongst cells associated with habitat suitable for beaver settlement;
                                                     it does not guarranty that the surrounding habitat will be sufficient for settlement.")),
                                                   ))  ,
                                   br(),                 
                                   h5("click on the map"),
                                   fluidRow(style="background-color:#8080803b;margin: 0;",
                                   fluidRow(column(width=3,  
                                                   p("click on the map"),
                                                   div(img(src="cursor.png", height='14px', width='14px') ,style="margin:4px!important;" ))  ,
                                            column(width=8, 
                                                   p("click on the map to generate a release location, until the target number is reached.") ,
                                                   p("For any adjustment: once all points are located (i.e. target number=number of clicks), each point can be deleted in the table to be relocated by reclicking on the map.")),
                                                   ))  ,
                                   br(),                                  
                                   h5("manual entry"),
                                   fluidRow(style="background-color:#8080803b;margin: 0;",
                                   fluidRow(column(width=3,  
                                                   p("type coordinates values"),
                                                   div(img(src="tabEdit.png", height='14px', width='14px')  ,style="margin:4px!important;" )) ,
                                            column( width=8,
                                                   p("inform each point location in the table.") ,
                                                   p("the table is filled initially with points located ithin the exploratory site, to be amended.")) 
                                                   ))) ,
                     tabPanel(class="aboutTabs2",
                                  title ="groups",
                                  uiOutput("ask_groups"),
 
                                fluidRow(style="background-color:#8080803b;margin: 0;",
                                   fluidRow(column(width=11,  
                                                   p("edit the table or use the arrows in this tab to inform the demographics of each group released."),
                                                   p("note that in the simulation, young individuals are those that will not disperse on their own, they will disperse along with the group they were released with."),
                                                   p("to release a sub-adult, likely to disperse to establish a territory on their own, enter the individual as a single adult; the individual will disperse and settle on its own."))          
                                   )) 
                                ),
                     
                     
                    tabPanel(class="aboutTabs2",
                                  title ="timing",
                                  uiOutput("ask_timing"),
                        
                             fluidRow(style="background-color:#8080803b;margin: 0;",
                                   fluidRow(column(width=11,  
                                                   p("edit the table to inform the translocation timeline."),
                                                   p("note that the start year of the simulation is automated to be the earliest year of translocation or earliest year of territory observation (depending on the input provided)"))          
                                   ))  
                             
                              ),
                                tabPanel(class="aboutTabs2",
                                  title ="landscape",
                                  uiOutput("ask_landscape"),
                                  p(style="subTextTitleTop",
                                    "the existing landscape may be augmented in order to create management scenarios, add territory observation records and areas of interest
                                    by drawing on the map, uploading spatial data or selecting existing map geometries"),
          
                                  
             h5("viewing layers"),  
             fluidRow(style="background-color:#8080803b;margin: 0;",
             fluidRow(column(width=2,  
                             p("land cover map"),
                             p("NBN Atlas") )  ,
                       column(width=9, p("toggle to view/reset. LCM layer is not reactive (read from WMS)") ),
                             )  ,
             hr(),
             fluidRow(column(width=2,
                             p("catchments") )   ,
                      column(width=9, p("toggle to view/reset."),
                             p("labels at boundaries will display river/operational/management catchment names.")
                             ) ) ), 
                                  
                                  
             br(),               
                              
             h5("adding spatial data layers"),  
             fluidRow(style="background-color:#8080803b;margin: 0;",
             fluidRow(column(width=2,  
                             p("upload"),
                             div(img(src="upload.png", height='14px', width='14px')) ,style="margin:7px!important;padding-right: 12px;" )  ,
                       column(width=9, p("the uploaded file can be in one of two formats:"),
                                  p(style="margin-left: 17px;",".csv : a table containing at least 2 columns for latitude/longitude or BNG coordinates x/y with one point per row"),
                                  p(style="margin-left: 17px;","associated columns should be clearly named as x/y X/Y or latitude/longitude lat/long lat/lng"),
                                  p(style="margin-left: 17px;",".shp/.shx, .dbf, .sbn/sbx, .prj : shapefiles containing point locations per territory; 4 files with the specified extensions are required"),
                                  p(style="margin-left: 17px;","point geometries are automatically incorporated as observation records, line and polygon geometries are associated with habitat modification or area of interest"),
                                  p(style="margin-left: 17px;",".csv only allows uploading point geometries"),

                                  p("Individual territory covariates can be included with each observation point: year of observation, number of males, number of females. Ensure all entries are complete and numeric before uploading.
                                  If uninformed, default values for demographics will be used. The year can be informed using the slider below.
                                  Note that this is a no-fail process, in that even points located within unsuitable habitat will result in a the simulation of a territory in the vicinity of the input coordinates)."),
                             ) ),
             ########### say that point means obs and others men habitat modif
             hr(),
             fluidRow(column(width=2,
                             p("draw on the map"),
                             div(img(src="pencil.png", height='14px', width='14px')) ,style="margin:7px!important;padding-right: 12px;" )    ,
                      column(width=9, p("lines for barriers, polygons for areas, points for territory obesrvation records"),
                             ) ),
             hr(),
             fluidRow(column(width=2,   
                             p("select geometries"),
                             div(img(src="map-select.png", height='14px', width='14px')) ,style="margin:7px!important;padding-right: 12px;" )  ,
                       column(width=9, p("geometries available are river catchments (requires selecting an exploratory site) and user-created geometry."),
                                      p("the functionality can be used to assign an additional value to the geometry. For instance make an area both unsuitable and an area of interest.")
                             ) ) 
             ),
              
             br(), 
             h5("modified landscape table"),
             fluidRow(style="background-color:#8080803b;margin: 0;",
             fluidRow(column(width=3,  p("use current landscape/modify mapped habitat")  )  ,
                      column( width=8,
                             p("toggle to include the geometries summarised in the table into the simulation or not.") ,
                             p("input deleted using the button in the table is non retrievable.") ,
                             p("inform the start year and duration of the effect in the modal dialogue available when first incorporating metadata, or click on the table to amend information displayed.")),
                             ))  ,
             br(), 
                     
             h5("observation records table") ,
             fluidRow(style="background-color:#8080803b;margin: 0;",
             fluidRow(column(width=3,  p("no existing population nearby/incorporate records")  )  ,
                      column(width=8,
                             p("toggle to include the observed territories displayed in the table into the simulation or not.") ,
                             p("input deleted using the button in the table is non retrievable.") ,

                             p("inform the start year and duration of the effect in the modal dialogue available when first incorporating records, or click on the table to amend the year of observation, name or group demographics.") ,
                             p("default values are used when no records for demographics are available."),
                             p("app will aim to simulate a territory in the vicinity or the input location but may vary as no territory is to be simulated within unsuitable habitat."))
                      )),
             br(), 
                     
             h5("area of interest table") ,
             fluidRow(style="background-color:#8080803b;margin: 0;",
             fluidRow(column(width=3,  p("report overall growth/area of interest")  )  ,
                      column(width=8,
                             p("toggle to include the areas of interest displayed in the table on the map or not.") ,
                             p("input deleted using the button in the table is non retrievable.") ,

                             p("areas of interest can be used once the population simulation is complete, to obtain metrics describing the potential for the landscape to be occupied by beavers given the starting conditions of the simulation,
                               in relation to the geometry") ,
                             
                                
                             p("an annual report is generated that quantifies (depending on the geometry and overlap): 
                               the distance to the nearest cell that may be occupied,
                               the area within 1km buffer of the area of interest that has potential for being occupied,
                               the shared area, or the area within the area of interest that has potential for being occupied ++++++ ADD UNIT BINARY >80%  ") 
                       )))
              
                                    
                                
 )  )),## tab landscape
   
   
    
     tabPanel( 'Action Buttons' , 
               
                fluidRow( style="text-align:center;color:beige;",
                                     div('Action Buttons', style='color:magenta;'),  
                                     hr(style='border-color:magenta;width:80%') ,  
                                     p("action buttons become visible as the simulation parameters are informed",style='color:magenta;')
                                     ),
                            
              fluidRow( column(width=6,offset=3,style="color: beige;",
             fluidRow( style="background-color:#8080803b;margin: 0;border-radius: 30px;",
               fluidRow( 
               column(width=3,actionButton("demoBtn_adjust_view", div(style="text-align:center;padding:0px;","adjust", br(),icon("binoculars" ),br(),"view" ), class="btn_adj",class="btn_Simulate" )   )  ,
               column(width=9, p("click to focus the map view onto the exploratory site extent (or at the last click location on the map if no exporatory site is used)", style="margin-top:11px;")
                              )), 
              fluidRow( 
               column(width=3,  actionButton("demoBtn_view_input",  div(style="text-align:center;padding:0px;","check",br(),icon("list-check"),br(),"input"),class = "btn_check",class="btn_Simulate" )) ,
               column(width=9, p("click to switch to the input summary tab", style="margin-top: 21px;")
                              ))),
             
             
             br(),
             fluidRow( style="background-color:#8080803b;margin: 0;border-radius: 30px;",
              fluidRow( 
               column(width=3,actionButton("demoBtn_try_settling",  div(style="text-align:center;padding:0px;","try",br(),icon("house"),br(),"settling"),  class = "btn_simterrs",class="btn_Simulate" )   )  ,
               column(width=9, p("click to simulate dispersal from the release locations given the landscape conditions on the specified settlement test year;
                                 each click will generate a different outcome", style="margin-top:11px;")
                              )),
             
             fluidRow( 
               column(width=3,actionButton("demoBtn_undo_settling",  div(style="text-align:center;padding:0px;","undo",br(),icon("undo"),br(),"settling"), class = "btn_simterrsUndo",class="btn_Simulate" )  ),
               column(width=9, p("click to clear the settlement test output", style="margin-top: 21px;")
                              ))),
             br(),
             
             fluidRow( style="background-color:#8080803b;margin: 0;border-radius: 30px;",
              fluidRow( 
                column(width=3, actionButton("demoBtn_start_sim",  div(style="text-align:center;padding:0px;","start",br(),icon("play"),br(),"growing"), class="btn_Simulate" ,class = "btn_simPop"))  ,
               column(width=9, p("click to simulate population growth within the conditions specified for the duration and number of repetitions selected in the input summary", style="margin-top:11px;")
                              )),
             
               fluidRow( 
               column(width=3, actionButton("demoBtn_undo_sim",  div(style="text-align:center;padding:0px;","undo",br(),icon("undo"),br(),"growing"), class = "btn_simPopUndo",class="btn_Simulate" ) )  ,
               column(width=9, p("click to clear the population growth simulation output", style="margin-top: 21px;")
                              )))
                                 
     ))           
               ),
 
     tabPanel( 'Simulation Outputs' ,  
                    fluidRow( style="color:beige;",
                                     div('Simulation Outputs', style='color:orange;text-align:center;'),  
                                     hr(style='border-color:orange;width:80%') ), 
                                                             
                fluidRow( style="color:beige;", column(11,offset=1,                           
                      p("potential for occupancy across the landscape: mapped output"),              
                      p('A simulation run includes the step-by-step execution of the population expansion model from year 0 (initial territories) for the selected number of years.
                      All simulation runs start from a similar set of environmental conditions. 
                       Once all simulation runs are complete, the number of runs predicting occupancy on a given year is summed for each cell. 
                       The total number of runs predicting occupancy is then partitioned into pre-determined categories to facilitate summary mapping as a percentage of runs associated with a cell being occupied. 
                       Due to the stochasticity in dispersal routes and choice of habitat for building territories, low beaver densities in relation to suitable habitat area will often be associated with low numbers of runs per cell.'
                       ) ,
                      
                      br(),
                       p("trends in abundance: simulated beaver populations"),              
                      p('lalala' ) ,
                      
                      
                      br(),
                       p("area of intereset observations"),              
                      p('lalala' ) ,
                      
                      
                      br(),
                       p("settlement test"),              
                      p('lalala' )  
)),


 fluidRow( style="color:beige;",
                                     div('Exporting data', style='color:orange;text-align:center;'),  
                                     hr(style='border-color:orange;width:80%') ,  
                      p("lalala"),              
                      
) 
),  
 
     tabPanel( 'Map Legend' ,  style="color:#e3d3d3;", 
              
                  div('Map Legend', style='color:yellow;text-align:center;'),  
                  hr(style='border-color:yellow;width:80%') , 
                                  
               
               
               
              p("beaver habitat suitability"),  
             fluidRow(style="background-color: grey; border-radius: 20px;",
             fluidRow(column(width=1,offset=1,   div(img(src='grayscaleGrad.png', width="60px", height="30px") ,style="margin:7px!important;float: right;padding-right: 12px;" ) ),
                      column(width=4, p("as the proportion of catchment suitable for settlement")),
                      column(width=6, p("computed as the number of cells suitable over total number of cells in catchment"))),
              fluidRow( column(width=1,offset=1, div(img(src='suitdispBoxes.png', width="60px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  ) ),
                column(width=4, p("as SDM habitat category output per cell")),
                      column(width=6, p("habitat score per cell as per Exeter SDM.........."))) 
              ),
             br(), 
              
              
              p("catchments features"),  
             fluidRow(style="background-color: grey; border-radius: 20px;",
             fluidRow(column(width=1,offset=1,   div(img(src='line-magenta.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  ) ),
                      column(width=4, 
                             p("lines represent catchments boundaries"),
                             p("the area of each river catchment is colored according to the proportion of suitable habitat it contains (darker means higher proportion)"),
                             p("to activate this feature, select an exploratory site; catchments features will be displayed for the overlapping river catchments"),

                              p("same dataset used, simplified and aggregated for visualising river, operational and management catchments.")),
                      column(width=6,  
             p("Scotland | © SEPA. Some features of this information are based on digital spatial data licensed from the Centre for Ecology and Hydrology © NERC (CEH). Contains OS data © Crown copyright [and database right]."),
             p("Wales | Contains Natural Resources Wales information © Natural Resources Wales and Database Right. All rights Reserved. Contains Ordnance Survey Data. Ordnance Survey Licence number AC0000849444. Crown Copyright and Database Right. Derived in part from 1:50,000 and 1:250,000 scale digital data under permission from British Geological Survey. ©NERC."),
             p("England | © Environment Agency copyright and/or database right 2015. All rights reserved.")  
                             )),
              fluidRow( column(width=1,offset=1,div(img(src='square-suit.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  ) ),
                column(width=4, p("as SDM habitat category output per cell")),
                      column(width=6, p("cells highlighted were categorised as suitable for settlement as per Exeter SDM.........."))) 
              ) ,
             br(), 
           
             
              
              p("translocation set-up") ,
             fluidRow(style="background-color: grey; border-radius: 20px;",

             fluidRow(column(width=1,offset=1,div(img(src='beaver-facing-right.png', width="40px", height="25px") ,style="margin:7px!important;float: right;padding-right: 12px;" ) ),
                      column(width=4, p("release location")),
                      column(width=6, p("marks the initial location where a group is to be released; points that are not located within suitable or dispersal habitat will not allow initial dispersal and the group will fail"))),
             fluidRow( column(width=1,offset=1,  div(img(src='beaver-facing-right-small_sh.png', width="30px", height="18px"),style="margin:7px!important;float: right;padding-right: 12px;")),
                        column(width=4, p("with young")),
                       column(width=6, p("if the translocated groups include young: indicates which groups include 'young' beavers not in age of dispersing, that will remain associated with the rest of the group as a family unit and share a territory on the first year at least")))    ,
             fluidRow( column(width=1,offset=1, div(img(src='clock-solid.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;" ) ),
                        column(width=4, p("lagged release")),
                       column(width=6, p("if the translocation design includes releases on different years (lagged), the clock indicates the groups released on any year that is not the earliest")))
             )  ,   
            br(), 
           

             p("augmented landscape"),
             fluidRow(style="background-color: grey; border-radius: 20px;",

             fluidRow(column(width=1,offset=1,div(img(src='square-modif.png', width="30px", height="30px") ,style="margin:7px!important;float: right;padding-right: 12px;" )),
                      column(width=4, p("modified habitat")),
                      column(width=6, p("cells that overlap with a modified landscape line or zone will be amended accordingly in the model for the specified duration; 
                                        landscape modifications can be of 3 types: unsuitable, 50% mortality, 75% mortality"))
                       ),
             fluidRow( column(width=1,offset=1,  div(img(src='house2.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                        column(width=4,  p("observation records")),
                       column(width=6, p("the house is located at the input coordinates, the area occupied by the associated simulated territory is in yellow"))
                     
                       )    ,
             fluidRow(  column(width=1,offset=1,div(img(src='square-areaOfInt.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                        column(width=4, p("area of interest")),
                        column(width=6, p("areas of interest appear as green boundaries with light fill"))

                       )
             ),   
            br(), 
             

             p("simulated occupancy"), 
              fluidRow(style="background-color: grey; border-radius: 20px;",
                       fluidRow(column(width=1,offset=1,div(img(src='simleg.png', width="80px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=6, p(" "))) 
             ),
            br(), 
              
 
             p("settlement test") ,
             fluidRow(style="background-color: grey; border-radius: 20px;",
                 fluidRow(column(width=1,offset=1,div(img(src='wanderline.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=4, p("initial wander phase of dispersal")),
                      column(width=6, p("in this phase of the dispersal route, the dispersing sub-adult travels away from its natal territory for an initial exploration;
                                        an area is identified that includes cells of dispersal or suitable habita values that can be reached from the deprature location if the individual travels for 1500m;
                                        at the end of this stage, the individual may be located anywhere within this area (although preferrably 500m away at least)"))

                      
                      ) ,
                fluidRow(column(width=1,offset=1,div(img(src='rout.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=4, p("second phase of dispersal")),
                      column(width=6, p("in this phase of the dispersal route, the dispersing sub-adult travels from cell to cell while assessing whether it is possible to create a territory given the local conditions at each step;
                                        i will attempts to settle each time a suitable habitat is met and settle at the first oppourtinuty from that stage;
                                         cells that can be crossed but not occupied include dispersal habitat, suitable habitat that is too small for settling, occupied habitat that the individual cannot join (full family)")) ,

                      ) ,
                fluidRow(column(width=1,offset=1,div(img(src='square-initSetlmnt.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=4, p("initial territory")),
                      column(width=6, p("a successful dispersal route ends at a location where the dispersing sub-adult settles and create an initial territory"))) ,
                 fluidRow(column(width=1,offset=1,div(img(src='flag2.png', width="30px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=4, p("failed dispersal")),
                      column(width=6, p("dispersal is considered failed when the dispersing sub-adult does reach a location where it settles")) 
                 )),
             
  
           p("landscape cover map") ,  
            fluidRow(style="background-color: grey; border-radius: 20px;",

             fluidRow(column(width=1,offset=1,div(img(src='LCMLeg.png', width="80px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )),
                      column(width=4, p("retrieved 2023")),
                      column(width=6, p("Based upon LCM2020 © UKCEH 2021")))  
              ),
             br(), 
              
              

             p("NBN Atlas records") ,
            fluidRow(style="background-color: grey; border-radius: 20px;",
             fluidRow(column(width=1,offset=1,div(img(src='NBNleg.png', width="80px", height="30px"),style="margin:7px!important;float: right;padding-right: 12px;"  )), 
                      column(width=4, p("records summarised annually by provider as of nov. 2023")),
                      column(width=6, p("NBN Atlas website at http://www.nbnatlas.org")))  
            )
               
               ),#tab legend  
 
                       
                    tabPanel( 'R Packages' , 
                              fluidRow(style='padding:70px;color:pink;',  
                                       div('Main R Packages in use', style='text-align:center;'),  
                                       hr(style='border-color:pink;width:80%') ,
 fluidRow(style="    display: inline-flex; column-gap: 4vw;",
div(style="display:block",
                              div("app building"),
                              br(), br(),
                              p("shiny shinyGizmo shinyjs shinybusy stringr shinyBS shinyWidgets shinyEffects htmlwidgets")),
div(style="display:block",
                              p("interactive mapping"),
                              p("leaflet leafpm leaflet.extras leaflet.extras2 leafem leaflegend")),
div(style="display:block",
                              p("data handling"),
                              p("Rfast dplyr grid stringr zip png here magrittr future.callr promises future")),
div(style="display:block",
                              p("geometry, spatial"),
                              br(), 
                              p("lwgeom nngeo  terra rgeos stars exactextractr SpaDES.tools spatstat lwgeom rgdal rgeos stars nngeo")),
div(style="display:block",
                              p("plots, tables"),
                              br(), 
                              p("ggplot2 ggspatial ggnewscale tibble grid DT") )))
                                       
                                        
                                       
                        ) ,
                    
                    tabPanel( 'References' , 
                             fluidRow(  
                                      div('References', style='color:#c0c008;text-align:center;'),
                                      hr(style='border-color:#c0c008;width:80%') ,
                                       
                             div(style="color: #c0c008;white-space: pre-wrap;width: 88%;padding-left: 10vw;",         
                                    p("Beaver Management Report for 2020, Published by NatureScot, Scotland's Nature Agency, in August 2021. Available from www.nature.scot/doc/beaver-management-report-2020"),
                                    p("Shirley, M.D.F., Harrington, L.A. & Mill, A.C. 2015. A model simulating potential colonisation by Eurasian beaver (Castor fiber) following reintroduction to Scotland. Scottish Natural Heritage Commissioned Report No. 814"),
                                    p("Macdonald, D, Maitland, P, Rao, S, Rushton, S, Strachan, R, and Tattersall, F (1997). Development of a protocol for identifying beaver release sites. SNH Research, Survey & Monitoring 93, Battleby"),
                                    p("Beavers in Scotland report to Scottish Government, and other reports are available from: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/beavers-scotland"),
                                    p("Management Framework for Beavers in Scotland: www.nature.scot/professional-advice/safeguarding-protected-areas-and-species/protected-species/protected-species-z-guide/protected-species-beaver/management")
                             )
                                      
                        )) ,







 




                    
                    tabPanel( 'Acknowledgements' , 
                             fluidRow(  style='color:magenta;text-align:center;',
                                      div('Acknowledgements' ),
                                      hr(style='border-color:magenta;width:80%;')) ,
                                      
                                        
              fluidRow(style="color:magenta;text-align:left;align-items:center;display: flex;",
                   column(width=6,offset=1,             
                    fluidRow(column(width=4,        
                          div(style="display:inline-flex;column-gap: 3vw;",                
                              div(style="text-wrap: nowrap;padding-top: 7px;",
                                  div('an app developed by'),
                                  div('Dr Zelda van der Waal')))),
                           column(width=8,
                              div('zelda.vanderwaal@newcastle.ac.uk'),
                              div('zeldavdwaal@gmail.com'),
                              div('github.com/zeldavanderwaal'))
                              
                          ),
                              br(),
                     fluidRow(column(width=4,
                        div(style="display:inline-flex;column-gap: 3vw;align-items: center;",                
                           div(style="text-wrap: nowrap;padding-top: 7px;", div('Prof Aileen Mill')))),
                        
                         column(width=8, 
                           div(div('aileen.mill@newcastle.ac.uk'),
                                div('Professor of Modelling, Evidence and Policy')),
     
                             ))
                    ),
     column(width=4,     
div(style="align-items: center;",
    div('Modelling Evidence and Policy Group'),
    div('School of Natural and Environmental Sciences'),
    div('Newcastle University, UK'))
     )),
                              
                              br(),
      
fluidRow(column(width=8,offset=2, 
                div(style="color:magenta;text-align:center;padding-top:17vh;",
                       div('Prepared for research collaboration with'),
                       div('Natural England, NatureScot, Natural Resources Wales and the Environment Agency')))),   
                                      
                                      
                                      
              absolutePanel(id = "about_loadingLogos",  class = "panel panel-default", fixed = FALSE, draggable = FALSE, top =  "4vh", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto",
                                    style = 'position: relative;padding: 2px 35px;background-color:white;padding: 6px;border-radius: 25vh;border:0;display: flex;align-items: center;justify-content: space-evenly;',
                                         div( tags$img(src="logo_EA.png", height='50px', width='140px') ,   style="margin: 0 9px;"),
                                         div( tags$img(src="logo_NE.jpg", height='50px', width='50px'),    style="margin: 0 9px;"),
                                         div( tags$img(src="logo_NatScot.png", height='50px', width='50px'),   style="margin: 0 9px;"), 
                                         div( tags$img(src="logo_NRW_wide.png", height='35px', width='170px'),    style="margin: 0 9px;"),
                                         div( tags$img(src="logo_NU.png", height='40px', width='120px'),   style="margin: 0 9px;")  
                                         ) #abspanel
                         )) 
                             
            
                  




 ) )
)

