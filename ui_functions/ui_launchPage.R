 
        
cat("launch page  - ") 
                          


launchPage_ui_txt <-
 tabsetPanel(id ="show_launchPage_tab",  type = "tabs", 
       tabPanel(id="Hello", style="border:0;padding:0;display:block;margin:0;",
       div(leaflet::leafletOutput("mapUK_init" , height="96vh", width="99vw" ), 
           style="position:fixed;padding:0;margin:0;border:0;border-radius:0 0 0 51px;overflow:hidden;", class="animate__animated animate__fadeIn animate__slower" ),

 
       
###################################################################################################################  init panel on app starting  
  
  
tags$div(id="div_startW",           absolutePanel(id = "txt_startW", class="animate__animated animate__slideInLeft" ,
                                    class = "panel panel-default",    
                                    fixed = TRUE, style = 'border:0;padding: 6px;z-index:0;position: fixed;background-color:transparent;',
                                    draggable = FALSE, top = 57, right = "auto", left ="2vw", bottom = "auto", width =700, height = 147,  
                div("This App is designed to simulate the growth of beaver populations in Scotland, England and Wales.", style="color:#dccb39;font-weight:600;")
 )),
            
             
            
            absolutePanel(id = "txt_start", 
                                    class = "panel panel-default",  class="txt_start" , 
                                    fixed = TRUE, style = 'position: fixed;background-color:transparent;padding: 6px;border-radius:25vh;border:0;align-items: center;',
                                    draggable = FALSE, top = 57, right = "auto", left ="2vw", bottom = "auto", width =700, height = 147,  
        
                div("This App is designed to simulate the growth of beaver populations in Scotland, England and Wales.", style="z-index:4;font-weight:600;",  class="animate__animated animate__fadeIn animate__delay-2s",),
                
                div(style="padding:0 12px;column-width:150px;column-gap:24px;white-space: pre-line;color:white;column-fill: balance;",class="animate__animated animate__lightSpeedInLeft animate__delay-1s" ,
                "The app integrates academic research outputs into an interactive interface that allows the user to simulate beaver population growth. 
                      Users specify a range of parameters to create scenarios representative of a conservation management action, landscape and objectives. 
                      The generated simulation runs provide an array of potential outcomes aiming to provide useful insights to support decision-making."  
                )) 

                     
                     
                #                  div("The simulation parameters are derived from academic research combining a habitat map with a population model initialised on the Tay beaver population",style="padding:0 12px;font-weight:600;"), 
    
           #           br(),
          #            "The underlying model is individual-based and includes two main components.",
          #            br(),
          #            "a spatial component, during which each beaver dispersal route is simulated individually as sub-adults aim to establish their own territory" ,
        #             br(),
          #            "population dynamics, defined using parameters values from literature",
          #            br(),
          #            "The integration of the model into an interacive tool allows the simulation of populations within conditions that can be amended by the user and will affect either component of the model to generate an array of possibe outcome given the user-defined conditions.",style="padding:0 12px;font-weight:600;") 
  
, 


# tags$div(id="div_steps",
#           absolutePanel(id = "txt_steps",class = "panel panel-default", fixed = TRUE, style = 'position: fixed;background-color:transparent;padding: 6px;border-radius:25vh;border:0;align-items: center;',
#                                    draggable = FALSE, top = "27vh", right = "auto", left ="7vw", bottom = "auto", width = "24vw", height = "auto",  
#                         class="animate__animated animate__fadeInLeft animate__delay-2s",  
#                div("this is done in a few steps..",style="padding:12px;font-weight:600;") 
#)),

# tags$div(id="div_steps2",
#           absolutePanel(id = "txt_steps",class = "panel panel-default", fixed = TRUE, style = 'position: fixed;background-color:transparent;padding: 6px;border-radius:25vh;border:0;align-items: center;',
#                                    draggable = FALSE, top = "auto", right = "auto", left ="20vw", bottom = "12vh", width = "59vw", height = "auto",  
#                         class="animate__animated animate__fadeInLeft animate__delay-4s",  
#                div("..refer to the 'about' tab in-app for more instructions",style="padding:12px;font-weight:600;") 
#)),

 
 absolutePanel(id ="howtos", style="position: fixed;display: block;color: black;min-width:40vw;width: fit-content;", top="26vh",left="4vw",width="auto",height="55vh",  class="howTos",
 tags$div(id="div_steps",  class="animate__animated animate__fadeInLeft animate__delay-2s",  
  div("this is done in a few steps..",style="padding:12px;font-weight:600;")
  ),
 div(style="position:absolute;",
  hidden( 
     tags$div(id="div_howto1",      
             absolutePanel(id = "txt_div_loadingHowto1", class = "panel panel-default",class="animate__animated animate__rubberBand animate__delay-1s",  
                   fixed = TRUE, style = 'height:min(9vh, 96px);width: min(14vh, 96px);z-index:4;align-items: center;padding:14px;border-radius:15vh;background-color:#dccb39d4;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto",
 div("1.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
 div("design an operation")), 
 
              absolutePanel(id = "txt_div_loadingHowto1b", class = "panel panel-default",class="steExpl", 
                   fixed = TRUE, style = 'height:min(9vh, 96px);column-gap:2vw;align-items: center;padding:14px;background-color:#efee4c3d;border-color:#dccb39d4;display:inline-flex;border-radius:25vh;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "40vw", height = "auto",
 
div("> select between two operation types depending on the beaver population to be simulated", style="padding-left: 132px;min-width: 330px;" , class="steExplTxt"), 

div(class="stepsEgs",span(style="font-style: italic;","for instance.."),style="font-size: 90%;display: inline-flex;flex-direction: column;width: -webkit-fill-available;padding-left: 3vw;",
div(style="display:inline-flex;flex-direction: column;",
div("observe an existing population",style="list-style-type: circle; list-style-position: inside;display:list-item;"),
#tags$img(src="fig_howto1.png", height='70px', width='70px') , style="display: inline-flex;
#    align-items: center; width: 170px;
#    background: #ffffe02e; 
#    height: inherit;text-wrap: balance;
 #   padding: 0 4px;"), 
div("set-up a translocation",style="list-style-type: circle; list-style-position: inside;display:list-item;")),
#tags$img(src="fig_howto2.png", height='70px', width='70px'), 
#style="display: inline-flex;
#    align-items: center;
#    background: #ffffe02e;  width: 170px;
#    height: inherit;text-wrap: balance;
#    padding: 0 4px;"), 
) ))
  ),   



hidden( 
     tags$div(id="div_howto2",      
             absolutePanel(id = "txt_div_loadingHowto2", class = "panel panel-default",class="animate__animated animate__rubberBand animate__delay-1s",  
                   fixed = TRUE, style = 'top:min(12vh,116px);height:min(9vh, 96px);width: min(14vh, 96px);position:absolute;z-index:4;align-items: center;padding:14px;border-radius:15vh;background-color:#dc9f39d4;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto",
  div("2.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
 div("consider the landscape")),
 
             absolutePanel(id = "txt_div_loadingHowto2b", class = "panel panel-default",class="steExpl",  
                   fixed = TRUE, style = 'top:min(12vh,116px);height:min(9vh, 96px);column-gap:2vw;position:absolute;align-items: center;padding:14px;background-color:#efee4c3d;border-color:#dc9f39d4;display:inline-flex;border-radius:25vh;',  
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "40vw", height = "auto", 
div("> create management scenarios and add expert knowledge directly onto the map", style="padding-left: 132px; min-width: 330px;" , class="steExplTxt"), 

div(class="stepsEgs",span(style="font-style: italic;","for instance.."),style="font-size: 90%;display: inline-flex;flex-direction: column;width: -webkit-fill-available;padding-left: 3vw;",
div(style="display:inline-flex;flex-direction: column;",
div("licensing zones, where some or all beavers are removed" ,style="list-style-type: circle; list-style-position: inside;display:list-item;"),
div("barriers to dispersal",style="list-style-type: circle; list-style-position: inside;display:list-item;"),
))))), 
    


hidden( 
     tags$div(id="div_howto3",      
             absolutePanel(id = "txt_div_loadingHowto3", class = "panel panel-default",class="animate__animated animate__rubberBand animate__delay-1s",  
                   fixed = TRUE, style = 'top:min(24vh,232px);height:min(9vh, 96px);width: min(14vh, 96px);position:absolute;z-index:4;align-items: center;padding:14px;border-radius:15vh;background-color:#dc39d3a3;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto",
  div("3.",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
div("let the population grow")) ,
 
             absolutePanel(id = "txt_div_loadingHowto3b", class = "panel panel-default",class="steExpl", 
                   fixed = TRUE, style = 'top:min(24vh,232px);height:min(9vh, 96px);column-gap:2vw;position:absolute;align-items: center;padding:14px;background-color:#efee4c3d;border-color:#dc39d3a3;display:flex;border-radius:25vh;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "40vw", height = "auto", 

div("> simulate population growth in similar starting conditions for a number of repetitions", style="padding-left:132px;min-width:330px;", class="steExplTxt" )  ,
 

div(class="stepsEgs",span(style="font-style: italic;","for instance.."),style="font-size: 90%;display: inline-flex;flex-direction: column;width: -webkit-fill-available;padding-left: 3vw;",
div(style="display:inline-flex;flex-direction: column;",
div("simulate 15 runs of population growth over 5 years" ,style="list-style-type: circle; list-style-position: inside;display:list-item;"),
)) 

 ))),    


hidden( 
     tags$div(id="div_howto4",      
             absolutePanel(id = "txt_div_loadingHowto4", class = "panel panel-default",class="animate__animated animate__rubberBand  animate__delay-1s", 
                   fixed = TRUE, style = 'top:min(36vh,348px);height:min(9vh, 96px);width: min(14vh, 96px);position:absolute;z-index:4;display:inline-flex;align-items: center;padding:14px;border-radius:15vh;background-color:#a8c61da3;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width = "auto", height = "auto",
  div("4. ",style="font-size: 17px;margin: -5px;padding-right: 7px;"),
 div("assess potential outcomes")) ,
 
             absolutePanel(id = "txt_div_loadingHowto4b", class = "panel panel-default", class="steExpl", 
                   fixed = TRUE, style = 'top:min(36vh,348px);height:min(9vh, 96px);column-gap:2vw;position:absolute;display:inline-flex;align-items: center;padding:14px;background-color:#efee4c3d;border-color:#a8c61da3;border-radius:25vh;',
                   draggable =FALSE,  top ="auto", right = "auto", left ="auto", bottom = "auto", width ="40vw", height = "auto", 
div("> assess trends to gain insight on the potential outcomes of a management action", style="padding-left:132px;min-width:330px;", class="steExplTxt" ) ,


div(class="stepsEgs",span(style="font-style: italic;","for instance.."),style="font-size: 90%;display: inline-flex;flex-direction: column;width: -webkit-fill-available;padding-left: 3vw;",
div(style="display:inline-flex;flex-direction: column;",
div("visualise landscape potential for occupancy each year" ,style="list-style-type: circle; list-style-position: inside;display:list-item;"),
div("generate exportable outputs" ,style="list-style-type: circle; list-style-position: inside;display:list-item;") 
)) )))



  

)
   
     
###################################################################################################################  end init panel on app starting  

 ,
 tags$div(id="div_steps2",  class="animate__animated animate__fadeInLeft animate__delay-4s",   
        div("..refer to the 'about' tab in-app for more instructions",style="font-weight:600;padding: 46vh 0 0 20vw;",class="txt_steps2" ) 
))
       ,   
  
 
  tags$div( id="div_loadingLogos",      
    absolutePanel(id = "txt_loadingLogos",  class = "panel panel-default", class="animate__animated animate__fadeIn animate__slower",class="txt_loadingLogos",
                  fixed = TRUE, style = 'padding: 2px 35px;background-color:white;padding: 6px;border-radius: 25vh;border:0;display: inline-flex;align-items: center;',
                       draggable = FALSE, top =  "auto", right = "auto", left ="2vw", bottom = 0, width = "auto", height = "auto",
             div( tags$img(src="logo_EA.png", height='50px', width='140px') ,   style="margin: 0 9px;",class="logos"),
             div( tags$img(src="logo_NE.jpg", height='50px', width='50px'),    style="margin: 0 9px;",class="logos"),
             div( tags$img(src="logo_NatScot.png", height='50px', width='50px'),   style="margin: 0 9px;",class="logos"), 
             div( tags$img(src="logo_NRW_wide.png", height='35px', width='170px'),    style="margin: 0 9px;",class="logos"),
             div( tags$img(src="logo_NU.png", height='40px', width='120px'),   style="margin: 0 9px;",class="logos")  
)) ,

 hidden(
  tags$div(id="div_txt_prog", 
             absolutePanel(id = "txt_start2",   class = "panel panel-default",  class="txt_start2",# help panel expl
                           fixed = TRUE, style = 'opacity:.82;display: flex;align-items: center;padding:14px; border-radius: 5vh 0vh 5vh 5vh;max-width: 20vw;',
                                  draggable = FALSE,top =  "84vh",  right = "30vw", left = "auto", bottom = "auto",   width = "auto", height = "fit-content" ,
                div("Instructions and progress will be displayed in this frame throughout the set-up.",style="text-align:center;")
))),
     
hidden( 
     tags$div(id="launchP_selcntry",      
             absolutePanel(id = "txt_start_selcntry", class = "panel panel-default",class="animate__animated animate__bounce animate__delay-4s", class="txt_start_selcntry", 
                   fixed = TRUE, style = 'align-items: center;padding:14px;border-radius:15vh;background-color:#dccb39d4;border-color:transparent;display: inline-flex;justify-content: space-evenly;',
                   draggable =TRUE,  top =150, right = "9vw", left ="auto", bottom = "auto", width = "25vh", height = "25vh",
          div("Click on a country",br(), "to start a simulation." ,style="color:darkmagenta;text-align:center;" ) 
))
) 
))
 #))
