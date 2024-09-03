
###################   password-protected instances:
#  appCountry <-  "Wales"
#  appCountry <-  "England"
#  appCountry <-  "Scotland"

###################   no password (incl all countries dat)
#  appCountry <- "testBranch"



###################  async prep ####                                      
      library(promises)
      library(future)                                          
      library(future.callr)
      #plan(callr) #   best plan for bg
       ncores <- max(3,round(length(availableWorkers())/2)) # i.e. locally opens as many bg R sessions (behind Rstudio)
      #   plan(multisession, workers=ncores) #comes with race issues # max numb to use would be future::availableCores() anyway, automated here
        #cat("GLOBAL: setting",ncores,"localhost (",Sys.info()[["nodename"]],") parallel workers with ",availableCores(),"CPU cores available for R process: ",Sys.getpid(),"\n")
        future::plan(future.callr::callr, workers =  ncores)#as.numeric(availableCores()) ) # length(availableWorkers()))                        
        curPlan <-future::plan() 
      onStop(function() {  
        cat("───  app stopped ",format(Sys.time(),"%H:%M"),".. process id:", Sys.getpid()," ──────────────────────────────────────────────")
      }) 
       options(future.globals.onReference = "ignore") 
       options(future.globals.maxSize= 1572864000)# for 1.5g limit891289600) #limit calculated  for an 850mb limit: 850*1024^2 = 891289600 
      # =maximum amount of data that is passed to a new future process, not how much RAM it can use!


    #### country-specific creds #load only country-level config on deploy
     UK_cntry <- c("Wales", "England", "Scotland") [which(c("Wales", "England", "Scotland") %in% list.files(here::here()))] 
     zoomThreshold=11   
     appTitle="A BEAVER POPULATION SIMULATION APP"
     if("demo" %in% list.files(here::here())) {UK_cntry <-"demo"}
     
     if(length(UK_cntry)==1){   source( paste0(here::here(UK_cntry),"/",UK_cntry,".R")) } else { appCountry <- "testBranch" } 
       
     sim_buff_div <- div( shinyWidgets::radioGroupButtons(inputId ='buffer_sel', label=NULL, choiceNames=c( "2km", "5km", "10km", "25km"), choiceValues=c(2000, 5000,10000, 25000), individual=FALSE, 
                                          selected=2000, width="100%" ), style="margin-bottom:17px;white-space:wrap;")
     
     
     
     if(appCountry != "testBranch" ){
         countryPassword <- appPassword
         countryAllowed  <- appCountry
         passwordProtect=TRUE
         sim_Nyrs_div <- div(shinyWidgets::radioGroupButtons(inputId = "sim_Nyrs" ,  label = NULL, individual = TRUE,
                                                                       choiceNames =c("3 years","5 years","10 years"), choiceValues =c(3,5,10),
                                                                       selected=3),style="display:inline-block;margin-right:72px;float:right;margin-top:1vh;") 
         sim_Nreps_div <-  div(shinyWidgets::radioGroupButtons(inputId = "sim_Nreps" ,  label = NULL, individual = TRUE,
                                 choiceNames =c("5","15","30" ), choiceValues =c(5,15,30), 
                                 selected=5),style="display:inline-block;margin-right:22px;float:right;margin-top:1vh;")
         if(UK_cntry =="demo" ) {
           appTitle="A BEAVER POPULATION SIMULATION APP * DEMO *"
           zoomThreshold=12
           passwordProtect=FALSE 
           sim_Nreps_div <-  div(shinyWidgets::radioGroupButtons(inputId = "sim_Nreps" ,  label = NULL, individual = TRUE,
                                                                 choiceNames =c("3","5","10" ), choiceValues =c(3,5,10), 
                                                                 selected=3),style="display:inline-block;margin-right:22px;float:right;margin-top:1vh;")
           sim_buff_div <-div( shinyWidgets::radioGroupButtons(inputId ='buffer_sel', label=NULL, choiceNames=c( "1km", "2km","5km"), 
                                                               choiceValues=c(1000, 2000, 5000), individual=FALSE, 
                                                selected=2000, width="100%" ), style="margin-bottom:17px;white-space:wrap;") 
         } 
    } else {  
         countryAllowed <- c("Wales","Scotland","England")
         passwordProtect=FALSE 
         sim_Nyrs_div <- div(shinyWidgets::radioGroupButtons(inputId = "sim_Nyrs" ,  label = NULL, individual = TRUE,
                                                                       choiceNames =c("1 years","3 years","5 years","10 years"), choiceValues =c(1,3,5,10),
                                                                       selected=1),style="display:inline-block;margin-right:72px;float:right;margin-top:1vh;") 
         sim_Nreps_div <-  div(shinyWidgets::radioGroupButtons(inputId = "sim_Nreps" ,  label = NULL, individual = TRUE,
                                 choiceNames =c("1","2","5","15","30" ), choiceValues =c(1,2,5,15,30), 
                                 selected=1),style="display:inline-block;margin-right:22px;float:right;margin-top:1vh;")
    } 
                       
                                                    
####  
## load global env. scripts ####
####   
  initLoading_scripts <- paste0(here::here("init_loading"),"/",list.files(here::here("init_loading")) )
  ui_scripts          <- paste0(here::here("ui_functions"),"/",list.files(here::here("ui_functions")) )
  ibm_scripts         <- paste0(here::here("ibm_functions"),"/",list.files(here::here("ibm_functions")) )
  gis_scripts         <- paste0(here::here("gis_functions"),"/",list.files(here::here("gis_functions")) ) 
  addResourcePath("www", paste0(here::here(),"/www")) 
  cat("─── hello! loading global env ────────────────────────────────────────────────────────────\n   init: ")
                                          for(scr in initLoading_scripts){   source(scr) } 
  cat("\n   ui  :  ")
                                          for(scr in ui_scripts)         {   source(scr) } 
  cat("\n   ibm :  ")
                                          for(scr in ibm_scripts)        {   source(scr) }   
  cat("\n   gis :  ")
                                          for(scr in gis_scripts)        {   source(scr) }    
                                          font_add_google("Comfortaa", "Comfortaa")
                                          showtext_auto()
  cat("\n")
  cat("   workers   : ",ncores,"\n   cores     : ",availableCores(),"\n   process id:", Sys.getpid(),"\n")
  
        