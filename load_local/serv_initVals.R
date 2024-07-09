## used in conditional panels
cat("server init values  - ")
show_metamodifHab <- reactiveVal(FALSE)
show_metaRecords <- reactiveVal(FALSE)
show_metaAreaOfInt <- reactiveVal(FALSE)
show_metaMgmt <- reactiveVal(FALSE)


  input_mapReleaseSite_click <- reactiveVal(NULL)

rv_dis  <- reactiveValues(pts_bng=NULL)

rv_temp <- reactiveValues(pt_buffer=NULL, pt_buffer_focus=NULL, newpts_ll_coords=NULL, pts_ll=NULL,   # temp df collated in initValues
                          validatedPts=0, ptstp_nro=0,                          # rel pts ID of temp missing pts
                          counter_clickPts=0,  addMapHilights=NULL)             # counter and trigger for highlights
  

click_Site_disabled <- reactiveVal(FALSE)


sitedat <- reactiveVal( 
    list(     click_lat= 50, 
              click_lng= -2,
              buffer_val = 2000, 
              shape= "init")  )
      




        
simModelOut <- reactiveVal(0) # slider panel
modelOut    <- reactiveVal(0) 
modelPrep   <- reactiveVal(0)
modelRunning<- reactiveVal(0)
simOutput   <- reactiveVal(0) # simulation otput tab - panel with output plots/tables
sTestOutput <- reactiveVal(0)

newfeature_title <- reactiveVal(NULL) 
launchTab2 <- reactiveVal(FALSE) 

 
# also intercatch_on_map+ init_poly+show_help+show_trySettle+show_simCanStart+a+howEachRep reaOfInterestTline + pointUpload show_simOutput but in main as reactive()








 
overlays  <- reactiveVal(NULL) 
  
  
#### initialize reactive values #### 
  rv_mapctry <- reactiveValues(bnd_cntry=NULL,  country_indx=NULL,  intercatch_count=0, ctry=NULL, country_infotext="to start, select a country",trig_countrydat=0, release_design_infotext =NULL)
   rv_init <- reactiveValues(sitePoly=NULL,intersectingCatchIndx=0, sitePoly_catchments=NULL,
                                     infotext_instructions_siteSel=NULL)
 
val <- reactiveValues(clickx =NULL, clicky =NULL ) 
 
  

              
trigger <- reactiveValues(demographics=0,   fam.start_newplot=0,updateSamplemsg=NULL, newPtsLabs=0 ) 
 
rv_sim <- reactiveValues(  fam.start_new=0,
                           candsite_lab = "candidate release site",
                           candsite_col = "black", 
                           candsite_weight = 4,
                           candcatch_col="black")
  
sim_in <- reactiveValues(initValues=NULL)
sim_out <- reactiveValues(fam_sim=NULL, ter_sim=NULL)

  
 NBNAtlas <- reactiveVal()
 NBNAtlas_providerID <- reactiveVal()
  
 
 

click_Pts_disabled <- reactiveVal(FALSE)  
   click_catch_disabled <- reactiveVal(FALSE) 
   infotext_def_shape   <- reactiveVal(NULL) 
   update_suitras_catch <- reactiveVal(0) 
   initValues <- reactiveVal(NULL)
   released_labs <- reactiveVal()
   counter_initPopChange <- reactiveVal(0)
   Nfams_init_actual  <- reactiveVal(NULL)  
   demog_group <- reactiveVal()
   update_initVals <- reactiveVal(0)  
   rerun_demog <- reactiveVal(TRUE) # to start with
  local_suitras <- reactiveVal() 
# local_suitras_catch <- reactiveVal()
   update_catch_plot <- reactiveVal(0)
#update_catch_plot <- reactive({list(intercatch_on_map_plot(),intercatch_on_map())})
 
uploadRestrict_shp <- reactiveVal(NULL) 
 #newUploadedPoly <- reactiveVal(NULL)
 newFeature <- reactiveVal(0)
 polyFeature <- reactiveVal(NULL)
 lineFeature<- reactiveVal(NULL)
 pointFeature<- reactiveVal(NULL)
 mapGeomFeature<- reactiveVal(NULL)
 uploadFeature<- reactiveVal(NULL)
 
  