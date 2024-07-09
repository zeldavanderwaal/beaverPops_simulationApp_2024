cat("adding tooltip text  - ")

  
    addTooltip(session, id ="modelsTestOutput_panel","test output..",placement = "bottom", trigger = "hover") 
    addTooltip(session, id ="contrPanelTitle","toggle to display mapped layers",placement = "right", trigger = "hover")           
    addTooltip(session, id ="parsPanelTitle" , "design the operation to simulate",placement = "right", trigger = "hover") 
          
    addTooltip(session, id ="DTobsRecs_famDf","inform existing population demographics and observation year",placement = "top", trigger = "hover") 
    addTooltip(session, id ="metadataTline","metadataTline",placement = "right", trigger = "hover") 
 
    addTooltip(session, id ="obsRecs_submit","obsRecs_submit",placement = "top", trigger = "hover") 
    addTooltip(session, id ="metadata_drawName","enter a name to attribute a specific ID",placement = "bottom", trigger = "hover") 
    addTooltip(session, id ="metadata_effect","metadata_effect",placement = "left", trigger = "hover") 
    addTooltip(session, id ="metadata_duration_title","inform start and end year of the effect",placement = "left", trigger = "hover") 
    addTooltip(session, id ="ui_metadata_makeWhat","ui_metadata_makeWhat",placement = "left", trigger = "hover") 
 
    addTooltip(session, id = "uiMetaSelect_intermap","uiMetaSelect_intermap",placement = "right", trigger = "hover") 
        
    addTooltip(session, id ="map_catchSelect","map_catchSelect",placement = "top", trigger = "hover") 
    addTooltip(session, id ="metaselect_listGlob","global layers",placement = "right", trigger = "hover") 
    addTooltip(session, id ="metaselect_listCustom","geometries created by user",placement = "left", trigger = "hover") 
    addTooltip(session, id ="metaselect_submit","add selected feature to metadata",placement = "top", trigger = "hover")  
    addTooltip(session, id ="metaselect_cancel","cancel feature",placement = "top", trigger = "hover")  
    addTooltip(session, id ="metadata_submit","add drawn feature to metadata",placement = "top", trigger = "hover") 
    
    addTooltip(session, id ="showSumm_fams","plotted values summarised in table output for simulated territories",placement = "top", trigger = "hover") 
    addTooltip(session, id ="showSumm_N","plotted values summarised in table output for simulated adult abundance",placement = "top", trigger = "hover")  
    addTooltip(session, id ="showEachRep_N","plotted values summarised in table output for simulated adult abundance",placement = "top", trigger = "hover") 
    addTooltip(session, id ="showEachRep_fams","plotted values summarised in table output for simulated territories",placement = "top", trigger = "hover")  
    addTooltip(session, id ="recapTableOut_fams","simulation output for number of territories (or families)",placement = "top", trigger = "hover")
    addTooltip(session, id ="recapTableOut_N" ,"simulation output for number of adult beavers" ,placement = "top", trigger = "hover")
    addTooltip(session, id ="recapTableOut_headers" ,"year of simulated output" ,placement = "top", trigger = "hover")
    addTooltip(session, id ="show_outputType","display population trends for each simulation runs or show average (min-max)",placement = "top", trigger = "hover")  
    
    addTooltip(session, id ="timing","reset to same year, or introduce lags",placement = "bottom", trigger = "hover")
    addTooltip(session, id ="startYr_trySettl","any landscape modification on the tested year will be included",placement = "bottom", trigger = "hover")
    
    addTooltip(session, id ="save_layout",  "download all spatial layout data",placement = "right", trigger = "hover")
    addTooltip(session, id ="save_metadata","download all metadata included in the simulation",placement = "right", trigger = "hover")
    addTooltip(session, id ="save_simpop", "download all population growth simulation output",placement = "right", trigger = "hover")
    addTooltip(session, id ="save_settleTest","download all settlement test output" ,placement = "right", trigger = "hover")
      
    addTooltip(session, id ="addMetadata_modifHab","modified habitat geometries to include in the simulation",placement = "right", trigger = "hover")
    addTooltip(session, id ="addMetadata_records","observed beaver territories records to include in the simulation",placement = "right", trigger = "hover")
    addTooltip(session, id ="addMetadata_areaOfInt","areas of interest for detailed simulation output" ,placement = "right", trigger = "hover")
    
      
    addTooltip(session, id ="div_lcm", "overlay land cover map",placement = "top", trigger = "hover")   
    addTooltip(session, id ="div_NBNAtlas", "overlay NBNAtlas records",placement = "top", trigger = "hover")   
    addTooltip(session, id ="div_rivC", "overlay river catchments boundaries",placement = "top", trigger = "hover") 
    addTooltip(session, id ="div_opeC", "overlay operational catchments boundaries",placement = "top", trigger = "hover") 
    addTooltip(session, id ="div_mgmtC", "overlay management catchments boundaries",placement = "top", trigger = "hover") 
    addTooltip(session, id ="div_customLayer", "overlay custom layer to visualise",placement = "top", trigger = "hover") 



    addTooltip(session, id ="metadata_upload", "upload a spatial file",placement = "right", trigger = "hover")  
    addTooltip(session, id ="metadata_draw",   "draw a geometry on map",placement = "right", trigger = "hover") 
    addTooltip(session, id ="metadata_select", "select a mapped geometry",placement = "right", trigger = "hover") 
    
    
    
    addTooltip(session, id ="try_settling","trigger a settlement test" ,placement = "bottom", trigger = "hover")
    addTooltip(session, id ="undo_settling","delete settlement test output" ,placement = "bottom", trigger = "hover")
    addTooltip(session, id ="start_sim","trigger population growth simulation" ,placement = "bottom", trigger = "hover")
    addTooltip(session, id ="show_simYrs","display simulation output for selected year" ,placement = "bottom", trigger = "hover")
    addTooltip(session, id = "undo_sim","delete simulation output",placement = "bottom", trigger = "hover")
    addTooltip(session, id = "adjust_view" , "zoom to mapped beaver data" ,placement = "bottom", trigger = "hover") 
    addTooltip(session, id = "view_input" ,"go to input summary"  ,placement = "bottom", trigger = "hover") 
   
     
    addTooltip(session, id = "target", "number of groups to be released",placement = "top", trigger = "hover")
    addTooltip(session, id = "actual", "number of groups located",placement = "top", trigger = "hover") 
    addTooltip(session, id = "relPts_method", "select a method to generate the release point locations",placement = "top", trigger = "hover")
    addTooltip(session, id ="simRandPts","sample points automatically within settlement habitat",placement = "right", trigger = "hover")
    addTooltip(session, id ="lock_pts", "release locations are locked",placement = "bottom", trigger = "hover")
    addTooltip(session, id ="demog", "click to change demographics",placement = "bottom", trigger = "hover")
   
    addTooltip(session, id ="playSound", "play sound alert when simulation is complete",placement = "right", trigger = "hover")
    addTooltip(session, id ="show_help", "toggle display of tooltips and instructions",placement = "right", trigger = "hover")
    addTooltip(session, id ="uiSave_name", "name the project to identify downloaded data",placement = "right", trigger = "hover")
    
    addTooltip(session, id = "metaTitle_modifHab","available GIS layers of modified habitat",placement = "left", trigger = "hover")
    addTooltip(session, id = "metaTitle_records","beaver observation records to include in the simulation" ,placement = "left", trigger = "hover")
    addTooltip(session, id = "metaTitle_areaOfInt","GIS layers current selected to generate a detailed output" ,placement = "left", trigger = "hover") 
    
    
    addTooltip(session, id ="sim_assumption_txtNyr" ,"how many years the simulation should run" ,placement = "left", trigger = "hover")
    addTooltip(session, id ="sim_assumption_txtNrep" ,"how many times the simulation should run" ,placement = "left", trigger = "hover")
    addTooltip(session, id ="sim_assumption_txtStartYr","year the simulation will start given current input data",placement = "left", trigger = "hover")
    addTooltip(session, id ="buffer_sel",title="vary the radius to modify the size of the site",placement = "right", trigger = "hover") 
    addTooltip(session, id ="shape","define boundaries using a round buffer or select catchment geometries",placement = "right", trigger = "hover") 
    addTooltip(session, id ="operation","select observation if the opration does not include translocations",placement = "right", trigger = "hover")  