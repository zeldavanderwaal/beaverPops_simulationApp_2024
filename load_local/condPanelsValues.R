cat("conditional panel values  - ")

 


#output$launchTab2<- reactive({ as.numeric(launchTab2())  }) # show slider for mapped output  
#outputOptions(output, "launchTab2", suspendWhenHidden = FALSE)  
 


output$pointUpload<- reactive({ as.numeric(pointUpload())==1  }) # show slider for mapped output  
outputOptions(output, "pointUpload", suspendWhenHidden = FALSE)  
 

output$but_simOutput<- reactive({ as.numeric(show_simOutput())==1  }) # show slider for mapped output  
outputOptions(output, "but_simOutput", suspendWhenHidden = FALSE)  

output$simOutput<- reactive({ as.numeric(simOutput())  }) # show output plots and table 
outputOptions(output, "simOutput", suspendWhenHidden = FALSE)  


output$but_sTestOutput<- reactive({ as.numeric(sTestOutput())  }) # show panel with year of test
outputOptions(output, "but_sTestOutput", suspendWhenHidden = FALSE)  
 
 
output$modelRunning <- reactive({ as.numeric(modelRunning())  })  
outputOptions(output, "modelRunning", suspendWhenHidden = FALSE)    
  

output$modelPrep <- reactive({ as.numeric(modelPrep())  })  
outputOptions(output, "modelPrep", suspendWhenHidden = FALSE)    
  

output$modelOut <- reactive({ as.numeric(modelOut())  })  
outputOptions(output, "modelOut", suspendWhenHidden = FALSE)    
   
 output$simModelOut <- reactive({ as.numeric(simModelOut())  })  
outputOptions(output, "simModelOut", suspendWhenHidden = FALSE)    
   

 output$showEachRep<- reactive({ as.numeric(showEachRep())  })  
outputOptions(output, "showEachRep", suspendWhenHidden = FALSE)    
    
 output$metadataTline<- reactive({ as.numeric(metadataTline())  })  
outputOptions(output, "metadataTline", suspendWhenHidden = FALSE)    # show gannt chart panel for metadadata effects
   
output$areaOfInterestTline<- reactive({ as.numeric(areaOfInterestTline())  })  
outputOptions(output, "areaOfInterestTline", suspendWhenHidden = FALSE)    # show gannt chart panel for metadadata effects
   
output$but_simCanStart <- reactive({ as.numeric(show_simCanStart())  })  
outputOptions(output, "but_simCanStart", suspendWhenHidden = FALSE)    

output$but_initTerrs <- reactive({ as.numeric(show_trySettle())  })  
outputOptions(output, "but_initTerrs", suspendWhenHidden = FALSE)     

output$show_helpBox <- reactive({ as.numeric(show_help()==1) })  
outputOptions(output, "show_helpBox", suspendWhenHidden = FALSE)          

output$countryOK <- reactive({as.numeric(mapExists()) })  #as.numeric(!is.null(rv_mapctry$bnd_cntry))==1}) #==1 then upload shp panel is on
outputOptions(output, "countryOK", suspendWhenHidden = FALSE)          

   
                            

 output$SiteSelected0 <- reactive({
     return( focusReady())
   #any(!is.null(rv_temp$pt_buffer),
  #                !is.null(rv_temp$pt_buffer_focus),
  #                !is.null(init_poly())))==1)
 }) 
outputOptions(output, "SiteSelected0", suspendWhenHidden = FALSE) 

output$SiteSelected <- reactive({  
      req(sitedat()$shape)
   if(sitedat()$shape  == "catchment") {return( as.numeric(length(selected_catchments_names())>0))
     } else {
     if(sitedat()$shape  == "buffer")    {return( as.numeric(!is.null(rv_temp$pt_buffer))==1) } else {
     return( 0)
     }}
}) 
  outputOptions(output, "SiteSelected", suspendWhenHidden = FALSE) 
   
 
    
output$catchment_selection <- reactive({ as.numeric(sitedat()$shape  == "catchment") + as.numeric(!is.null(rv_temp$pt_buffer) ) ==2 } )
 outputOptions(output, "catchment_selection", suspendWhenHidden = FALSE) 
  
output$catchment_selection_noSite <- reactive({ as.numeric(sitedat()$shape  == "catchment") + as.numeric(is.null(rv_temp$pt_buffer) ) + as.numeric( isFALSE(click_Site_disabled())) ==2 } )
 outputOptions(output, "catchment_selection_noSite", suspendWhenHidden = FALSE) 
 

  output$releasePointsOK_tolock <- reactive({  as.numeric(length(which(!is.na( rv_dis$pts_bng[,2]))) == input$Nfams_init) })
 outputOptions(output, "releasePointsOK_tolock", suspendWhenHidden = FALSE)  

 
 output$releasePointsOK <- reactive({  as.numeric(nrow( rv_dis$pts_bng) == input$Nfams_init) })
 outputOptions(output, "releasePointsOK", suspendWhenHidden = FALSE)  

  
 output$automatePts  <- reactive(as.numeric(input$relPts_method  =="random_location_across" ))    
 outputOptions(output, "automatePts", suspendWhenHidden = FALSE)
                                 
  

output$recapProximToNonNuLL<- reactive({ as.numeric(!is.null(proximToTable())) }) #==1 then panel is on
outputOptions(output, "recapProximToNonNuLL", suspendWhenHidden = FALSE) 


output$show_metamodifHab <- reactive({ as.numeric(show_metamodifHab()) }) #==1 then panel is on
outputOptions(output, "show_metamodifHab", suspendWhenHidden = FALSE) 
output$show_metaRecords <- reactive({ as.numeric(show_metaRecords()) }) #==1 then panel is on
outputOptions(output, "show_metaRecords", suspendWhenHidden = FALSE) 
       
    
output$show_metaAreaOfInt <- reactive({ as.numeric(show_metaAreaOfInt()) }) #==1 then panel is on
outputOptions(output, "show_metaAreaOfInt", suspendWhenHidden = FALSE) 
 
  