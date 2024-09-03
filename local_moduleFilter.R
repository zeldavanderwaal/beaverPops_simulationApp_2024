

################ below was used for filtering functionalities during focus group sessions


library("SpaDES.tools")

enable_all_features <<- TRUE
enable_sim_initTerrs <<- TRUE


if(isTRUE(enable_all_features)) {
  user_select_country <<-TRUE
}
 

     modFilter <- "none"
     modFilter_sub <- "all features enabled"
#    modFilter <-  "site selection" # sess1
#    modFilter <-  "locating release points"  
#    modFilter <-  "metadata"  
#   modFilter <- "metadata & init settlement"
     

if(modFilter =="metadata & init settlement") {  # nov28th session 
 # in code  "beaver habitat" %>% cur_layers()
  enable_sim_initTerrs  <<- TRUE
  user_select_country <<-FALSE
  modFilter_sub <- "skip launch page - no simulation trigger - try settling enabled" 
  list(  click_lat= 57.4956, 
            click_lng= -3.1716,
            buffer_val = 5000, 
            shape= "buffer") %>% sitedat()
  rv_mapctry$bnd_cntry <- "Scotland"
  }                
 
 
 
if(modFilter =="metadata & init settlement") {  # nov15th session
  # in code  "beaver habitat" %>% cur_layers()
  enable_sim_initTerrs  <<- TRUE
  user_select_country <<-FALSE
  modFilter_sub <- "skip launch page - no simulation trigger - try settling enabled"  
}           
 
if(modFilter =="metadata") {  
  # in code  "beaver habitat" %>% cur_layers()
  enable_sim_initTerrs  <<- FALSE
  user_select_country <<-FALSE
  modFilter_sub <- "skip launch page - no simulation trigger"  
}      
           
 
####################### modFilter = site selection (session #1) 
if(modFilter =="site selection") {  
   print(paste0(" * modFilter: ",modFilter, " *"))
  user_select_country <<- FALSE #(defaults to Scotland when FALSE)
  modFilter_sub <- "session #1 - 4oct23 - layout & site selection"
}
 
 

####################### modFilter = points location (site not pre-selected) (session #2)
 if(modFilter ==  "locating release points") {
     modFilter_sub <- "session #2 - 18oct23 - locating release points"
 user_select_country <<- FALSE #(defaults to Scotland when FALSE)
#   list(  click_lat= 57.4956, 
#           click_lng= -3.1716,
#           buffer_val = 5000, 
#           shape= "buffer") %>% sitedat()  
     rv_mapctry$bnd_cntry <- bnd_UK[1,] 
     modFilter_sub <- "session #2 - 18oct23 - site selection & group characteristics"
## in script -  click_Site_disabled(TRUE)  
}





### assume country is Scotland if skiping country selection (launch page)
 if(isFALSE(user_select_country)){ 
  rv_mapctry$bnd_cntry <- bnd_UK[1,]  
    }

 

 