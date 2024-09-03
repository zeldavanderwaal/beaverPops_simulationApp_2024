cat("map reading  - ")


## local datasets uploaded in-app, per country:
## country_shape4326    - 
## rivCatch27700        -
## rivCatch4326         -
## suit_perCatch_noNA   -
## habitat_3857         -
    

    load_cntryBdry_4326 <- function(country) { 
      if(!is.null(country)){
        return(st_read( here::here(country,"country_shape4326.shp") , quiet=TRUE))
      }
    }
    
    load_intercatch_27700 <- function(country) {
      if(!is.null(country)){
       return(st_read(paste0(here::here(country,"rivCatch27700.shp")), quiet=TRUE)) # 10% simplified geoms
     } 
    } 
    
    load_intercatch_4326 <- function(country) {
     if(!is.null(country)){
      return(st_read(paste0(here::here(country,"rivCatch4326.shp")), quiet=TRUE)) # 10% simplified geoms
     }
    }
    
    load_suitPerCatch_4326  <- function(country) {
     if(!is.null(country)){
      return( st_read(here::here( country,"suitablePatches4326.shp"), quiet=TRUE  )  )
     }
    }
      
    load_habitat_3857 <- function(country) {
     if(!is.null(country)){ 
    habras <- terra::rast(here::here(country,'habitat3857.tif')) 
    habras <- terra::wrap(habras, proxy=FALSE)
    return(habras )
    }
    }
    
    
    load_opeSurf_4326 <- function(country) {
     if(!is.null(country)){
       return(st_read(paste0(here::here(country,"opeSurf4326.shp")), quiet=TRUE) )
     }
    }
    
    load_mgmtSurf_4326 <- function(country) {
     if(!is.null(country)){
      return( st_read(paste0(here::here(country,"mgmtSurf4326.shp")), quiet=TRUE)  )
     }
    }

 # sf_use_s2(FALSE)
