cat("demographics  - ")

func_Nyng <- function(demog_gp,Nfams, Nmin, Nmax){ 
 if(demog_gp =="all adults") { Nyg_options<-  rep(0,Nfams)    
      } else {
      Nyg_options <-sample(seq(Nmin, Nmax), Nfams, replace=TRUE)
        
      if(demog_gp =="combination") { Nyg_options[sample(Nfams, round(Nfams/2))] <- 0
         }  #  some have no young  
      }
  return(Nyg_options)
}
  
  