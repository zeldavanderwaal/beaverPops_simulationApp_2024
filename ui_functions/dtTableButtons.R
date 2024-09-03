cat("dt butns  -")

addDeleteButtons <- function ( trigger, condition, suffname, Nbuttons, df) {
   df <-  as.data.frame(df)
   if(base::isTRUE(condition)) {  
   df <-  as.data.frame(df)  %>% dplyr::bind_cols(tibble("Buttons" =create_btns(paste0(suffname,1:Nbuttons)))) 
   df$Buttons[which(is.na(df[,2]))] <- "" 
  }
  return(df)
}
 