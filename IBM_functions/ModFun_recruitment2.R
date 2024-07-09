cat("recruitment2()  -  ")



recruitment2 <- function(id, young, fam, famsize.max) {
#print(c("recruit, fam#",paste0(id )))
 cat("+ ")  
  
  famsize <- fam$num.m[id] + fam$num.f[id]
  young.born <- young[id]
 
  if (famsize==famsize.max | young.born==0) return (c(young.born,0,0))
  
  new.m <- new.f <- 0
  
  if (fam$num.m[id]==0 & fam$num.f[id]==0 & young.born >=2) {
    new.m <- new.f <- 1
    surplus <- young.born - 2
  } else if (fam$num.m[id]==0 & young.born >=1) {
    new.m <- 1
    surplus <- young.born - 1
  } else if (fam$num.f[id]==0 & young.born >=1) {
    new.f <- 1
    surplus <- young.born - 1
  } else {
    new.m <- new.f <- 0
    surplus <- young.born
  }  
   
  return(c(surplus, new.m, new.f))
}


 