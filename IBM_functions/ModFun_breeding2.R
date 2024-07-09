cat("breeding2()  -  ")


breeding2 <- function(id, fam, litter.size, breed.prob) {
cat("b ")  
  if(fam$num.m[id]>0 & fam$num.f[id]>0 & runif(1)<breed.prob) {
    young <- rpois(1, litter.size)
  } else {
    young <- 0
  } 
  return(young)
}

 
