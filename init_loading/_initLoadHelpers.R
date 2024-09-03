cat("helpers  - ")

############## some  helper functions 
 
st_erase = function(x, y) st_difference(x, st_union(st_combine(y))) # erase all of y from x




matrix.to.SpatPolys <- function (coords, proj=CRS(as.character(NA))) {
  poly <- Polygon(coords)
  polys <- lapply(list(poly), function(x) {Polygons(list(x), ID="foo")}) #works
  for(i in 1:length(polys)) polys[[i]]@ID <- as.character(i)
  spolys <- SpatialPolygons(polys, proj4string=proj)
  return(spolys)
}





psample <- function(x, ...) x[sample(length(x), ...)]






`%nin%` = Negate(`%in%`)





# aes several scales made easy
new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}


options(ggrepel.max.overlaps = Inf) 