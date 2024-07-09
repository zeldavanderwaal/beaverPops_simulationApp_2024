cat("param values  (IBM, GIS, UI)  -  ") 
 
## proj used
mercproj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs"

## hab quality threshold value
hab.tot.quality <- 15 ### FOR TESTING OTHERWISE 15

#### life history parameters
litter.size <- 1.95
mort.juv <- 1 - 0.92
mort.sub <- 1 - 0.87
mort.adt <- 1 - 0.87
famsize.max <- 9 #11?
move.max    <- 210 #21km in 100m grid squares
breed.prob  <- 0.6 #0.6


#### set up for output storage
out.all  <- routes.all <- NULL
fam.all  <- ter.all    <- list()
fam.all2 <- ter.all2   <- list()
fam_y6   <- ter_y6     <- list()

 
mgmt.reps  <- 15





side_width = 4  # for the ui