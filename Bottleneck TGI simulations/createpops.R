################################################################################
###### Julian Wittische - Spring 2024 - Simulating bottleneck for TGI test #####
################################################################################
# Most of this code comes from the "Landscape Genetic Data Analysis in R" editor by Helen Wagner
# The section it comes from has also be contributed by Bernd Gruber and Erin Landguth (hi!!)

# Selected means habitat selected for populating
# Remaining means the rest of the unpopulated habitat
# HabitatCells = Selected + Remaining
# Populations are minimally isolated by the parameter minDist 

createpops <- function(n=10, minDist=5, landscape=r, habitat=1, plot=TRUE)
{ 
  HabitatCells <- c(1:length(values(landscape)))[values(landscape)==habitat]
  Selected <- sample(HabitatCells, 1)
  Remaining <- HabitatCells[!is.element(HabitatCells, Selected)]
  while (length(Selected) < n & length(Remaining) > 0)
  {
    Candidate <- sample(Remaining, 1)
    Remaining <- Remaining[!is.element(Remaining, Candidate)]
    Distances <- raster::pointDistance(raster::xyFromCell(landscape, Candidate), 
                                       raster::xyFromCell(landscape, Selected), 
                                       lonlat=FALSE)
    if(min(Distances) > minDist)
    {
      Selected <- append(Selected, Candidate)
    }
  }
  if(plot==TRUE) 
  {
    plot(landscape, col=grey.colors(2, rev=TRUE))  
    points(xyFromCell(landscape, Selected), pch=17, col="blue")
  }
  return(Selected)
}