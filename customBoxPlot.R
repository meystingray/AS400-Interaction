

customBoxPlot <- function(thisFrame) {
  
# get unique resins

analysisFrame <- thisFrame
go <- TRUE
while (go == TRUE) {
  uniqueResins <- as.data.frame(unique(analysisFrame$Prime.Resin))  
  print(uniqueResins)
  skipNum <- readline("Enter a resin number to exclude from analysis, 0 to skip: ")
  if (skipNum == 0) {go <- FALSE
  } else {
    analysisFrame <- subset(analysisFrame,!(uniqueResins[skipNum] %in% analysisFrame$Prime.Resin))
  }
}
  
  
  
  
}