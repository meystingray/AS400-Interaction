

customBoxPlot <- function(thisFrame) {
  
  # get unique resins
  analysisFrame <- thisFrame
  go <- TRUE
  while (go == TRUE) {
    uniqueResins <- unique(analysisFrame$Prime.Resin)  
    print(as.data.frame(uniqueResins))
    skipNum <- as.numeric(readline("Enter a resin number to exclude from analysis, 0 to exit: "))
    if (skipNum == 0) {go <- FALSE}
    else {analysisFrame <- analysisFrame[!uniqueResins[skipNum] == analysisFrame$Prime.Resin,]}
  }

uniqueResins <- unique(analysisFrame$Prime.Resin)
labelstring = c()
for (i in 1:length(uniqueResins)) {
    labelstring[i] <- median(subset(analysisFrame,Prime.Resin == uniqueResins[i],select = Dart.Offline)$Dart.Offline)
}

print(labelstring)  
boxplot(analysisFrame$Dart.Offline ~ analysisFrame$Prime.Resin,col="green")
text(x = 1.25, labels = labelstring)

}