read.Data <- function(filePathArg=NULL,dataType=NULL) {

thisWD <- getwd()
setwd("C:/Users/seanco/Desktop/R/AS400")

# ensure dataType ________________________________________________
if (is.null(dataType))  {
  go <- FALSE
  while (go == FALSE) {
    dataType <- readline("What is the data type: LAB, PRG, PQF, PCY: ")
  if (dataType == "LAB" | dataType == "PRG" | dataType == "PQF" | dataType == "PCY") {go <- TRUE}
  }
}
# ________________________________________________________________


# use dataType to set values _____________________________________
if (dataType == "LAB") {header <- read.table("LabHeaders.csv",header=FALSE,sep=",")}
if (dataType == "PRG") {header <- read.table("PRGHeaders.csv",header=FALSE,sep=",")}
if (dataType == "PQF") {header <- read.table("PQFHeaders.csv",header=FALSE,sep=",")}
if (dataType == "PCY") {header <- read.table("PCYHeaders.csv",header=FALSE,sep=",")}
# ________________________________________________________________

allFlag = FALSE
# check filePathArg
if (is.null(filePathArg)) { # if null, have user choose file
  thisFile = choose.files()
  numFiles = 1
  # else if filePathArg is not null and not a file, then set allFlag to TRUE, setwd that path
} else if (grepl(".csv",filePathArg) == 0) { 
    setwd(filePathArg)
    allFlag <- TRUE
    thisDir <- dir()
    numFiles = length(thisDir)
} else { # not null, is a .csv
    thisFile = filePathArg
    numFiles = 1
}


useHeader <- as.factor(t(header[1,]))
useClasses <- t(header[2,])

# At this point:
# if filepatharg was null, it now has the file name
# if filepathArg was not null and wasn't a .csv, the path is now set to it
# if filePath was not null and was a .csv, it is now thisFile 

thisData <- data.frame()
completeData = data.frame()

for (i in 1:numFiles) {
  if (allFlag == TRUE) { thisFile <- thisDir[i] }

  if (dataType == "PRG") {
    thisData <- read.table(thisFile,header=FALSE,sep=",",comment.char="",skip=1,strip.white=TRUE,na.strings="EMPTY",fill=TRUE,stringsAsFactors=TRUE, col.names = useHeader,colClasses = useClasses)
    thisData[["GRS.WGT"]] <- as.numeric(thisData[["GRS.WGT"]])
    thisData[["NET.WGT"]] <- as.numeric(thisData[["NET.WGT"]])
    thisData[["VAR.ERROR"]] <- thisData$GRS.WGT - thisData$NOM.WGT
    #thisData$DATE  <- as.Date(thisData$DATE,"%m/%d/%Y")
      
 } else if (dataType == "LAB") {
     thisData <- read.table(thisFile,header=FALSE,sep=",",comment.char="",skip=4,strip.white=TRUE,na.strings="EMPTY",fill=TRUE,stringsAsFactors=TRUE, col.names = useHeader,colClasses = useClasses)
     thisData$Date  <- as.Date(thisData$Date,"%Y/%m/%d")
     
 } else if (dataType == "PQF") {
   #thisData[,1]  <- as.Date(thisData[,1],"%m/%d/%Y")
   thisData[["Skid.ID"]] <- as.numeric(thisData[["Skid.ID"]])
   
 } else if (dataType == "PCY") {
    thisData[["Convert.Line"]] <- as.numeric(thisData[["Convert.Line"]])
    thisData[["Rollstock.Line"]] <- as.numeric(thisData[["Rollstock.Line"]])
    times <- thisData$Convert.Mfg.Time
    timelen <- sapply(times,nchar)
    times <- paste("0",times[timelen==1])
 }
  
  
  completeData <- rbind(completeData,thisData)
  
}

x <- readline("Output to file? 1 for Yes, 0 for No: ")
if (x == 1) {
  y <- readline("Name to use for the output file?  ")  
  if (grepl(".csv",y) == 0) {y <- paste(y,".csv",sep="")}
  write.csv(completeData,y)
}

return(completeData)
setwd(thisWD)
}