# analyse AS400 Lab Data

explore <- function(thisDataFrame)  {

#oldpar <- par()
par(mfrow=c(2,1))

# What products are in this data?
tout <- sort(table(thisDataFrame$Product))
lbls <- paste(names(tout),round(100*tout/sum(tout),1))
lbls <- paste(lbls,"%", sep=" ")
print(sort(table(thisDataFrame$Product)))
pie(sort(table(thisDataFrame$Product)),cex=0.5,labels=lbls)


# What Lines generated this data?
uniqueLines <- unique(thisDataFrame$Line)
numUniqueLines <- length(uniqueLines)
bp = barplot(table(d$Line),las=2,col=gray(0:numUniqueLines/numUniqueLines  ))
axis(1,at=uniqueLines,tck=1)
text(bp, table(thisDataFrame$Line), table(thisDataFrame$Line),cex=1,pos=3) 


#par(oldpar)
}