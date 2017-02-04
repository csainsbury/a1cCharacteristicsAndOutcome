library(data.table)

cleanTimeSeriesData<-function(valNumeric,dateplustime1,timeBinDays) {
  cleanFrame<-data.frame(valNumeric,dateplustime1)
  if (nrow(cleanFrame)>1) {
    cleanFrame$timeDisplaced<-0; cleanFrame$timeDisplaced[2:nrow(cleanFrame)]<-cleanFrame$dateplustime1[1:nrow(cleanFrame)-1]
    cleanFrame$timeDiff<-cleanFrame$dateplustime1-cleanFrame$timeDisplaced
    cleanFrame$useDataPoint<-ifelse(cleanFrame$timeDiff>(60*60*24*timeBinDays),1,0)
  }
  return(cleanFrame$useDataPoint)
}

returnUnixDateTime<-function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

extractDate<-"2016-11-05"; extractDateUnix<-returnUnixDateTime(extractDate)

##### times to exclude neighbouring values for cleaning function
hba1cTimeBinDays<-60
sbpTimeBinDays<-14

################## ###########################

hba1cSet<-paste("../GlCoSy/SDsource/partialGlucoseDataExport.txt",sep="")
hba1cSetDF<-read.csv(hba1cSet); hba1cSetDT<-data.table(hba1cSetDF)
hba1cDT<-hba1cSetDT[DataItemId==1]
hba1cDT$hba1cNumeric<-as.numeric(levels(hba1cDT$DataItemValue))[hba1cDT$DataItemValue]
hba1cDT$dateplustime1<-as.numeric(as.POSIXct(hba1cDT$DataItemDate, format="%Y-%m-%d", tz="GMT"))


hba1cDT<-hba1cDT[dateplustime1>returnUnixDateTime("1980-01-01") & dateplustime1<returnUnixDateTime("2017-01-01")]
hba1cDT<-hba1cDT[hba1cNumeric>0 & hba1cNumeric<200]

## turn % values into mmolmol values for those values <20
hba1cDT$newNumeric<-ifelse(hba1cDT$hba1cNumeric<20,(hba1cDT$hba1cNumeric-2.152)/0.09148,hba1cDT$hba1cNumeric)
hba1cDT<-hba1cDT[order(LinkId,dateplustime1),]

hba1cDT<-hba1cDT[newNumeric>0]
hba1cDT[, c("useDataPoint") := cleanTimeSeriesData(newNumeric,dateplustime1,hba1cTimeBinDays) , by=.(LinkId)]

hba1cDTclean<-hba1cDT[useDataPoint==1]

write.table(hba1cDTclean, file="../GlCoSy/SD_workingSource/hba1cDTclean2.csv", sep=",", col.names = TRUE)

################## ###########################