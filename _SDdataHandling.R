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

## load diagnosis/age/duration data

diagnosisSet<-paste("../GlCoSy/SDsource/diagnosisDateDeathDate.txt",sep="")
diagnosisSetDF<-read.csv(diagnosisSet); diagnosisSetDT<-data.table(diagnosisSetDF)
  
  diagnosisSetDT$birthDateUnix<-returnUnixDateTime(diagnosisSetDT$BirthDate)
  diagnosisSetDT$diagnosisDateUnix<-returnUnixDateTime(diagnosisSetDT$DateOfDiagnosisDiabetes_Date)
  diagnosisSetDT$DeathDateUnix<-ifelse(diagnosisSetDT$DeathDate!="",returnUnixDateTime(diagnosisSetDT$DeathDate),0)
  diagnosisSetDT$diabetesDurationYears<-ifelse(diagnosisSetDT$DeathDateUnix!=0,(diagnosisSetDT$DeathDateUnix-diagnosisSetDT$diagnosisDateUnix)/(60*60*24*365.25),(extractDateUnix-diagnosisSetDT$diagnosisDateUnix)/(60*60*24*365.25))
  diagnosisSetDT$ageAtExtractOrDeath<-ifelse(diagnosisSetDT$DeathDateUnix!=0,(diagnosisSetDT$DeathDateUnix-diagnosisSetDT$birthDateUnix)/(60*60*24*365.25),(extractDateUnix-diagnosisSetDT$birthDateUnix)/(60*60*24*365.25))
  
  diagnosisSetDT<-diagnosisSetDT[diabetesDurationYears>0 & diabetesDurationYears<100]
  
  type1DiagnosisSetDT<-diagnosisSetDT[DiabetesMellitusType_Mapped=="Type 1 Diabetes Mellitus"]
  type2DiagnosisSetDT<-diagnosisSetDT[DiabetesMellitusType_Mapped=="Type 2 Diabetes Mellitus"]
  

hba1cSet<-paste("../GlCoSy/SDsource/partialGlucoseDataExport.txt",sep="")
hba1cSetDF<-read.csv(hba1cSet); hba1cSetDT<-data.table(hba1cSetDF)
  hba1cDT<-hba1cSetDT[DataItemId==1]
  hba1cDT$hba1cNumeric<-as.numeric(levels(hba1cDT$DataItemValue))[hba1cDT$DataItemValue]
  hba1cDT<-hba1cDT[hba1cNumeric>20 & hba1cNumeric<200]
  hba1cDT$dateplustime1<-as.numeric(as.POSIXct(hba1cDT$DataItemDate, format="%Y-%m-%d", tz="GMT"))
  hba1cDT<-hba1cDT[order(LinkId,dateplustime1),]
  
  # clean data -remove duplicate and early repeat values. lockout time period set above
  hba1cDT[, c("useDataPoint") := cleanTimeSeriesData(hba1cNumeric,dateplustime1,hba1cTimeBinDays) , by=.(LinkId)]
  hba1cDTclean<-hba1cDT[useDataPoint==1]
  
  write.table(hba1cDTclean, file="../GlCoSy/SD_workingSource/hba1cDTclean.csv", sep=",", col.names = TRUE)
  
  # t1dm_hba1c<-merge(hba1cDT,type1DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  # t2dm_hba1c<-merge(hba1cDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")

bpSet<-paste("../GlCoSy/SDsource/bpData.txt",sep="")
bpSetDF<-read.csv(bpSet); bpSetDT<-data.table(bpSetDF)
  SBPsetDT<-bpSetDT[DataItemLabel=="SystolicBloodPressure"]
    SBPsetDT$sbpNumeric<-as.numeric(levels(SBPsetDT$DataItemValue))[SBPsetDT$DataItemValue]
    SBPsetDT<-SBPsetDT[sbpNumeric>0 & sbpNumeric<280]
    SBPsetDT$dateplustime1<-as.numeric(as.POSIXct(SBPsetDT$DataItemDate, format="%Y-%m-%d", tz="GMT"))
    SBPsetDT<-SBPsetDT[dateplustime1>473385600 & dateplustime1<as.numeric(Sys.time())]
    SBPsetDT<-SBPsetDT[order(LinkId,dateplustime1),]
    
    # clean data -remove duplicate and early repeat values. lockout time period set above: sbpTimeBinDays
    SBPsetDT[, c("useDataPoint") := cleanTimeSeriesData(sbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
    SBPsetDTclean<-SBPsetDT[useDataPoint==1]
    
    write.table(SBPsetDTclean, file="../GlCoSy/SD_workingSource/SBPsetDTclean.csv", sep=",", col.names = TRUE)
    
    
  DBPsetDT<-bpSetDT[DataItemLabel=="DiastolicBloodPressure"]
    DBPsetDT$dbpNumeric<-as.numeric(levels(DBPsetDT$DataItemValue))[DBPsetDT$DataItemValue]
    DBPsetDT<-DBPsetDT[dbpNumeric>0 & dbpNumeric<280]
    DBPsetDT$dateplustime1<-as.numeric(as.POSIXct(DBPsetDT$DataItemDate, format="%Y-%m-%d", tz="GMT"))
    DBPsetDT<-DBPsetDT[dateplustime1>473385600 & dateplustime1<as.numeric(Sys.time())]
    DBPsetDT<-DBPsetDT[order(LinkId,dateplustime1),]
    
    # clean data -remove duplicate and early repeat values. lockout time period set above: sbpTimeBinDays
    DBPsetDT[, c("useDataPoint") := cleanTimeSeriesData(dbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
    DBPsetDTclean<-DBPsetDT[useDataPoint==1]
    
    write.table(DBPsetDTclean, file="../GlCoSy/SD_workingSource/DBPsetDTclean.csv", sep=",", col.names = TRUE)
    
  
 #  t1dm_sbp<-merge(SBPsetDT,type1DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  # t2dm_sbp<-merge(SBPsetDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  #  t2dm_dbp<-merge(DBPsetDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
    
    
  albuminSet<-paste("../GlCoSy/SDsource/albumin.txt",sep="")
  albuminSetDF<-read.csv(albuminSet); albuminSetDT<-data.table(albuminSetDF)
  albuminSetACRDT<-albuminSetDT[DataItemLabel=="AlbuminCreatinineRatio"]
  albuminSetACRDT$dateplustime1<-returnUnixDateTime(albuminSetACRDT$DataItemDate)
  albuminSetACRDT<-albuminSetACRDT[dateplustime1>473385600]
  albuminSetACRDT$acrNumeric<-as.numeric(levels(albuminSetACRDT$DataItemValue))[albuminSetACRDT$DataItemValue]
  albuminSetACRDT<-albuminSetACRDT[acrNumeric>=0 & acrNumeric<1000]
  albuminSetACRDT<-albuminSetACRDT[order(LinkId,dateplustime1),]
  
  # clean data -remove duplicate and early repeat values. lockout time period set above: hba1cTimeBinDays
  albuminSetACRDT[, c("useDataPoint") := cleanTimeSeriesData(acrNumeric,dateplustime1,hba1cTimeBinDays) , by=.(LinkId)]
  albuminSetACRDTclean<-albuminSetACRDT[useDataPoint==1]
  
  write.table(albuminSetACRDTclean, file="../GlCoSy/SD_workingSource/albuminSetACRDTclean.csv", sep=",", col.names = TRUE)
  
  
  
  

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  
  variabilityLastNVals<-function(dateplustime1,numericValue,numberOfTests) {
    
    lengthOriginalNumericValue<-length(numericValue)
    
  #  cleanDataSet<-cleanTimeSeriesData(numericValue,dateplustime1,timeBinDays)
   # numericValue<-cleanDataSet$valNumeric
    #dateplustime1<-cleanDataSet$dateplustime1
    
   # if (length(numericValue)<numberOfTests) {variability=rep(-10,lengthOriginalNumericValue)}
  #  if(length(numericValue)>=numberOfTests) {
     testSUB<-tail(numericValue,numberOfTests)
     # testSUB<-numericValue
      IQRval<-quantile(testSUB)[4]-quantile(testSUB)[2]
      variability<-rep(IQRval,lengthOriginalNumericValue)
      return(variability)
  #  }
  }
  
  variabilityLastNYears<-function(numericValue,dateplustime1,numberOfYears) {
    dateOfFirstValue<-min(dateplustime1)
    dateOfLastValue<- max(dateplustime1)
    intervalInSeconds<-numberOfYears*(60*60*24*365.25)
    
    ## add buffer of 1y (alter with diagnosis date when available)
    intervalInSecondsPlusBuffer<-intervalInSeconds+(60*60*24*365.25)
    
    if (length(numericValue)<numberOfTests) {variability=rep(-10,length(numericValue))}
    if(length(numericValue)>=numberOfTests) {
      testSUB<-tail(numericValue,numberOfTests)
      # testSUB<-numericValue
      IQRval<-quantile(testSUB)[4]-quantile(testSUB)[2]
      variability<-rep(IQRval,length(numericValue))
      return(variability)
    }
  }
  
  
  testNumber<-5
  hba1cTimeBinDays<-60
  sbpTimeBinDays<-14
  timeDurationYears<-2
  

  ###########################################################################
  ## type 1 data
  
  ## generate dataset from raw data
    t1dm_hba1c<-merge(hba1cDT,type1DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
    t1dm_hba1c[, c("useDataPoint") := cleanTimeSeriesData(hba1cNumeric,dateplustime1,hba1cTimeBinDays) , by=.(LinkId)]
    t1dm_hba1c<-t1dm_hba1c[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
    t1dm_hba1c[, nValsPerID := .N , by=.(LinkId)]
    t1dm_hba1c[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
    t1dm_hba1c[, totalDurationHbA1c := (max(dateplustime1)-min(dateplustime1)) , by=.(LinkId)]
  ## remove all IDs with less than N values
    t1dm_hba1c<-t1dm_hba1c[nValsPerID>=testNumber]
  ## generate timeGate set
  #  t1dm_hba1c_Time<-t1dm_hba1c[totalDurationHbA1c>(timeDurationYears*(60*60*24*365.25))+(60*60*24*365.25)]
  ## calculate IQR values
  t1dm_hba1c[, c("hba1cIQR") := variabilityLastNVals(dateplustime1,hba1cNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t1dm_singleRowPerID_hba1c<-t1dm_hba1c[valInSequencePerID==1]
  #  t1dm_singleRowPerID_hba1c<-t1dm_singleRowPerID_hba1c[hba1cIQR>-10]
  
  ## generate dataset from raw data
  t1dm_sbp<-merge(SBPsetDT,type1DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
  t1dm_sbp[, c("useDataPoint") := cleanTimeSeriesData(sbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
  t1dm_sbp<-t1dm_sbp[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
  t1dm_sbp[, nValsPerID := .N , by=.(LinkId)]
  t1dm_sbp[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
  t1dm_sbp[, totalDurationSBP := (max(dateplustime1)-min(dateplustime1)) , by=.(LinkId)]
  ## remove all IDs with less than N values
  t1dm_sbp<-t1dm_sbp[nValsPerID>=testNumber]
  ## calculate IQR values
  t1dm_sbp[, c("sbpIQR") := variabilityLastNVals(dateplustime1,sbpNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t1dm_singleRowPerID_sbp<-t1dm_sbp[valInSequencePerID==1]
  #  t1dm_singleRowPerID_hba1c<-t1dm_singleRowPerID_hba1c[hba1cIQR>-10]
  
  ## generate dataset from raw data
  t1dm_dbp<-merge(DBPsetDT,type1DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
  t1dm_dbp[, c("useDataPoint") := cleanTimeSeriesData(dbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
  t1dm_dbp<-t1dm_dbp[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
  t1dm_dbp[, nValsPerID := .N , by=.(LinkId)]
  t1dm_dbp[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
  t1dm_dbp[, totalDurationDBP := (max(dateplustime1)-min(dateplustime1)) , by=.(LinkId)]
  ## remove all IDs with less than N values
  t1dm_dbp<-t1dm_dbp[nValsPerID>=testNumber]
  ## calculate IQR values
  t1dm_dbp[, c("dbpIQR") := variabilityLastNVals(dateplustime1,dbpNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t1dm_singleRowPerID_dbp<-t1dm_dbp[valInSequencePerID==1]
  #  t1dm_singleRowPerID_hba1c<-t1dm_singleRowPerID_hba1c[hba1cIQR>-10]
  
  #############################################################################
  ## type 2 data
  
  ## generate dataset from raw data
  t2dm_hba1c<-merge(hba1cDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
  t2dm_hba1c[, c("useDataPoint") := cleanTimeSeriesData(hba1cNumeric,dateplustime1,hba1cTimeBinDays) , by=.(LinkId)]
  t2dm_hba1c<-t2dm_hba1c[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
  t2dm_hba1c[, nValsPerID := .N , by=.(LinkId)]
  t2dm_hba1c[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
  ## remove all IDs with less than N values
  t2dm_hba1c<-t2dm_hba1c[nValsPerID>=testNumber]
  ## calculate IQR values
  t2dm_hba1c[, c("hba1cIQR") := variabilityLastNVals(dateplustime1,hba1cNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t2dm_singleRowPerID_hba1c<-t2dm_hba1c[valInSequencePerID==1]

  ## generate dataset from raw data
  t2dm_sbp<-merge(SBPsetDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
  t2dm_sbp[, c("useDataPoint") := cleanTimeSeriesData(sbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
  t2dm_sbp<-t2dm_sbp[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
  t2dm_sbp[, nValsPerID := .N , by=.(LinkId)]
  t2dm_sbp[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
  ## remove all IDs with less than N values
  t2dm_sbp<-t2dm_sbp[nValsPerID>=testNumber]
  ## calculate IQR values
  t2dm_sbp[, c("sbpIQR") := variabilityLastNVals(dateplustime1,sbpNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t2dm_singleRowPerID_sbp<-t2dm_sbp[valInSequencePerID==1]

  ## generate dataset from raw data
  t2dm_dbp<-merge(DBPsetDT,type2DiagnosisSetDT,by.x="LinkId",by.y="LinkId")
  ## find useable values
  t2dm_dbp[, c("useDataPoint") := cleanTimeSeriesData(dbpNumeric,dateplustime1,sbpTimeBinDays) , by=.(LinkId)]
  t2dm_dbp<-t2dm_dbp[useDataPoint==1]
  ## generate indexing columns - number of hba1cs / sequence
  t2dm_dbp[, nValsPerID := .N , by=.(LinkId)]
  t2dm_dbp[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
  t2dm_dbp[, totalDurationDBP := (max(dateplustime1)-min(dateplustime1)) , by=.(LinkId)]
  ## remove all IDs with less than N values
  t2dm_dbp<-t2dm_dbp[nValsPerID>=testNumber]
  ## calculate IQR values
  t2dm_dbp[, c("dbpIQR") := variabilityLastNVals(dateplustime1,dbpNumeric,testNumber) , by=.(LinkId)]
  ## subset to generate single admission per Row dataFrame
  t2dm_singleRowPerID_dbp<-t2dm_dbp[valInSequencePerID==1]

  
  plotSet_T1<-merge(t1dm_singleRowPerID_sbp,t1dm_singleRowPerID_hba1c,by.x="LinkId",by.y="LinkId")
  plotSet_T2<-merge(t2dm_singleRowPerID_sbp,t2dm_singleRowPerID_hba1c,by.x="LinkId",by.y="LinkId")
  
    plotSet_T1_dbp<-merge(t1dm_singleRowPerID_dbp,t1dm_singleRowPerID_hba1c,by.x="LinkId",by.y="LinkId")
    plotSet_T2_dbp<-merge(t2dm_singleRowPerID_dbp,t2dm_singleRowPerID_hba1c,by.x="LinkId",by.y="LinkId")
  
  t1cortest<-cor.test(plotSet_T1$hba1cIQR,plotSet_T1$sbpIQR)
  t2cortest<-cor.test(plotSet_T2$hba1cIQR,plotSet_T2$sbpIQR)
  
  x<-boxplot(plotSet_T1$hba1cIQR ~ cut(plotSet_T1$sbpIQR,breaks=seq(0,80,10)),ylim=c(5,15),varwidth=T,main=paste("T1DM. number of measures=",testNumber,"\nn=",nrow(plotSet_T1),"\ncor.test coef=",round(t1cortest$estimate,3),". p.val=",t1cortest$p.value,sep=""))
  y<-boxplot(plotSet_T2$hba1cIQR ~ cut(plotSet_T2$sbpIQR,breaks=seq(0,80,10)),ylim=c(5,15),varwidth=T,main=paste("T2DM. number of measures=",testNumber,"\nn=",nrow(plotSet_T2),"\ncor.test coef=",round(t2cortest$estimate,3),". p.val=",t2cortest$p.value,sep=""))
 
  plot(plotSet_T1$sbpIQR,plotSet_T1$hba1cIQR,main=paste("n=",nrow(plotSet_T1),sep=""))
  type1fit <- lm(plotSet_T1$sbpIQR ~ plotSet_T1$hba1cIQR)
  abline(type1fit,col="red")
  
  plot(plotSet_T2$sbpIQR,plotSet_T2$hba1cIQR,main=paste("n=",nrow(plotSet_T2),sep=""))
  type2fit <- lm(plotSet_T2$sbpIQR ~ plotSet_T2$hba1cIQR)
  abline(type2fit,col="red")
  
  #####################################
  ## dbp plots
  
  t1cortest_dbp<-cor.test(plotSet_T1_dbp$hba1cIQR,plotSet_T1_dbp$dbpIQR)
  t2cortest_dbp<-cor.test(plotSet_T2_dbp$hba1cIQR,plotSet_T2_dbp$dbpIQR)
  
  x<-boxplot(plotSet_T1_dbp$hba1cIQR ~ cut(plotSet_T1_dbp$dbpIQR,breaks=seq(0,50,5)),ylim=c(5,15),varwidth=T,main=paste("T1DM. number of measures=",testNumber,"\nn=",nrow(plotSet_T1_dbp),"\ncor.test coef=",round(t1cortest_dbp$estimate,3),". p.val=",t1cortest_dbp$p.value,sep=""))
  y<-boxplot(plotSet_T2_dbp$hba1cIQR ~ cut(plotSet_T2_dbp$dbpIQR,breaks=seq(0,50,5)),ylim=c(5,15),varwidth=T,main=paste("T2DM. number of measures=",testNumber,"\nn=",nrow(plotSet_T2_dbp),"\ncor.test coef=",round(t2cortest_dbp$estimate,3),". p.val=",t2cortest_dbp$p.value,sep=""))
  
  
  plot(plotSet_T1_dbp$dbpIQR,plotSet_T1_dbp$hba1cIQR,main=paste("n=",nrow(plotSet_T1_dbp),sep=""),xlim=c(0,30))
  type1fit <- lm(plotSet_T1_dbp$dbpIQR ~ plotSet_T1_dbp$hba1cIQR)
  abline(type1fit,col="red")
  
  plot(plotSet_T2_dbp$dbpIQR,plotSet_T2_dbp$hba1cIQR,main=paste("n=",nrow(plotSet_T2_dbp),sep=""),xlim=c(0,30))
  type2fit <- lm(plotSet_T2_dbp$dbpIQR ~ plotSet_T2_dbp$hba1cIQR)
  abline(type2fit,col="red")
  