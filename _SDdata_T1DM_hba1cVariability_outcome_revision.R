library(data.table)
library(ggplot2)
library(randomForest)
library(survival)

set.seed(42)

## functions
returnUnixDateTime <- function(date) {
  returnVal<-as.numeric(as.POSIXct(date, format="%Y-%m-%d", tz="GMT"))
  return(returnVal)
}

## return flag for values that are within either endDate-predictionDurationYears or deathDate-predictionDurationYears
flagValuesWithinRange<-function(LinkId,dateplustime1,predictionDurationYears,runInMonths,endDateUnix,deathDateUnix) {
  
  # print(LinkId)
  
  runIn<-runInMonths*(60*60*24*(365.25/12))
  predictionDuration<-predictionDurationYears*(60*60*24*365.25)
  testDF<-data.frame(dateplustime1)
  isDead<-ifelse(sum(deathDateUnix)>0,1,0)
  
  testTimePoint<-ifelse(isDead==1,max(deathDateUnix)-predictionDuration,endDateUnix-predictionDuration)
  startRunIn<-testTimePoint-runIn
  
  testDF$flagForUse<-ifelse(testDF$dateplustime1>startRunIn & testDF$dateplustime1<testTimePoint,1,0)
  
  return(testDF$flagForUse)
}

## return flag for values that are within either endDate-predictionDurationYears or deathDate-predictionDurationYears
flagValuesWithinRangeForSpecifiedTimePoint<-function(LinkId,dateplustime1,runInMonths,sampleDateUnix) {
  
  # print(LinkId)
  
  runIn<-runInMonths*(60*60*24*(365.25/12))
  testDF<-data.frame(dateplustime1)
  # isDead<-ifelse(sum(deathDateUnix)>0,1,0)
  
  testTimePoint<-sampleDateUnix
  startRunIn<-testTimePoint-runIn
  
  testDF$flagForUse<-ifelse(testDF$dateplustime1>startRunIn & testDF$dateplustime1<testTimePoint,1,0)
  
  return(testDF$flagForUse)
}

simpleSurvivalPlot<-function(inputFrame,endDateUnix,sampleDateUnix,ylimMin) {
  
  SurvivalData<-inputFrame
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$DeathDateUnix-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=5,ylim=c(ylimMin,1))
  
 # mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  
 mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+diabetesDurationYears+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("topright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlot_factor<-function(inputFrame,factor_cvF,endDateUnix,sampleDateUnix,ylimMin) {
  
  SurvivalData<-inputFrame
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$DeathDateUnix-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ factor_cvF, data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty = 1:6, conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=5,ylim=c(ylimMin,1))
  
  # mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+nValsPerIDinRange+nValsPerIDinRange_sbp+factor_cvF, data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6, lwd = 3,col=c("black","red","blue","green","orange","purple"),cex=1.2) # ; legend("topright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlot_noMedianCoV<-function(inputFrame,endDateUnix,sampleDateUnix,ylimMin) {
  
  SurvivalData<-inputFrame
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$DeathDateUnix-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=5,ylim=c(ylimMin,1))
  
  # mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+diabetesDurationYears+nValsPerIDinRange+(hba1cIQRinRange>=quantile(SurvivalData$hba1cIQRinRange)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("topright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlotMultiTest<-function(inputFrame,endDateUnix,sampleDateUnix,testMetric,ylimMin) {
  
  SurvivalData<-data.frame(inputFrame,testMetric)
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$DeathDateUnix-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3,ylim=c(ylimMin,1))
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlotMultiTest_noMedian<-function(inputFrame,endDateUnix,sampleDateUnix,testMetric,ylimMin) {
  
  SurvivalData<-data.frame(inputFrame,testMetric)
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$isDead==1,(SurvivalData$DeathDateUnix-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$isDead==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$isDead
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$testMetric)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3,ylim=c(ylimMin,1))
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+nValsPerIDinRange+(testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlotVariableOutcome<-function(inputFrame,endDateUnix,sampleDateUnix,outcomeData,testMetric,ylimMin) {
  
  SurvivalData<-data.frame(inputFrame,testMetric,outcomeData)
  
  SurvivalData$reachesOutcome<-ifelse(SurvivalData$outcomeData>0 & SurvivalData$outcomeData < max(SurvivalData$outcomeData),1,0)
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$reachesOutcome==1,(SurvivalData$outcomeData-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$reachesOutcome==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$reachesOutcome
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  boxplot(SurvivalData$timeToDeathInterval ~ cut(SurvivalData$hba1cIQRinRange,breaks=seq(0,80,2)),varwidth=T)
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3,ylim=c(ylimMin,1))
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+diabetesDurationYears+medianHbA1cInRange+nValsPerIDinRange+(testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlotVariableOutcome_noMedian<-function(inputFrame,endDateUnix,sampleDateUnix,outcomeData,testMetric,ylimMin) {
  
  SurvivalData<-data.frame(inputFrame,testMetric,outcomeData)
  
  SurvivalData$reachesOutcome<-ifelse(SurvivalData$outcomeData>0 & SurvivalData$outcomeData < max(SurvivalData$outcomeData),1,0)
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$reachesOutcome==1,(SurvivalData$outcomeData-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$reachesOutcome==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$reachesOutcome
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  boxplot(SurvivalData$timeToDeathInterval ~ cut(SurvivalData$hba1cIQRinRange,breaks=seq(0,80,2)),varwidth=T)
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3,ylim=c(ylimMin,1))
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+diabetesDurationYears+nValsPerIDinRange+(testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}

simpleSurvivalPlotVariableOutcome_noDD<-function(inputFrame,endDateUnix,sampleDateUnix,outcomeData,testMetric,ylimMin) {
  
  SurvivalData<-data.frame(inputFrame,testMetric,outcomeData)
  
  SurvivalData$reachesOutcome<-ifelse(SurvivalData$outcomeData>0,1,0)
  
  DaySeconds<-(60*60*24)
  shortCensorPeriodStartDay  <- DaySeconds
  shortCensorPeriodEndDay    <- DaySeconds*10000
  
  lastDOD<-endDateUnix
  SurvivalData$dateOfDischarge<-sampleDateUnix
  SurvivalData$timeToDeath<-ifelse(SurvivalData$reachesOutcome==1,(SurvivalData$outcomeData-SurvivalData$dateOfDischarge),0)
  #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
  SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$reachesOutcome==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
  SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
  # SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/(60*60*24*365.25)
  
  SurvivalData$shortDeathEvent <- SurvivalData$reachesOutcome
  # SurvivalData$shortDeathEvent <- ifelse(SurvivalData$isDead==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
  
  #  SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
  # SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
  #  SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
  
  boxplot(SurvivalData$timeToDeathInterval ~ cut(SurvivalData$hba1cIQRinRange,breaks=seq(0,80,2)),varwidth=T)
  
  mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds," days\n n= ",nrow(SurvivalData),", threshold: ",quantile(SurvivalData$hba1cIQRinRange)[3],sep="")
  plot(mfitAge50,mark.time=T,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3,ylim=c(ylimMin,1))
  mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age_atSampleTime+medianHbA1cInRange+nValsPerIDinRange+(testMetric>=quantile(SurvivalData$testMetric)[3]), data = SurvivalData)
  pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
  legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
  summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
  legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
  
  print(mfitAge50.coxph)
  
}



########################################################################################
## load in datasets

diagnosisSetDF<-read.csv("~/R/GlCoSy/SD_workingSource/diagnosisSetDT.csv")
diagnosisSetDF<-subset(diagnosisSetDF,diagnosisDateUnix>returnUnixDateTime("1900-01-01"))
diagnosisSetDF<-subset(diagnosisSetDF,birthDateUnix>returnUnixDateTime("1900-01-01"))
limitedDeathSetDF<-data.frame(diagnosisSetDF$LinkId,diagnosisSetDF$DeathDateUnix); colnames(limitedDeathSetDF)<-c("LinkId","DeathDateUnix")

hba1cDF<-read.csv("~/R/GlCoSy/SD_workingSource/hba1cDTclean2.csv")
hba1cDF<-merge(hba1cDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")

SBPsetDF<-read.csv("~/R/GlCoSy/SD_workingSource/SBPsetDTclean.csv")
SBPsetDF<-merge(SBPsetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")

DBPsetDF<-read.csv("~/R/GlCoSy/SD_workingSource/DBPsetDTclean.csv")
DBPsetDF<-merge(DBPsetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# 
# # albuminSetACRDF<-read.csv("../GlCoSy/SD_workingSource/albuminSetACRDTclean.csv")
# # albuminSetACRDF<-merge(albuminSetACRDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# 
# eyeSetDF<-read.csv("~/R/GlCoSy/SD_workingSource/eyeSetDT.csv")
# eyeSetDF<-merge(eyeSetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# 
# bmiSetDF<-read.csv("~/R/GlCoSy/SD_workingSource/BMISetDTclean.csv")
# bmiSetDF<-merge(bmiSetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# 
# renalSetDF<-read.csv("~/R/GlCoSy/SD_workingSource/renalSetDTclean.csv")
# renalSetDF<-merge(renalSetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# 
# albuminSetDF<-read.csv("~/R/GlCoSy/SD_workingSource/albuminSetACRDTclean.csv")
# albuminSetDF<-merge(albuminSetDF,limitedDeathSetDF,by.x="LinkId",by.y="LinkId")
# albuminSetDF$logACRnumeric<-log(albuminSetDF$acrNumeric)

#########################
# predictionDurationYears<-outputTNframe$valueToInject[jj]
## endDate - right censor point for survival
endDate<-"2016-12-01"; endDateUnix<-returnUnixDateTime(endDate)
##  sampleDate - arbitrary time point for sampling
sampleDate<-"2013-01-01"; sampleDateUnix<-returnUnixDateTime(sampleDate)

runInMonths<-30


diagnosisSetDT<-data.table(diagnosisSetDF)
diagnosisSetDT$isDead<-ifelse(diagnosisSetDT$DeathDateUnix>0,1,0)
hba1cDT<-data.table(hba1cDF)
SBPsetDT<-data.table(SBPsetDF)
DBPsetDT<-data.table(DBPsetDF)
# eyeSetDT<-data.table(eyeSetDF)
# bmiSetDT<-data.table(bmiSetDF)
# renalSetDT<-data.table(renalSetDF)
# albuminSetDT<-data.table(albuminSetDF)

########################################################################################
## set up values to pass to RF for each paramter
coreDataPrepDT<-data.table(diagnosisSetDT$birthDateUnix,diagnosisSetDT$LinkId,diagnosisSetDT$DeprivationQuintile,diagnosisSetDT$DiabetesMellitusType_Mapped,diagnosisSetDT$ageAtExtractOrDeath,diagnosisSetDT$diabetesDurationYears,diagnosisSetDT$isDead,diagnosisSetDT$diagnosisDateUnix,diagnosisSetDT$DeathDateUnix)
colnames(coreDataPrepDT)<-c("birthDateUnix","LinkId","DeprivationQuintile","DiabetesMellitusType_Mapped","ageAtExtractOrDeath","diabetesDurationYears","isDead","diagnosisDateUnix","DeathDateUnix")
coreDataPrepDT$diabetesDurationYears<-(sampleDateUnix-coreDataPrepDT$diagnosisDateUnix)/(60*60*24*365.25)
# coreDataPrepDT$ageAtPredictionPoint<-coreDataPrepDT$ageAtExtractOrDeath-predictionDurationYears
# coreDataPrepDT$timeAtPredictionPoint<-coreDataPrepDT$birthDateUnix+(coreDataPrepDT$ageAtPredictionPoint*(60*60*24*365.25))
# coreDataPrepDT$timeAtExtractOrDeath<-coreDataPrepDT$birthDateUnix+(coreDataPrepDT$ageAtExtractOrDeath*(60*60*24*365.25))

# coreDataPrepDT$ageAtExtractOrDeath<-NULL

# find proportion of pumps in last year
t1_notDead <- coreDataPrepDT[DiabetesMellitusType_Mapped == 'Type 1 Diabetes Mellitus' & isDead == 0]



# hba1c
hba1cDT[, c("flagValuesWithinRangeForSpecifiedTimePoint") := flagValuesWithinRangeForSpecifiedTimePoint(LinkId,dateplustime1,runInMonths,sampleDateUnix) , by=.(LinkId)]
hba1cDT<-hba1cDT[flagValuesWithinRangeForSpecifiedTimePoint==1]
hba1cDT[, nValsPerIDinRange := .N , by=.(LinkId)]
hba1cDT[, valInSequencePerID := seq(1,.N,1) , by=.(LinkId)]
hba1cDT[, hba1cIQRinRange := (quantile(newNumeric)[4]-quantile(newNumeric)[2]) , by=.(LinkId)]
hba1cDT[, medianHbA1cInRange := (median(newNumeric)) , by=.(LinkId)]

hba1cDT[, meanHbA1cInRange := (mean(newNumeric)) , by=.(LinkId)]
hba1cDT[, sd_HbA1cInRange := (sd(newNumeric)) , by=.(LinkId)]

CV <- function(mean, sd){
  (sd/mean)*100
}

npCV <- function(median, IQR){
  (IQR/median)*100
}

qcD <- function(newNumeric) {
  q_1 <- quantile(newNumeric)[2]
  q_3 <- quantile(newNumeric)[4]
  
  qcD <- ((q_3 - q_1) / (q_1 + q_3))
}

hba1cDT[, CV_HbA1cInRange := (CV(meanHbA1cInRange, sd_HbA1cInRange)) , by=.(LinkId)]
hba1cDT[, npCV_HbA1cInRange := (npCV(medianHbA1cInRange, hba1cIQRinRange)) , by=.(LinkId)]
hba1cDT[, qcD_HbA1cInRange := (qcD(newNumeric)) , by=.(LinkId)]

hba1cDTforMerge<-hba1cDT[valInSequencePerID==1]
# hba1cDTforMerge<-hba1cDTforMerge[hba1cIQRinRange>0]
hba1cMergeSubset<-data.table(hba1cDTforMerge$LinkId,hba1cDTforMerge$hba1cIQRinRange,hba1cDTforMerge$medianHbA1cInRange,hba1cDTforMerge$nValsPerIDinRange, hba1cDTforMerge$CV_HbA1cInRange, hba1cDTforMerge$npCV_HbA1cInRange, hba1cDTforMerge$qcD_HbA1cInRange, hba1cDTforMerge$meanHbA1cInRange)
colnames(hba1cMergeSubset)<-c("LinkId", "hba1cIQRinRange", "medianHbA1cInRange", "nValsPerIDinRange", "CV_HbA1cInRange", "npCV_HbA1cInRange", "qcD_HbA1cInRange", "meanHbA1cInRange")


###############################################################################################
masterAnalysisSet<-merge(coreDataPrepDT,hba1cMergeSubset,by.x="LinkId",by.y="LinkId",all.x=T); print(nrow(masterAnalysisSet))
#
# remove those who die during the run in - before the sampleDate
hbA1cAnalysisSet<-masterAnalysisSet[DeathDateUnix>sampleDateUnix | DeathDateUnix==0]
#
## add age at sample time
hbA1cAnalysisSet$age_atSampleTime<-(sampleDateUnix-hbA1cAnalysisSet$birthDateUnix)/(60*60*24*365.25)
#
## show distribution of number of hba1c measures during runin
hist(hbA1cAnalysisSet$nValsPerIDinRange)
hbA1cAnalysisSet<-hbA1cAnalysisSet[nValsPerIDinRange>1]
#
T1_hbA1cAnalysisSet<-hbA1cAnalysisSet[DiabetesMellitusType_Mapped=="Type 1 Diabetes Mellitus"]
T2_hbA1cAnalysisSet<-hbA1cAnalysisSet[DiabetesMellitusType_Mapped=="Type 2 Diabetes Mellitus"]
#
simpleSurvivalPlot(T1_hbA1cAnalysisSet,endDateUnix,sampleDateUnix,0.9)
simpleSurvivalPlot(T2_hbA1cAnalysisSet,endDateUnix,sampleDateUnix,0.8)
#
# survival in the subsets who have a duration of diabetes longer than the runin period
simpleSurvivalPlot(subset(T1_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12)),endDateUnix,sampleDateUnix,0.9)
simpleSurvivalPlot(subset(T1_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12)),endDateUnix,sampleDateUnix,0.9)
simpleSurvivalPlot(subset(T2_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12)),endDateUnix,sampleDateUnix,0.6)

simpleSurvivalPlot(subset(T1_hbA1cAnalysisSet,diabetesDurationYears>((runInMonths/12)) & age_atSampleTime<25),endDateUnix,sampleDateUnix,0.9)

fit <- glm(formula = isDead ~ (age_atSampleTime + diabetesDurationYears + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange), family = binomial(link = "logit"), data = subset(T1_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12)))
# remove diabetes duration as non-significant in logistic regression
fit <- glm(formula = isDead ~ (age_atSampleTime + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange), family = binomial(link = "logit"), data = subset(T1_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12)))

##
# test for collinearity
subset_colTest <- subset(T1_hbA1cAnalysisSet,diabetesDurationYears>((runInMonths/12)))
subset_colTest$LinkId <- NULL
subset_colTest$birthDateUnix <- NULL
subset_colTest$DeprivationQuintile <- NULL
subset_colTest$DiabetesMellitusType_Mapped <- NULL
subset_colTest$ageAtExtractOrDeath <- NULL
subset_colTest$diagnosisDateUnix <- NULL
subset_colTest$DeathDateUnix <- NULL
subset_colTest$medianHbA1cInRange <- NULL
subset_colTest$hba1cIQRinRange <- NULL

fit1 = glm(isDead ~ ., family = binomial(logit), data = subset_colTest)
fit1sw = step(fit1)  # Keeps all variables

library(car)
vif(fit1sw)

sqrt(vif(fit1sw)) # none above 2

## coef of variability
testData <- subset(T1_hbA1cAnalysisSet,diabetesDurationYears>((runInMonths/12)+1))

quart <- ifelse(testData$CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[2], 1, 0)
quart <- ifelse(testData$CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[2] | testData$CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3], 2, quart)
quart <- ifelse(testData$CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3] | testData$CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[4], 3, quart)
quart <- ifelse(testData$CV_HbA1cInRange > quantile(testData$CV_HbA1cInRange)[4], 4, quart)



testMetric = testData$CV_HbA1cInRange
# testMetric = testData$npCV_HbA1cInRange
# testMetric = testData$qcD_HbA1cInRange

simpleSurvivalPlotMultiTest_noMedian(testData,endDateUnix,sampleDateUnix,testMetric, 0.9)

logitThreshYears <- 3
logitThreshSeconds <- logitThreshYears * (60*60*24*365.25)

testData$logit_isDead <- ifelse((testData$DeathDateUnix - sampleDateUnix) < logitThreshSeconds & testData$isDead == 1, 1, 0)
    fit <- glm(formula = logit_isDead ~ (age_atSampleTime + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange), family = binomial(link = "logit"), data = testData)
    fit <- glm(formula = logit_isDead ~ (age_atSampleTime + nValsPerIDinRange + CV_HbA1cInRange), family = binomial(link = "logit"), data = testData)
    fit <- glm(formula = logit_isDead ~ (age_atSampleTime + nValsPerIDinRange + qcD_HbA1cInRange), family = binomial(link = "logit"), data = testData)
    
testData <- subset(T2_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12))
testData <- subset(T1_hbA1cAnalysisSet,diabetesDurationYears>(runInMonths/12))


testMetric = testData$CV_HbA1cInRange
# testMetric = testData$npCV_HbA1cInRange
# testMetric = testData$qcD_HbA1cInRange

simpleSurvivalPlotMultiTest_noMedian(testData,endDateUnix,sampleDateUnix,testMetric, 0.8)

# ? need to remove diabetes duration from the cox model?
#
#
#############################
# admission data
T1_admissions<-read.csv("~/R/GlCoSy/source/admissionDataDT_T1DM.csv")
T1_admissions_sub<-data.frame(T1_admissions$ID,T1_admissions$dateplustime1,T1_admissions$admissionNumberFlag,T1_admissions$nCBGperAdmission,T1_admissions$admissionDurationDays); colnames(T1_admissions_sub)<-c("ID","dateplustime1","admissionNumberFlag","nCBGperAdmission","admissionDurationDays")

T2_admissions<-read.csv("~/R/GlCoSy/source/admissionDataDT_T2DM.csv")
T2_admissions_sub<-data.frame(T2_admissions$ID,T2_admissions$dateplustime1,T2_admissions$admissionNumberFlag,T2_admissions$nCBGperAdmission,T2_admissions$admissionDurationDays); colnames(T2_admissions_sub)<-c("ID","dateplustime1","admissionNumberFlag","nCBGperAdmission","admissionDurationDays")

admissions<-rbind(T1_admissions_sub,T2_admissions_sub)
admissionsDT<-data.table(admissions)

## more than 2 CBGs to count as an admission
# 
admissionsDT<-admissionsDT[nCBGperAdmission>2]
# admissionsDT<-admissionsDT[admissionDurationDays>0.5]

## cut admissions to all those after the sampleDate
admissionsDTafterSampleDate<-admissionsDT[dateplustime1>sampleDateUnix]
admissionsDTafterSampleDate[, minAdmissionNumberFlag := (min(admissionNumberFlag)) , by=.(ID)]
admissionsDTafterSampleDate$flagForFirstAdmissionPostSampleDate<-ifelse(admissionsDTafterSampleDate$admissionNumberFlag==admissionsDTafterSampleDate$minAdmissionNumberFlag,1,0)

## load in all data to translate ID and LinkID
demogALL<-read.csv("~/R/GlCoSy/SDsource/diagnosisDateDeathDate.txt")
admissionsDTafterSampleDateWithLinkID<-merge(admissionsDTafterSampleDate,demogALL,by.x="ID",by.y="PatId")
# merge death and admission Data
hba1c_admission_mortalitySet<-merge(hbA1cAnalysisSet,admissionsDTafterSampleDateWithLinkID,by.x="LinkId",by.y="LinkId",all.x=T)
hba1c_admission_mortalitySet$dateplustime1[is.na(hba1c_admission_mortalitySet$dateplustime1)]<-0
hba1c_admission_mortalitySet<-hba1c_admission_mortalitySet[dateplustime1<max(dateplustime1) | DeathDateUnix<max(dateplustime1)]


## without death data added
firstAdmissionsDTafterSampleDate <- admissionsDTafterSampleDate[flagForFirstAdmissionPostSampleDate == 1]
## add in demog data
firstAdmissionsDTafterSampleDate_addLinkID <- merge(firstAdmissionsDTafterSampleDate, demogALL, by.x = "ID", by.y = "PatId")
## add hb IQR data
firstAdmissionsDTafterSampleDate_addLinkID_hbIQR <- merge(firstAdmissionsDTafterSampleDate_addLinkID, hbA1cAnalysisSet, by.x = "LinkId", by.y = "LinkId")
## plot survival to admission 
simpleSurvivalPlotVariableOutcome(firstAdmissionsDTafterSampleDate_addLinkID_hbIQR,max(admissionsDT$dateplustime1),sampleDateUnix,firstAdmissionsDTafterSampleDate_addLinkID_hbIQR$dateplustime1,firstAdmissionsDTafterSampleDate_addLinkID_hbIQR$hba1cIQRinRange,0)
## if you are admitted, this is a good predictor of early admission

######################################

## for all admitted OR not admitted
all_hbIQR_patients <- merge(hbA1cAnalysisSet, firstAdmissionsDTafterSampleDate_addLinkID, by.x = "LinkId", by.y = "LinkId", all.x = T)
## make all non-admitted patients date of admission 0
all_hbIQR_patients$dateplustime1[is.na(all_hbIQR_patients$dateplustime1)]<-0
## make all zeros equal to the max followup period
all_hbIQR_patients$dateplustime1 <- ifelse(all_hbIQR_patients$dateplustime1 == 0, max(all_hbIQR_patients$dateplustime1), all_hbIQR_patients$dateplustime1)
## plot survival to admission. all patients
simpleSurvivalPlotVariableOutcome(all_hbIQR_patients,max(admissionsDT$dateplustime1),sampleDateUnix,all_hbIQR_patients$dateplustime1,all_hbIQR_patients$hba1cIQRinRange,0)
## plot survival to admission. T1DM patients
t1_plotset <- all_hbIQR_patients[DiabetesMellitusType_Mapped.x == "Type 1 Diabetes Mellitus" & diabetesDurationYears>((runInMonths/12)+1)]
simpleSurvivalPlotVariableOutcome(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$hba1cIQRinRange,0)
# cv
simpleSurvivalPlotVariableOutcome(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$CV_HbA1cInRange,0)
# ncv
simpleSurvivalPlotVariableOutcome(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$npCV_HbA1cInRange,0)

## plot survival to admission. T2DM patients
t2_plotset <- all_hbIQR_patients[DiabetesMellitusType_Mapped.x == "Type 2 Diabetes Mellitus" & diabetesDurationYears>((runInMonths/12))]
simpleSurvivalPlotVariableOutcome(t2_plotset,max(admissionsDT$dateplustime1),sampleDateUnix,t2_plotset$dateplustime1,t2_plotset$hba1cIQRinRange,0)

######################################
# add competing endpoint of death
all_hbIQR_patients <- merge(hbA1cAnalysisSet, firstAdmissionsDTafterSampleDate_addLinkID, by.x = "LinkId", by.y = "LinkId", all.x = T)
## make all non-admitted patients date of admission 0
all_hbIQR_patients$dateplustime1[is.na(all_hbIQR_patients$dateplustime1)]<-0
## make all zeros equal to the max followup period
all_hbIQR_patients$dateplustime1 <- ifelse(all_hbIQR_patients$dateplustime1 == 0, max(all_hbIQR_patients$dateplustime1), all_hbIQR_patients$dateplustime1)
## add mortality as competing endpoint:
all_hbIQR_patients$dateplustime1 <- ifelse(all_hbIQR_patients$DeathDateUnix > 0 & all_hbIQR_patients$DeathDateUnix < all_hbIQR_patients$dateplustime1, all_hbIQR_patients$DeathDateUnix, all_hbIQR_patients$dateplustime1)
## plot survival to admission. T1DM patients
t1_plotset <- all_hbIQR_patients[DiabetesMellitusType_Mapped.x == "Type 1 Diabetes Mellitus" & diabetesDurationYears>((runInMonths/12))]
simpleSurvivalPlotVariableOutcome(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$hba1cIQRinRange,0)
  # cv
  simpleSurvivalPlotVariableOutcome_noMedian(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$CV_HbA1cInRange,0.6)
  # np cv
  simpleSurvivalPlotVariableOutcome(t1_plotset,max(t1_plotset$dateplustime1),sampleDateUnix,t1_plotset$dateplustime1,t1_plotset$npCV_HbA1cInRange,0)

## plot survival to admission. T2DM patients
t2_plotset <- all_hbIQR_patients[DiabetesMellitusType_Mapped.x == "Type 2 Diabetes Mellitus" & diabetesDurationYears>((runInMonths/12)+12)]
simpleSurvivalPlotVariableOutcome(t2_plotset,max(admissionsDT$dateplustime1),sampleDateUnix,t2_plotset$dateplustime1,t2_plotset$hba1cIQRinRange,0)

# logistic regression
addmittedOrDead_T1 <- ifelse(t1_plotset$dateplustime1 > 0 & t1_plotset$dateplustime1 < max(t1_plotset$dateplustime1), 1, 0)
fit <- glm(formula = addmittedOrDead_T1 ~ (age_atSampleTime + diabetesDurationYears + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange), family = binomial(link = "logit"), data = t1_plotset)


    ## version to produce plots and numbers for paper
    
    
#    simpleSurvivalPlotVariableOutcome(testSubset,max(admissionsDT$dateplustime1),sampleDateUnix,testSubset$firstAdmission_or_death,testSubset$hba1cIQRinRange,0)
    

###############################################################################################
## hba1c sbp variability assessment

#sbp
SBPsetDT[, c("flagValuesWithinRangeForSpecifiedTimePoint") := flagValuesWithinRangeForSpecifiedTimePoint(LinkId,dateplustime1,runInMonths,sampleDateUnix) , by=.(LinkId)]

SBPsetDT<-SBPsetDT[flagValuesWithinRangeForSpecifiedTimePoint==1]
SBPsetDT[, nValsPerIDinRange_sbp := .N , by=.(LinkId)]
SBPsetDT[, valInSequencePerID_sbp := seq(1,.N,1) , by=.(LinkId)]
SBPsetDT[, sbpIQRinRange := (quantile(sbpNumeric)[4]-quantile(sbpNumeric)[2]) , by=.(LinkId)]
SBPsetDT[, medianSBPInRange := (median(sbpNumeric)) , by=.(LinkId)]

SBPsetDT[, meanSBPInRange := (mean(sbpNumeric)) , by=.(LinkId)]
SBPsetDT[, sd_SBPInRange := (sd(sbpNumeric)) , by=.(LinkId)]

CV <- function(mean, sd){
  (sd/mean)*100
}

npCV <- function(median, IQR){
  (IQR/median)*100
}

SBPsetDT[, CV_SBPInRange := (CV(meanSBPInRange, sd_SBPInRange)) , by=.(LinkId)]
SBPsetDT[, npCV_SBPInRange := (npCV(medianSBPInRange, sbpIQRinRange)) , by=.(LinkId)]

SBPsetDTforMerge<-SBPsetDT[valInSequencePerID_sbp==1]
# SBPsetDTforMerge<-SBPsetDTforMerge[nValsPerIDinRange>0]
sbpMergeSubset<-data.table(SBPsetDTforMerge$LinkId,SBPsetDTforMerge$sbpIQRinRange,SBPsetDTforMerge$medianSBPInRange, SBPsetDTforMerge$CV_SBPInRange, SBPsetDTforMerge$npCV_SBPInRange, SBPsetDTforMerge$nValsPerIDinRange_sbp)
colnames(sbpMergeSubset)<-c("LinkId","sbpIQRinRange","medianSBPInRange", "CV_SBPInRange", "npCV_SBPInRange", "nValsPerIDinRange_sbp")

masterAnalysisSet<-merge(coreDataPrepDT,hba1cMergeSubset,by.x="LinkId",by.y="LinkId"); print(nrow(masterAnalysisSet))
masterAnalysisSet<-merge(masterAnalysisSet,sbpMergeSubset,by.x="LinkId",by.y="LinkId"); print(nrow(masterAnalysisSet))

# remove those who die during the run in - before the sampleDate
sbpAnalysisSet<-masterAnalysisSet[DeathDateUnix>sampleDateUnix | DeathDateUnix==0]
#
## add age at sample time
sbpAnalysisSet$age_atSampleTime<-(sampleDateUnix-sbpAnalysisSet$birthDateUnix)/(60*60*24*365.25)
#
## show distribution of number of hba1c measures during runin
hist(sbpAnalysisSet$nValsPerIDinRange)
sbpAnalysisSet<-sbpAnalysisSet[nValsPerIDinRange_sbp>1]
#

T1_hbA1c_bp_AnalysisSet<-sbpAnalysisSet[DiabetesMellitusType_Mapped=="Type 1 Diabetes Mellitus"]
T2_hbA1c_bp_AnalysisSet<-sbpAnalysisSet[DiabetesMellitusType_Mapped=="Type 2 Diabetes Mellitus"]

simpleSurvivalPlotMultiTest(T1_hbA1c_bp_AnalysisSet,endDateUnix,sampleDateUnix,T1_hbA1c_bp_AnalysisSet$sbpIQRinRange,0.6)
simpleSurvivalPlotMultiTest(T1_hbA1c_bp_AnalysisSet,endDateUnix,sampleDateUnix,T1_hbA1c_bp_AnalysisSet$CV_SBPInRange,0.6)


plotSet<-T2_hbA1c_bp_AnalysisSet

plot(plotSet$hba1cIQRinRange,plotSet$sbpIQRinRange)
  fit<-lm(plotSet$sbpIQRinRange ~ plotSet$hba1cIQRinRange); print(fit)
  abline(fit,col="red")
boxplot(plotSet$sbpIQRinRange ~ cut(plotSet$hba1cIQRinRange,breaks=seq(0,105,1)),varwidth=T,xlim=c(0,20),ylim=c(5,15))
boxplot(plotSet$CV_SBPInRange ~ cut(plotSet$CV_HbA1cInRange,breaks=seq(0,105,1)),varwidth=T,xlim=c(0,20),ylim=c(5,15))


simpleSurvivalPlotMultiTest(T2_hbA1c_bp_AnalysisSet,endDateUnix,sampleDateUnix,T2_hbA1c_bp_AnalysisSet$sbpIQRinRange,0.6)
simpleSurvivalPlotMultiTest(T2_hbA1c_bp_AnalysisSet,endDateUnix,sampleDateUnix,T2_hbA1c_bp_AnalysisSet$CV_SBPInRange,0.6)

##################################
# by bp / hba1v cv either, or both

factorSet <-T1_hbA1c_bp_AnalysisSet
# factorSet <-T2_hbA1c_bp_AnalysisSet

factorSet$CV_HbA1cInRange[is.na(factorSet$CV_HbA1cInRange)] <- (-5)
factorSet <- factorSet[CV_HbA1cInRange >= 0]

cv_hb1ac_thresh <- quantile(factorSet$CV_HbA1cInRange)[3]
cv_sbp_thresh <- quantile(factorSet$CV_SBPInRange)[3]


# 0 lower bp and hb, 1 hba1c upper cv, 2 sbp upper cv, 3 both
factor_cv <- ifelse(factorSet$CV_HbA1cInRange > cv_hb1ac_thresh & factorSet$CV_SBPInRange < cv_sbp_thresh, 1, 0)
factor_cv <- ifelse(factorSet$CV_SBPInRange > cv_sbp_thresh & factorSet$CV_HbA1cInRange < cv_hb1ac_thresh, 2, factor_cv)
factor_cv <- ifelse((factorSet$CV_HbA1cInRange > cv_hb1ac_thresh) & (factorSet$CV_SBPInRange > cv_sbp_thresh), 3, factor_cv)

factor_cvF <- factor(factor_cv)

factorSet$factor_cvF <- factor_cvF

simpleSurvivalPlot_factor(factorSet, factor_cvF, endDateUnix, sampleDateUnix, 0.9)

# removing IDs with duration of diabetes < runin duration
simpleSurvivalPlot_factor(factorSet[diabetesDurationYears > (runInMonths/12)], factorSet[diabetesDurationYears > (runInMonths/12)]$factor_cvF, endDateUnix, sampleDateUnix, 0.85)

# produce summary stats for table
# all
summary(factorSet[diabetesDurationYears > (runInMonths/12)])

# low hb sbp
summary(factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 0])

# high hb low sbp
summary(factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 1])

# low hb high sbp
summary(factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 2])

# high hb high sbp
summary(factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 3])

## tests for table 1
reference <- factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 0]
fac1 <- factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 1]
fac2 <- factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 2]
fac3 <- factorSet[diabetesDurationYears > (runInMonths/12) & factor_cvF == 3]

# age
wilcox.test(reference$age_atSampleTime, fac1$age_atSampleTime)
wilcox.test(reference$age_atSampleTime, fac2$age_atSampleTime)
wilcox.test(reference$age_atSampleTime, fac3$age_atSampleTime)

# DM duration
wilcox.test(reference$diabetesDurationYears, fac1$diabetesDurationYears)
wilcox.test(reference$diabetesDurationYears, fac2$diabetesDurationYears)
wilcox.test(reference$diabetesDurationYears, fac3$diabetesDurationYears)

# n tests hb
wilcox.test(reference$nValsPerIDinRange, fac1$nValsPerIDinRange)
wilcox.test(reference$nValsPerIDinRange, fac2$nValsPerIDinRange)
wilcox.test(reference$nValsPerIDinRange, fac3$nValsPerIDinRange)

# n tests sbp
wilcox.test(reference$nValsPerIDinRange_sbp, fac1$nValsPerIDinRange_sbp)
wilcox.test(reference$nValsPerIDinRange_sbp, fac2$nValsPerIDinRange_sbp)
wilcox.test(reference$nValsPerIDinRange_sbp, fac3$nValsPerIDinRange_sbp)







#####################
# logit for death - test

fit <- glm(formula = isDead ~ (age_atSampleTime + diabetesDurationYears + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange + sbpIQRinRange + medianSBPInRange + nValsPerIDinRange_sbp), family = binomial(link = "logit"), data = factorSet[diabetesDurationYears > (runInMonths/12)])

fit <- glm(formula = isDead ~ (age_atSampleTime + CV_HbA1cInRange), family = binomial(link = "logit"), data = factorSet[diabetesDurationYears > (runInMonths/12)])

fit <- glm(formula = isDead ~ (age_atSampleTime + diabetesDurationYears + medianHbA1cInRange + nValsPerIDinRange + hba1cIQRinRange), family = binomial(link = "logit"), data = factorSet[diabetesDurationYears > (runInMonths/12)])




###########################
# matching attempt for T1_hbA1cAnalysisSet. match hba1c median to compare IQR difference and death at 1430 days.
# for paper review

T1_hbA1cAnalysisSet_matchSet <- T1_hbA1cAnalysisSet
T1_hbA1cAnalysisSet_matchSet$available_for_matching <- 0
median_window = 0.25

T1_hbA1cAnalysisSet_matchSet$low_IQR <- ifelse(T1_hbA1cAnalysisSet_matchSet$hba1cIQRinRange < quantile(T1_hbA1cAnalysisSet_matchSet$hba1cIQRinRange)[3], 1, 0)

T1_hbA1cAnalysisSet_matchSet[low_IQR == 0]$available_for_matching <- 1

print(nrow(T1_hbA1cAnalysisSet_matchSet[low_IQR == 1]))

for (jj in seq(1, nrow(T1_hbA1cAnalysisSet_matchSet[low_IQR == 1]), 1)) {
  
  if(jj %% 100 == 0) {print(jj)}
  
  indexID <- T1_hbA1cAnalysisSet_matchSet[low_IQR == 1][jj]
  indexID$matching_number <- jj

  # matchPool <- T1_hbA1cAnalysisSet_matchSet[low_IQR == 0 & available_for_matching == 1][medianHbA1cInRange > (indexID$medianHbA1cInRange - (median_window / 2)) & medianHbA1cInRange < (indexID$medianHbA1cInRange + (median_window / 2))]
  
  matchPool <- T1_hbA1cAnalysisSet_matchSet[low_IQR == 0 & available_for_matching == 1][medianHbA1cInRange == indexID$medianHbA1cInRange]
  
  matchID <- matchPool[sample(1:nrow(matchPool), 1), ]
  matchID$matching_number <- jj
  
  T1_hbA1cAnalysisSet_matchSet[LinkId == matchID$LinkId]$available_for_matching <- 0
  
  if (jj == 1) { matchedFrame <- rbind(indexID, matchID)}
  if (jj > 1)  { matchedFrame <- rbind(matchedFrame, indexID, matchID)}
  
}

matchedFrame$LinkId[is.na(matchedFrame$LinkId)] <- 0
matchedFrame <- matchedFrame[LinkId > 0]

matchedFrame[, c("flag_matchFound") := ifelse(.N == 2, 1, 0) , by=.(matching_number)]
matchedFrame <- matchedFrame[flag_matchFound == 1]

quantile(matchedFrame[low_IQR == 0]$medianHbA1cInRange)
quantile(matchedFrame[low_IQR == 1]$medianHbA1cInRange)

quantile(matchedFrame[low_IQR == 0]$hba1cIQRinRange)
quantile(matchedFrame[low_IQR == 1]$hba1cIQRinRange)


quantile(matchedFrame[low_IQR == 0]$nValsPerIDinRange)
quantile(matchedFrame[low_IQR == 1]$nValsPerIDinRange)

simpleSurvivalPlot_noMedianCoV(matchedFrame,endDateUnix,sampleDateUnix,0.9)

# for table 1
dim(testData)

#n 
dim(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]])
dim(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]])

# age
mean(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime)
sd(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime)

mean(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime)
sd(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime)

wilcox.test(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime, testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$age_atSampleTime, pair = F)

# n vals
quantile(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$nValsPerIDinRange)
quantile(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$nValsPerIDinRange)

quantile(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$medianHbA1cInRange)
quantile(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$medianHbA1cInRange)

wilcox.test(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$nValsPerIDinRange, testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$nValsPerIDinRange, pair = F)

# mean hba1c
mean(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange)
sd(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange)

mean(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange)
sd(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange)

wilcox.test(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange, testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$meanHbA1cInRange, pair = F)


# diabetes duration
quantile(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$diabetesDurationYears)
quantile(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$diabetesDurationYears)

wilcox.test(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$diabetesDurationYears, testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$diabetesDurationYears, pair = F)


# is dead
sum(testData[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$isDead)
sum(testData[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$isDead)

# 
t1_plotset$admissionNumberFlag[is.na(t1_plotset$admissionNumberFlag)] <- 0
t1_plotset$deadOrAdmitted <- ifelse(t1_plotset$admissionNumberFlag > 0 | t1_plotset$isDead == 1, 1, 0)

sum(t1_plotset[CV_HbA1cInRange < quantile(testData$CV_HbA1cInRange)[3]]$deadOrAdmitted)
sum(t1_plotset[CV_HbA1cInRange >= quantile(testData$CV_HbA1cInRange)[3]]$deadOrAdmitted)

















