## Type 1 pre admission HbA1c and admission characteristics / post admission characteristics

library(data.table)
library(readxl)
library(survival)
library(pracma)


process_HBA1c<-function(HBdata) {
  d<-strptime(HBdata$DataItemDate,"%d/%m/%Y")
  dd<-as.numeric(d)
  HBdata$dateplustime1<-dd
  
  HBdata$hba1c<-as.numeric(levels(HBdata$DataItemValue))[HBdata$DataItemValue]
  
  HBdata$hba1c[is.na(HBdata$hba1c)] <- 0
  HBdata<-subset(HBdata,hba1c>0)
  
  return(HBdata)
}

# existingDF <- as.data.frame(matrix(seq(20),nrow=5,ncol=4))
# r <- 3
# newrow <- seq(4)
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

> insertRow(existingDF, newrow, r)


AUCbelowThreshWithTimeResolution<-function(yyyy,dateplustime1,threshold,timeThresholdPost,timeThresholdPrior) {
  
  yy<-data.frame(yyyy,dateplustime1)
  reportingYY<-yy
  
  if (length(yyyy)>1) {
  
  ## set up all calculation columns
  yy$index<-seq(nrow(yy))
    yy$yyyyBelowThreshNoFollowingCBG<-0; yy$dateplustime1NextDateplustime<-0; yy$dateplustime1PriorDateplustime<-0; yy$prioryyyy<-0
    yy$prioryyyy[2:nrow(yy)]<-yy$yyyy[1:nrow(yy)-1]
      yy$dateplustime1PriorDateplustime[2:nrow(yy)]<-yy$dateplustime1[1:nrow(yy)-1]
    yy$diffToNextDateplustime<-yy$dateplustime1NextDateplustime - yy$dateplustime1
    yy$diffToPriorDateplustime<-yy$dateplustime1 - yy$dateplustime1PriorDateplustime
    
    #yy$yyyyBelowThreshNoFollowingCBG<-ifelse(yy$yyyy<threshold & (yy$diffToNextDateplustime / (60*timeThresholdPost))>timeThreshold,1,0)

    ## flag for insertion of a >threshold value before hypoCBG:
    ## if prior CBG>=threshold AND time to prior CBG longer than timeThresholdPrior OR
    ## if prior CBG <threshold AND time to prior CBG more than 60 mins away (ie not part of the same episode)
    yy$toInsertPriorValue<-ifelse((yy$yyyy<threshold & yy$prioryyyy>=threshold & yy$diffToPriorDateplustime>(timeThresholdPrior*60)) | (yy$yyyy<threshold & yy$prioryyyy<threshold & yy$diffToPriorDateplustime>=(60*60)),1,0)
    
    # replaceLoops
    # prior
    priorSet<-yy[yy$toInsertPriorValue==1,]
    
    if (nrow(priorSet)>0) {
    for (jj in seq(1,nrow(priorSet),1)) {
      row<-priorSet$index[jj]
      yy<-insertRow(yy, c(threshold,priorSet$dateplustime1[jj]-(timeThresholdPrior*60),0,0,0,0,0,0,0,0), row)
    }
    }
    
    yy$dateplustime1NextDateplustime[1:nrow(yy)-1]<-yy$dateplustime1[2:nrow(yy)]
    yy$diffToNextDateplustime<-yy$dateplustime1NextDateplustime - yy$dateplustime1
    
    ## flag for insertion of a >threshold value after hypoCBG:
    ## if time to next CBG (regardless of value) > timeThresholdPost
    yy$toInsertPostValue<-ifelse(yy$yyyy<threshold & yy$diffToNextDateplustime>=(timeThresholdPost*60),1,0)
    
    # post
    # prior
    postSet<-yy[yy$toInsertPostValue==1,]
    
    if (nrow(postSet)>0) {
    for (kk in seq(1,nrow(postSet),1)) {
      row<-postSet$index[kk]
      yy<-insertRow(yy, c(threshold,postSet$dateplustime1[kk]+(timeThresholdPost*60),0,0,0,0,0,0,0,0,0), row+2)
    }
    }
    # mapply(insertRow(reportingYY,c(1,1),yy$index-1), yy$toInsertPriorValue==1)
  }
      
    yy$belowThresh<-ifelse(yy$yyyy>threshold,threshold,yy$yyyy)
    yy$belowThresh<-yy$belowThresh-threshold
    admissionAUCbelowThresh<-trapz((yy$dateplustime1/(60*60)),yy$belowThresh)
    
    admissionAUCbelowThresh<-sqrt(admissionAUCbelowThresh^2)

return(admissionAUCbelowThresh)

}

AUCaboveThresh<-function(yyyy,dateplustime1,threshold) {
  
  yy<-data.frame(yyyy,dateplustime1)
  
  yy$aboveThresh<-ifelse(yy$yyyy<threshold,threshold,yy$yyyy)
  yy$aboveThresh<-yy$aboveThresh-threshold
  admissionAUCaboveThresh<-trapz((yy$dateplustime1/(60*60)),yy$aboveThresh)
  
  return(admissionAUCaboveThresh)
  
}

AUCbelowThresh<-function(yyyy,dateplustime1,threshold) {
  
  # yyyy<-DT$yyyy[1:1000]; dateplustime1<-DT$dateplustime1[1:1000]; threshold<-4
  
  yy<-data.frame(yyyy,dateplustime1)
  
  yy$belowThresh<-ifelse(yy$yyyy>threshold,threshold,yy$yyyy)
  yy$belowThresh<-yy$belowThresh-threshold
  admissionAUCbelowThresh<-trapz((yy$dateplustime1/(60*60)),yy$belowThresh)
  
  admissionAUCbelowThresh<-sqrt(admissionAUCbelowThresh^2)
  
  return(admissionAUCbelowThresh)
  
}



# load pre calculated data.table per admission data
summaryOutputName <- paste("../GlCoSy/source/DTwithPerIDdata.csv",sep=""); DT<-read.csv(summaryOutputName, header=TRUE , sep="," , row.names=NULL)
DT<-as.data.frame(DT)
# extract unix epoch date for deathDate
datetime = DT$DeathDate
dateExtract = substr(datetime,1,10)						# extract time only from date / time information
dateplustime = strptime(datetime,"%Y-%m-%d") 	# convert date and time to useable format
dateplustime1 <- as.numeric(dateplustime) # convert date time data to numerical values (absolute value is in seconds)
DT$deathDateUnix<-dateplustime1; DT$deathDateUnix[is.na(DT$deathDateUnix)]<-0 # ? an hour out - may need adjusted

# extract unix epoch date of diagnosis
datetime = DT$DateOfDiagnosisDiabetes_Date
dateExtract = substr(datetime,1,10)						# extract time only from date / time information
dateplustime = strptime(datetime,"%Y-%m-%d") 	# convert date and time to useable format
dateplustime1 <- as.numeric(dateplustime) # convert date time data to numerical values (absolute value is in seconds)
DT$diagnosisDateUnix<-dateplustime1; DT$diagnosisDateUnix[is.na(DT$diagnosisDateUnix)]<-0 # ? an hour out - may need adjusted


DT<-data.table(DT)


DT[, c("AUCbelow4") := AUCbelowThresh(yyyy,dateplustime1,4) , by=.(ID,admissionNumberFlag)]
DT[, c("AUCbelow3") := AUCbelowThresh(yyyy,dateplustime1,3) , by=.(ID,admissionNumberFlag)]

    DT[, c("AUCbelow4") := AUCbelowThreshWithTimeResolution(yyyy,dateplustime1,4,60,10) , by=.(ID,admissionNumberFlag)]
    DT[, c("AUCbelow3") := AUCbelowThreshWithTimeResolution(yyyy,dateplustime1,3,60,10) , by=.(ID,admissionNumberFlag)]

DT[, c("AUCabove15") := AUCaboveThresh(yyyy,dateplustime1,15) , by=.(ID,admissionNumberFlag)]
DT[, c("AUCabove20") := AUCaboveThresh(yyyy,dateplustime1,20) , by=.(ID,admissionNumberFlag)]
DT[, c("AUCabove25") := AUCaboveThresh(yyyy,dateplustime1,25) , by=.(ID,admissionNumberFlag)]


# reportingOutputName<-paste("../GlCoSy/source/DTwithAUCvaluesTimeAdjusted_10_60.csv",sep="")
# write.csv(DT,file=reportingOutputName)



T1set<-subset(DT,DiabetesMellitusType_Mapped=="Type 1 Diabetes Mellitus")
T1set_admissions<-subset(T1set,CBGinSequencePerAdmission==1)

T2set<-subset(DT,DiabetesMellitusType_Mapped=="Type 2 Diabetes Mellitus")
T2set_admissions<-subset(T2set,CBGinSequencePerAdmission==1)

admissionDataDT<-data.table(T1set_admissions)
# admissionDataDT<-data.table(T2set_admissions)

# admissionDataDT$dateplustime1<-1325376000

T1set[, c("AUCbelow4TimeAdjusted") := AUCbelowThreshWithTimeResolution(yyyy,dateplustime1,4,60,10) ,  by=.(ID,admissionNumberFlag)]

#########################################################################################################
# biochemistry HbA1c data
# read hba1c data in here and process
inPutName<-paste("../GlCoSy/source/HBA1CS_Old_DB.csv",sep=",")
HbA1cOld <- read.csv(inPutName, header=TRUE , sep="," , row.names=NULL)
HbA1cOld<-HbA1cOld[,1:13]

inPutNameHbNew<-paste("../GlCoSy/source/HBA1CS_NEW_DB.xlsx",sep=",")
HbA1cNew<-read_excel(inPutNameHbNew)
HbA1cNew$RegNo<-NULL

HbA1c<-rbind(HbA1cOld,HbA1cNew)

# convert hba1c from factor to numeric
HbA1c$hb<-as.numeric(levels(HbA1c$HBA1CS))[HbA1c$HBA1CS]
HbA1c$hb[is.na(HbA1c$hb)] <- 0
# only rows with an hba1c value
HbA1c<-HbA1c[which(HbA1c$hb>0),]

d<-strptime(HbA1c$DateCollected,"%d.%m.%y")
dd<-as.numeric(d)
HbA1c$dateplustime1<-dd

HbA1c$nineCharID<-ifelse(substr(HbA1c$CHI,1,1)==0,substr(HbA1c$CHI,2,10),HbA1c$CHI)
# HbA1cDT<-data.table(HbA1c)

HbA1c_mergeSUB<-data.frame(HbA1c$CHI,HbA1c$hb,HbA1c$dateplustime1); colnames(HbA1c_mergeSUB)<-c("CHI","hb","dateplustime1")


# SCI diabetes HbA1c data
HBdataName<-paste("../GlCoSy/source/GGC_HBA1c.txt",sep="")
HBdata<-read.csv(HBdataName, header=TRUE , sep="," , row.names=NULL)

HBdata<-process_HBA1c(HBdata)
# HBdataDT<-data.table(HBdata)

HBmerge_mergeSUB<-data.table(HBdata$PatId,HBdata$hba1c,HBdata$dateplustime1)
colnames(HBmerge_mergeSUB)<-c("CHI","hb","dateplustime1")

HbA1cMerge<-rbind(HbA1c_mergeSUB,HBmerge_mergeSUB)
HbA1cMerge$hb[is.na(HbA1cMerge$hb)] <- 0
HbA1cMerge<-subset(HbA1cMerge,hb>0)
HbA1cMergeDT<-data.table(HbA1cMerge)

HbA1cDT<-HbA1cMergeDT
HbA1cDT<-unique(HbA1cDT)

  ## remove duplicate values within a window
  windowForRepeatDays<-7 # window to remove duplicate values (+/-)
  windowForRepeatSeconds<-(60*60*24)*windowForRepeatDays
  
  HbA1cDT<-HbA1cDT[order(HbA1cDT$CHI,HbA1cDT$hb,HbA1cDT$dateplustime1),]
  
  HbA1cDT$diffCHI<-0; HbA1cDT$diffCHI[1:(nrow(HbA1cDT)-1)]<-diff(HbA1cDT$CHI)
  HbA1cDT$diffHb<-0; HbA1cDT$diffHb[1:(nrow(HbA1cDT)-1)]<-diff(HbA1cDT$hb)
  HbA1cDT$diffTime<-0; HbA1cDT$diffTime[1:(nrow(HbA1cDT)-1)]<-diff(HbA1cDT$dateplustime1)
  
  HbA1cDT$flagForUse<-(ifelse(HbA1cDT$diffCHI==0 & HbA1cDT$diffHb==0 & (HbA1cDT$diffTime>(-windowForRepeatSeconds) & HbA1cDT$diffTime<windowForRepeatSeconds),0,1))
  
  HbA1cDT<-HbA1cDT[flagForUse==1]


#########################################################################################################
  
  hba1cWindowMonths<-15 # if value within +/- 15 months of admission extract
  hba1cWindowMonthsInSeconds<-hba1cWindowMonths*(60*60*24*(365.25/12))
  
  hba1cExtractFunction<-function(admissionID,admissionAdmissionNumberFlag,admissionDateplustime1,hba1cWindowMonthsInSeconds) {
    # admissionDateplustime1=1350779580; admissionID=101250320; admissionAdmissionNumberFlag=1
    
    allHbA1cValues<-HbA1cDT[CHI==admissionID]
    HbA1cValuesInFrame<-HbA1cDT[CHI==admissionID & dateplustime1<admissionDateplustime1 & dateplustime1>(admissionDateplustime1-hba1cWindowMonthsInSeconds)]
    cbgList<-list(DT[ID==admissionID & admissionNumberFlag==admissionAdmissionNumberFlag]$yyyy)
    cbgTimeList<-list(DT[ID==admissionID & admissionNumberFlag==admissionAdmissionNumberFlag]$dateplustime1)
    
    hba1cList<-list(allHbA1cValues$hb)
    hba1cListTime<-list(allHbA1cValues$dateplustime1)
    
    allHbA1cValues_IQR<-quantile(allHbA1cValues$hb)[4]- quantile(allHbA1cValues$hb)[2]
    nAllHbA1cValues<-nrow(allHbA1cValues)
    
    HbA1cValuesInFrame<-HbA1cValuesInFrame[order(dateplustime1),]
    if (nrow(HbA1cValuesInFrame)>0) {
      lastHbA1cInFrame<-tail(HbA1cValuesInFrame,1)$hb
      lastHbA1cInFrameTime<-tail(HbA1cValuesInFrame,1)$dateplustime1
      IQRHbA1cInFrame<-(quantile(HbA1cValuesInFrame$hb)[4]-quantile(HbA1cValuesInFrame$hb)[2])
      }
    if (nrow(HbA1cValuesInFrame)==0) {
      lastHbA1cInFrame<-0
      lastHbA1cInFrameTime<-0
      IQRHbA1cInFrame<-0
      }
    
    if (length(cbgList[[1]])>1) {admission90_10<-(quantile(cbgList[[1]],prob = seq(0, 1, length = (10+1)), type = 5)[9])/(quantile(cbgList[[1]],prob = seq(0, 1, length = (10+1)), type = 5)[2]) }
    if (length(cbgList[[1]])<=1) {admission90_10<-0}
    
    outputList<-list(nAllHbA1cValues,allHbA1cValues_IQR,nrow(HbA1cValuesInFrame),IQRHbA1cInFrame,lastHbA1cInFrame,lastHbA1cInFrameTime,admission90_10,cbgList,cbgTimeList,hba1cList,hba1cListTime)
    return(outputList)
    
  }
  
        ## output function for targettedHba1cExtractFunction
        outputFunction<-function(hb,dateplustime1) {
          hbVector<-hb
          hbTimeVector<-dateplustime1
          hbTimeDiffVector<-diff(hbTimeVector)
          
          nVals<-length(hb)
            hbMedian<-median(hbVector)
            hbIQR<-quantile(hbVector)[4]-quantile(hbVector)[2]
              timeIntervalMedian<-median(hbTimeDiffVector)
              timeIntervalIQR<-quantile(hbTimeDiffVector)[4]-quantile(hbTimeDiffVector)[2]
                gradient<-(tail(hb,1)-head(hb,1))/(tail(dateplustime1,1)-head(dateplustime1,1))
                
        outputList<-list(nVals,hbMedian,hbIQR,timeIntervalMedian,timeIntervalIQR,gradient)
        return(outputList)
        }

       targettedHba1cExtractFunction<-function(admissionID,admissionAdmissionNumberFlag,admissionDateplustime1,hba1cWindowMonthsInSeconds) {
        # print(admissionID)
        # admissionDateplustime1=1360931100; admissionID=101250320; admissionAdmissionNumberFlag=1
        
        ## ALL HBA1C recorded - and in order (may pre and post date admission)
        allHbA1cValues<-HbA1cDT[CHI==admissionID]
        allHbA1cValues<-allHbA1cValues[order(allHbA1cValues$dateplustime1),]
        
        ## subs for analysis
        allHbPriorToAdmission<-allHbA1cValues[dateplustime1<admissionDateplustime1]
        hbInWindowPriorToAdmission<-allHbA1cValues[dateplustime1<admissionDateplustime1 & (dateplustime1>(admissionDateplustime1-hba1cWindowMonthsInSeconds))]
        
        ## 
        if (nrow(allHbPriorToAdmission)>1) {
        allPriorHbValues<-outputFunction(allHbPriorToAdmission$hb,allHbPriorToAdmission$dateplustime1)

          allPrior_nVals<-as.numeric(allPriorHbValues[[1]])
          allPrior_hbMedian<-as.numeric(allPriorHbValues[[2]])
          allPrior_hbIQR<-as.numeric(allPriorHbValues[[3]])
          allPrior_timeIntervalMedian<-as.numeric(allPriorHbValues[[4]])
          allPrior_timeIntervalIQR<-as.numeric(allPriorHbValues[[5]])
          allPrior_gradient<-as.numeric(allPriorHbValues[[6]])
        }
        
               if (nrow(allHbPriorToAdmission)<=1) {
      
                allPrior_nVals<-as.numeric(nrow(allHbPriorToAdmission))
                allPrior_hbMedian<-0
                allPrior_hbIQR<-0
                allPrior_timeIntervalMedian<-0
                allPrior_timeIntervalIQR<-0
                allPrior_gradient<-0
              }
        
        if (nrow(hbInWindowPriorToAdmission)>1) {
        inWindowHbValues<-outputFunction(hbInWindowPriorToAdmission$hb,hbInWindowPriorToAdmission$dateplustime1)
        
          inWindow_nVals<-as.numeric(inWindowHbValues[[1]])
          inWindow_hbMedian<-as.numeric(inWindowHbValues[[2]])
          inWindow_hbIQR<-as.numeric(inWindowHbValues[[3]])
          inWindow_timeIntervalMedian<-as.numeric(inWindowHbValues[[4]])
          inWindow_timeIntervalIQR<-as.numeric(inWindowHbValues[[5]])
          inWindow_gradient<-as.numeric(inWindowHbValues[[6]])
        }
        
              if (nrow(hbInWindowPriorToAdmission)<=1) {
              inWindowHbValues<-outputFunction(hbInWindowPriorToAdmission$hb,hbInWindowPriorToAdmission$dateplustime1)
              
                inWindow_nVals<-as.numeric(nrow(hbInWindowPriorToAdmission))
                inWindow_hbMedian<-0
                inWindow_hbIQR<-0
                inWindow_timeIntervalMedian<-0
                inWindow_timeIntervalIQR<-0
                inWindow_gradient<-0
              }
                
        outputList<-list(allPrior_nVals,allPrior_hbMedian,allPrior_hbIQR,allPrior_timeIntervalMedian, allPrior_timeIntervalIQR,allPrior_gradient,inWindow_nVals,inWindow_hbMedian,inWindow_hbIQR,inWindow_timeIntervalMedian,inWindow_timeIntervalIQR,inWindow_gradient)
        # outputList<-list(allPrior_nVals)
        return(outputList)
       }
  
  medianFirstnCBGs<-function(parameter,numberOfValues) {
    numericalParameter<-parameter[[1]]
    medianValue<-ifelse(length(numericalParameter)>=numberOfValues,median(numericalParameter[1:numberOfValues]),-10)
    return(medianValue)
  }
  
  CBGnValue<-function(parameter,valueNumberToReport) {
    numericalParameter<-parameter[[1]]
    cbgValueReport<-numericalParameter[valueNumberToReport]
    return(cbgValueReport)
  }
  
  IQRFirstnCBGs<-function(parameter,numberOfValues) {
    numericalParameter<-parameter[[1]]
    numericalParameter<-numericalParameter[1:numberOfValues]
    numericalParameter[is.na(numericalParameter)] <- 0
    numericalParameter<-numericalParameter[numericalParameter>0]
  
    if (length(numericalParameter)>=numberOfValues) {
      IQRvalue<- quantile(numericalParameter)[4] - quantile(numericalParameter)[2]
    }
    
    if (length(numericalParameter)<numberOfValues) {
      IQRvalue<-(-10)
    }
    return(IQRvalue)
  }

  maxMinFirstnCBGs<-function(parameter,numberOfValues) {
    numericalParameter<-parameter[[1]]
    numericalParameter<-numericalParameter[1:numberOfValues]
    numericalParameter[is.na(numericalParameter)] <- 0
    numericalParameter<-numericalParameter[numericalParameter>0]
    
    if (length(numericalParameter)>=numberOfValues) {
      maxMinvalue<- max(numericalParameter)-min(numericalParameter)
    }
    
    if (length(numericalParameter)<numberOfValues) {
      maxMinvalue<-(-10)
    }
    return(maxMinvalue)
  }
  
  IQRtimeDiffFirstnCBGs<-function(parameter,numberOfValues) {
    numericalParameter<-parameter[[1]]
    numericalParameter<-numericalParameter[1:numberOfValues]
    numericalParameter[is.na(numericalParameter)] <- 0
    numericalParameter<-numericalParameter[numericalParameter>0]
    
    if (length(numericalParameter)==2) {
      diffValue<- diff(numericalParameter)
    }
    
    if (length(numericalParameter)>=numberOfValues & length(numericalParameter)>2) {
      diffValue<- quantile(diff(numericalParameter))[4]- quantile(diff(numericalParameter))[2]
    }
    
    if (length(numericalParameter)<numberOfValues) {
      diffValue<-(-10)
    }
    return(diffValue)
  }
  
  IQRTimeFunction<-function(parameter) {
    numericalParameter<-parameter[[1]]
    diffParam<-diff(numericalParameter)
    IQR<-quantile(diffParam)[4]-quantile(diffParam)[2]
    return(IQR)
  }
  
  IQRTimeFunctionInFrame<-function(parameter,dateplustime1,hba1cWindowMonthsInSeconds) {
    numericalParameter<-parameter[[1]]
    numericalParameter<-numericalParameter[numericalParameter>(dateplustime1-hba1cWindowMonthsInSeconds)]
    diffParam<-diff(numericalParameter)
    IQR<-quantile(diffParam)[4]-quantile(diffParam)[2]
    return(IQR)
  }
  
  timeToFirstHypo<-function(cbgList,cbgTimeList) {
    numericTime<-cbgTimeList[[1]]
    numericCBG<-cbgList[[1]]
    timeOfFirstHypo<-numericTime[numericCBG<4]
    
    outputReturn<-list(numericCBG[numericCBG<4],timeOfFirstHypo)
    return(outputReturn)
      }

  admissionDataDT[, c("nAllHbA1cValues","allHbA1cValues_IQR","nHbA1cValuesInFrame","IQRHbA1cInFrame","lastHbA1cInFrame","lastHbA1cInFrameTime","ninetyTen","cbgList","cbgTimeList","hba1cList","hba1cListTime") := hba1cExtractFunction(ID,admissionNumberFlag,dateplustime1,hba1cWindowMonthsInSeconds) , by=.(ID,admissionNumberFlag)]
  
  admissionDataDT[, c("HbA1cTimeIQR") := IQRTimeFunctionInFrame(hba1cListTime,dateplustime1,hba1cWindowMonthsInSeconds) , by=.(row.names)]
  admissionDataDT[, c("allHbA1cTimeIQR") := IQRTimeFunction(hba1cListTime) , by=.(row.names)]
  admissionDataDT[, c("CBGTimeIQR") := IQRTimeFunction(cbgTimeList) , by=.(row.names)]
  
  ## revised simple analysis to find IQR of values and time for prior HbA1cs
  admissionDataDT[, c("allPrior_nVals","allPrior_hbMedian","allPrior_hbIQR","allPrior_timeIntervalMedian", "allPrior_timeIntervalIQR","allPrior_gradient","inWindow_nVals","inWindow_hbMedian","inWindow_hbIQR","inWindow_timeIntervalMedian","inWindow_timeIntervalIQR","inWindow_gradient") := targettedHba1cExtractFunction(ID,admissionNumberFlag,dateplustime1,hba1cWindowMonthsInSeconds) , by=.(ID,admissionNumberFlag)]
  
  
  hba1cPercent<-(admissionDataDT$lastHbA1cInFrame/10.929) + 2.15
  admissionDataDT$eAG<- ((28.7*hba1cPercent)-46.7)/18
  admissionDataDT$HbA1cGradient<-admissionDataDT$eAG/((admissionDataDT$dateplustime1 - admissionDataDT$lastHbA1cInFrameTime)/(60*60*24*365.25))
  
  admissionDataDT[, c("medianFirst2CBGs") := medianFirstnCBGs(cbgList,2) , by=.(row.names)]
  admissionDataDT[, c("medianFirst3CBGs") := medianFirstnCBGs(cbgList,3) , by=.(row.names)]
  admissionDataDT[, c("medianFirst4CBGs") := medianFirstnCBGs(cbgList,4) , by=.(row.names)]
  
  admissionDataDT[, c("IQRFirst2CBGs") := IQRFirstnCBGs(cbgList,2) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst3CBGs") := IQRFirstnCBGs(cbgList,3) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst4CBGs") := IQRFirstnCBGs(cbgList,4) , by=.(row.names)]
  
  admissionDataDT[, c("IQRFirst2CBGsTime_diffTime") := IQRtimeDiffFirstnCBGs(cbgTimeList,2) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst3CBGsTime_diffTime") := IQRtimeDiffFirstnCBGs(cbgTimeList,3) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst4CBGsTime_diffTime") := IQRtimeDiffFirstnCBGs(cbgTimeList,4) , by=.(row.names)]
  
  admissionDataDT[, c("IQRFirst2CBGsTime") := IQRFirstnCBGs(cbgTimeList,2) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst3CBGsTime") := IQRFirstnCBGs(cbgTimeList,3) , by=.(row.names)]
  admissionDataDT[, c("IQRFirst4CBGsTime") := IQRFirstnCBGs(cbgTimeList,4) , by=.(row.names)]
  
  admissionDataDT[, c("maxMinFirst2CBGs") := maxMinFirstnCBGs(cbgList,2) , by=.(row.names)]
  admissionDataDT[, c("maxMinFirst3CBGs") := maxMinFirstnCBGs(cbgList,3) , by=.(row.names)]
  admissionDataDT[, c("maxMinFirst4CBGs") := maxMinFirstnCBGs(cbgList,4) , by=.(row.names)]
  
  admissionDataDT[, c("firstHypo","timeOfFirstHypo") := timeToFirstHypo(cbgList,cbgTimeList) , by=.(row.names)]
  
  admissionDataDT[, c("yyyy1") := CBGnValue(cbgList,1) , by=.(row.names)]
  admissionDataDT[, c("yyyy2") := CBGnValue(cbgList,2) , by=.(row.names)]
  admissionDataDT[, c("yyyy3") := CBGnValue(cbgList,3) , by=.(row.names)]
  admissionDataDT[, c("yyyy4") := CBGnValue(cbgList,4) , by=.(row.names)]
  
  
  
  
  ### save out Type 1
  ## lists need to be converted into characters prior to saving out.
  saveOutDT<-admissionDataDT
  saveOutDT$cbgList<-as.character(saveOutDT$cbgList)
  saveOutDT$cbgTimeList<-as.character(saveOutDT$cbgTimeList)
  saveOutDT$hba1cList<-as.character(saveOutDT$hba1cList)
  saveOutDT$hba1cListTime<-as.character(saveOutDT$hba1cListTime)
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T1DM.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  saveOutDT<-admissionDataDT
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T1DM_hba1cRevision.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  
  saveOutDT<-admissionDataDT
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T1DM_hba1cRevision_010112date.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  saveOutDT<-admissionDataDT
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T1DM_hba1cRevision_010510date.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  
  
  ### save out Type 2
  ## lists need to be converted into characters prior to saving out.
  saveOutDT<-admissionDataDT
  saveOutDT$cbgList<-as.character(saveOutDT$cbgList)
  saveOutDT$cbgTimeList<-as.character(saveOutDT$cbgTimeList)
  saveOutDT$hba1cList<-as.character(saveOutDT$hba1cList)
  saveOutDT$hba1cListTime<-as.character(saveOutDT$hba1cListTime)
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T2DM.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  saveOutDT<-admissionDataDT
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T2DM_hba1cRevision.csv",sep="")
  write.csv(saveOutDT,file=reportingOutputName)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ######################################################  ######################################################
  # workings below
  
  ######################################################  ######################################################
  ######################################################  ######################################################
  ######################################################  ######################################################
  
  # load in full dataset with drug data where available
  OPfilename <- paste("../GlCoSy/source/DTwithDrugData.csv",sep="")
  myDataWithDrugDataDT<-read.csv(OPfilename)
  myDataWithDrugData[,1]<-NULL
  
  type2withDrugs<-merge(reportingDF,myDataWithDrugDataDT,by.x=c("ID","dateplustime1"),by.y=c("ID","dateplustime1"))
  reportingOutputName<-paste("../GlCoSy/source/admissionDataDT_T2DM_withDrugs.csv",sep="")
  write.csv(type2withDrugs,file=reportingOutputName)
  
  
  
  ###### type 1
  #
  # tempWriteFile <- paste("../GlCoSy/source/hba1cReferenceFile_allT1DM-admissions_allHbA1cVals.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF)
  #
  ######################################################
  ###### type 2
  #
  # tempWriteFile <- paste("../GlCoSy/source/hba1cReferenceFile_allT2DM-admissions_allHbA1cVals.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF)
  #
  ######################################################
  
  ######################################################  ######################################################
  ######################################################  ######################################################
  ### below here is old
  ######################################################  ######################################################
  ######################################################  ######################################################
  
  
  
hba1cWindowMonths<-15 # if value within +/- 15 months of admission extract
hba1cWindowMonthsInSeconds<-hba1cWindowMonths*(60*60*24*(365.25/12))

listOfUniqueIDs<-unique(admissionDataDT$ID)
reportingDF<-as.data.frame(matrix(0,nrow=1,ncol=15)); colnames(reportingDF)<-c("nAllHbA1c","medianHbA1c","IQRallHbA1c","nHbA1cInFrame","medianHbA1cInFrame","IQRHbA1cInFrame","HbA1c", "medianGlu", "ID", "nCBGinAdmission", "admissionDurationDays","hypoEpisodes4","yyyy","IQR","minGlu")

for (i in seq(1, length(listOfUniqueIDs), 1)) {
  if (i%%100==0) {print(i)}
  
  setOfAdmissionsPerUniqueID<-admissionDataDT[ID==listOfUniqueIDs[i]]
  
  for (a in seq(1, nrow(setOfAdmissionsPerUniqueID), 1)) {
    
    admissionDataToTest<-setOfAdmissionsPerUniqueID[a,]
    HbA1cValuesInFrame<-HbA1cDT[CHI==admissionDataToTest$ID & dateplustime1<admissionDataToTest$dateplustime1 & dateplustime1>(admissionDataToTest$dateplustime1-hba1cWindowMonthsInSeconds)]
    
    allHbA1c<-HbA1cDT[CHI==admissionDataToTest$ID & dateplustime1>0]
    if (nrow(allHbA1c)>0) {
      allHbA1c<-unique(allHbA1c)
      allHbA1c<-allHbA1c[order(allHbA1c$dateplustime1),]
      
      DataToAddToReportAll<-c(nrow(allHbA1c),median(allHbA1c$hb),quantile(allHbA1c$hb)[4]-quantile(allHbA1c$hb)[2],nrow(HbA1cValuesInFrame))
    }
    
    if (nrow(allHbA1c)==0) {DataToAddToReportAll<-c(0,0,0,0)}
    
    if (nrow(HbA1cValuesInFrame)>0) {
      HbA1cValuesInFrame<-HbA1cValuesInFrame[order(HbA1cValuesInFrame$dateplustime1),]
      HbA1cValuesInFrame<-unique(HbA1cValuesInFrame)
      MostRecentHbA1cValueInFrame<-tail(HbA1cValuesInFrame,1)
      
      DataToAddToReportInFrame<-c(median(HbA1cValuesInFrame$hb),quantile(HbA1cValuesInFrame$hb)[4]-quantile(HbA1cValuesInFrame$hb)[2],MostRecentHbA1cValueInFrame$hb)
    }
    
    ## need to manage multiple reports of same value with a date window to exclude values
    
    if (nrow(HbA1cValuesInFrame)==0) {DataToAddToReportInFrame<-c(0,0,0)}
    
    DataToAddToReport<-c(DataToAddToReportAll,DataToAddToReportInFrame,admissionDataToTest$medianGlu, admissionDataToTest$ID, admissionDataToTest$nCBGperAdmission, admissionDataToTest$admissionDurationDays, admissionDataToTest$ID_ADMISSIONhypoEpisodes4.60, admissionDataToTest$yyyy, admissionDataToTest$IQR,admissionDataToTest$minGlu)
    
    reportingDF<-rbind(reportingDF,DataToAddToReport)
    }
    
  }
  

## plotting
reportingDF<-reportingDF[-1,]
hba1cPercent<-(reportingDF$HbA1c/10.929) + 2.15
reportingDF$eAG<- ((28.7*hba1cPercent)-46.7)/18

### save out
reportingOutputName<-paste("../GlCoSy/source/hba1cReferenceFile_allT2DM-admissions_allHbA1cVals_withminGlu_noDuplicates.csv",sep="")
write.table(reportingDF,file=reportingOutputName,sep=",",append=F,col.names=T)

######################################################
###### load back in if necessary
###### type 2
#
# tempWriteFile <- paste("../GlCoSy/source/hba1cReferenceFile_allT2DM-admissions_allHbA1cVals_withminGlu_noDuplicates.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF)
#
###### type 1
#
# tempWriteFile <- paste("../GlCoSy/source/hba1cReferenceFile_allT1DM-admissions_allHbA1cVals.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF)
#
######################################################

#### eAG / yyyy relationship

hba1cPercent_inFrame<-(reportingDF$medianHbA1cInFrame/10.929) + 2.15
reportingDF$eAG_inFrame<- ((28.7*hba1cPercent_inFrame)-46.7)/18

reportingDF$eAGyyyyDiff<-reportingDF$eAG-reportingDF$yyyy
reportingDF$eAGyyyyDiff_inFrame<-reportingDF$eAG_inFrame-reportingDF$yyyy

reportingDF$hypo<-ifelse(reportingDF$ID_ADMISSIONhypoEpisodes4.60>0,1,0)

######################################################
plotReportingDF<-subset(reportingDF,nCBGinAdmission>=2)
plotReportingDF<-subset(plotReportingDF,nHbA1cInFrame>0)
plotReportingDF<-data.table(plotReportingDF)
######################################################

plotfilename <- paste("../GlCoSy/plots/_scratchOutput_correlations.pdf",sep="")
pdf(plotfilename, width=16, height=9)



## eAG-yyyy ie difference between eAG and first CBG
boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff,breaks=10),las=3,varwidth=T,ylim=c(0,10),plot=T,main=paste("IQR vs (eAG from last measured HbA1c - first measured CBG value) (x axis 10 divisions)\n",nrow(plotReportingDF)," admissions",sep=""))
    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff,breaks=100),las=3,varwidth=T,ylim=c(0,10),plot=T,main=paste("IQR vs (eAG from last measured HbA1c - first measured CBG value) (x axis 100 divisions)\n",nrow(plotReportingDF)," admissions",sep=""))
    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff,breaks=quantile(plotReportingDF$eAGyyyyDiff, prob = seq(0, 1, length = 11), type = 5)),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs deciles of (eAG from last measured HbA1c- first measured CBG value) (x axis)")
boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff_inFrame,breaks=10),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs (eAG from median HbA1c in Frame (15 months)- first measured CBG value) (x axis 10 divisions)")
    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff_inFrame,breaks=100),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs (eAG from median HbA1c in Frame (15 months)- first measured CBG value) (x axis 100 divisions)")

    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$eAGyyyyDiff_inFrame,breaks=quantile(plotReportingDF$eAGyyyyDiff_inFrame, prob = seq(0, 1, length = 11), type = 5)),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs deciles of (eAG from median HbA1c in Frame (15 months)- first measured CBG value) (x axis)")
  

boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$eAGyyyyDiff,breaks=10),las=3,varwidth=T,ylim=c(0,10),plot=T,main="admission Duration (days) vs (eAG from last measured HbA1c - first measured CBG value) (x axis)")
    boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$eAGyyyyDiff,breaks=quantile(plotReportingDF$eAGyyyyDiff, prob = seq(0, 1, length = 11), type = 5)),las=3,varwidth=T,ylim=c(0,6),plot=T,main="admissionDurationDays vs deciles of (eAG from last measured HbA1c- first measured CBG value) (x axis)")
boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$eAGyyyyDiff_inFrame,breaks=10),las=3,varwidth=T,ylim=c(0,10),plot=T,main="admission Duration (days) vs (eAG from median HbA1c in Frame (15 months)- first measured CBG value) (x axis)")

boxplot(plotReportingDF$medianGlu ~ cut(plotReportingDF$eAGyyyyDiff,breaks=100),las=3,varwidth=T,ylim=c(0,20),plot=T,main="median Glu vs (eAG from last measured HbA1c - first measured CBG value) (x axis)")
boxplot(plotReportingDF$medianGlu ~ cut(plotReportingDF$eAGyyyyDiff_inFrame,breaks=10),las=3,varwidth=T,ylim=c(0,20),plot=T,main="median Glu vs (eAG from median HbA1c in Frame (15 months)- first measured CBG value) (x axis)")

boxplot(subset(plotReportingDF,yyyy>=4)$minGlu ~ cut(subset(plotReportingDF,yyyy>=4)$eAGyyyyDiff,breaks=10),las=3,varwidth=T,ylim=c(0,10),plot=T,main=paste("minGlu vs (eAG from last measured HbA1c - first measured CBG value) (x axis 100 divisions)\n",nrow(plotReportingDF)," admissions",sep=""))
    boxplot(plotReportingDF$minGlu ~ cut(plotReportingDF$eAGyyyyDiff,breaks=100),las=3,varwidth=T,ylim=c(0,10),plot=T,main=paste("minGlu vs (eAG from last measured HbA1c - first measured CBG value) (x axis 100 divisions)\n",nrow(plotReportingDF)," admissions",sep=""))


### proportiong of admissions with hypoglycaemia
numberOfDivisions<-10
quantileVals<-quantile(plotReportingDF$eAGyyyyDiff,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)

propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]

# for (h in seq(-20,20,1)) {
for (h in seq(1,nrow(propPlot),1)) {
  if (h<nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff>=propPlot$xAxisVals[h] & eAGyyyyDiff<propPlot$xAxisVals[h+1] & yyyy>=4] }
  if (h==nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff>=propPlot$xAxisVals[h] & eAGyyyyDiff<max(eAGyyyyDiff) & yyyy>=4] }
  
  nInSet<-nrow(hypoTestSet)
  nInSetWithHypo<-nrow(hypoTestSet[ID_ADMISSIONhypoEpisodes4.60>0])
  
  proportionWithHypo<-nInSetWithHypo/nInSet
  
  propPlot$nInSet[h]<-nInSet
  propPlot$prop[h]<-proportionWithHypo
  
  
  # print(proportionWithHypo)
}

plot(propPlot$xAxisVals,propPlot$prop,ylim=c(0,0.6),pch=16,cex=2,
     xlab="(eAG-first CBG value)",
     ylab="proprtion of admissions with >=1 episodes of hypoglycaemia",
     main=paste("relationship between difference between (eAG-(firstCBG)) (centiles) and proportion of admissions with hypoglycaemia \n admissions where first value is <4 excluded. eAG estimated from last measured HbA1c prior to admission.\nn admissions=",nrow(plotReportingDF),sep=""))
# lines(propPlot$xAxisVals,propPlot$prop)


    
      ### proportiong of admissions with hypoglycaemia
      numberOfDivisions<-100
      quantileVals<-quantile(plotReportingDF$eAGyyyyDiff_inFrame,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)
      propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
      propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]
      
      # for (h in seq(-20,20,1)) {
        for (h in seq(1,nrow(propPlot),1)) {
        if (h<nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff_inFrame>=propPlot$xAxisVals[h] & eAGyyyyDiff_inFrame<propPlot$xAxisVals[h+1] & yyyy>=4] }
          if (h==nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff_inFrame>=propPlot$xAxisVals[h] & eAGyyyyDiff_inFrame<max(eAGyyyyDiff_inFrame) & yyyy>=4] }
        
        nInSet<-nrow(hypoTestSet)
        nInSetWithHypo<-nrow(hypoTestSet[ID_ADMISSIONhypoEpisodes4.60>0])
        
        proportionWithHypo<-nInSetWithHypo/nInSet
        
        propPlot$nInSet[h]<-nInSet
        propPlot$prop[h]<-proportionWithHypo
        
        
        # print(proportionWithHypo)
      }
      
      plot(propPlot$xAxisVals,propPlot$prop,ylim=c(0.2,0.6),pch=16,cex=2,
           xlab="(eAG-first CBG value)",
           ylab="proprtion of admissions with >=1 episodes of hypoglycaemia",
           main=paste("relationship between difference between (eAG-(firstCBG)) (centiles) and proportion of admissions with hypoglycaemia \n admissions where first value is <4 excluded. eAG estimated from median HbA1c in 15/12 prior to admission.\nn admissions=",nrow(plotReportingDF),sep=""))
     # lines(propPlot$xAxisVals,propPlot$prop)
      
  
  
  #######################################################
  ### association between HbA1c IQR and probablity of hypoglycaemia during admission
  
      ### proportiong of admissions with hypoglycaemia
    numberOfDivisions<-10
    IQRHbA1c_plotReportingDF<-plotReportingDF[nHbA1cValuesInFrame>1]
     # IQRHbA1c_plotReportingDF<-IQRHbA1c_plotReportingDF[IQRHbA1cInFrame>0]
     quantileVals<-quantile(IQRHbA1c_plotReportingDF$IQRHbA1cInFrame,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)
    #quantileVals<-seq(min(IQRHbA1c_plotReportingDF$IQRHbA1cInFrame),max(IQRHbA1c_plotReportingDF$IQRHbA1cInFrame),((max(IQRHbA1c_plotReportingDF$IQRHbA1cInFrame)-min(IQRHbA1c_plotReportingDF$IQRHbA1cInFrame))/10))
      
      propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
      propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]
      
      # for (h in seq(-20,20,1)) {
      for (h in seq(1,nrow(propPlot),1)) {
        if (h<nrow(propPlot)) {hypoTestSet<-IQRHbA1c_plotReportingDF[IQRHbA1cInFrame>=propPlot$xAxisVals[h] & IQRHbA1cInFrame<propPlot$xAxisVals[h+1] & yyyy>=4] }
        if (h==nrow(propPlot)) {hypoTestSet<-IQRHbA1c_plotReportingDF[IQRHbA1cInFrame>=propPlot$xAxisVals[h] & IQRHbA1cInFrame<max(IQRHbA1cInFrame) & yyyy>=4] }
        
        nInSet<-nrow(hypoTestSet)
        nInSetWithHypo<-nrow(hypoTestSet[ID_ADMISSIONhypoEpisodes4.60>0])
        
        proportionWithHypo<-nInSetWithHypo/nInSet
        
        propPlot$nInSet[h]<-nInSet
        
        propPlot$prop[h]<-proportionWithHypo
        
        
        # print(proportionWithHypo)
      }
      
      plot(propPlot$xAxisVals,propPlot$prop,ylim=c(0.1,0.3),pch=16,cex=2,
           xlab="(eAG-first CBG value)",
           ylab="proprtion of admissions with >=1 episodes of hypoglycaemia",
           main=paste("relationship between difference between IQR HbAc (centiles) and proportion of admissions with hypoglycaemia \n admissions where first value is <4 excluded. eAG estimated from median HbA1c in 15/12 prior to admission.\nn admissions=",nrow(plotReportingDF),sep=""))      # lines(propPlot$xAxisVals,propPlot$prop)
      


## IQR
## most recent HbA1c in range (15 months)
boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs last measured HbA1c (x axis)")
boxplot(plotReportingDF$IQR ~ cut(log(plotReportingDF$HbA1c),breaks=30),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs log of last measured HbA1c (x axis)")

plot(log(subset(plotReportingDF,IQR>0)$HbA1c),subset(plotReportingDF,IQR>0)$IQR,cex=sqrt(plotReportingDF$nCBGinAdmission)/2,col=ifelse(plotReportingDF$nCBGinAdmission<5,"black","black"),main="IQR vs last measured log HbA1c. circle size represents nCBG in admission")

IQR_hba1cDF<-as.data.frame(matrix(0,nrow=0,ncol=4)); colnames(IQR_hba1cDF)<-c("nCBG","nAdmissions","pval","cor")
for (ii in seq(1,20,1)) {
plot(log(subset(plotReportingDF,nCBGinAdmission==ii)$HbA1c),subset(plotReportingDF,nCBGinAdmission==ii)$IQR)
ct<-cor.test(log(subset(plotReportingDF,nCBGinAdmission==ii)$HbA1c),subset(plotReportingDF,nCBGinAdmission==ii)$IQR)

output<-data.frame(ii,length(log(subset(plotReportingDF,nCBGinAdmission==ii)$HbA1c)),ct$p.value,ct$estimate)
colnames(output)<-c("nCBG","nAdmissions","pval","cor")
IQR_hba1cDF<-rbind(IQR_hba1cDF,output)
}

for (i in seq(1,20,1)) {
  x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$IQR ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,10),plot=F)
  if (i==4) {plot(log(c(1:20)),x$stats[3,1:20],ylim=c(0,10),pch=16,cex=3*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="IQR vs log last measured HbA1c. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of IQR increasing with increasing numbers of CBGs measured during an admission")
    lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=2)
    }
  if (i>4)  {points(log(c(1:20)),x$stats[3,1:20],pch=16,cex=3*(sqrt(x$n))/max(sqrt(x$n)),col=i)
    lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=2)
    }
  
}

    ## median HbA1c in range (15 months)
    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs median of HbA1c measures in 15 months prior to admission")
    boxplot(plotReportingDF$IQR ~ cut(log(plotReportingDF$medianHbA1cInFrame),breaks=30),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs median of log HbA1c measures in 15 months prior to admission")
    
    plot(log(subset(plotReportingDF,IQR>0)$medianHbA1cInFrame),subset(plotReportingDF,IQR>0)$IQR,cex=sqrt(plotReportingDF$nCBGinAdmission)/2,col=ifelse(plotReportingDF$nCBGinAdmission<5,"black","black"),main="IQR vs median log HbA1c 15 months prior to admission. circle size represents nCBG in admission")
    
    for (i in seq(4,10,1)) {
      x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$IQR ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,10),plot=F)
      if (i==4) {plot(log(c(1:20)),x$stats[3,1:20],ylim=c(0,10),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="IQR vs log median HbA1c in last 15 months. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of IQR increasing with increasing numbers of CBGs measured during an admission"); lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=3)}
      if (i>4)  {points(log(c(1:20)),x$stats[3,1:20],pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=3)}
      
    }
    
    ## HbA1c IQR
    boxplot(plotReportingDF$IQR ~ cut(plotReportingDF$IQRallHbA1c,breaks=seq(1,30,1)),las=3,varwidth=T,ylim=c(0,10),plot=T,main="IQR vs median of all prior HbA1c measures")

    # plot(log(subset(plotReportingDF,IQR>0)$IQRHbA1cInFrame),subset(plotReportingDF,IQR>0)$IQR,cex=sqrt(plotReportingDF$nCBGinAdmission)/2,col=ifelse(plotReportingDF$nCBGinAdmission<5,"black","black"))
    
    for (i in seq(4,10,1)) {
      x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$IQR ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$IQRallHbA1c,breaks=seq(1,80,1)),las=3,varwidth=T,ylim=c(0,10),plot=F)
      
      if (i==4) {plot(log(c(1:20)),x$stats[3,1:20],ylim=c(0,10),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="IQR vs log median prior HbA1c. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of IQR increasing with increasing numbers of CBGs measured during an admission"); lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=3)}
      if (i>4)  {points(log(c(1:20)),x$stats[3,1:20],pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(log(c(1:20)),x$stats[3,1:20],col=i,lwd=3)}
      
    }

######################################################
## admission duration
    
## most recent HbA1c in range (15 months)
boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,4),plot=T,main="admission duration vs last measured HbA1c value")

for (i in seq(4,30,1)) {
  x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$admissionDurationDays ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,5),plot=F)
  
  if (i==4) {plot(c(1:30),x$stats[3,1:30],ylim=c(0,20),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="admission duration vs last measured HbA1c. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of admission duration increasing with increasing numbers of CBGs measured during an admission"); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
  if (i>4) {points(c(1:30),x$stats[3,1:30],ylim=c(0,10),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
  
}

## median HbA1c in range (15 months)
boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,4),plot=T,main="admission duration vs median HbA1c value over past 15 months")

    for (i in seq(4,10,1)) {
      x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$admissionDurationDays ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,5),plot=F)
      if (i==4) {plot(c(1:30),x$stats[3,1:30],ylim=c(0,8),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="admission duration vs  median HbA1c over 15 months. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of admission duration increasing with increasing numbers of CBGs measured during an admission"); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
      if (i>4) {points(c(1:30),x$stats[3,1:30],ylim=c(0,10),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
      
    }

## median all HbA1c
boxplot(plotReportingDF$admissionDurationDays ~ cut(plotReportingDF$medianHbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,4),plot=T,main="admission duration vs median all prior HbA1c value")

for (i in seq(4,10,1)) {
  x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$admissionDurationDays ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$medianHbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,5),plot=F)
  if (i==4) {plot(c(1:30),x$stats[3,1:30],ylim=c(0,8),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="admission duration vs  median all prior HbA1c. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of admission duration increasing with increasing numbers of CBGs measured during an admission"); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
  if (i>4) {points(c(1:30),x$stats[3,1:30],ylim=c(0,10),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(c(1:30),x$stats[3,1:30],col=i,lwd=3)}
  
}

######################################################
## median glucose during admission 

## most recent HbA1c in range (15 months)
boxplot(plotReportingDF$medianGlu ~ cut(plotReportingDF$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(5,20),plot=T,main="admission median glucose vs most recent HbA1c value")

for (i in seq(1,30,1)) {
  x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$medianGlu ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$HbA1c,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,5),plot=F)
  if (i==1) {plot(c(1:20),x$stats[3,1:20],ylim=c(0,20),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="admission median glucose vs most recent HbA1c. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of admission duration increasing with increasing numbers of CBGs measured during an admission"); lines(c(1:20),x$stats[3,1:20],col=i,lwd=3)}
  if (i>1)  {points(c(1:20),x$stats[3,1:20],pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(c(1:20),x$stats[3,1:20],col=i,lwd=3)}
  
}

## median HbA1c in range (15 months)
boxplot(plotReportingDF$medianGlu ~ cut(plotReportingDF$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(5,20),plot=T,main="admission median glucose vs median HbA1c value in past 15 months")

for (i in seq(1,10,1)) {
  x<-boxplot(subset(plotReportingDF,nCBGinAdmission==i)$medianGlu ~ cut(subset(plotReportingDF,nCBGinAdmission==i)$medianHbA1cInFrame,breaks=seq(30,200,5)),las=3,varwidth=T,ylim=c(0,5),plot=F)
  if (i==1) {plot(c(1:20),x$stats[3,1:20],ylim=c(0,20),pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i,main="admission median glucose vs median HbA1c in past 15 months. each line represents an admission with n CBGs. point size corresponding to number of values\nthis controls for the issue of admission duration increasing with increasing numbers of CBGs measured during an admission"); lines(c(1:20),x$stats[3,1:20],col=i,lwd=3)}
  if (i>1)  {points(c(1:20),x$stats[3,1:20],pch=16,cex=4*(sqrt(x$n))/max(sqrt(x$n)),col=i); lines(c(1:20),x$stats[3,1:20],col=i,lwd=3)}
  
}


dev.off()
