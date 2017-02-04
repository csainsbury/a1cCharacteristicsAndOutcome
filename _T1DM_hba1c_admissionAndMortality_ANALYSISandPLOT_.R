library(data.table)
library(survival)
#source("./_T1DM_hba1c_admissionAndMortality.R")
#####################################################################################################
#####################################################################################################
## read in diabetes / hba1c prediction data, analyse and plot
#####################################################################################################
#####################################################################################################

###### type 1
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T1DM.csv",sep=""); reportingDF<-read.csv(tempWriteFile,stringsAsFactors=F); reportingDF<-data.table(reportingDF); diabetesType="Type 1 Diabetes. "
#

###### type 1 - hba1c revised code
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T1DM_hba1cRevision.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF); diabetesType="Type 1 Diabetes. "
#

###### type 1 - hba1c revised code. imposed date of 010510 for analysis
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T1DM_hba1cRevision_010510date.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF); diabetesType="Type 1 Diabetes. "
#
#####################################################################################################
###### type 2
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T2DM.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF); diabetesType="Type 2 Diabetes. "
#

###### type 2 - hba1c revised code
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T2DM_hba1cRevision.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF); diabetesType="Type 2 Diabetes. "
#
#####################################################################################################
###### type 2 with drug data
#
# tempWriteFile <- paste("../GlCoSy/source/admissionDataDT_T2DM_withDrugs.csv",sep=""); reportingDF<-read.csv(tempWriteFile); reportingDF<-data.table(reportingDF); diabetesType="Type 2 Diabetes. "; reportingDF$nCBGperAdmission<-reportingDF$nCBGperAdmission.x; reportingDF$yyyy<-reportingDF$yyyy.x; reportingDF$age<-reportingDF$age.x; reportingDF$countHypo3<-reportingDF$countHypo3.x; reportingDF$admissionDurationDays<-reportingDF$admissionDurationDays.x
#
#####################################################################################################

## add further calculation rows

#hba1cPercent_inFrame<-(reportingDF$medianHbA1cInFrame/10.929) + 2.15
#reportingDF$eAG_inFrame<- ((28.7*hba1cPercent_inFrame)-46.7)/18

reportingDF$eAGyyyyDiff<-reportingDF$eAG-reportingDF$yyyy
# reportingDF$eAGyyyyDiff_inFrame<-reportingDF$eAG_inFrame-reportingDF$yyyy
reportingDF$AGN2<-reportingDF$eAG-reportingDF$medianFirst2CBGs
reportingDF$AGN3<-reportingDF$eAG-reportingDF$medianFirst3CBGs
#
reportingDF$AGN4<-reportingDF$eAG-reportingDF$medianFirst4CBGs


reportingDF$hypo<-ifelse(reportingDF$ID_ADMISSIONhypoEpisodes4.60>0,1,0)

#####################################################################################################
## apply conditions for all analyses
plotReportingDF<-subset(reportingDF,nCBGperAdmission>=2)   # for admisisons: remove single CBG admissions
plotReportingDF<-subset(plotReportingDF,nHbA1cValuesInFrame>0)  # for HbA1c perior: remove those without a CBG in 15 month window
plotReportingDF<-data.table(plotReportingDF)
#####################################################################################################

#####################################################################################################
## ANALYSIS 1
## Relationship between last HbA1c and Properties of Admission

# diabetesType="Type 1 Diabetes. "
# 
diabetesType="Type 2 Diabetes. "
  
  ## generate individual plot function
  hbA1c_VS_admission<-function(hbA1cCharacteristic,admissionCharacteristic,breaksType,ylimMin,ylimMax,title) {
    
    plotTitle<-paste(diabetesType,"\n",title,"\n number of admissions: ",length(admissionCharacteristic),sep="")
    boxplot(admissionCharacteristic ~ cut(hbA1cCharacteristic,breaks=breaksType),las=3,ylim=c(ylimMin,ylimMax),varwidth=T,plot=T,main=plotTitle)
    
  }
  
  ## function to generate series of plots linking paramter to admission characteristics
  testParameterVSadmissionParameter<-function(testParameter,testParamBreaks,testParamDescription) {
    
    hbA1c_VS_admission(testParameter,plotReportingDF$IQR,testParamBreaks,0,8,paste(testParamDescription," vs admission IQR",sep=""))
    
    ## last HbA1c vs admission ninetyTen
    hbA1c_VS_admission(testParameter,plotReportingDF$ninetyTen,testParamBreaks,1.2,3,paste(testParamDescription," vs admission 90c / 10c",sep=""))
    
    ## last HbA1c vs admission duration
    hbA1c_VS_admission(testParameter,plotReportingDF$admissionDurationDays,testParamBreaks,0,10,paste(testParamDescription," vs admission duration",sep=""))
    
    hbA1c_VS_admission(testParameter,plotReportingDF$admissionDurationDays,10,0,10,paste(testParamDescription," vs admission duration. 10 divisions",sep=""))
    
    ## last HbA1c vs admission median
    hbA1c_VS_admission(testParameter,plotReportingDF$medianGlu,testParamBreaks,4,16,paste(testParamDescription," vs admission median glucose",sep=""))
    
    ## last HbA1c vs yyyy
    hbA1c_VS_admission(testParameter,plotReportingDF$yyyy,testParamBreaks,4,20,paste(testParamDescription," vs admission first CBG",sep=""))
    
    ## last HbA1c vs minimum glucose
    hbA1c_VS_admission(testParameter,plotReportingDF$minGlu,testParamBreaks,1,8,paste(testParamDescription," vs admission minimum glucose",sep=""))
    
    
    ## last HbA1c vs maximum glucose
    hbA1c_VS_admission(testParameter,plotReportingDF$maxGlu,testParamBreaks,4,26,paste(testParamDescription," vs admission maximum glucose",sep=""))
    
    ## last HbA1c vs age
    # ageReportingDF<-plotReportingDF
    # ageReportingDF[, admissionNumberForPlotting := .N , by=ID]
    # ageReportingDF<-ageReportingDF[admissionNumberForPlotting==1]
    # hbA1c_VS_admission(testParameter,ageReportingDF$age,testParamBreaks,14,100,"Last HbA1c vs admission maximum Glucose. Unique IDs.")
    
  }
  
  ## function to generate proportion of admissions with hypos
  plotHypoProbability<-function(hypoThresh,testParameter,numberOfDivisions,plotReportingDF,testParameterName,ylimMin,ylimMax) {
  
        quantileVals<-quantile(testParameter,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)
        propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
        propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]
        
        # for (h in seq(-20,20,1)) {
        for (h in seq(1,nrow(propPlot),1)) {
          if (h<nrow(propPlot)) {hypoTestSet<-subset(plotReportingDF,testParameter>=propPlot$xAxisVals[h] & testParameter<propPlot$xAxisVals[h+1] & yyyy>=hypoThresh) }
          if (h==nrow(propPlot)) {hypoTestSet<-subset(plotReportingDF,testParameter>=propPlot$xAxisVals[h] & testParameter<max(testParameter) & yyyy>=hypoThresh) }
          
          nInSet<-nrow(hypoTestSet)
          
          if (hypoThresh==4) { nInSetWithHypo<-nrow(hypoTestSet[ID_ADMISSIONhypoEpisodes4.60>0]) }
          if (hypoThresh==3) { nInSetWithHypo<-nrow(hypoTestSet[countHypo3>0]) }
          
          proportionWithHypo<-nInSetWithHypo/nInSet
          
          propPlot$nInSet[h]<-nInSet
          propPlot$prop[h]<-proportionWithHypo
          
          
          # print(proportionWithHypo)
        }
        
        plot(propPlot$xAxisVals,propPlot$prop,ylim=c(ylimMin,ylimMax),pch=16,cex=2,
             xlab=paste(testParameterName,sep=""),
             ylab="proprtion of admissions with >=1 episodes of hypoglycaemia",
             main=paste("relationship between difference between ",testParameterName," (centiles) and proportion of admissions with hypoglycaemia \n admissions where first value is <4 excluded.\nn admissions=",nrow(plotReportingDF),sep=""))
  # lines(propPlot$xAxisVals,propPlot$prop)
  }
  
  ## function to generate proportion of admissions with hypos
  plotHypoProbabilityReturnFrame<-function(hypoThresh,testParameter,numberOfDivisions,plotReportingDF,testParameterName,ylimMin,ylimMax) {
    
    quantileVals<-quantile(testParameter,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)
    propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
    propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]
    
    # for (h in seq(-20,20,1)) {
    for (h in seq(1,nrow(propPlot),1)) {
      if (h<nrow(propPlot)) {hypoTestSet<-subset(plotReportingDF,testParameter>=propPlot$xAxisVals[h] & testParameter<propPlot$xAxisVals[h+1] & yyyy>=hypoThresh) }
      if (h==nrow(propPlot)) {hypoTestSet<-subset(plotReportingDF,testParameter>=propPlot$xAxisVals[h] & testParameter<max(testParameter) & yyyy>=hypoThresh) }
      
      nInSet<-nrow(hypoTestSet)
      
      if (hypoThresh==4) { nInSetWithHypo<-nrow(hypoTestSet[ID_ADMISSIONhypoEpisodes4.60>0]) }
      if (hypoThresh==3) { nInSetWithHypo<-nrow(hypoTestSet[countHypo3>0]) }
      
      proportionWithHypo<-nInSetWithHypo/nInSet
      
      propPlot$nInSet[h]<-nInSet
      propPlot$prop[h]<-proportionWithHypo
      
      
      # print(proportionWithHypo)
    }
    
    return(propPlot)
    
 #   plot(propPlot$xAxisVals,propPlot$prop,ylim=c(ylimMin,ylimMax),pch=16,cex=2,
  #       xlab=paste(testParameterName,sep=""),
   #      ylab="proprtion of admissions with >=1 episodes of hypoglycaemia",
    #     main=paste("relationship between difference between ",testParameterName," (centiles) and proportion of admissions with hypoglycaemia \n admissions where first value is <4 excluded.\nn admissions=",nrow(plotReportingDF),sep=""))
    # lines(propPlot$xAxisVals,propPlot$prop)
  }
  

  plotReportingDF.all<-plotReportingDF
  # plotReportingDF.SUorIns<-subset(plotReportingDF.all,preSU==1 | preIns==1); plotReportingDF<-plotReportingDF.SUorIns
  # plotReportingDF.Ins<-subset(plotReportingDF.all,preIns==1); plotReportingDF<-plotReportingDF.Ins
  # plotReportingDF.NO_SUorIns<-subset(plotReportingDF.all,preSU==0 & preIns==0); plotReportingDF<-plotReportingDF.NO_SUorIns
  # plotReportingDF<- plotReportingDF.all
#####################################################################################################
## ANALYSIS 2
## predictive capability - train/test and prediction analysis

  ## generate train / test sets
  # set.seed(12)  ## T2
 # set.seed(46)  ## T1
  n = nrow(plotReportingDF)
  train = sample(1:n, size = round(0.75*n), replace=FALSE)
  plotReportingDF.train = plotReportingDF[train,]
  plotReportingDF.test = plotReportingDF[-train,]
  
  numberOfDivisionsSet<-50
  
  hypoThresh=3
  hypoProbability_yLim=0.3
  
      AGN_set <-subset(plotReportingDF.train,nCBGperAdmission>2 & yyyy1>hypoThresh & yyyy2>hypoThresh)
      AGN2_set<-subset(plotReportingDF.train,nCBGperAdmission>2 & yyyy1>hypoThresh & yyyy2>hypoThresh)
      AGN3_set<-subset(plotReportingDF.train,nCBGperAdmission>3 & yyyy1>hypoThresh& yyyy2>hypoThresh & yyyy3>hypoThresh)
      AGN4_set<-subset(plotReportingDF.train,nCBGperAdmission>4 & yyyy1>hypoThresh & yyyy2>hypoThresh & yyyy3>hypoThresh & yyyy4>hypoThresh)
      
      AGN_testSet <-subset(plotReportingDF.test,nCBGperAdmission>2 & yyyy1>hypoThresh & yyyy2>hypoThresh)
      AGN2_testSet<-subset(plotReportingDF.test,nCBGperAdmission>2 & yyyy1>hypoThresh & yyyy2>hypoThresh)
      AGN3_testSet<-subset(plotReportingDF.test,nCBGperAdmission>3 & yyyy1>hypoThresh& yyyy2>hypoThresh & yyyy3>hypoThresh)
      AGN4_testSet<-subset(plotReportingDF.test,nCBGperAdmission>4 & yyyy1>hypoThresh & yyyy2>hypoThresh & yyyy3>hypoThresh & yyyy4>hypoThresh)
  
  plotHypoProbability(hypoThresh,AGN_set$eAGyyyyDiff,numberOfDivisionsSet,AGN_set,"eAGyyyyDiff",0.1,hypoProbability_yLim)
  plotHypoProbability(hypoThresh,AGN2_set$AGN2,numberOfDivisionsSet,AGN2_set,"AGN2",0.1,hypoProbability_yLim)
  plotHypoProbability(hypoThresh,AGN3_set$AGN3,numberOfDivisionsSet,AGN3_set,"AGN3",0.1,hypoProbability_yLim)
  plotHypoProbability(hypoThresh,AGN4_set$AGN4,numberOfDivisionsSet,AGN4_set,"AGN4",0.1,hypoProbability_yLim)
  
## regression for test set AGN >=0  
    AGNgreaterThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN_set,eAGyyyyDiff>=0)$eAGyyyyDiff,numberOfDivisionsSet,subset(AGN_set,eAGyyyyDiff>=0),"eAGyyyyDiff",0.1,hypoProbability_yLim)
    fit1 <- lm(AGNgreaterThanZero$prop ~ AGNgreaterThanZero$xAxisVals)
    
    plot(AGNgreaterThanZero$xAxisVals,AGNgreaterThanZero$prop)
    abline(fit1,col="red")
    
    ## regression for test set AGN <0  
    
    AGNlessThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN_set,eAGyyyyDiff<0)$eAGyyyyDiff,numberOfDivisionsSet,subset(AGN_set,eAGyyyyDiff<0),"eAGyyyyDiff",0.1,hypoProbability_yLim)
  fit2 <- lm(AGNlessThanZero$prop ~ AGNlessThanZero$xAxisVals)
  
  plot(AGNlessThanZero$xAxisVals,AGNlessThanZero$prop)
  abline(fit2,col="red")
  
  #####################################################
        ## regression for test set AGN2 >=0  
        AGN2greaterThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN2_set,AGN2>=0)$AGN2,numberOfDivisionsSet,subset(AGN2_set,AGN2>=0),"AGN2",0.1,hypoProbability_yLim)
        fit1_AGN2 <- lm(AGN2greaterThanZero$prop ~ AGN2greaterThanZero$xAxisVals)
        
        plot(AGN2greaterThanZero$xAxisVals,AGN2greaterThanZero$prop)
        abline(fit1_AGN2,col="red")
        
        ## regression for test set AGN2 <0  
        
        AGN2lessThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN2_set,AGN2<0)$AGN2,numberOfDivisionsSet,subset(AGN2_set,AGN2<0),"AGN2",0.1,hypoProbability_yLim)
        fit2_AGN2 <- lm(AGN2lessThanZero$prop ~ AGN2lessThanZero$xAxisVals)
        
        plot(AGN2lessThanZero$xAxisVals,AGN2lessThanZero$prop)
        abline(fit2_AGN2,col="red")
        
          #####################################################
          ## regression for test set AGN3 >=0  
          AGN3greaterThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN3_set,AGN3>=0)$AGN3,numberOfDivisionsSet,subset(AGN3_set,AGN3>=0),"AGN3",0.1,hypoProbability_yLim)
          fit1_AGN3 <- lm(AGN3greaterThanZero$prop ~ AGN3greaterThanZero$xAxisVals)
          
          plot(AGN3greaterThanZero$xAxisVals,AGN3greaterThanZero$prop)
          abline(fit1_AGN3,col="red")
          
          ## regression for test set AGN3 <0  
          
          AGN3lessThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN3_set,AGN3<0)$AGN3,numberOfDivisionsSet,subset(AGN3_set,AGN3<0),"AGN3",0.1,hypoProbability_yLim)
          fit2_AGN3 <- lm(AGN3lessThanZero$prop ~ AGN3lessThanZero$xAxisVals)
          
          plot(AGN3lessThanZero$xAxisVals,AGN3lessThanZero$prop)
          abline(fit2_AGN3,col="red")
          
              #####################################################
              ## regression for test set AGN4 >=0  
              AGN4greaterThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN4_set,AGN4>=0)$AGN4,numberOfDivisionsSet,subset(AGN4_set,AGN4>=0),"AGN4",0.1,hypoProbability_yLim)
              fit1_AGN4 <- lm(AGN4greaterThanZero$prop ~ AGN4greaterThanZero$xAxisVals)
              
              plot(AGN4greaterThanZero$xAxisVals,AGN4greaterThanZero$prop)
              abline(fit1_AGN4,col="red")
              
              ## regression for test set AGN4 <0  
              
              AGN4lessThanZero<-plotHypoProbabilityReturnFrame(hypoThresh,subset(AGN4_set,AGN4<0)$AGN4,numberOfDivisionsSet,subset(AGN4_set,AGN4<0),"AGN4",0.1,hypoProbability_yLim)
              fit2_AGN4 <- lm(AGN4lessThanZero$prop ~ AGN4lessThanZero$xAxisVals)
              
              plot(AGN4lessThanZero$xAxisVals,AGN4lessThanZero$prop)
              abline(fit2_AGN4,col="red")
              
  
###################################################################################################################
  ## apply the predicted hypo risk scores to the test dataset
  # AGN
              AGN_testSet$AGN_hypoProb<- ifelse(AGN_testSet$eAGyyyyDiff>=0,
                                             (fit1$coefficients[2]*AGN_testSet$eAGyyyyDiff)+ fit1$coefficients[1],
                                             (fit2$coefficients[2]*AGN_testSet$eAGyyyyDiff)+ fit2$coefficients[1]
                                             )
  # AGN2
              AGN2_testSet$AGN2_hypoProb<- ifelse(AGN2_testSet$AGN2>=0,
                                             (fit1_AGN2$coefficients[2]*AGN2_testSet$AGN2)+ fit1_AGN2$coefficients[1],
                                             (fit2_AGN2$coefficients[2]*AGN2_testSet$AGN2)+ fit2_AGN2$coefficients[1]
  )
  # AGN3
              AGN3_testSet$AGN3_hypoProb<- ifelse(AGN3_testSet$AGN3>=0,
                                              (fit1_AGN3$coefficients[2]*AGN3_testSet$AGN3)+ fit1_AGN3$coefficients[1],
                                              (fit2_AGN3$coefficients[2]*AGN3_testSet$AGN3)+ fit2_AGN3$coefficients[1]
  )
              
  # AGN4
              AGN4_testSet$AGN4_hypoProb<- ifelse(AGN4_testSet$AGN4>=0,
                                  (fit1_AGN4$coefficients[2]*AGN4_testSet$AGN4)+ fit1_AGN4$coefficients[1],
                                  (fit2_AGN4$coefficients[2]*AGN4_testSet$AGN4)+ fit2_AGN4$coefficients[1]
  )
  
  ## generate regression of observed vs predicted
      # AGN
      predictedVsObservedHypo<-plotHypoProbabilityReturnFrame(hypoThresh,AGN_testSet$AGN_hypoProb,numberOfDivisionsSet,AGN_testSet,"predicted probability of hypo",0,0.4)
     # fit3_log <- lm(predictedVsObservedHypo$prop ~ log(predictedVsObservedHypo$xAxisVals))
      fit3 <- lm(predictedVsObservedHypo$prop ~ predictedVsObservedHypo$xAxisVals)
      
      # AGN2
      predictedVsObservedHypoAGN2<-plotHypoProbabilityReturnFrame(hypoThresh,AGN2_testSet$AGN2_hypoProb,numberOfDivisionsSet,AGN2_testSet,"predicted probability of hypo",0,0.4)
      # fit3_log <- lm(predictedVsObservedHypo$prop ~ log(predictedVsObservedHypo$xAxisVals))
      fit3_AGN2 <- lm(predictedVsObservedHypoAGN2$prop ~ predictedVsObservedHypoAGN2$xAxisVals)
      
      # AGN3
      predictedVsObservedHypoAGN3<-plotHypoProbabilityReturnFrame(hypoThresh,AGN3_testSet$AGN3_hypoProb,numberOfDivisionsSet,AGN3_testSet,"predicted probability of hypo",0,0.4)
      # fit3_log <- lm(predictedVsObservedHypo$prop ~ log(predictedVsObservedHypo$xAxisVals))
      fit3_AGN3 <- lm(predictedVsObservedHypoAGN3$prop ~ predictedVsObservedHypoAGN3$xAxisVals)
      
      # AGN4
      predictedVsObservedHypoAGN4<-plotHypoProbabilityReturnFrame(hypoThresh,AGN4_testSet$AGN4_hypoProb,numberOfDivisionsSet,AGN4_testSet,"predicted probability of hypo",0,0.4)
      # fit3_log <- lm(predictedVsObservedHypo$prop ~ log(predictedVsObservedHypo$xAxisVals))
      fit3_AGN4 <- lm(predictedVsObservedHypoAGN4$prop ~ predictedVsObservedHypoAGN4$xAxisVals)
      
      
      # AGN
      ## plot observed vs predicted
      ## plot(log(predictedVsObservedHypo$xAxisVals),predictedVsObservedHypo$prop,ylim=c(0,0.4))
      plot(predictedVsObservedHypo$xAxisVals,predictedVsObservedHypo$prop,ylim=c(0,hypoProbability_yLim))
      abline(fit3,col="red")
      
      summary(fit3)
      
      ## test whether AGN_hypoProb is a predictor when other factors accounted for
      logitRegression<-glm(hypo ~ AGN_hypoProb + age,data=subset(AGN_testSet,yyyy>=4))
      summary(logitRegression)
      
        # AGN2
        ## plot observed vs predicted
        ## plot(log(predictedVsObservedHypo$xAxisVals),predictedVsObservedHypo$prop,ylim=c(0,0.4))
        plot(predictedVsObservedHypoAGN2$xAxisVals,predictedVsObservedHypoAGN2$prop,ylim=c(0,hypoProbability_yLim))
        abline(fit3_AGN2,col="red")
        
        summary(fit3_AGN2)
        
        ## test whether AGN_hypoProb is a predictor when other factors accounted for
        logitRegression_AGN2<-glm(hypo ~ AGN2_hypoProb + age,data=subset(AGN2_testSet,yyyy>=4))
        summary(logitRegression_AGN2)
      
      # AGN3
      ## plot observed vs predicted
      ## plot(log(predictedVsObservedHypo$xAxisVals),predictedVsObservedHypo$prop,ylim=c(0,0.4))
      plot(predictedVsObservedHypoAGN3$xAxisVals,predictedVsObservedHypoAGN3$prop,ylim=c(0,hypoProbability_yLim))
      abline(fit3_AGN3,col="red")
      
      summary(fit3_AGN3)
      
      ## test whether AGN_hypoProb is a predictor when other factors accounted for
      logitRegression_AGN3<-glm(hypo ~ AGN3_hypoProb + age,data=subset(AGN3_testSet,yyyy>=4))
      summary(logitRegression_AGN3)
      
        # AGN4
        ## plot observed vs predicted
        ## plot(log(predictedVsObservedHypo$xAxisVals),predictedVsObservedHypo$prop,ylim=c(0,0.4))
        plot(predictedVsObservedHypoAGN4$xAxisVals,predictedVsObservedHypoAGN4$prop,ylim=c(0,hypoProbability_yLim))
        abline(fit3_AGN4,col="red")
        
        summary(fit3_AGN4)
        
        ## test whether AGN_hypoProb is a predictor when other factors accounted for
        logitRegression_AGN4<-glm(hypo ~ AGN4_hypoProb + age,data=subset(AGN4_testSet,yyyy>=4))
        summary(logitRegression_AGN4)
      
      
###################################################################################################################
        ## try a combined risk score:
        AGN2_set$AGN2_hypoProb<- ifelse(AGN2_set$AGN2>=0,
                                       (fit1_AGN2$coefficients[2]*AGN2_set$AGN2)+ fit1_AGN2$coefficients[1],
                                       (fit2_AGN2$coefficients[2]*AGN2_set$AGN2)+ fit2_AGN2$coefficients[1]
        )
        
        #logitRegression_forRisk<-glm(hypo ~ AGN2_hypoProb  + IQRFirst2CBGs + IQRFirst2CBGsTime,data=subset(AGN2_set,yyyy>=4))
        logitRegression_forRisk<-glm(hypo ~ AGN2_hypoProb  + IQRFirst2CBGs,data=subset(AGN2_set,yyyy>=4))
        summary(logitRegression_forRisk)
        
        AGN_testSet$AGN2_hypoProb<- ifelse(AGN_testSet$AGN2>=0,
                                           (fit1_AGN2$coefficients[2]*AGN_testSet$AGN2)+ fit1_AGN2$coefficients[1],
                                           (fit2_AGN2$coefficients[2]*AGN_testSet$AGN2)+ fit2_AGN2$coefficients[1]
        )
        
        
        AGN_testSet$compositeRisk<-( (AGN_testSet$AGN2_hypoProb * logitRegression_forRisk$coefficients[2])+
                                       #   (plotReportingDF.test$age * logitRegression_forRisk$coefficients[3])+
                                       (AGN_testSet$IQRFirst2CBGs * logitRegression_forRisk$coefficients[3])
                                       # (AGN_testSet$IQRFirst2CBGsTime * logitRegression_forRisk$coefficients[4])
        )
        #(plotReportingDF.test$IQRHbA1cInFrame * logitRegression_forRisk$coefficients[4])
        #(plotReportingDF.test$nHbA1cValuesInFrame * logitRegression_forRisk$coefficients[5])
        
        ## plot
        predictedVsObservedHypo_composite<-plotHypoProbabilityReturnFrame(hypoThresh,AGN_testSet$compositeRisk,numberOfDivisionsSet,AGN_testSet,"predicted probability of hypo",0,0.4)
        fit4 <- lm(predictedVsObservedHypo_composite$prop ~ predictedVsObservedHypo_composite$xAxisVals)
        
        plot(predictedVsObservedHypo_composite$xAxisVals,predictedVsObservedHypo_composite$prop,ylim=c(0,hypoProbability_yLim),main=paste(diabetesType," composite risk score (x) vs observed proportion of admisison with hypoglycaemia in test set. ",nrow(AGN_testSet)," admissions.\nregression coefficient: ", summary(fit4)$coefficient[2]," R2: ",summary(fit4)$r.squared,sep=""))
        abline(fit4,col="red")
        
        summary(fit4)
        
        print(predictedVsObservedHypo_composite)
        
##########################################################################################################################
## what are characteristics of top and bottom quartiles of composite risk score?

  # age
        # top
        hist(subset(AGN_testSet,compositeRisk>quantile(AGN_testSet$compositeRisk)[4])$age)
        # bottom
        hist(subset(AGN_testSet,compositeRisk< quantile(AGN_testSet$compositeRisk)[2])$age)
  # admission duration log
        # top
        hist(log(subset(AGN_testSet,compositeRisk>quantile(AGN_testSet$compositeRisk)[4])$admissionDurationDays.x))
        # bottom
        hist(log(subset(AGN_testSet,compositeRisk<quantile(AGN_testSet$compositeRisk)[2])$admissionDurationDays.x))
        
        compositeRiskAndAdmissionDuration<-boxplot(AGN_testSet$admissionDurationDays ~ cut(AGN_testSet$compositeRisk,breaks=quantile(AGN_testSet$compositeRisk)),ylim=c(0,10),varwidth=T)
        
        compositeRiskAndAdmissionDuration<-boxplot(AGN_testSet$admissionDurationDays ~ cut(AGN_testSet$compositeRisk,breaks=20,ylim=c(0,10),varwidth=T))
        
        
        
      
  
      ## save out plots
      plotfilename <- paste("../GlCoSy/plots/_AGN_predictingHypoGlycaemia_T2.pdf",sep="")
      pdf(plotfilename, width=16, height=9)
      
            plotHypoProbability(hypoThresh,plotReportingDF.train$eAGyyyyDiff,numberOfDivisionsSet,plotReportingDF.train,"eAGyyyyDiff",0,hypoProbability_yLim)
            
            plot(AGNlessThanZero$xAxisVals,AGNlessThanZero$prop,pch=16,cex=2,main="AGN vs proportion of admissions with >=1 hypo where AGN <0 with regression line")
            abline(fit2,col="red")
            
            plot(AGNgreaterThanZero$xAxisVals,AGNgreaterThanZero$prop,pch=16,cex=2,main="AGN vs proportion of admissions with >=1 hypo where AGN >=0 with regression line")
            abline(fit1,col="red")
            
            plot(predictedVsObservedHypo$xAxisVals,predictedVsObservedHypo$prop,ylim=c(0.1,hypoProbability_yLim),pch=16,cex=2,main=paste("observed vs predicted proportion of admissions with hypoglycaemia.\nslope of regression line",fit3$coefficients[2],". r-squared", summary(fit3)$r.squared))
            abline(fit3,col="red")
        
        dev.off()
      
      
      
      

  
#  noisy.y<-propPlot$prop
#  q<-propPlot$xAxisVals
#  model <- lm(noisy.y ~ poly(q,3))
  
#  predicted.intervals <- predict(model,data.frame(x=q),interval='confidence',level=0.99)
#  lines(q,predicted.intervals[,1],col='green',lwd=3)
#  lines(q,predicted.intervals[,2],col='black',lwd=1)
#  lines(q,predicted.intervals[,3],col='black',lwd=1)
#   legend("bottomright",c("Observ.","Signal","Predicted"), col=c("deepskyblue4","red","green"), lwd=3)
  
  
#####################################################################################################
## ANALYSIS 3
## HbA1c and HbA1c/yyyy properties and survival
  
  
  simpleSurvivalPlot<-function(inputFrame,parameterToTest,postDischargeStartDay,analysisPlotTitle,quantileDivisions) {
    
    # inputFrame<-survivalPlotReportingDF; parameterToTest<-survivalPlotReportingDF$inWindow_timeIntervalIQR; postDischargeStartDay<-30; analysisPlotTitle<-""; quantileDivisions<-2
    
    SurvivalData<-inputFrame
  #  SurvivalData<-survivalPloReportingDF[eAGyyyyDiff<0]
    
    # SurvivalData<-subset(SurvivalData,diagnosisDateUnix>(-2208988800))
    SurvivalData$diabetesDuration<-SurvivalData$dateplustime1 - SurvivalData$diagnosisDateUnix
    
    DaySeconds<-(60*60*24)
    shortCensorPeriodStartDay  <- DaySeconds*postDischargeStartDay
    shortCensorPeriodEndDay    <- DaySeconds*10000
    
    lastDOD<-max(SurvivalData$deathDateUnix)
    SurvivalData$dateOfDischarge<-SurvivalData$dateplustime1+SurvivalData$admissionDuration
    SurvivalData$deathEvent<-ifelse(SurvivalData$deathDateUnix>0,1,0)
    
    SurvivalData$timeToDeath<-ifelse(SurvivalData$deathEvent==1,(SurvivalData$deathDateUnix-SurvivalData$dateOfDischarge),0)
    #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
    SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$deathEvent==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
    SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0 # ; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
    #		SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/DaySeconds
    
    SurvivalData$shortDeathEvent <- SurvivalData$deathEvent
    SurvivalData$shortDeathEvent <- ifelse(SurvivalData$deathEvent==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
    
    SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
    SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
    SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
    
    
    mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (parameterToTest>=quantile(parameterToTest,prob = seq(0, 1, length = (quantileDivisions+1)), type = 5)[quantileDivisions]), data = SurvivalData)
    shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds,"days\nParameter tested:",analysisPlotTitle,", threshold: ",quantile(parameterToTest)[3],"\nn=",nrow(SurvivalData),". covariables: age, number of hba1c values during period of interest, duration of diabetes",sep="")
    plot(mfitAge50,mark.time=F,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3)
    mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age+admissionDuration+diabetesDuration+(parameterToTest>=quantile(parameterToTest,prob = seq(0, 1, length = (quantileDivisions+1)), type = 5)[quantileDivisions]), data = SurvivalData)
    pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
    legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
    summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
    legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
    
  }
  
  
  
  simpleSurvivalPlot.modified<-function(inputFrame,parameterToTest,postDischargeStartDay,analysisPlotTitle,covariable2,quantileDivisions) {
    
    # inputFrame<-survivalPlotReportingDF; parameterToTest<-survivalPlotReportingDF$inWindow_timeIntervalIQR; postDischargeStartDay<-30; analysisPlotTitle<-""; quantileDivisions<-2
    
    SurvivalData<-inputFrame
    #  SurvivalData<-survivalPloReportingDF[eAGyyyyDiff<0]
    
    # SurvivalData<-subset(SurvivalData,diagnosisDateUnix>(-2208988800))
    SurvivalData$diabetesDuration<-SurvivalData$dateplustime1 - SurvivalData$diagnosisDateUnix
    
    DaySeconds<-(60*60*24)
    shortCensorPeriodStartDay  <- DaySeconds*postDischargeStartDay
    shortCensorPeriodEndDay    <- DaySeconds*10000
    
    lastDOD<-max(SurvivalData$deathDateUnix)
    SurvivalData$dateOfDischarge<-SurvivalData$dateplustime1+SurvivalData$admissionDuration
    SurvivalData$deathEvent<-ifelse(SurvivalData$deathDateUnix>0,1,0)
    
    SurvivalData$timeToDeath<-ifelse(SurvivalData$deathEvent==1,(SurvivalData$deathDateUnix-SurvivalData$dateOfDischarge),0)
    #		SurvivalData$timeToDeath<-SurvivalData$timeToDeath/DaySeconds
    SurvivalData$timeToDeathInterval<-ifelse(SurvivalData$deathEvent==0,(lastDOD-SurvivalData$dateOfDischarge),SurvivalData$timeToDeath)
    SurvivalData$timeToDeathInterval[is.na(SurvivalData$timeToDeathInterval)]<-0 # ; SurvivalData<-subset(SurvivalData,timeToDeathInterval>0)
    #		SurvivalData$timeToDeathInterval<-SurvivalData$timeToDeathInterval/DaySeconds
    
    SurvivalData$shortDeathEvent <- SurvivalData$deathEvent
    SurvivalData$shortDeathEvent <- ifelse(SurvivalData$deathEvent==1 & SurvivalData$timeToDeath>=(shortCensorPeriodStartDay) & SurvivalData$timeToDeath<(shortCensorPeriodEndDay),1,0)	
    
    SurvivalData$sexDigit<-ifelse(nchar(SurvivalData$charID==9),as.numeric(substr(SurvivalData$charID,8,8)),as.numeric(substr(SurvivalData$charID,9,9)))
    SurvivalData$sexNumber<-ifelse(SurvivalData$sexDigit%%2==0,1,0)
    SurvivalData$sex<-factor(1*(SurvivalData$sexNumber <1),levels=0:1,labels=c("F","M"))
    
    
    mfitAge50<-survfit(Surv(timeToDeathInterval, shortDeathEvent) ~ (parameterToTest>=quantile(parameterToTest,prob = seq(0, 1, length = (quantileDivisions+1)), type = 5)[quantileDivisions]), data = SurvivalData)
    shortPlotTitle <- paste("Mortality, time ",round(shortCensorPeriodStartDay)/DaySeconds," to ",round(max(SurvivalData$timeToDeathInterval))/DaySeconds,"days\nParameter tested:",analysisPlotTitle,", threshold: ",quantile(parameterToTest)[3],"\nn=",nrow(SurvivalData),". covariables: age, number of hba1c values during period of interest, duration of diabetes",sep="")
    plot(mfitAge50,mark.time=F,lty=1:6,conf.int=F,col=c("black","red","blue","green","orange","purple"),main=shortPlotTitle,xlim=c(shortCensorPeriodStartDay,round(max(SurvivalData$timeToDeathInterval))),lwd=3)
    mfitAge50.coxph<-coxph(Surv(timeToDeathInterval, shortDeathEvent) ~ age+covariable2+(parameterToTest>=quantile(parameterToTest,prob = seq(0, 1, length = (quantileDivisions+1)), type = 5)[quantileDivisions]), data = SurvivalData)
    pVal <- summary(mfitAge50.coxph)$coef[,5]; HR <- round(exp(coef(mfitAge50.coxph)),2)
    legendText <- paste("p = ",pVal," | HR = ",HR,sep="")
    summarySurvfit <- summary(mfitAge50); legendNames <- row.names(summarySurvfit$table)
    legend("bottomleft",c(legendNames),lty=1:6,col=c("black","red","blue","green","orange","purple"),cex=0.8); legend("bottomright",legendText,cex=0.6)
    
  }
  
  findFirstAdmission<-function(admissionNumberFlag) {
    reportDF_anf<-as.data.frame(matrix(0,nrow=length(admissionNumberFlag),ncol=2))
    colnames(reportDF_anf)<-c("admissionNumberFlag","minFlag")
    
    reportDF_anf$admissionNumberFlag<-admissionNumberFlag
    reportDF_anf$minFlag<-ifelse(reportDF_anf$admissionNumberFlag==min(admissionNumberFlag),1,0)
    
    return(reportDF_anf$minFlag)
  }
  
  orderedPloReportingDF<-plotReportingDF[order(plotReportingDF$ID,plotReportingDF$dateplustime1),]
  orderedPloReportingDF[, c("flagFirstAdmission") := findFirstAdmission(admissionNumberFlag) , by=.(ID)]
  
  survivalPlotReportingDF<-orderedPloReportingDF[flagFirstAdmission==1]
  survivalPlotReportingDF<-subset(survivalPlotReportingDF,diagnosisDateUnix>(-2208988800))
  
  survivalPlotReportingDF$diabetesDuration<-survivalPlotReportingDF$dateplustime1 - survivalPlotReportingDF$diagnosisDateUnix
  
  
  plotfilename <- paste("../GlCoSy/plots/_T1DM-hba1cValueAndTimeVariabilityAndSurvival_imposedAdmissionDate010510.pdf",sep="")
  pdf(plotfilename, width=16, height=9)
  
    inWindowPlotset<-subset(survivalPlotReportingDF,inWindow_nVals>2)
  
  simpleSurvivalPlot(inWindowPlotset,inWindowPlotset$inWindow_hbIQR,0,paste("T1DM in window (15 months) IQR hba1c",sep=""),inWindowPlotset$inWindow_nVals,2)
  simpleSurvivalPlot(inWindowPlotset,inWindowPlotset$inWindow_hbMedian,0,"T1DM in window (15 months) median hba1c",inWindowPlotset$inWindow_nVals,2)
  simpleSurvivalPlot(inWindowPlotset,inWindowPlotset$inWindow_timeIntervalIQR,0,"T1DM in window (15 months) IQR time interval hba1c",inWindowPlotset$inWindow_nVals,2)
  simpleSurvivalPlot(inWindowPlotset,inWindowPlotset$inWindow_gradient,0,"T1DM in window (15 months) rate of change of hba1c",inWindowPlotset$inWindow_nVals,2)
  
  allPriorPlotset<-subset(survivalPlotReportingDF,allPrior_nVals>2)
  
  simpleSurvivalPlot(allPriorPlotset,allPriorPlotset$allPrior_hbIQR,0,"T1DM all prior values IQR hba1c",allPriorPlotset$allPrior_nVals,2)
  simpleSurvivalPlot(allPriorPlotset,allPriorPlotset$allPrior_hbMedian,0,"T1DM all prior values median hba1c",allPriorPlotset$allPrior_nVals,2)
  simpleSurvivalPlot(allPriorPlotset,allPriorPlotset$allPrior_timeIntervalIQR,0,"T1DM all prior values IQR time interval hba1c",allPriorPlotset$allPrior_nVals,2)

  dev.off()
  
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$eAG,30,"T1DM eAG",2)
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$yyyy,30,"T1DM first CBG",2)
    
###########################################################
## these are the plots for abstracts to duk (t2) and attd (t1)
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$eAGyyyyDiff,90,"T1DM AGN",2)
      simpleSurvivalPlot(survivalPlotReportingDF,sqrt((survivalPlotReportingDF$eAGyyyyDiff - (quantile(survivalPlotReportingDF$eAGyyyyDiff)[3]))^2),60,"T1DM AGN",2)
###########################################################

    
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$AGN2,30,"T1DM AGN2",2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nCBGperAdmission>=3),subset(survivalPlotReportingDF,nCBGperAdmission>=3)$AGN3,30,"T1DM AGN3",2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nCBGperAdmission>=4),subset(survivalPlotReportingDF,nCBGperAdmission>=4)$AGN4,30,"T1DM AGN4",2)
    
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$IQRFirst2CBGs,30,"T1DM IQR_2",2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nCBGperAdmission>=3),subset(survivalPlotReportingDF,nCBGperAdmission>=3)$IQRFirst3CBGs,30,"T1DM IQR_3",2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nCBGperAdmission>=4),subset(survivalPlotReportingDF,nCBGperAdmission>=4)$IQRFirst4CBGs,30,"T1DM IQR_4",2)
    
    #
    simpleSurvivalPlot(subset(survivalPlotReportingDF,age<40),subset(survivalPlotReportingDF,age<40)$CBGTimeIQR,30,paste("T2DM. Age <40. IQR of intervals between CBG tests\nn=",nrow(subset(survivalPlotReportingDF,age<40))," individuals",sep=""),2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,age<50),subset(survivalPlotReportingDF,age<50)$CBGTimeIQR,30,paste("T2DM. Age <50. IQR of intervals between CBG tests\nn=",nrow(subset(survivalPlotReportingDF,age<50))," individuals",sep=""),2)
    
        simpleSurvivalPlot(subset(survivalPlotReportingDF,age>60),subset(survivalPlotReportingDF,age>60)$CBGTimeIQR,30,paste("T1DM. Age >60. IQR of intervals between CBG tests\nn=",nrow(subset(survivalPlotReportingDF,age>60))," individuals",sep=""),2)
    
    
    
    simpleSurvivalPlot(survivalPlotReportingDF,survivalPlotReportingDF$HbA1cGradient,30,"T1DM HbA1cGradient",2)
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nHbA1cValuesInFrame>1),subset(survivalPlotReportingDF,nHbA1cValuesInFrame>1)$HbA1cTimeIQR,90,"T1DM HbA1c Time Interval IQR in 15 months")
      simpleSurvivalPlot(subset(survivalPlotReportingDF,nHbA1cValuesInFrame==3),subset(survivalPlotReportingDF,nHbA1cValuesInFrame==3)$HbA1cTimeIQR,90,"T1DM HbA1c Time Interval IQR in 15 months. nHBA1c in 15/12 == 3")
    simpleSurvivalPlot(subset(survivalPlotReportingDF,nHbA1cValuesInFrame>1),subset(survivalPlotReportingDF,nHbA1cValuesInFrame>1)$allHbA1cTimeIQR,90,"T1DM HbA1c all")
      simpleSurvivalPlot(subset(survivalPlotReportingDF,nHbA1cValuesInFrame==3),subset(survivalPlotReportingDF,nHbA1cValuesInFrame==3)$allHbA1cTimeIQR,90,"T1DM HbA1c all. n prior HbA1c == 3")
    
  dev.off()
  
  ## load in survival / drug data
  # OPfilename <- paste("../GlCoSy/source/admissionsWithDrugData_eGFR_91dayRunIn_redo.csv",sep="")
  # admissionsWithDrugData<-read.csv(OPfilename, header=TRUE , sep="," , row.names=NULL)
  # admissionsWithDrugData<-admissionsWithDrugData[,-1]
  
  
#####################################################################################################
## Plotting
  
plotfilename <- paste("../GlCoSy/plots/_abstractOutput_correlations_T1.pdf",sep="")
pdf(plotfilename, width=16, height=9)
  
      if (diabetesType=="Type 1 Diabetes. ") {hypoProbability_yLim=0.6}
      if (diabetesType=="Type 2 Diabetes. ") {hypoProbability_yLim=0.4}

      hypoThresh=4
        
      testParameterVSadmissionParameter(plotReportingDF$lastHbA1cInFrame,seq(2,300,5),"Last HbA1c")
      testParameterVSadmissionParameter(plotReportingDF$yyyy,seq(1,28,1),"first CBG")
      testParameterVSadmissionParameter(plotReportingDF$eAGyyyyDiff,10,"AGN")
      testParameterVSadmissionParameter(log(plotReportingDF$HbA1cGradient),100,"log(HbA1c-first CBG gradient)")
      
      plotHypoProbability(hypoThresh,plotReportingDF$eAG,100,plotReportingDF,"last hba1c as eAG",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,plotReportingDF$yyyy,100,plotReportingDF,"first CBG of admission",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,plotReportingDF$IQRHbA1cInFrame,100,plotReportingDF,"IQRHbA1cInFrame",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nHbA1cValuesInFrame>1)$HbA1cTimeIQR,100,subset(plotReportingDF,nHbA1cValuesInFrame>1),"HbA1cTimeIQR",0,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nHbA1cValuesInFrame>1)$allHbA1cTimeIQR,100,subset(plotReportingDF,nHbA1cValuesInFrame>1),"all prior hba1c values - HbA1cTimeIQR",0,hypoProbability_yLim)
    
      plotHypoProbability(hypoThresh,plotReportingDF$eAGyyyyDiff,100,plotReportingDF,"eAGyyyyDiff",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,plotReportingDF$AGN2,100,plotReportingDF,"AGN2",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nCBGperAdmission>=3)$AGN3,100,subset(plotReportingDF,nCBGperAdmission>=3),"AGN3",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nCBGperAdmission>=4)$AGN4,100,subset(plotReportingDF,nCBGperAdmission>=4),"AGN4",0.1,hypoProbability_yLim)
      
      plotHypoProbability(hypoThresh,plotReportingDF$IQRFirst2CBGs,100,plotReportingDF,"IQRFirst2CBGs",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nCBGperAdmission>=3)$IQRFirst3CBGs,100,subset(plotReportingDF,nCBGperAdmission>=3),"IQRFirst3CBGs",0.1,hypoProbability_yLim)
      plotHypoProbability(hypoThresh,subset(plotReportingDF,nCBGperAdmission>=4)$IQRFirst4CBGs,100,subset(plotReportingDF,nCBGperAdmission>=4),"IQRFirst4CBGs",0.1,hypoProbability_yLim)
      
dev.off()