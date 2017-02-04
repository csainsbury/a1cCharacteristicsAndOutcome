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

reportingDF$hypo<-ifelse(reportingDF$hypoEpisodes4>0,1,0)

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
numberOfDivisions<-100
quantileVals<-quantile(plotReportingDF$eAGyyyyDiff,prob = seq(0, 1, length = (numberOfDivisions+1)), type = 5)
propPlot<-as.data.frame(matrix(0,nrow=numberOfDivisions,ncol=5));colnames(propPlot)<-c("xAxisVals","diff","nInSet","nInSetWithHypo","prop")
propPlot$xAxisVals<-quantileVals[1:length(quantileVals)-1]

# for (h in seq(-20,20,1)) {
for (h in seq(1,nrow(propPlot),1)) {
  if (h<nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff>=propPlot$xAxisVals[h] & eAGyyyyDiff<propPlot$xAxisVals[h+1] & yyyy>=4] }
  if (h==nrow(propPlot)) {hypoTestSet<-plotReportingDF[eAGyyyyDiff>=propPlot$xAxisVals[h] & eAGyyyyDiff<max(eAGyyyyDiff) & yyyy>=4] }
  
  nInSet<-nrow(hypoTestSet)
  nInSetWithHypo<-nrow(hypoTestSet[hypoEpisodes4>0])
  
  proportionWithHypo<-nInSetWithHypo/nInSet
  
  propPlot$nInSet[h]<-nInSet
  propPlot$prop[h]<-proportionWithHypo
  
  
  # print(proportionWithHypo)
}

plot(propPlot$xAxisVals,propPlot$prop,ylim=c(0.2,0.6),pch=16,cex=2,
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
  nInSetWithHypo<-nrow(hypoTestSet[hypoEpisodes4>0])
  
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
IQRHbA1c_plotReportingDF<-plotReportingDF[nHbA1cInFrame>2]
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
  nInSetWithHypo<-nrow(hypoTestSet[hypoEpisodes4>0])
  
  proportionWithHypo<-nInSetWithHypo/nInSet
  
  propPlot$nInSet[h]<-nInSet
  
  propPlot$prop[h]<-proportionWithHypo
  
  
  # print(proportionWithHypo)
}

plot(propPlot$xAxisVals,propPlot$prop,ylim=c(0.1,0.25),pch=16,cex=2,
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
