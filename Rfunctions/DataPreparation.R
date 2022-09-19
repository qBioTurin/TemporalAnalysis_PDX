library(knitr)
library(plyr)
library(ggplot2)
library(connector)
library(readr)
library(dplyr)

FromDateToInt<-function(x){
  return( cumsum(c(0,diff(x))) )
}
data.generation<-function(data,min.point,type){
  
  length.curves=table(data$longen)
  id.delete<-names(length.curves[length.curves < min.point])
  
  #AnnotationFileALL<-AnnotationFileALL[!AnnotationFileALL$ID %in% id.delete,]
  #GrowDataFile<-GrowDataFile[!GrowDataFile$ID %in% id.delete,]
  data<-data[!data$longen %in% id.delete,]
  
  IDlong<-unique(data$longen)
  ID <-1 : length(IDlong)
  names(ID) <- IDlong
  
  GrowDataFile<- data.frame(ID = ID[data$longen],
                            Vol = data$volume,
                            Time = data$measure_date)
  NewTime <- aggregate(GrowDataFile$Time,
                       by = list(GrowDataFile$ID),
                       FUN = "FromDateToInt")
  
  GrowDataFile$Time <- unlist(NewTime$x)
  
  return(list(GrowDataFile,ID,data))
}
days.before <- 3

cetuxiCurves <- read_delim("./Data/cetuxi_long_header.tsv", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
cetuxiCurves$longen <- paste0(cetuxiCurves$longen,"cetuxi")
cetuxiCurves$sgen<-substring(cetuxiCurves$longen,1,7)

NewData<-data.generation(cetuxiCurves,4)

GrowthCurve<-NewData[[1]]
ID<-NewData[[2]]
data<-NewData[[3]]

TimeExpGroup = aggregate(data$measure_date,            
                         by=list(data$longen,
                                 data$sgen,
                                 data$exp_group),
                         FUN ="min")

AnnotationFileALL <-  data.frame(ID = ID[TimeExpGroup$Group.1],
                                 LongID = TimeExpGroup$Group.1,
                                 ShortID = TimeExpGroup$Group.2,
                                 ExpGroup = TimeExpGroup$Group.3)



###### check if there are double points
data$ID <- ID[data$longen]

data.list<-lapply(unique(data$ID), function(i){
  sub.data <- filter(data, ID == i)
  index0 <- which(diff(sub.data$measure_date)==0)
  if(length(index0) == 0) return(sub.data)
  else return(sub.data[-( index0 + 1 ),])
})
data<-do.call("rbind",data.list)
timediff<-aggregate(data$measure_date,by=list(data$ID),FUN = "diff")
length(which(unlist(timediff) <= 0))

GrowthCurve.list<-lapply(unique(GrowthCurve$ID), function(i){
  sub.data <- filter(GrowthCurve, ID == i)
  index0 <- which(diff(sub.data$Time)==0)
  if(length(index0) == 0) return(sub.data)
  else return(sub.data[-( index0 + 1 ),])
})
GrowDataFile<-do.call("rbind",GrowthCurve.list)
AnnotationFileALL <-AnnotationFileALL[AnnotationFileALL$ID %in% unique(GrowDataFile$ID),]
####################

colnames(data)<-c("LongID","ExpGroup","start_date","end_date","measure_date","volume","ShortID","ID")

DataAllInfo<-merge(AnnotationFileALL,
                   data[,-c(which(colnames(data)%in%c("ShortID","ExpGroup","ID")))],
                   by=c("LongID"))
DataAllInfo <- DataAllInfo[with(DataAllInfo, order(ID, measure_date)),]

timediff<-aggregate(DataAllInfo$measure_date,by=list(DataAllInfo$ID),FUN = "diff")
length(which(unlist(timediff) <= 0))

DataAllInfo$Time <- GrowDataFile$Time

CONNECTORList = DataFrameImport(GrowDataFile,
                                AnnotationFileALL)

#### before start date
DataAllInfo.pre <- DataAllInfo[as.Date(DataAllInfo$start_date) >= as.Date(DataAllInfo$measure_date) ,]
LongId.pre.todelete<-names(table(DataAllInfo.pre$LongID)[table(DataAllInfo.pre$LongID)<3])

GrowDataFile.pre <- DataAllInfo.pre[!DataAllInfo.pre$LongID %in% LongId.pre.todelete, c("ID","volume","Time")]
AnnotationFile.pre<-AnnotationFileALL[AnnotationFileALL$ID %in% GrowDataFile.pre$ID,]
TimeMax<-aggregate(GrowDataFile.pre$Time,by=list(ID=GrowDataFile.pre$ID),FUN="max")
GrowDataFile.pre.tmp<-merge(GrowDataFile.pre,TimeMax,by="ID")
GrowDataFile.pre.tmp$Time <- GrowDataFile.pre.tmp$Time - GrowDataFile.pre.tmp$x 
GrowDataFile.pre<-GrowDataFile.pre.tmp[,-which(colnames(GrowDataFile.pre.tmp)=="x")]

### after start date
DataAllInfo.post <- DataAllInfo[as.Date(DataAllInfo$measure_date) >= (as.Date(DataAllInfo$start_date) -days.before) ,]
LongId.post.todelete<-names(table(DataAllInfo.post$LongID)[table(DataAllInfo.post$LongID)<3])
GrowDataFile.post <- DataAllInfo.post[!DataAllInfo.post$LongID %in% LongId.post.todelete, c("ID","volume","Time")]
AnnotationFile.post<-AnnotationFileALL[AnnotationFileALL$ID %in% GrowDataFile.post$ID,]

# set the differetn starting tme to zero for each model
TruncTimeMin<-aggregate(GrowDataFile.post$Time,by=list(ID=GrowDataFile.post$ID),FUN="min")
GrowDataFile.post.tmp<-merge(GrowDataFile.post,TruncTimeMin,by="ID")
GrowDataFile.post.tmp$Time <- GrowDataFile.post.tmp$Time - GrowDataFile.post.tmp$x 
GrowDataFile.post<-GrowDataFile.post.tmp[,-which(colnames(GrowDataFile.post.tmp)=="x")]

### after start date before end date
DataAllInfo.post.tillEndDate <- DataAllInfo[as.Date(DataAllInfo$measure_date) >= (as.Date(DataAllInfo$start_date) -days.before)
                                            & as.Date(DataAllInfo$measure_date) <= as.Date(DataAllInfo$end_date) ,]
LongId.post.todelete<-names(table(DataAllInfo.post.tillEndDate$LongID)[table(DataAllInfo.post.tillEndDate$LongID)<3])
GrowDataFile.post.tillEndDate <- DataAllInfo.post.tillEndDate[!DataAllInfo.post.tillEndDate$LongID %in% LongId.post.todelete, c("ID","volume","Time")]
AnnotationFile.post.tillEndDate<-AnnotationFileALL[AnnotationFileALL$ID %in% GrowDataFile.post.tillEndDate$ID,]

# set the differetn starting tme to zero for each model
TruncTimeMin.tillEndDate<-aggregate(GrowDataFile.post.tillEndDate$Time,by=list(ID=GrowDataFile.post.tillEndDate$ID),FUN="min")
GrowDataFile.post.tmp<-merge(GrowDataFile.post.tillEndDate,TruncTimeMin.tillEndDate,by="ID")
GrowDataFile.post.tmp$Time <- GrowDataFile.post.tmp$Time - GrowDataFile.post.tmp$x 
GrowDataFile.post.tillEndDate<-GrowDataFile.post.tmp[,-which(colnames(GrowDataFile.post.tmp)=="x")]


## Removing all the curves from the CRC0456 model, leaving only the starting exp group.

CRCtoDelete = which(AnnotationFileALL$ExpGroup != "CRC0456LMX0A.20121010.Cetuximab Standard.0" & AnnotationFileALL$ShortID == "CRC0456")
IDrm <- unique(AnnotationFileALL$ID[CRCtoDelete])

GrowDataFile = GrowDataFile[-which(GrowDataFile$ID %in% IDrm),]
AnnotationFileALL = AnnotationFileALL[-which(AnnotationFileALL$ID %in% IDrm),]

# The Pre curves
CRCtoDelete = which(AnnotationFile.pre$ExpGroup != "CRC0456LMX0A.20121010.Cetuximab Standard.0" & AnnotationFile.pre$ShortID == "CRC0456")
IDrm <- unique(AnnotationFile.pre$ID[CRCtoDelete])

GrowDataFile.pre = GrowDataFile.pre[-which(GrowDataFile.pre$ID %in% IDrm),]
AnnotationFile.pre = AnnotationFile.pre[-which(AnnotationFile.pre$ID %in% IDrm),]

# The Post curves
CRCtoDelete = which(AnnotationFile.post.tillEndDate$ExpGroup != "CRC0456LMX0A.20121010.Cetuximab Standard.0" & AnnotationFile.post.tillEndDate$ShortID == "CRC0456")
IDrm <- unique(AnnotationFile.post.tillEndDate$ID[CRCtoDelete])

GrowDataFile.post.tillEndDate = GrowDataFile.post.tillEndDate[-which(GrowDataFile.post.tillEndDate$ID %in% IDrm),]
AnnotationFile.post.tillEndDate = AnnotationFile.post.tillEndDate[-which(AnnotationFile.post.tillEndDate$ID %in% IDrm),]

## the connector analysis are run just considering the 3 weeks after the starting point of the treatments.
pdx3w.ID = unlist(GrowDataFile.post.tillEndDate %>% filter(Time>= 18 ) %>% select(ID) %>% distinct() )
GrowDataFile.post.tillEndDate = GrowDataFile.post.tillEndDate %>% filter(Time<=23 & (ID %in% pdx3w.ID) ) %>%
  group_by(ID) %>%
  filter(length(Time) > 2)

AnnotationFile.post.tillEndDate = AnnotationFile.post.tillEndDate[which(AnnotationFile.post.tillEndDate$ID %in% GrowDataFile.post.tillEndDate$ID),]

AnnotationFile.pre = AnnotationFile.pre[AnnotationFile.pre$LongID %in% AnnotationFile.post.tillEndDate$LongID,]
GrowDataFile.pre = GrowDataFile.pre[GrowDataFile.pre$ID %in% AnnotationFile.pre$ID,]


save(AnnotationFile.post.tillEndDate,
     GrowDataFile.post.tillEndDate,
     GrowDataFile,
     AnnotationFileALL,
     file = "./Data/TimeSeries.RData")


