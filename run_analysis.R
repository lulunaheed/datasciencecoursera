run_analysis <- function(){
 
    setwd("~/Desktop")
    loc<-"./Coursera/UCI HAR Dataset"
    setwd(loc)
 
  ##Gather General Info
    
    colNames<-read.table("features.txt",header=FALSE) #Column names
    activityLabels<-read.table("activity_labels.txt",header=FALSE) #Activity Key
    
  ##Gather test data 
   
    loctest <-"./test"
    setwd(loctest)
    
    subjectTest<-read.table("subject_test.txt",header=FALSE) #Test subject identifiers
    tasks<-read.table("y_test.txt",header=FALSE) #Activity values
    typeTest<-rep("Test",dim(tasks)[1])
    taskTest<-vector()
    
    for (i in 1:length(tasks[[1]])){
      currentVal<-tasks[[1]][i]
      taskTest[i]<-as.character(activityLabels[[2]][currentVal])
    }
    
    subjectDataTest<-read.table("X_test.txt",header=FALSE) #Test data values
    
    newcolumnNames1<-t(colNames[2]) #Pick up column names and transpose for binding
    
    meanLocs<-grep("[Mm]ean",newcolumnNames1)
    stdLocs<-grep("[Ss][Tt][Dd]",newcolumnNames1)
    allLocs<-c(meanLocs,stdLocs)
    
    newdat2<-cbind(subjectTest,typeTest,taskTest,subjectDataTest[,allLocs])
  
  ##Gather train data
    
    loctrain <-"./train"
    setwd('..')
    setwd(loctrain)
    
    subjectTrain<-read.table("subject_train.txt",header=FALSE) #Train subject identifiers
    taskTrain<-read.table("y_train.txt",header=FALSE) #Activity values
    typeTrain<-rep("Train",dim(taskTrain)[1])
    
    t<-vector()
    
    for (i in 1:length(taskTrain[[1]])){
      currentVal<-taskTrain[[1]][i]
      t[i]<-as.character(activityLabels[[2]][currentVal])
    }
    
    subjectDataTrain<-read.table("X_train.txt",header=FALSE) #Train data values
      
    n3<-cbind(subjectTrain,typeTrain,t,subjectDataTrain[allLocs])
    newNames<-t(newcolumnNames1[allLocs])
    
    names(newdat2)<-newNames
    names(n3)<-newNames
    finaldat<-rbind(newdat2,n3)
    
    for (h in 1:length(newNames)){
      curName<-newNames[h]
      firstletter<-substring(names(finaldat)[h],1,1)
      remainder<-substring(names(finaldat)[h],2)
       if (firstletter=="t"){
        newNames[h]<-paste("time",remainder)
       }else if (firstletter=="f"){
        newNames[h]<-paste("FFTfreq",remainder)
       }else {
        
       }
    }
  onlySome<-newNames
   newNames<-cbind("Subject.Number","Type","Activity",newNames)
   names(finaldat)<-newNames
    
##Create Dataset with average of each variable for each activity and each subject
    
    #Split Dataset By Activity
    activities<-finaldat[,3]
    subjects<-finaldat[,1]
    subjectActivitySplit<-split(finaldat,paste(finaldat[,3],finaldat[,1]))
    meanSubjectActivity<-as.data.frame(sapply(subjectActivitySplit,function(x) colMeans(x[,onlySome])))
  
}