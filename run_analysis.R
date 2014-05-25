setwd("/home//ines.huertas/Escritorio/ADA/data_scientist/curso3//project/week/UCI HAR Dataset/")

#The working directory will be UCI HAR Dataset


features <- read.table( "features.txt", header=FALSE, stringsAsFactors=FALSE)
activities <- read.table("activity_labels.txt", header=FALSE, stringsAsFactors=FALSE)
datos<-c("test","train", header=FALSE)
datatest<-NULL
datatrain<-NULL
for(data in datos){
  #Read id user for test and data and store in id_user variable
  id_string<-paste(data,"subject_",sep="/")
  id_string<-paste(id_string,".txt",sep=data)
  str(id_string)
  id_user<-read.table(id_string)
  
  #Read sets of boths train and test  
  set_string<-paste(data,"X_",sep="/")
  set_string<-paste(set_string,".txt",sep=data)
  str(set_string)
  sets<-read.table(set_string, header=FALSE)
  
  #Read labels of boths train and test
  set_string<-paste(data,"y_",sep="/")
  set_string<-paste(set_string,".txt",sep=data)
  str(set_string)
  labels<-read.table(set_string, header=FALSE)
  
  col<-c("idUser","activity",features$V2)
  
  if(data=="test"){
    datatest<-data.frame(id_user,labels,sets)
    colnames(datatest)<-col
    
  }else{
    mytrain<-data.frame(id_user,labels,sets)
    colnames(mytrain)<-col
  }
  
}

#Merge datasets
data<-rbind(mytrain,datatest)


#Rename de activity by name
activity<-factor(data$activity, labels = activities$V2)
data$activity<-activity 

#Select mean an standar desviation variable from dataset

col<-c("idUser","activity",features$V2[grep("mean\\(|std\\(", features$V2)])
newvalues<-features$V2[grep("mean\\(|std\\(", features$V2)]
newvalues<-append(newvalues,"idUser",0)
newvalues<-append(newvalues,"activity",1)
selectmeasure<-data[,colnames(data)%in%newvalues] 

#Rename names more intuitive
columnVar<-colnames(selectmeasure)

vectorOriginal<-c("\\(\\)-","tBody","tGravity","fBody","fGravity","\\-mean\\(\\)\\-","\\-std\\(\\)\\-","\\-mean\\(\\)","\\-std\\(\\)")
vectorChange<-c("","timeBody", "timeGravity","BodyFft","GravityFft","Mean","Std", "Mean","Std")
control<-1
res<-columnVar
while(control<=length(vectorChange)){
  res<-gsub(vectorOriginal[control],vectorChange[control],res)
  control<-control+1
}
colnames(selectmeasure)<-res


#Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subject<-unique(selectmeasure$idUser)
resultado<-NULL
for(subj in subject){
  user<-selectmeasure[selectmeasure$idUser==subj,]
  act<-unique(user$activity)
  for(acti in act){
    midata<-user[user$activity==acti,]
    mimedia<-midata[-1]
    mimedia<-mimedia[-1]
    media<-colMeans(mimedia)
    media<-append(media,subj,0)
    media<-append(media,acti,1)
    resultado<-rbind(resultado,media)
  }
  
}
selectmeasure<-append(selectmeasure,"C",0)
columnVar<-colnames(selectmeasure)
colnames(resultado)<-columnVar
resu<-as.data.frame(resultado)
colnames(resu)<-columnVar

#write the datatidy
write.csv(resu,"datatidy.csv")



