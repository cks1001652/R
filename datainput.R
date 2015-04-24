#input and rough analyze 
getwd()
setwd("/Users/cheeseloveicecream/Documents/missouri/STAT4999")
rawdata<-data.frame(read.csv("data.csv",sep=",",quote=""))
rawdata <- rawdata[,-1:-4]
rawdata<-rawdata[,-6:-9]
attach(rawdata)
rawdata$home<-0
rawdata_num1<-as.matrix(sapply(rawdata[,2],as.numeric))
rawdata_num2<-data.matrix(rawdata[,4:6])
rawdata_cha1<-rawdata[,1]
rawdata_cha2<-rawdata[,3]
rawdata1<-data.frame(rawdata_cha1,rawdata_num1,rawdata_cha2,rawdata_num2)


i<-1
for (i in 1:256){
				if(rawdata1[i,2]==1){
									rawdata1[i,6]<-1
									}
				else{
					rawdata1[i,6]<-0
					}
}
#extract crutial data
data<-matrix(0,256,2)
i<-1
for(i in 1:256){
				if(rawdata1[i,6]==1){
									data[i,1]<-1
									}
				else{
					data[i,2]<-1
					}
				}
x<- matrix(0,256,33)


	for(i in 1:256){
		if( rawdata1[i,6]==1){
			x[i,rawdata1[i,1]]=1
			x[i,rawdata1[i,3]]=-1
			}
		else{
			x[i,rawdata1[i,1]]=-1
			x[i,rawdata1[i,3]]=1
			}
					}
					x[,1]=1
#levels(rawdata1[,1])