setwd("C:\\Users\\it24100363\\Desktop\\IT24100363")
getwd()

#1
Delivery_Times<-read.table("Exercise - Lab 05.txt",header=TRUE,sep=",")
attach(Delivery_Times)

#2
histogram<-hist(Delivery_Times$Delivery,breaks=seq(20,70,length=10),right = TRUE,main="Histogram Of Delivery Times", xlab="Delivery Times", ylab="Frequency")

#3- symmetrical histogram



#4
breaks<-round(histogram$breaks) #break point
breaks

freq<-histogram$counts  #freq for class
freq

#mid values for class
mids<-histogram$mids
mids

#Creating vector for freq distribution
classes<-c()

#using for loop to break point
for(i in 1:length(breaks)-1){
  classes[i]<-paste0("[",breaks[i],",",breaks[i+1],"]")
}

cbind(classes=classes,frequency=freq)


#Portray the distribution in the form of a frequency polygon
lines(mids,freq)

#new plot
plot(mids,freq,type="l", main="frequency polygon for deliver",xlab="deliver times",ylim=c(0,max(freq)))

#Portray the distribution in a cumulative frequency polygon
cum.freq<-cumsum(freq)
cum.freq

new <-c() # create null variable

for(i in 1:length(breaks)){
  if(i==1){
    new[i]=0
  }else{
    new[i]=cum.freq[i-1]
  }
}

par(mar=c(4,4,2,1))

plot(breaks,new,type="l",main="cumulative frequency polygon for deliver times",xlab="deliver times",ylab="cumulative frequency",ylim=c(0,max(freq)))

cbind(upper=breaks,cumfreq=new)








