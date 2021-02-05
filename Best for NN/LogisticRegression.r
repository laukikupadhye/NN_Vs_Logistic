##Ionosphere data
##351 oberservation and 34 variable
tmp<-read.table("ionosphere.data",sep=",");
n<-nrow(tmp); p<-ncol(tmp);

t=100
runs=c(1:t)
Accuracy=c(1:t)*0

x<-matrix(0,n,p);
for(j in 1:p) { x[,j]<-tmp[,j]; }
for(j in 1:t){
##Split the data in testing 25% and training 75%
idx2<-sample(1:n,n/4, replace=FALSE);
xts<-x[idx2,];
xtr<-x[-idx2,];

mylogit<-glm(as.factor(xtr[,p]) ~ xtr[,1:(p-1)], family=binomial);
b<-mylogit$coefficients;
b[is.na(b)]<-mean(b,na.rm = TRUE)

logits<-matrix(0,nrow(xts),1);
for (i in 1:nrow(xts))
{
  logits[i]<-b[1]+sum(xts[i,1:(p-1)]*b[2:p]);
}
logits<-exp(logits)/(1+exp(logits));
classDF <- data.frame(response = xts[,p], predicted = round(logits,0))
z<-xtabs(~ predicted + response, data = classDF)
Accuracy[j]<-sum(diag(z))/sum(z);
}
cat("The max accuracy on the test set is", max(Accuracy[i]),"\n")
cat("Mean accuracy: ",mean(Accuracy));