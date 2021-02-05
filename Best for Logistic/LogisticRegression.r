##Logistic regression example
##The Spam data
tmp<-read.table("Diabetes_dataset.data",sep=",");
n<-nrow(tmp); p<-ncol(tmp);
#test<-as.factor(tmp[,p]);
#View(as.factor(test));

x<-matrix(0,n,p);
for(j in 1:p) { x[,j]<-tmp[,j]; }


##Split the data according to HTF for comparison
#T<-n;
idx2<-sample(1:n,n/4, replace=FALSE);
xts<-x[idx2,];
xtr<-x[-idx2,];

#View(x[-idx2,])

mylogit<-glm(as.factor(xtr[,p]) ~ xtr[,1:(p-1)], family=binomial);
b<-mylogit$coefficients;

logits<-matrix(0,nrow(xts),1);
for (i in 1:nrow(xts))
{
  logits[i]<-b[1]+sum(xts[i,1:(p-1)]*b[2:p]);
}
logits<-exp(logits)/(1+exp(logits));

classDF <- data.frame(response = xts[,p], predicted = round(logits,0))
z<-xtabs(~ predicted + response, data = classDF)
acc<-sum(diag(z))/sum(z);
cat("The accuracy on the test set is", acc,"\n")
