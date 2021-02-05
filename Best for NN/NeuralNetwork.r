##Uncomment if use this package for the first time
##install.packages("nnet");
library(nnet);

##The spam data
x<-read.table("ionosphere.data",sep=",");
n<-nrow(x);
p<-ncol(x);
x[,p]<-as.factor(x[,p]);

colnames(x)[p]<-"class";

##Split the data according to HTF for comparison
#T<-3065;
set.seed(122)
idx2<-sample(1:n,n/4, replace=FALSE);
spam_test<-x[idx2,1:(p-1)];
spam_train<-x[-idx2,1:(p-1)];
labels<-class.ind(x[,p]);

x=c(1:10)
Accuracy=c(1:10)*0

test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres);
}

for(i in 1:10){
nnmodel<-nnet(spam_train, labels[-idx2,],
             size=i, rang=0.1,
             decay=5e-4, maxit=200);

conf<-test.cl(labels[idx2,], predict(nnmodel, spam_test));
acc<-sum(diag(conf))/sum(conf);
Accuracy[i]=acc;
}
#cat("The accuracy on the test set is", acc,"\n")
plot(x,Accuracy)
