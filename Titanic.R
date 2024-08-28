#train data
data=read.csv("C:/Users/HP/Downloads/train.csv")
dim(data)
str(data)
colnames(data)
ndata=data[-c(1,4,9,11)]
attach(ndata)
age.mean=mean(Age,na.rm=TRUE)
ndata$Age=replace(Age,is.na(Age)==1,age.mean)
ndata$Sex=as.factor(data$Sex)
ndata$Embarked=as.factor(Embarked)
ndata$Pclass=as.factor(Pclass)
index=which(ndata$Embarked=="")
ndata=ndata[-index,]
library("ggplot2")
#Survived
data=data.frame("Category"=c("0","1"),"values"=c(sum(ndata$Survived=="0"),sum(ndata$Survived=="1")))
ggplot(data,aes(x=Category,y=values,fill=Category))+geom_bar(stat="identity")+geom_text(aes(label=values))+scale_fill_manual(values=c("orange","blue"))+scale_x_discrete(limits =c("0","1"))+labs(title="Survived")
#Pclass
data=data.frame("Category"=c("1","2","3"),"values"=c(sum(ndata$Pclass=="1"),sum(ndata$Pclass=="2"),sum(ndata$Pclass=="3")))
ggplot(data,aes(x=Category,y=values,fill=Category))+geom_bar(stat="identity")+geom_text(aes(label=values))+scale_fill_manual(values=c("green","#993300","#666666"))+scale_x_discrete(limits =c("1","2","3"))+labs(title="Pclass")
#Sex
data1=data.frame("cat1"=c("female","male"),"val1"=c(sum(ndata$Sex=="female"),sum(ndata$Sex=="male")))
slices1=c(sum(ndata$Sex=="female"),sum(ndata$Sex=="male"))
frac1=(slices1/sum(slices1))
ymax1=cumsum(frac1)
ymin1=c(0,head(ymax1,n=-1))
labposi1=(ymax1+ymin1)/2
labls1=paste0(c("female","male"),"\n value:",paste(round(frac1*100)),"%",sep="")
ggplot(data1,aes(ymax=ymax1,ymin=ymin1,xmax=4,xmin=3,fill=cat1))+geom_rect()+geom_label(x=3.5,aes(y=labposi1,label=labls1),size=3)+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position = "none")+labs(title=" Sex")+scale_fill_manual(values=c("yellow","purple"))
#Age

ggplot(data=ndata,aes(x=Age))+geom_histogram(aes(y=..density..),bins=24,col="black",fill="#00FF99")+labs(title="Age")+geom_density()
#Fare
ggplot(data=ndata,aes(x=Fare))+geom_histogram(aes(y=..density..),bins=24,col="black",fill="#66CCFF")+labs(title="Fare")+geom_density()
#Parch
ggplot(data=ndata,aes(x=Parch))+geom_histogram(aes(y=..density..),bins=24,col="black",fill="#FFFF99")+labs(title="Parch")+geom_density()

#Embarked
data=data.frame("Category"=c("C","Q","S"),"values"=c(sum(ndata$Embarked=="C"),sum(ndata$Embarked=="Q"),sum(ndata$Embarked=="S")))
ggplot(data,aes(x=Category,y=values,fill=Category))+geom_bar(stat="identity")+geom_text(aes(label=values))+scale_fill_manual(values=c("green","#993300","yellow"))+scale_x_discrete(limits =c("C","Q","S"))+labs(title=" Embarked",x="",y="Count")

# logistic regression
model=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,family="binomial",data=ndata)
summary(model)
model2=step(model,direction="backward")
#hnp plot
library("hnp")
hnp(model2)
plot(residuals(model2,"pearson"),main="Residuals Plot",xlab="Fitted Values",ylab="Pearson Residuals")
library("caret")
library("ROCR")
pred=predict(model2,type="response",newdata=ndata)
pred1=prediction(pred,ndata$Survived)
pref=performance(pred1,"tpr","fpr")
plot(pref,print.cutoffs.at=seq(0,1,0.1),colorize=TRUE) 
abline(a=0,b=1)
auc=performance(pred1,"auc")@y.values[[1]]
legend(0.6,0.4,auc,title="AUC",cex=0.9)

#test data

dat=read.csv("C:\\Users\\HP\\Downloads\\test.csv")
ndat=dat[-c(1,4,9,11)]
age.mean=mean(ndat$Age,na.rm=TRUE)
ndat$Age=replace(ndat$Age,is.na(ndat$Age)==1,age.mean)
ndat$Sex=as.factor(dat$Sex)
ndat$Embarked=as.factor(dat$Embarked)
ndat$Pclass=as.factor(dat$Pclass)

predi=predict(model2,ndat)
val=ifelse((predi)>0.5,1,0)
r=data.frame("PassengerId"=dat[,1],"Survived"=val)
paged_table(r)


