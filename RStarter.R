library(e1071)
D <- read.csv("bikeDataTrainingUpload.csv")
sqtemp <- (D$temp)^2
sqatemp <- (D$atemp)^2
season1<-as.factor(D$season)
D$mnth<-as.factor(D$mnth)
D$weekday<-as.factor(D$weekday)
D$weathersit<-as.factor(D$weathersit)
D1<-data.frame(D,sqtemp,sqatemp,season1)
tune.mod<-tune(svm,cnt~season+yr+mnth+holiday+weekday+workingday+weathersit+temp+atemp+hum+windspeed+sqtemp+sqatemp+season1,data=D1,cost = 2^(11:18),gamma = 10^seq(-3,7,len=6))
model<-tune.mod$best.model
T <- read.csv("TestX.csv")
season1<-as.factor(T$season)
T$mnth<-as.factor(T$mnth)
T$weekday<-as.factor(T$weekday)
T$weathersit<-as.factor(T$weathersit)
sqtemp <- (T$temp)^2
sqatemp <- (T$atemp)^2
T1<-data.frame(T,sqtemp,sqatemp,season1)
res<-predict(model,T1)
id <- 0:(nrow(T1)-1)
result = data.frame(id,round(res,digits=0))
colnames(result) <- c("id", "cnt")
write.csv(result, file = "153050003_1.csv",row.names=FALSE)

