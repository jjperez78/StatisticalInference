# Loading data

data(ToothGrowth)
mns <- NULL
sds <- NULL
data <- NULL

# How it looks like.
# First we convert supp into factors to count them.

ToothGrowth$supp <- as.factor(ToothGrowth$supp)
supp <- table(ToothGrowth$supp)
dose <- table(ToothGrowth$dose)

# Now we plot the histograms on a combined panel.

par(mfrow = c(1,2))

h1<- hist(ToothGrowth$len, breaks = 10, xlab = "", main = "Length", col=rainbow(10), las=1)
h2<- hist(ToothGrowth$dose, breaks = 10, xlab = "", main = "Dose", col=rainbow(10), las=1)

# We check if there are some missing values on the given data.

complete <- ifelse((sum(complete.cases(ToothGrowth))==length(ToothGrowth$len)), "No missing data", "There is missing data")

if(complete == "There is missing data")
{
  ToothGrowth <- ToothGrowth[complete.cases(ToothGrowth),]
}

#ToothGrowth$dose <- as.factor(ToothGrowth$dose)


# We start calculating the expected values.

dat1 <- ToothGrowth[ ToothGrowth[,2]=="OJ" & ToothGrowth[,3]==0.5 , ]
dat2 <- ToothGrowth[ ToothGrowth[,2]=="OJ" & ToothGrowth[,3]==1 , ]
dat3 <- ToothGrowth[ ToothGrowth[,2]=="OJ" & ToothGrowth[,3]==2 , ]
dat4 <- ToothGrowth[ ToothGrowth[,2]=="VC" & ToothGrowth[,3]==0.5 , ]
dat5 <- ToothGrowth[ ToothGrowth[,2]=="VC" & ToothGrowth[,3]==1 , ]
dat6 <- ToothGrowth[ ToothGrowth[,2]=="VC" & ToothGrowth[,3]==2 , ]

par(mfrow = c(2,3))

h1<- hist(x = dat1[,1], main = "OJ / 0.5", xlab ="", col=rainbow(5))
h2<- hist(x = dat2[,1], main = "OJ / 1", xlab ="", col=rainbow(5), las=1)
h3<- hist(x = dat3[,1], main = "OJ / 2", xlab ="", col=rainbow(5), las=1)
h4<- hist(x = dat4[,1], main = "VC / 0.5", xlab ="", col=rainbow(5), las=1)
h5<- hist(x = dat5[,1], main = "VC / 1", xlab ="", col=rainbow(5), las=1)
h6<- hist(x = dat6[,1], main = "VC / 2", xlab ="", col=rainbow(5), las=1)

data <- matrix(NA, 6, 5)
colnames(data) <- c("supp","dose","mean","sd","n")

data[1,]<-c("OJ","0.5", round(mean(dat1[,1]),2), round(sd(dat1[,1]),2), length(dat1[,1]))
data[2,]<-c("OJ","1",round(mean(dat2[,1]),2), round(sd(dat2[,1]),2), length(dat2[,1]))
data[3,]<-c("OJ","2",round(mean(dat3[,1]),2), round(sd(dat3[,1]),2), length(dat3[,1]))
data[4,]<-c("VC","0.5",round(mean(dat4[,1]),2), round(sd(dat4[,1]),2), length(dat4[,1]))
data[5,]<-c("VC","1",round(mean(dat5[,1]),2), round(sd(dat5[,1]),2), length(dat5[,1]))
data[6,]<-c("VC","2",round(mean(dat6[,1]),2), round(sd(dat6[,1]),2), length(dat6[,1]))


# Last step will be calculate the confidence intervals for each cathegory


tconf <- matrix(NA, 6, 5)
colnames(tconf) <- c("supp","dose","LL","mean","UL")

tconf[1,]<-c("OJ","0.5", round(t.test(dat1[,1])$conf[1],2), round(mean(dat1[,1]),2), round(t.test(dat1[,1])$conf[2],2) ) 
tconf[2,]<-c("OJ","1",round(t.test(dat2[,1])$conf[1],2), round(mean(dat2[,1]),2), round(t.test(dat2[,1])$conf[2],2) ) 
tconf[3,]<-c("OJ","2",round(t.test(dat3[,1])$conf[1],2), round(mean(dat3[,1]),2), round(t.test(dat3[,1])$conf[2],2) ) 
tconf[4,]<-c("VC","0.5",round(t.test(dat4[,1])$conf[1],2), round(mean(dat4[,1]),2), round(t.test(dat4[,1])$conf[2],2) ) 
tconf[5,]<-c("VC","1",round(t.test(dat5[,1])$conf[1],2), round(mean(dat5[,1]),2), round(t.test(dat5[,1])$conf[2],2) ) 
tconf[6,]<-c("VC","2",round(t.test(dat6[,1])$conf[1],2), round(mean(dat6[,1]),2), round(t.test(dat6[,1])$conf[2],2) ) 
