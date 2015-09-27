#Default values for our simulation

lambda <- 0.2
n <- 1000
m <- 40

sim <- NULL
mns <- NULL
sds <- NULL
theo_mean <- NULL
theo_sd <- NULL
sample_mean <- NULL
sample_sd <- NULL

# Generated data

for (i in 1 : m) sim = rbind(sim, rexp(n,lambda))

# Theorical values

theo_mean <- 1/lambda
theo_sd <- 1/lambda

#First we calculate the mean distribution

for (i in 1 : m) mns = c(mns,mean(sim[i,]))

# Now we calculate the standard deviations

for (i in 1 : m) sds = c(sds,sd(sim[i,]))

# Now calculate the estimated population values.

sample_mean <- mean(mns)
sample_sd <- mean(sds)

# Now we plot the histograms on a combined panel.

par(mfrow = c(1,2))

h1<- hist(mns, breaks = 10, xlab = "Sample means distribution", main = "Mean", col=rainbow(10), las=1)
xfit<-seq(min(mns),max(mns),length=40) 
yfit<-dnorm(xfit,mean=mean(mns),sd=sd(mns)) 
yfit <- yfit*diff(h1$mids[1:2])*length(mns) 
lines(xfit, yfit, col="black", lwd=2)

h2<- hist(sds, breaks = 10, xlab = "Sample standard deviation distribution", main = "Standard Deviation", col=rainbow(10), las=1)
xfit<-seq(min(sds),max(sds),length=40) 
yfit<-dnorm(xfit,mean=mean(sds),sd=sd(sds)) 
yfit <- yfit*diff(h2$mids[1:2])*length(sds) 
lines(xfit, yfit, col="black", lwd=2)


