# Section 1

# part a

singer <- read_csv("C:/Users/mastr/Desktop/singer.txt")
bass=subset(singer, singer$voice.part =="Bass" )
alto=subset(singer, singer$voice.part =="Alto" )
tenor=subset(singer, singer$voice.part =="Tenor" )
Soprano=subset(singer, singer$voice.part =="Soprano" )
summary(singer)
summary(bass)
summary(tenor)
##Boxplots
par(mfrow=c(1,2))
boxplot(bass[,1],range=1.5, main = "Boxplot of Bass ",xlab = "Bass")
boxplot(tenor[,1],range=1.5,main = "Boxplot of Tenor ",xlab = "Tenor")
boxplot(alto[,1],range=1.5,main = "Boxplot of Alto ",xlab = "Alto")
boxplot(Soprano[,1],range=1.5,main = "Boxplot of Soprano ",xlab = "Soprano")
##Histograms
par(mfrow=c(1,2))
hist(bass$height,freq=FALSE,xlab="Bass", ylab=" Relative Heights", main="Density Histogram- Bass")
hist(tenor$height, freq=FALSE,xlab="Tenor", ylab="Relative Heights", main="Density Histogram- Tenor")
hist(alto$height, freq=FALSE,xlab="Alto", ylab="Relative Heights", main="Density Histogram- Alto")
hist(Soprano$height, freq=FALSE,xlab="Soprano", ylab="Relative Heights", main="Density Histogram- Soprano")
##QQ Plots
par(mfrow=c(1,2))
qqnorm(bass$height,main = "Normal Q-Q plot for Bass", xlab = "Normal Theoretical Quantiles", ylab = "Bass Quantiles")
qqline(bass$height)
qqnorm(tenor$height,main = "Normal Q-Q plot for Tenor", xlab = "Normal Theoretical Quantiles", ylab = "Tenor Quantiles")
qqline(tenor$height)
qqnorm(alto$height,main = "Normal Q-Q plot for Alto", xlab = "Normal Theoretical Quantiles", ylab = "Alto Quantiles")
qqline(alto$height)
qqnorm(Soprano$height,main = "Normal Q-Q plot for Soprano", xlab = "Normal Theoretical Quantiles", ylab = "Soprano Quantiles")
qqline(Soprano$height)

#part b

x.bar=mean(bass$height)
y.bar=mean(tenor$height)
alpha=0.05
t.test(bass$height, tenor$height, alternative = "greater", conf.level = (1-alpha))
#Performs one and two sample t-tests on vectors of data
#reject H0 since pval < alplha, we reject H0
CI=(x.bar-y.bar) +c(-1,1)*qnorm(1-alpha/2)*denom

# Section 2

mu.zero=10
x.bar=9.02
x.sigma=2.22
n.x=20
se=x.sigma/sqrt(n.x)
#test statistic
t.stat=(x.bar-mu.zero)/se
# random generation for the t distribution with df degrees of freedom
pvalue=1-pt(t.stat, df=n.x-1)
#Monte Carlo Simulation
randvalues = replicate (10000, rt(1,df=n.x-1))
#MC.pvalue
mean(randvalues>=t.stat)
#critical.point=0.05
# since p-value and pvalue1 both are greater than critical point, we accept H0