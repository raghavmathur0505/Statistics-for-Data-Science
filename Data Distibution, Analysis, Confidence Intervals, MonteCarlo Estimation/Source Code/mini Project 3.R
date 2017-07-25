install.packages("reshape2")
asetwd("C:/Users/mastr/OneDrive/Documents/UTD NOTES/UTD-STATISTICS/mini project 3")
data= read.table("bp.txt", header = TRUE)
data=data.frame(data)
arm_method= (data[,1])
finger_method= (data[,2])
summary(arm_method)
summary(finger_method)
par(mfrow=c(1,2)) 
boxplot(arm_method, col=(c("gold","darkgreen")),range=1.5, main = "Boxplot of Arm-Method Bp ",xlab = "Arm Method")
boxplot(finger_method, col=(c("pink","darkgreen")),range=1.5,main = "Boxplot of Finger-Method Bp ",xlab = "Finger Method")
var(arm_method)
sd(arm_method)
# frequency histogram by default

par(mfrow=c(1,2))
hist(arm_method, col=(c("gold","darkgreen")),xlab="Arm_Method BP ", ylab="Frequency", main="Frequency Histogram- Arm BP") 
hist(finger_method, col=(c("gold","darkgreen")),xlab="Finger_Method BP", ylab="Frequency", main="Frequency Histogram- Finger BP") 


par(mfrow=c(1,2))
hist(arm_method, col=(c("gold","darkgreen")),freq=FALSE,xlab="Arm_Method BP ", ylab="Relative Frequency", main="Density Histogram- Arm BP") 
lines(density(arm_method), col = "red", lwd = 2)
hist(finger_method, col=(c("gold","darkgreen")), freq=FALSE,xlab="Finger_Method BP", ylab="Relative Frequency", main="Density Histogram- Finger BP") 
lines(density(finger_method), col = "red", lwd = 2)


# relative frequency (density) histogram

hist(arm_method, freq=FALSE, xlab="Arm_Method BP", ylab="Frequency", main="Relative Frequency Histogram of Arm BP") 



# checking for outlier

iqr1 <- IQR(arm_method)
lower_arm <- quantile(arm_method, prob=0.25) - 1.5*iqr1
upper_arm <- quantile(arm_method, prob=0.75) + 1.5*iqr1
c(lower_arm, upper_arm)


iqr2 <- IQR(finger_method)
lower_finger <- quantile(finger_method, prob=0.25) - 1.5*iqr2
upper_finger <- quantile(finger_method, prob=0.75) + 1.5*iqr2
c(lower_finger, upper_finger)


par(mfrow=c(1,2))
qqnorm(arm_method,main = "Normal Q-Q plot for Arm_Method", xlab = "Normal Theoretical Quantiles", ylab = "Arm_Method Quantiles")
qqline(arm_method)
qqnorm(finger_method,main = "Normal Q-Q plot for finger_Method", xlab = "Normal Theoretical Quantiles", ylab = "Finder_Method Quantiles")
qqline(finger_method)

data.diff=data$armsys-data$fingsys
mean(arm_method)
mean(finger_method)
dbar=mean(data.diff)
qqnorm
qqline(data.diff)
x=sd(data.diff)*(1/(length(data.diff)*(1/2)))
CI= dbar +c(-1,1)*(qnorm(1-0.025)*x)
CI


x <- rnorm(200)
hist(x, col = "blue", freq = FALSE)
lines(density(x), col = "red", lwd = 2)



n <- 10
p= 0.05
alpha <- 0.05

phat=mean(rbinom(n,p,size=1))



ConInt=phat + c(-1,1)*qnorm(1-(alpha/2))*(phat*(1-phat)/n)^(1/2)
> ciforp
#>[1] 0.02159645 0.89507021

#for diff values of n and p-
x= rep(rbinom(n,p=0.05,size=1),1000)
#> mean(x)



#xmean + c(-1, 1)*qnorm(1-(alpha/2)) * (xsd/sqrt(n))


###################################

#estimate phat for the given n and p
no_of_times = 500
n=1000
p=0.5
count = 0

a=replicate(no_of_times,rbinom(n,1,p))

i=1
repeat{
  #print(i)
  if(i>no_of_times){
    break
  }
  
  #estimate phta for the given n and p
  pages = 1:n
  phat= length(pages[a[,i]==1])/n
  #print(phat[i])
  
  #find CI for the estimated phat
  ci_upper = phat + (qnorm(0.975) * sqrt((phat*(1-phat))/n))
  ci_lower = phat - (qnorm(0.975) * sqrt((phat*(1-phat))/n))
  
  print ("ci_upper and ci_lower")
  print (ci_lower)
  print(ci_upper)
  
  
  #find the coverage probability
  if(ci_lower <= p && p <= ci_upper ){
    count=count+1
    
  }else{
    #print("False")
  }
  
  
  # print("\n")  
  i=i+1
}
print (count)
coverage = (count/no_of_times)*100
print(coverage)



