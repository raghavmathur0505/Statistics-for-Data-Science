install.packages("boot")
library(boot)
cpu <- scan(file="C:/Users/mastr/OneDrive/Documents/UTD NOTES/UTD-STATISTICS/mini project 6/cputime.txt")

mean.non.par=function(x,indices){
  result= log(mean(x[indices]))
  return(result)
}
#bootstrap mean, bias and sd error
mean.non.par.boot=boot(cpu,mean.non.par,R=1000,
                       sim="ordinary", stype = "i")

#
#    ORDINARY NONPARAMETRIC BOOTSTRAP
#    
#    Call:
#    boot(data = cpu, statistic = mean.non.par, R = 1000, sim = "ordinary", 
#        stype = "i")
#    Bootstrap Statistics :
#        original       bias    std. error
#    t1*  3.87605 -0.004040205  0.09648449
#     

#using boot method
bootstrap.mean=mean.non.par.boot$t0
bootstrap.mean
bootstrap.sd.error=sd(mean.non.par.boot$t)
bootstrap.sd.error

##Manual Calculation 
#(theta)
sample.est = log(mean(cpu))
sample.est
#(theta.hat)
x = replicate(b, log(mean(sample(cpu,length(cpu), replace=TRUE))))

#Bias Function
estimate.bias.resample <- function(x , sample.est){
  return (mean(x)- sample.est)
}

#Variance Function
variance.resample <- function(x){
  sum.val = sum((x-mean(x))*(x-mean(x)))
  return((sum.val)/(b-1))
}

#Standard Error
std.Error = sqrt(variance.resample(x))
std.Error
#Bias
resample.bias = estimate.bias.resample(x,sample.est)
resample.bias
#2.5th percentile of Theta.hat
quantile(x,0.025)

#97.5th percentile of Theta.hat
quantile(x,0.975)

#2.5th percentile of Theta.hat-theta
quantile(x-sample.est,0.025)

#97.5th percentile of Theta.hat-theta
quantile(x-sample.est,0.975)


#bootstrap CI
boot.ci(mean.non.par.boot)

#no of resamples
b=1000
alpha = 1 - .95
sorted.x = sort(x)
#a.alphath.quantile.of.resample = sorted.x[(b+1)*.90]
#normal approximation '''resample.bia's'''
normal.approx.ci<-function(x,sample.est,alpha,resample.bias){
  c(sample.est - resample.bias  - qnorm(1-alpha/2)*std.Error,sample.est - resample.bias - qnorm(alpha/2)*std.Error)
}

critical.point <- function(data,b,alpha){
  return((data[(b+1)*alpha]))
}

basic.bootstrap.ci <- function(data,b,alpha, sample.est){
  c(2*sample.est - critical.point(data,b,(1-alpha/2)) ,2*sample.est - critical.point(data,b,(alpha/2)))
}

percentile.bootstrap.ci <- function(sorted.x,b,alpha){
  c(critical.point(sorted.x,b,(alpha/2)), critical.point(sorted.x,b,(1-alpha/2)))
}

normal.approx.ci(x,sample.est,alpha,resample.bias)
basic.bootstrap.ci(sorted.x,b,alpha,sample.est)
percentile.bootstrap.ci(sorted.x,b,alpha)

#sort(mean.non.par.boot$t)[c(25,975)]
