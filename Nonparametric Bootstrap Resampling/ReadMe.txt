Dataset: CPU time data (available in the file cputime.txt)

Technology: R

About the Project:

Performed inference on the natural logarithm of the population mean,
i.e., the parameter of interest is 𝜃 = log (E(X)). It is estimated by the natural logarithm of the sample mean, say, 𝜃.hat. 

Used nonparametric bootstrap with 1,000 resamples to estimate the following:
	• bias and standard error of 𝜃.hat
	• 2.5th and 97.5th percentiles of the sampling distribution of 𝜃.hat
	• 2.5th and 97.5th percentiles of the sampling distribution of 𝜃.hat - 𝜃
	• 95% confidence interval for 𝜃 using three bootstrap methods — normal approximation, basic bootstrap, and percentile bootstrap

Observation:

1) We observed that bootstrap with CI normal approximation is helpful compared to z- critical point which may not be accurate as n (30) is not very large. 
2) Bootstrap Bias values is very small signifying a good CI. Also the values obtained using boot() method and manual created functions are comparable and the slight difference in values arise due to resampling of data.