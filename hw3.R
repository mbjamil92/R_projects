stock_data=read.table('D:/Yeshiva/Summer 2021/datasets/stock.txt',header=T)
size.stock_data=dim(stock_data)
apple=stock_data$Apple
snp=stock_data$SP100
dell=stock_data$Dell
ford=stock_data$Ford
ibm=stock_data$IBM
intel=stock_data$Intel
ms=stock_data$MS
hp=stock_data$HP
rt1=apple[2:size.stock_data[1]]/apple[1:(size.stock_data[1]-1)]-1
rt2=snp[2:size.stock_data[1]]/snp[1:(size.stock_data[1]-1)]-1
rt3=dell[2:size.stock_data[1]]/dell[1:(size.stock_data[1]-1)]-1
rt4=ford[2:size.stock_data[1]]/ford[1:(size.stock_data[1]-1)]-1
rt5=ibm[2:size.stock_data[1]]/ibm[1:(size.stock_data[1]-1)]-1
rt6=intel[2:size.stock_data[1]]/intel[1:(size.stock_data[1]-1)]-1
rt7=ms[2:size.stock_data[1]]/ms[1:(size.stock_data[1]-1)]-1
rt8=hp[2:size.stock_data[1]]/hp[1:(size.stock_data[1]-1)]-1

# For apple:
fit=lm(rt7~rt2)
fit$coef
### priors
library(MASS)
Xmat=model.matrix(~rt2)
prior_mean=matrix(c(0,0),2,1)
prior_var=0.5*diag(1,2,2)
prior_a=5
prior_b=5
sigma2=0.1
n=5000
n.ave=2500
beta.gs=matrix(0,2,1)

### posterior draws
for(iter in 1:n){
  post_var= solve(solve(prior_var) + (1/sigma2)*t(Xmat)%*%Xmat)
  post_mean=post_var%*%((1/sigma2)*t(Xmat)%*%rt7 + solve(prior_var)%*%prior_mean)
  beta=mvrnorm(1,post_mean,post_var)
  post_a=15
  post_b=1/((1/5) + (1/2)*t(rt7-Xmat%*%beta)%*%(rt7-Xmat%*%beta))
  isigma2=rgamma(1,shape=post_a, scale=post_b)
  if(iter > (n-n.ave)){
    beta.gs=beta.gs+beta
  }
}
mcmc_beta=beta.gs/n.ave
