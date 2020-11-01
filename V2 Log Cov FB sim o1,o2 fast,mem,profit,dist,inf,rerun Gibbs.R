#setwd("/home-b/mandrei/Desktop/Research")

#err <- file("error.txt", open="wt")
#sink(err, type="message")

library(tseries)
library(chron)
library(MASS)
library(invgamma)
library(data.table)
library(bigmemory)

###########################
#Inputs
###########################

#tickers<-c("AAPL","AMZN","GOOG","MSFT")
tickers<-c("AAPL","FB","GOOG","MSFT")

n<-length(tickers)
lt<-length(tickers)
d<-n*(n+1)/2
#sigma1.2<-1
#sigma2.2<-2

##########################
#Get the returns
###########################

AAPL<-read.table("AAPL Dec 2017.csv",header=T,fill=T,sep=",")
#
#AMZN<-read.table("AMZN Dec 2017.csv",header=T,fill=T,sep=",")
FB<-read.table("FB Dec 2017.csv",header=T,fill=T,sep=",")
#
GOOG<-read.table("GOOG Dec 2017.csv",header=T,fill=T,sep=",")
MSFT<-read.table("MSFT Dec 2017.csv",header=T,fill=T,sep=",")


ld<-nrow(AAPL)
prices<-matrix(0,nrow=ld,ncol=n+1)

prices[,2]<-as.numeric(AAPL$Close)
#
#prices[,3]<-as.numeric(AMZN$Close)
prices[,3]<-as.numeric(FB$Close)
#
prices[,4]<-as.numeric(GOOG$Close)
prices[,5]<-as.numeric(MSFT$Close)

#
#colnames(prices)<-c("date","AAPL","AMZN","GOOG","MSFT")
colnames(prices)<-c("date","AAPL","FB","GOOG","MSFT")
#

r<-matrix(0,nrow=ld-1,ncol=n+1)
r[,-1]<-(prices[2:(ld),-1]-prices[1:(ld-1),-1])/prices[1:(ld-1),-1]

#library(lubridate)

#prices[,1]<-format(strptime(as.numeric(as.character(AAPL$Date)),"%m/%d/%Y"),"%d/%m/%Y")
prices[,1]<-format(as.Date(AAPL$Date,format="%Y-%m-%d"),"%Y-%m-%d")
r[,1]<-prices[-1,1]
#
#r<-data.frame("AAPL"=as.numeric(r[,2]),"AMZN"=as.numeric(r[,3]),"GOOG"=as.numeric(r[,4]),"MSFT"=as.numeric(r[,5]),"date"=r[,1])
r<-data.frame("AAPL"=as.numeric(r[,2]),"FB"=as.numeric(r[,3]),"GOOG"=as.numeric(r[,4]),"MSFT"=as.numeric(r[,5]),"date"=r[,1])
#
m<-nrow(r)

#############################
#Get data for the monnth of Jan 2018
############################

AAPL.test<-read.table("AAPL Jan 2018 only.csv",header=T,fill=T,sep=",")
#
#AMZN.test<-read.table("AMZN Jan 2018 only.csv",header=T,fill=T,sep=",")
FB.test<-read.table("FB Jan 2018 only.csv",header=T,fill=T,sep=",")
#
GOOG.test<-read.table("GOOG Jan 2018 only.csv",header=T,fill=T,sep=",")
MSFT.test<-read.table("MSFT Jan 2018 only.csv",header=T,fill=T,sep=",")


ld.test<-nrow(AAPL.test)
prices.test<-matrix(0,nrow=ld.test,ncol=n+1)

prices.test[,2]<-as.numeric(AAPL.test$Close)
#
#prices.test[,3]<-as.numeric(AMZN.test$Close)
prices.test[,3]<-as.numeric(FB.test$Close)
#
prices.test[,4]<-as.numeric(GOOG.test$Close)
prices.test[,5]<-as.numeric(MSFT.test$Close)

#
#colnames(prices.test)<-c("date","AAPL","AMZN","GOOG","MSFT")
colnames(prices.test)<-c("date","AAPL","FB","GOOG","MSFT")
#

prices.test[,1]<-format(as.Date(AAPL.test$Date,format="%m/%d/%Y"),"%m/%d/%Y")

########################################
#Personal Views(create q exactly like the future and create also 2*q, -q,-2q)
########################################

#P<-matrix(c(1,0,0,1,-1,0,0,-1),nrow=2,ncol=4)
#P<-matrix(c(1,0,-1,0,0,1,0,-1),nrow=2,ncol=4)
#P<-matrix(c(-1/3,0,1,0,-1/3,1,-1/3,-1),nrow=2,ncol=4)
#P<-diag(lt)

#q<-matrix(c(0.02,0.05),ncol=1)

prices.test.init<-prices.test[1,2:(n+1)]
prices.test.init<-as.numeric(prices.test.init)
names(prices.test.init)<-tickers
prices.test.fin<-prices.test[ld.test,2:(n+1)]
prices.test.fin<-as.numeric(prices.test.fin)
names(prices.test.fin)<-tickers
returns.test<-(prices.test.fin-prices.test.init)/prices.test.init

P<-matrix(c(-1,0,1,0,0,1,0,-1),nrow=2,ncol=lt)
colnames(P)<-tickers
P

#q<-P%*%returns.test
#q<--q
q<-matrix(c(0.02,0.05),ncol=1)


################################
#Functions
###############################

library(expm)

vec.star<-function(A)
{
  nr<-nrow(A)
  d<-nr*(nr+1)/2
  ans<-matrix(0,nrow=d)
  count<-1
  for (j in 0:(nr-1))
  {
    i<-1
    while (i+j<=nr)
    {
      ans[count]<-A[i,i+j]
      i<-i+1
      count<-count+1
    }
  }
  return(ans)
}

vec.star.inv<-function(v)
{
  d<-length(v)
  n<-(-1+sqrt(1+8*d))/2
  B<-matrix(0,nrow=n,ncol=n)
  a<-n:1
  count<-0
  start<-0
  while (start-1<=d && count<=n-1)
  {
    if (0<count && count<=n-1)
    {
      start<-start-1
    }
    for (i in 1:a[count+1])
    {
      if (count==0)
      {
        B[i,i+count]<-v[start+i]
      }
      if (0<count && count<n-1)
      {
        B[i,i+count]<-v[start+i]
      }
      if (count==n-1)
      {
        B[1,n]<-v[d]
      }
    }
    start<-start+a[count+1]+1
    count<-count+1
  }
  B<-B+t(B)
  diag(B)<-diag(B)/2
  return(B)
}

compute.J<-function()
{
  J<-matrix(0,nrow=d,ncol=2)
  J[1:n,1]<-rep(1,times=n)
  J[(n+1):d,2]<-rep(1,times=d-n) 
  return(J)
}

compute.delta<-function(sigma1.2,sigma2.2)
{
  delta<-matrix(cbind(rbind(sigma1.2*diag(n),matrix(0,nrow=d-n,ncol=n)),rbind(matrix(0,nrow=n,ncol=d-n),sigma2.2*diag(d-n))),nrow=d,ncol=d)
  return(delta)
}

compute.G<-function(delta,J)
{
  delta.inv<-solve(delta)
  G<-t(diag(d)-J%*%solve(t(J)%*%delta.inv%*%J)%*%t(J)%*%delta.inv)%*%delta.inv%*%(diag(d)-J%*%solve(t(J)%*%delta.inv%*%J)%*%t(J)%*%delta.inv)
  return(G)
}

compute.Q<-function(S)
{
  #S<-matrix(diag(1:n),nrow=n,ncol=n)
  eig<-eigen(S)
  csi<-matrix(0,nrow=n,ncol=n)
  f.temp<-array(0,dim=c(n,n,n,n))
  f<-array(0,dim=c(n,n,d))
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      csi[i,j]<-(eig$values[i]-eig$values[j])^2/(eig$values[i]*eig$values[j]*(log(eig$values[i])-log(eig$values[j]))^2)
      for (k in 1:n)
      {
        for (l in k:n)
        {
          f.temp[i,j,k,l]<-eig$vectors[k,i]*eig$vectors[l,j]+eig$vectors[l,i]*eig$vectors[k,j]
        }
        f.temp[i,j,k,k]<-f.temp[i,j,k,k]/2
      }
      f[i,j,]<-vec.star(f.temp[i,j,,])
    }
  }
  Q<-matrix(0,nrow=d,ncol=d)
  for (i in 1:n)
  {
    for (j in i:n)
    {
      if (j==i)
      {
        Q<-Q+(m/2)*f[i,i,]%*%t(f[i,i,])
      }
      if(j>i)
      {
        Q<-Q+m*csi[i,j]*f[i,j,]%*%t(f[i,j,])
      }
    }
  }
  return(Q)
}

compute.Q.memory<-function(S)
{
  n<-ncol(S)
  eig<-eigen(S)
  d<-n*(n+1)/2
  e<-eig$vectors
  csi<-matrix(0,nrow=n,ncol=n)
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      csi[i,j]<-(eig$values[i]-eig$values[j])^2/(eig$values[i]*eig$values[j]*(log(eig$values[i])-log(eig$values[j]))^2)
    }
  }
  v<-matrix(0,nrow=d,ncol=2)
  count.row<-1
  count.start<-0
  for (i in n:1)
  {
    v[count.row:(count.row+i-1),1]<-1:i
    v[count.row:(count.row+i-1),2]<-count.start+(1:i)
    count.row<-count.row+i
    count.start<-count.start+1
  }
  #Q<-big.matrix(nrow=d,ncol=d,init=0)
  Q<-matrix(0,nrow=d,ncol=d)
  #e<-eig$vectors
  for (k in 1:d)
  {
    for (l in 1:d)
    {
      for (i in 1:n)
      {
        for (j in 1:n)
        {
          if (j==i)
          {
            if (k<=n && l<=n)
            {
              Q[k,l]<-Q[k,l]+(m/2)*(e[v[k,1],i]*e[v[k,2],i])*(e[v[l,1],i]*e[v[l,2],i])
            }
            if (k<=n && l>n)
            {
              Q[k,l]<-Q[k,l]+(m/2)*(e[v[k,1],i]*e[v[k,2],i])*(e[v[l,1],i]*e[v[l,2],i]+e[v[l,2],i]*e[v[l,1],i])
            }
            if (k>n && l<=n)
            {
              Q[k,l]<-Q[k,l]+(m/2)*(e[v[k,1],i]*e[v[k,2],i]+e[v[k,2],i]*e[v[k,1],i])*(e[v[l,1],i]*e[v[l,2],i])
            }
            if (k>n && l>n)
            {
              Q[k,l]<-Q[k,l]+(m/2)*(e[v[k,1],i]*e[v[k,2],i]+e[v[k,2],i]*e[v[k,1],i])*(e[v[l,1],i]*e[v[l,2],i]+e[v[l,2],i]*e[v[l,1],i])
            }
            #if (k<=n && l>n)
            #if (l<=n && k>n)#  need to modify the formula below since first n entries have another form
            #Q[k,l]<-Q[k,l]+(m/2)*(e[v[k,1],i]*e[v[k,2],i]+e[v[k,2],i]*e[v[k,1],i])*(e[v[l,1],i]*e[v[l,2],i]+e[v[l,2],i]*e[v[l,1],i])
          }
          if(j>i)
          {
            if (k<=n && l<=n)
            {
              Q[k,l]<-Q[k,l]+m*csi[i,j]*(e[v[k,1],i]*e[v[k,2],j])*(e[v[l,1],i]*e[v[l,2],j])
            }
            if (k<=n && l>n)
            {
              Q[k,l]<-Q[k,l]+m*csi[i,j]*(e[v[k,1],i]*e[v[k,2],j])*(e[v[l,1],i]*e[v[l,2],j]+e[v[l,2],i]*e[v[l,1],j])
            }
            if (k>n && l<=n)
            {
              Q[k,l]<-Q[k,l]+m*csi[i,j]*(e[v[k,1],i]*e[v[k,2],j]+e[v[k,2],i]*e[v[k,1],j])*(e[v[l,1],i]*e[v[l,2],j])
            }
            if (k>n && l>n)
            {
              Q[k,l]<-Q[k,l]+m*csi[i,j]*(e[v[k,1],i]*e[v[k,2],j]+e[v[k,2],i]*e[v[k,1],j])*(e[v[l,1],i]*e[v[l,2],j]+e[v[l,2],i]*e[v[l,1],j])
            }
            #Q[k,l]<-Q[k,l]+m*csi[i,j]*(e[v[k,1],i]*e[v[k,2],j]+e[v[k,2],i]*e[v[k,1],j])*(e[v[l,1],i]*e[v[l,2],j]+e[v[l,2],i]*e[v[l,1],j])
          }
        }
      }
    }
  }
  return(Q)
}

pi.pdf<-function(alpha,sigma1.2,sigma2.2,S)
{
  delta<-compute.delta(sigma1.2,sigma2.2)
  G<-compute.G(delta,J)
  A<-vec.star.inv(alpha)
  ans<-exp((-m/2)*sum(diag(A+S%*%solve(expm(A))))-(1/2)*t(alpha)%*%G%*%alpha)
  return(ans)
}

pi.star.pdf<-function(alpha,sigma1.2,sigma2.2,S)
{
  delta<-compute.delta(sigma1.2,sigma2.2)
  G<-compute.G(delta,J)
  #Q<-compute.Q(S)
  Q<-compute.Q.memory(S)
  lambda<-vec.star(logm(S))
  alpha.star<-solve(Q+G)%*%Q%*%lambda
  ans<-exp((-1/2)*t(alpha-alpha.star)%*%(Q+G)%*%(alpha-alpha.star))
  return(ans)
}

############################
#Crate functions for the ratio pi/pi and pi.star/pi.star and not for the individual pdf's
#so that the computational error is reduced
###############################

#pi.ratio<-function()
#pi.star.ratio<-function()

pi.ratio<-function(alpha.dummy,alpha.t,sigma1.2,sigma2.2,S)
{
  delta<-compute.delta(sigma1.2 =  sigma1.2,sigma2.2 = sigma2.2)
  G<-compute.G(delta,J)
  A.dummy<-vec.star.inv(alpha.dummy)
  A.t<-vec.star.inv(alpha.t)
  ans<-exp(-(m/2)*sum(diag(A.dummy-A.t+S%*%(solve(expm(A.dummy))-solve(expm(A.t)))))
           -(1/2)*(t(alpha.dummy)%*%G%*%alpha.dummy-t(alpha.t)%*%G%*%alpha.t))
  return(ans)
}

pi.ratio.star<-function(alpha.dummy,alpha.t,sigma1.2,sigma2.2,S)
{
  delta<-compute.delta(sigma1.2,sigma2.2)
  G<-compute.G(delta,J)
  #Q<-compute.Q(S)
  Q<-compute.Q.memory(S)
  lambda<-vec.star(logm(S))
  alpha.star<-solve(Q+G)%*%Q%*%lambda
  ans<-exp(-(1/2)*t(alpha.t-alpha.star)%*%(Q+G)%*%(alpha.t-alpha.star)+
             (1/2)*t(alpha.dummy-alpha.star)%*%(Q+G)%*%(alpha.dummy-alpha.star))
  return(ans)
}

compute.S<-function(r,mu)
{
  l.r<-nrow(r)
  for (i in 1:l.r)
  {
    S<-S+(r[i,]-mu)%*%t(r[i,]-mu)
  }
  S<-S/m
  return(S)
}


##########################
#Gibbs Sampler
###########################

######################
#Initial values
######################

nc.r<-ncol(r)
r<-r[,-nc.r]
r<-as.matrix(r)
#r<-r*1000
######################
#Inflate for numerical error
#####################
# infl<-10^3
# r<-infl*r
# q<-infl*q

r.bar<-apply(r,2,mean)

######################
#Cycle thorugh combinations of omegas
#OBS: Can't go to 10^(-7) because rho1 or rho2 becomes inf and the other 0 (10^(-6) works)
######################

o1.seq<-seq(from=10^(-5),to=10^(-4),length.out = 4)
o2.seq<-seq(from=10^(-5),to=10^(-4),length.out = 4)

#o1.seq<-rep(10^(-4),times=3)
#o2.seq<-rep(10^(-4),times=3)

l.o1<-length(o1.seq)
l.o2<-length(o2.seq)

#results.vec<-matrix(0,nrow=l.o1*l.o2,ncol=4+n)
#colnames(norm.vec)<-c("o1","o2","profit","distance",tickers)

burn<-10^3
steps<-10^4
lambda.stock<-2.5
capital.init<-10^5

library(tictoc)
tic("Time to run Gibbs in parallel:")

writeLines(c(""), "log Cov log.txt")
library(foreach)
library(doParallel)
no_cores <- 2
cl<-makeCluster(no_cores)
registerDoParallel(cl)


#o1<-10^(-4)
#o2<-10^(-4)


results.vec<-foreach(o1=o1.seq,.combine="rbind")%:%
  foreach(o2=o2.seq,.combine="rbind",.packages=c("MASS","expm","invgamma"))%dopar%
#for(o1 in o1.seq){
  #for(o2 in o2.seq)
  {
    ############
    #Burn period 
    ############
    
    cat(paste("\n","Starting iteration:",o1," ",o2," ",Sys.time(),"\n"),file = "log Cov log.txt",append = T)
    
    flag.nan<-1
    count.nan<-0
    count.nan.bound<-2
    
    while(flag.nan==1 && count.nan<=count.nan.bound)
    {
      omega<-diag(c(o1,o2))
      omega.inv<-diag(c(1/o1,1/o2))
      #sigma1.2<-3
      #sigma2.2<-4
      sigma1.2<-var(vec.star(logm(cov(r)))[1:n])
      sigma2.2<-var(vec.star(logm(cov(r)))[(n+1):d])
      alpha.sim<-matrix(0,nrow=burn,ncol=d)
      sigma1.2.sim<-matrix(0,nrow=burn,ncol=1)
      sigma1.2.sim[1]<-sigma1.2
      sigma2.2.sim<-matrix(0,nrow=burn,ncol=1)
      sigma2.2.sim[1]<-sigma2.2
      mu.sim<-matrix(0,nrow=burn,ncol=n)
      #alpha.sim[1,]<-(1:d)
      #alpha.sim[1,1]<-10
      mu.sim[1,]<-apply(r,2,mean)
      
      sigma.cov<-diag(rep(1,times=n))
      S<-cov(r)
      #Q<-compute.Q(S)
      Q<-compute.Q.memory(S)
      lambda<-vec.star(logm(S))
      J<-compute.J()
      delta<-compute.delta(sigma1.2.sim[1],sigma2.2.sim[1])
      G<-compute.G(delta,J)
      alpha.sim[1,]<-mvrnorm(n=1,mu=solve(Q+G)%*%Q%*%lambda,Sigma = solve(Q+G))
      
      for (i in 1:(burn-1))
      {
        S.sim<-compute.S(r=r,mu=mu.sim[i,])
        #Q.sim<-compute.Q(S.sim)
        Q.sim<-compute.Q.memory(S.sim)
        lambda.sim<-vec.star(logm(S.sim))
        delta.sim<-compute.delta(sigma1.2.sim[i],sigma2.2.sim[i])
        G.sim<-compute.G(delta.sim,J)
        alpha.cov<-solve(Q.sim+G.sim)
        alpha.dummy<-mvrnorm(n=1,mu=alpha.cov%*%Q.sim%*%lambda.sim,Sigma = alpha.cov)
        rho1<-pi.ratio(alpha.dummy = alpha.dummy, alpha.t = alpha.sim[i,], sigma1.2 = sigma1.2.sim[i], sigma2.2 = sigma2.2.sim[i],S=S.sim)
        rho2<-pi.ratio.star(alpha.dummy = alpha.dummy, alpha.t = alpha.sim[i,], sigma1.2 = sigma1.2.sim[i], sigma2.2 = sigma2.2.sim[i],S=S.sim)
        rho<-rho1*rho2
        if (!is.nan(rho))
        {
          u<-runif(n=1,min=0,max=1)
          if (u<=min(c(rho,1)))
          {
            alpha.sim[i+1,]<-alpha.dummy
            sigma.cov<-expm(vec.star.inv(alpha.sim[i+1,]))
          }
          else
          {
            alpha.sim[i+1,]<-alpha.sim[i,]
          }
          alpha.sim.v<-alpha.sim[i+1,1:n]
          alpha.sim.v.mean<-mean(alpha.sim.v)
          alpha.sim.c<-alpha.sim[i+1,(n+1):d]
          alpha.sim.c.mean<-mean(alpha.sim.c)
          sigma1.2.sim[i+1]<-rinvgamma(n=1, shape = (n-3)/2, rate = (1/2)*sum((alpha.sim.v-alpha.sim.v.mean)^2))
          sigma2.2.sim[i+1]<-rinvgamma(n=1, shape = (d-n-3)/2, rate = (1/2)*sum((alpha.sim.c-alpha.sim.c.mean)^2))
          mu.cov<-solve(m*solve(sigma.cov)+t(P)%*%omega.inv%*%P)
          mu.sim[i+1,]<-mvrnorm(n=1, mu=mu.cov%*%(m*solve(sigma.cov)%*%r.bar+t(P)%*%omega.inv%*%q) , Sigma = mu.cov)
          
          flag.nan<-0
        }
        else
        {
          flag.nan<-1
          count.nan<-count.nan+1
          break
        }
      }
      
      ##########
      #Iteration period
      ##########
      
      mu.sim.gibbs<-matrix(0,nrow=steps,ncol=n)
      mu.sim.gibbs[1,]<-mu.sim[burn,]
      sigma1.2.sim.gibbs<-matrix(0,nrow=steps,ncol=1)
      sigma2.2.sim.gibbs<-matrix(0,nrow=steps,ncol=1)
      sigma1.2.sim.gibbs[1]<-sigma1.2.sim[burn]
      sigma2.2.sim.gibbs[1]<-sigma2.2.sim[burn]
      alpha.sim.gibbs<-matrix(0,nrow=steps,ncol=d)
      alpha.sim.gibbs[1,]<-alpha.sim[burn,]
      sigma.cov.gibbs<-sigma.cov
      sigma.cov.post.mean<-sigma.cov.gibbs
      
      #rho<-matrix(0,nrow=steps-1,ncol=1)
      
      for (i in 1:(steps-1))
      {
        S.sim.gibbs<-compute.S(r=r,mu=mu.sim.gibbs[i,])
        #Q.sim.gibbs<-compute.Q(S.sim.gibbs)
        Q.sim.gibbs<-compute.Q.memory(S.sim.gibbs)
        lambda.sim.gibbs<-vec.star(logm(S.sim.gibbs))
        delta.sim.gibbs<-compute.delta(sigma1.2.sim.gibbs[i],sigma2.2.sim.gibbs[i])
        G.sim.gibbs<-compute.G(delta.sim.gibbs,J)
        alpha.cov.gibbs<-solve(Q.sim.gibbs+G.sim.gibbs)
        alpha.dummy.gibbs<-mvrnorm(n=1,mu=alpha.cov.gibbs%*%Q.sim.gibbs%*%lambda.sim.gibbs,Sigma = alpha.cov.gibbs)
        rho1<-pi.ratio(alpha.dummy = alpha.dummy.gibbs, alpha.t = alpha.sim.gibbs[i,], sigma1.2 = sigma1.2.sim.gibbs[i], sigma2.2 = sigma2.2.sim.gibbs[i],S=S.sim.gibbs)
        rho2<-pi.ratio.star(alpha.dummy = alpha.dummy.gibbs, alpha.t = alpha.sim.gibbs[i,], sigma1.2 = sigma1.2.sim.gibbs[i], sigma2.2 = sigma2.2.sim.gibbs[i],S=S.sim.gibbs)
        rho<-rho1*rho2
        #rho[i]<-rho1*rho2
        
        if(!is.nan(rho))
        {
          u<-runif(n=1,min=0,max=1)
          #if (u<=min(c(rho[i],1)))
          if (u<=min(c(rho,1)))
          {
            alpha.sim.gibbs[i+1,]<-alpha.dummy.gibbs
            sigma.cov.gibbs<-expm(vec.star.inv(alpha.sim.gibbs[i+1,]))
          }
          else
          {
            alpha.sim.gibbs[i+1,]<-alpha.sim.gibbs[i,]
          }
          sigma.cov.post.mean<-sigma.cov.post.mean+sigma.cov.gibbs
          alpha.sim.v.gibbs<-alpha.sim.gibbs[i+1,1:n]
          alpha.sim.v.mean.gibbs<-mean(alpha.sim.v.gibbs)
          alpha.sim.c.gibbs<-alpha.sim.gibbs[i+1,(n+1):d]
          alpha.sim.c.mean.gibbs<-mean(alpha.sim.c.gibbs)
          sigma1.2.sim.gibbs[i+1]<-rinvgamma(n=1, shape = (n-3)/2, rate = (1/2)*sum((alpha.sim.v.gibbs-alpha.sim.v.mean.gibbs)^2))
          sigma2.2.sim.gibbs[i+1]<-rinvgamma(n=1, shape = (d-n-3)/2, rate = (1/2)*sum((alpha.sim.c.gibbs-alpha.sim.c.mean.gibbs)^2))
          mu.cov.gibbs<-solve(m*solve(sigma.cov.gibbs)+t(P)%*%omega.inv%*%P)
          mu.sim.gibbs[i+1,]<-mvrnorm(n=1, mu=mu.cov.gibbs%*%(m*solve(sigma.cov.gibbs)%*%r.bar+t(P)%*%omega.inv%*%q) , Sigma = mu.cov.gibbs)
          
          flag.nan<-0
        }
        else
        {
          flag.nan<-1
          count.nan<-count.nan+1
          break
        }
      }
      
    }
    if (count.nan<=count.nan.bound && flag.nan==0)
    {
      mu.post.mean<-apply(mu.sim.gibbs,2,mean)
      sigma.cov.post.mean<-sigma.cov.post.mean/steps
      w<-(1/lambda.stock)*solve(sigma.cov.post.mean)%*%mu.post.mean
      w<-w/abs(sum(w))
      position<-floor(w*capital.init/prices.test.init)
      capital.stock.init<-position*prices.test.init
      capital.stock.fin<-position*prices.test.fin
      
      c(o1,o2,sum(capital.stock.fin-capital.stock.init),norm(P%*%mu.post.mean-q,type="2"),position,w)
    }
    else
    {
      cat(paste("\n","Too many Gibbs reruns because of NaNs in Metropolis Hastings ratio","\n"),file = "log Cov log.txt",append = T)
    }
    
    
  }
#}
stopCluster(cl)
#sink()
toc()

# #norm.vec[count,3]<-norm(P%*%mu.post.mean-q,type="2")

colnames(results.vec)<-c("o1","o2","profit","distance",paste0(tickers," shares"),paste0(tickers," weight"))

#write.table(results.vec,"/home-b/mandrei/Desktop/Results/Log Cov sim o1,o2 FB profit&distance q 2,5 10^4.csv",sep=",",row.names = FALSE)

write.table(results.vec,"Log Cov sim o1,o2 FB profit&distance q 2,5 10^4 10^-6 to 10^-5 2nd.csv",sep=",",row.names = FALSE)

#sink(type="message")
#close(err)

##############################
#Plots
##############################


#plot(x=1:steps,y=cumsum(mu.sim.gibbs[,1])/1:steps)
#plot(x=1:steps,y=cumsum(mu.sim.gibbs[,2])/1:steps)
#plot(x=1:steps,y=cumsum(mu.sim.gibbs[,3])/1:steps)
#plot(x=1:steps,y=cumsum(mu.sim.gibbs[,4])/1:steps)


# data<-norm.vec
# data<-as.data.frame(norm.vec)
# library(akima)
# im<-with(data,interp(o1,o2,distance))
# library(rgl)
# with(data,plot3d(o1,o2,distance,col="red"))
# surface3d(im$x,im$y,im$z,col="grey")
