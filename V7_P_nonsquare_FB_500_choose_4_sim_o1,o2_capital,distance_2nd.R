#Profit sensitivity to confidence levels
setwd("/home-b/mandrei/Desktop/Research")

err <- file("FB error 2nd.txt", open="wt")
sink(err, type="message")


#Sum of matrices function

sum.matrix<-function(r,pi,m)
{
  s<-matrix(0,nrow=length(pi),ncol=length(pi))
  r<-data.matrix(as.data.frame(r))
  pi<-data.matrix(as.data.frame(pi))
  for (i in 1:m)
  {
    s<-s+t(r[i,]-pi)%*%(r[i,]-pi)
  }
  return(s)
}

library(MCMCpack)
library(data.table)

##########################
#Get the returns
###########################


r<-read.table("S&P 500 Returns taking out Dec 2017.csv",header=T,fill=T,sep=",")
r<-r[,-1]
#r<-r*1000
tickers<-colnames(r)
lt<-length(tickers)
n<-length(tickers)

ld<-nrow(r)

#############################
#Get data for the month of Jan 2018 (drop stocks that do not have data for the month of Jan 2018)
############################


prices.test<-read.table("Prices Jan 2018.csv",header=T,fill=T,sep=",")
ld.test<-nrow(prices.test)
# returns.test<-matrix(0,nrow=1,ncol=lt)
# colnames(returns.test)<-tickers
# returns.test<-as.data.frame(returns.test)
drop<-c()

for (i in 1:lt)
{
  if(is.na(sum(prices.test[,i])))
  {
    drop<-c(drop,i)
  }
  if(all(prices.test[,i]==rep(0,times=ld.test)))
  {
    drop<-c(drop,i)
  }
}

r<-r[,-drop]
tickers<-colnames(r)
lt<-length(tickers)
n<-length(tickers)
prices.test<-prices.test[,-drop]
returns.test<-(prices.test[ld.test,]-prices.test[1,])/prices.test[1,]


# AAPL.test<-read.table("AAPL Jan 2018 only.csv",header=T,fill=T,sep=",")
# AMZN.test<-read.table("AMZN Jan 2018 only.csv",header=T,fill=T,sep=",")
# GOOG.test<-read.table("GOOG Jan 2018 only.csv",header=T,fill=T,sep=",")
# MSFT.test<-read.table("MSFT Jan 2018 only.csv",header=T,fill=T,sep=",")

# prices.test[,2]<-as.numeric(AAPL.test$Close)
# prices.test[,3]<-as.numeric(AMZN.test$Close)
# prices.test[,4]<-as.numeric(GOOG.test$Close)
# prices.test[,5]<-as.numeric(MSFT.test$Close)
#colnames(prices.test)<-c("date","AAPL","AMZN","GOOG","MSFT")
#prices.test[,1]<-format(strptime(as.character(AAPL.test$Date),"%m/%d/%Y"),"%d/%m/%Y")

# returns.test$AAPL<-(as.numeric(AAPL.test$Close[ld.test])-as.numeric(AAPL.test$Close[1]))/as.numeric(AAPL.test$Close[1])
# returns.test$AMZN<-(as.numeric(AMZN.test$Close[ld.test])-as.numeric(AMZN.test$Close[1]))/as.numeric(AMZN.test$Close[1])
# returns.test$GOOG<-(as.numeric(GOOG.test$Close[ld.test])-as.numeric(GOOG.test$Close[1]))/as.numeric(GOOG.test$Close[1])
# returns.test$MSFT<-(as.numeric(MSFT.test$Close[ld.test])-as.numeric(MSFT.test$Close[1]))/as.numeric(MSFT.test$Close[1])


#############################
#Personal view: exactly like reality or far from reality
#############################
#sectors <- read.csv("C:/School USB/2nd year grad/Summer 2016/Research/codes/S&P 500 stocks by Sectors.csv")
#colnames(sectors)<-c("symbol","sector")
#nr.s<-nrow(sectors)

#Fin the IT and Energy companies

#tickers.select.it<-sectors$symbol[which(sectors$sector=="Information Technology")]
#tickers.select.e<-sectors$symbol[which(sectors$sector=="Energy")]

#tickers.select<-c("AAPL","AMZN","GOOG","MSFT")
tickers.select<-c("AAPL","FB","GOOG","MSFT")

#Find the column index where the returns are.

tickers.select<-tickers.select[tickers.select %in% tickers  ]
lt.select<-length(tickers.select)
tickers.select.index<-matrix(0,nrow=lt.select,ncol=1)
for(i in 1:lt.select)
{
  tickers.select.index[i]<-which(tickers==tickers.select[i])
}


#Fill the matrix of views according to the columns where the selected tickers are.

P<-matrix(0,nrow=2,ncol=lt)
colnames(P)<-tickers
#tickers.select.b<-c("AAPL","GOOG")
#tickers.select.w<-c("AMZN","MSFT")

P[1,tickers.select.index[1]]<--1
P[1,tickers.select.index[2]]<-1
P[2,tickers.select.index[3]]<-1
P[2,tickers.select.index[4]]<--1

#q<-P%*%as.numeric(returns.test)
#q<--q
q<-matrix(c(0.02,0.05),ncol=1)



#################################
#Estimate parameters
#################################

m<-21
r.h<-r[1:(ld-m-1),]
r.c<-r[(ld-m):nrow(r),]


# min.y<-min(r.h$year)
# max.y<-max(r.h$year)
# count<-1
# mu<-matrix(0,nrow=(max.y-min.y+1)*12,ncol=4)
# 
# for (year in min.y:max.y)
# {
#   for (month in 1:12)
#   {
#     x<-r.h[r.h$month==month & r.h$year==year,]
#     x<-x[,1:4]
#     if (nrow(x)>=1)
#     {
#       mu[count,]<-apply(x,2,mean)
#       count<-count+1
#     }
#   }
# }
# 
# row.sub<-apply(mu, 1, function(row) any(row !=0 ))
# mu<-mu[row.sub,]
# colnames(mu)<-tickers<-c("AAPL","AMZN","GOOG","MSFT")

#library(foreach)
#Pmu.h<-foreach(i=1:nrow(mu),.combine = "rbind") %dopar% 
#{
#  t(P%*%mu[i,])
#}
#omega<-cov(Pmu.h)
#omega.star[1:nrow(P),1:nrow(P)]<-omega

#q2<-apply(mu.star,2,mean)[(nrow(P)+1):ncol(P)]
#q.star<-c(q,q2)

#names(q.star)<-tickers

sigma0<-cov(r.h[,1:lt])

r.c<-r.c[,1:lt]

#omega.inv<-solve(omega)
r.bar<-apply(r.h[,1:lt],2,mean)

#############################################
#Gibbs smapler
##############################################
library(MCMCpack)
library(MASS)

o1.seq<-seq(from=10^(-6),to=10^(-5),length.out = 7)
o2.seq<-seq(from=10^(-6),to=10^(-5),length.out = 7)

#o1.seq<-rep(10^(-6),times=4)
#o2.seq<-rep(10^(-6),times=3)
l.o1<-length(o1.seq)
l.o2<-length(o2.seq)

#norm.vec<-matrix(0,nrow=l.o1*l.o2,ncol=3)
#colnames(norm.vec)<-c("o1","o2","norm")
#profit<-matrix(0,nrow=l.o1*l.o2,ncol=2)
#w<-matrix(0,nrow=l.o1*l.o2,ncol=lt+1)

# pb = txtProgressBar(min = 0, max = l.o1*l.o2, initial = 0)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress = progress)

#count<-1
lambda<-2.5
capital.init<-10^5
burn<-10^2
steps<-10^3
nu<-nrow(r.h)
#nu<-lt+2
mu.init<-rep(0,times=length(r.c))

library(tictoc)
tic("Time to run Gibbs in parallel:")

writeLines(c(""), "FB log.txt")
library(foreach)
library(doParallel)
no_cores <- 49
cl<-makeCluster(no_cores)
registerDoParallel(cl)
#registerDoSNOW(makeCluster(no_cores))
profit.vec<-foreach(o1=o1.seq,.combine = "rbind")%:%
  foreach(o2=o2.seq,.combine="rbind",.packages = "MCMCpack")%dopar%
  {
    #sink("log.txt", append=TRUE)
    #cat(paste("Starting iteration:",o1," ",o2,"\n"))
    cat(paste("\n","Starting iteration:",o1,"     ", o2 ,"  ",Sys.time(),"\n"),file = "FB log.txt",append = T)
    omega<-diag(c(o1,o2))
    omega.inv<-diag(c(1/o1,1/o2))
    sigma.special<-(sigma0+sum.matrix(r.c,t(mu.init),m))
    sigma.sim<-riwish(nu+m,sigma.special)
    mu.sim<-matrix(0,nrow=burn,ncol=lt)
    sigma.sim.inv<-solve(sigma.sim)
    sigma.post<-solve(m*sigma.sim.inv+t(P)%*%omega.inv%*%P)
    mu.post<-sigma.post%*%(m*sigma.sim.inv%*%r.bar+t(P)%*%omega.inv%*%q)
    mu.sim[1,]<-mvrnorm(n=1,mu=mu.post,Sigma = sigma.post)
    
    for (i in 2:burn)
    {
      #sigma.special<-(nu+m-lt-1)*(sigma0+sum.matrix(r.star.c,t(mu.sim[i-1,]),m))
      sigma.special<-(sigma0+sum.matrix(r.c,t(mu.sim[i-1,]),m))
      sigma.sim<-riwish(nu+m,sigma.special)
      sigma.sim.inv<-solve(sigma.sim)
      sigma.post<-solve(m*sigma.sim.inv+t(P)%*%omega.inv%*%P)
      mu.post<-sigma.post%*%(m*sigma.sim.inv%*%r.bar+t(P)%*%omega.inv%*%q)
      mu.sim[i,]<-mvrnorm(n=1,mu=mu.post,Sigma = sigma.post)
    }
    
    sigma.post.mean<-matrix(0,nrow=lt,ncol=lt)
    mu.init<-mu.sim[burn,]
    sigma.special<-(sigma0+sum.matrix(r.c,t(mu.init),m))
    sigma.sim<-riwish(nu+m,sigma.special)
    mu.sim<-matrix(0,nrow=steps,ncol=lt)
    sigma.sim.inv<-solve(sigma.sim)
    sigma.post<-solve(m*sigma.sim.inv+t(P)%*%omega.inv%*%P)
    mu.post<-sigma.post%*%(m*sigma.sim.inv%*%r.bar+t(P)%*%omega.inv%*%q)
    mu.sim[1,]<-mvrnorm(n=1,mu=mu.post,Sigma = sigma.post)
    sigma.post.mean<-sigma.post.mean+sigma.sim
    
    for (i in 2:steps)
    {
      #sigma.special<-(nu+m-lt-1)*(sigma0+sum.matrix(r.star.c,t(mu.sim[i-1,]),m))
      sigma.special<-(sigma0+sum.matrix(r.c,t(mu.sim[i-1,]),m))
      sigma.sim<-riwish(nu+m,sigma.special)
      sigma.sim.inv<-solve(sigma.sim)
      sigma.post<-solve(m*sigma.sim.inv+t(P)%*%omega.inv%*%P)
      mu.post<-sigma.post%*%(m*sigma.sim.inv%*%r.bar+t(P)%*%omega.inv%*%q)
      mu.sim[i,]<-mvrnorm(n=1,mu=mu.post,Sigma = sigma.post)
      sigma.post.mean<-sigma.post.mean+sigma.sim
    }
    
    mu.post.mean<-apply(mu.sim,2,mean)
    sigma.post.mean<-sigma.post.mean/steps
    
    w<-(1/lambda)*solve(sigma.post.mean)%*%mu.post.mean
    w<-w/abs(sum(w))
    position<-floor(w*capital.init/prices.test[1,])
    capital.stock.init<-position*prices.test[1,]
    capital.stock.fin<-position*prices.test[ld.test,]
    
    c(o1,o2,sum(capital.stock.fin-capital.stock.init),norm(P%*%mu.post.mean-q,type="2"),position,w)
    
    
    #setTxtProgressBar(pb,count)
    #count<-count+1
  }

stopCluster(cl)
toc()

colnames(profit.vec)<-c("o1","o2","profit","distance",paste0(tickers," shares"),paste0(tickers," weight"))

write.table(profit.vec,"/home-b/mandrei/Desktop/Results/V7 unscaled P nonsquare choose 4 from 500 FB sim o1,o2 profit&distance q 2,5 10^-6 10^-5.csv",sep=",",row.names = FALSE)

sink(type="message")
close(err)

