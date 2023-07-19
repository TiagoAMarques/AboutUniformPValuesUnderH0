# image used on tweet
# https://twitter.com/TiagoALOMarques/status/1681601009687097345

#illustrating with 3 different distributions
# The distribution of the CDF of a random variable is uniform
par(mfrow=c(3,2))
n<-10000
set.seed(3)
xs0<-rnorm(n,mean=3,sd=2)
hist(xs0,main="A Gaussian",freq=FALSE,xlab="y",ylab="f(y)")
hist(pnorm(xs0,mean=3,sd=2),main="P(X<x)=P(F(y)<x)",freq=FALSE,ylab="f(x)",xlab="x")
xs1<-rgamma(n,shape=4,scale=10)
hist(xs1,main="A gamma",freq=FALSE,xlab="y",ylab="f(y)")
hist(pgamma(xs1,shape=4,scale=10),main="P(X<x)=P(F(y)<x)",freq=FALSE,ylab="f(x)",xlab="x")
xs2<-rbeta(n,shape1=4,shape2=1)
hist(xs2,main="A beta",freq=FALSE,xlab="y",ylab="f(y)")
hist(pbeta(xs2,shape1=4,shape2=1),main="P(X<x)=P(F(y)<x)",freq=FALSE,ylab="f(x)",xlab="x")


B<-10000
ts<-numeric(B)
res<-numeric(B)
n<-50
for(i in 1:B){
  xs<-rnorm(n,mean=0,sd=1)
  mytest<-t.test(xs,mu = 0)
  res[i]<-mytest$p.value
  ts[i]<-mytest$statistic
}

par(mfrow=c(1,3))
hist(res)
hist(ts,freq=FALSE)

xseq<-seq(-5,5,length=100)
points(xseq,dt(xseq,df=n-1),type="l")

#remeber the test stat has t with n-1 degrees of freedom
cols2<-data.frame(p1=pt(ts,df=n-1),p2=1-pt(ts,df=n-1))
cols2$pvals<-2*apply(cols2,1,min)

# the p-values manual
hist(cols2$pvals)