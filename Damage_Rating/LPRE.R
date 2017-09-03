#http://202.38.64.11/~zfw/LPRE.htm

##################################################################
################### Parameter estimate ###########################
##################################################################
findroot<-function(y,x,beta0,iter=0.00000001,dlen=0.001)
{
  beta1=beta0
  n1=length(y) 
  for(i1 in 1:1000)
  {
    tmp=x%*%beta1
    tmp1=-y*exp(-tmp)+1/y*exp(tmp)
    tmp2=y*exp(-tmp)+1/y*exp(tmp)
    score=rep(0,length(beta1))
    InM=matrix(0,length(beta1),length(beta1))
    for(j1 in 1:n1)
    {
      score=score+tmp1[j1]*x[j1,]
      InM=InM+tmp2[j1]*t(x[j1,,drop=F])%*%x[j1,,drop=F]
    }
    tmp=eigen(InM)$values 
    if(min(abs(tmp))<0.0001) {beta2=beta1-dlen*score} else {beta2=beta1-solve(InM)%*%score}
    if(sum(abs(beta1-beta2))<iter) {break} else {beta1=beta2}
    
  }
  
  tmp=x%*%beta1
  y/exp(tmp)->a
  b=mean((a-1/a)^2)/(2*mean(a))
  tmp1=-y*exp(-tmp)+1/y*exp(tmp)
  tmp2=y*exp(-tmp)+1/y*exp(tmp)
  score=rep(0,length(beta1))
  InM=matrix(0,length(beta1),length(beta1))
  for(j1 in 1:n1)
  {
    score=score+tmp1[j1]*x[j1,]
    InM=InM+tmp2[j1]*t(x[j1,,drop=F])%*%x[j1,,drop=F]
  }
  InM=solve(InM)*b
  aest=2/b
  res=list(beta.hat=beta1,score=score,cov.hat=InM,aest=aest)
  res
}

#########################
LPRE<-function(y,x,intercept=TRUE)
{
  if(intercept){x=cbind(1,x)}
  
  beta0=lm(log(y)~x-1)$coeff
  temp=findroot(y,x,beta0)
  res.beta=temp$beta.hat
  res.sd=sqrt(diag(temp$cov.hat))
  res=data.frame(est=res.beta,est.sd=res.sd)
  res
}

##################################################################
################### Hypothesis testing ###########################
##################################################################

LPRE.test<-function(y,x,intercept=TRUE,H0=NULL)
{
  if(intercept) {x=cbind(1,x)}
  ##### alternative ##############
  beta0=lm(log(y)~x-1)$coeff
  temp=findroot(y,x,beta0)
  alterest=temp$beta.hat
  aest1=temp$aest
  
  temp=x%*%alterest
  temp1=y*exp(-temp)+1/y*exp(temp)
  malter=sum(temp1)
  #### null #######################
  xtilde=x[,!(H0==1),drop=F]
  beta0=lm(log(y)~xtilde-1)$coeff
  temp=findroot(y,xtilde,beta0)
  
  temp1=temp$beta.hat
  nullest=rep(0,length(beta))
  nullest[!(H0==1)]=temp1
  
  temp=x%*%nullest
  temp1=y*exp(-temp)+1/y*exp(temp)
  mnull=sum(temp1)
  #################################
  temp=(mnull-malter)*aest1
  temp=data.frame(stat=temp,p.value=1-pchisq(temp,df=sum(H0==1)))
  temp
}
