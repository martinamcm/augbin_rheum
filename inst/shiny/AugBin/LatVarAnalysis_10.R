library(MASS)
library(stats)
library(mvtnorm)
library(nlme)
library(boot)
library(matrixcalc)
library(numDeriv)
library(cubature)
library(optimx)
library(brglm)
library(Matrix)
library(matrixcalc)


##Likelihood

f<-function(X,dat)
{
  #data
  #dat<-dat[!is.na(dat[,3]),]#change this to baseline measure when it is added? 
  
  #parameters
  alpha0 <- X[1]
    alpha1 <- X[2]

  #covariance parameters
  sig1 <- exp(X[3]) 
  
  #addition baseline parameters
  alpha2 <- X[4]
  
  print(X)
  
  #model means
  muz1<-alpha0+alpha1*dat[,2]+alpha2*dat[,4]
  
  ##Likelihood function
    
  #-log(likelihood)
  Tfinal<-sum(log(x=dnorm(dat[,3], mean=mean(muz1), sd=sig1)))
  
  #print(Tfinal)
  return(-Tfinal)
}

lowerlim <- c(-Inf,-Inf,-Inf,-Inf)
upperlim <- c(+Inf,+Inf,+Inf,+Inf)



############################
##PROBABILITY OF RESPONSE###
############################

###LATENT VARIABLE MODEL


##Probability of response
integrand<-function(Zint,meantreat,meanuntreat,mle)
{
 sigmahat<-(exp(mle[3]))^2
  
  xtreat<-(-meantreat[,1]+Zint[1])
  xuntreat<-(-meanuntreat[,1]+Zint[1])
  
  pdftreat=dnorm(xtreat, mean=0,sd=sqrt(sigmahat))
     pdfuntreat=dnorm(xuntreat, mean=0,sd=sqrt(sigmahat))
  
  return(c(mean(pdftreat),mean(pdfuntreat)))
}


probofsuccess<-function(mle,n,dat,eta)
{
  n=n
  
  meantreat=cbind(cbind(rep(1,n),rep(1,n),dat[,4])%*%c(mle[1:2],mle[4]))
  meanuntreat=cbind(cbind(rep(1,n),rep(0,n),dat[,4])%*%c(mle[1:2],mle[4]))
  
  #lower and upper bounds
  minmean1=min(c(meantreat[,1],meanuntreat[,1]))
  maxmean1=max(c(meantreat[,1],meanuntreat[,1]))
  
  lower=qnorm(1e-15,minmean1,exp(mle[3]))
  upper=eta
  
  a=cuhre(f=integrand,nComp=2,lower=lower,upper=upper,flags=list(verbose=0,final=1,pseudo.random=0,mersenne.seed=NULL),
          meantreat=meantreat,meanuntreat=meanuntreat,mle=mle)
  #return(c(a$value[1],a$value[2]))
  #return(c(a$value[1]-a$value[2],a$value[1],a$value[2]))
  return(c((log(a$integral[1]/(1-a$integral[1]))-log(a$integral[2]/(1-a$integral[2]))),log(a$integral[1]/a$integral[2]),
           a$integral[1]-a$integral[2],a$integral[1],a$integral[2]))
  #return(log(a$value[1]/a$value[2]))
}



partials<-function(mle,n,dat,eta)
{
  p=length(mle)
  fit1<-probofsuccess(mle,n,dat,eta)
  fitOR<-fit1[1]
  fitRR<-fit1[2]
  fitRD<-fit1[3]
  partials.augbinOR<-as.vector(rep(0,p))
  partials.augbinRR<-as.vector(rep(0,p))
  partials.augbinRD<-as.vector(rep(0,p))
  
  for(i in 1:p){
    valueupdate=mle
    valueupdate[i]=valueupdate[i]+0.000001
    
    updateprobOR=probofsuccess(valueupdate,n,dat,eta)[1]
    updateprobRR=probofsuccess(valueupdate,n,dat,eta)[2]
    updateprobRD=probofsuccess(valueupdate,n,dat,eta)[3]
    
    partials.augbinOR[i]=(updateprobOR-fitOR)/0.000001
    partials.augbinRR[i]=(updateprobRR-fitRR)/0.000001
    partials.augbinRD[i]=(updateprobRD-fitRD)/0.000001
    
  }
  
  return(c(partials.augbinOR,partials.augbinRR,partials.augbinRD,fit1))
}


###AUGMENTED BINARY 

boxcoxtransform=function(y,lambda)
{
  return((y^lambda-1)/lambda)
}





####STANDARD BINARY

differenceinprob.binary=function(glm1,t,x1)
{
  #get fitted probs for each arm from model:
  
  fittedvalues.control=as.double(inv.logit(cbind(rep(1,length(t[t==0])),rep(0,length(t[t==0])),x1[t==0])%*%glm1$coef))
  
  fittedvalues.exp=as.double(inv.logit(cbind(rep(1,length(t[t==1])),rep(1,length(t[t==1])),x1[t==1])%*%glm1$coef))
  
  
  return(c(log(mean(fittedvalues.exp,na.rm=T)/(1-mean(fittedvalues.exp,na.rm=T)))-log(mean(fittedvalues.control,na.rm=T)/(1-mean(fittedvalues.control,na.rm=T))),
           log(mean(fittedvalues.exp,na.rm=T)/mean(fittedvalues.control,na.rm=T)),mean(fittedvalues.exp,na.rm=T)-mean(fittedvalues.control,na.rm=T),
           mean(fittedvalues.exp,na.rm=T),mean(fittedvalues.control,na.rm=T)))
}


partialderivatives.binary=function(glm1,t,x1)
{
  
  value1=differenceinprob.binary(glm1,t,x1)
  valueOR=value1[1]
  valueRR=value1[2]
  valueRD=value1[3]
  
  partialsOR=rep(0,3)
  partialsRR=rep(0,3)
  partialsRD=rep(0,3)
  
  for(i in 1:3){
    tempglm1=glm1
    tempglm1$coef[i]=tempglm1$coef[i]+0.00001
    
    partialsOR[i]=(differenceinprob.binary(tempglm1,t,x1)[1]-valueOR)/0.00001
    partialsRR[i]=(differenceinprob.binary(tempglm1,t,x1)[2]-valueRR)/0.00001
    partialsRD[i]=(differenceinprob.binary(tempglm1,t,x1)[3]-valueRD)/0.00001
    
  }
  
  return(c(partialsOR,partialsRR,partialsRD,value1))
  
  
}


result.bin<-as.list(NULL)
result.latent<-as.list(NULL)
results<-as.list(NULL)

#dat<-data.frame(id,treat,Z1,Z2,Z3bin,Z10,Z20)
#dat<-dat20
#eta<-c(-1.5,-1.5)

LatVarfunc<-function(dat,eta){
  n=dim(dat)[1]
  
  ##Starting values
  lm1<-lm(dat[,3]~dat[,2]+dat[,4])
  sig1est<-log(var(dat[,3]))
  
  X<-c(lm1$coef[1],lm1$coef[2],sig1est,lm1$coef[3])
  X<-as.vector(X)
  
  ##LATENT VARIABLE
  
  mlefit=optimx(X,f,dat=dat,lower=lowerlim,upper=upperlim,method="nlminb",control=list(rel.tol=1e-12))
  mle<-coef(mlefit[1,])
  hess<-attr(mlefit,"details")["nlminb",]$nhatend
  print(hess)
  mlecov=ginv(hess)
  mlecov<-nearPD(mlecov)$mat
  se<-sqrt(diag(mlecov))
  print(se)
  
  part<-partials(mle,n,dat,eta)
  print(part)
  meanOR<-part[13]
  partsOR<-part[1:4]
  varianceOR=t(partsOR)%*%mlecov%*%partsOR
  varianceOR=varianceOR[1,1]
  
  meanRR<-part[14]
  partsRR<-part[5:8]
  varianceRR=t(partsRR)%*%mlecov%*%partsRR
  varianceRR=varianceRR[1,1]
  
  meanRD<-part[15]
  partsRD<-part[9:12]
  varianceRD=t(partsRD)%*%mlecov%*%partsRD
  varianceRD=varianceRD[1,1]
  
  CIOR<-c(meanOR-1.96*sqrt(varianceOR),meanOR,meanOR+1.96*sqrt(varianceOR))
  CIRR<-c(meanRR-1.96*sqrt(varianceRR),meanRR,meanRR+1.96*sqrt(varianceRR))
  CIRD<-c(meanRD-1.96*sqrt(varianceRD),meanRD,meanRD+1.96*sqrt(varianceRD))
  
  probresplat<-c(part[16],part[17])
  result.latent<-c(CIOR,CIRR,CIRD,probresplat)
  
  
  ###STANDARD BINARY
  
  dat$resp<-ifelse(dat[,3]<=(eta[1]), 1,0)
  success.binary=dat$resp
  
  glm1=brglm(success.binary~dat[,2]+dat[,4],family="binomial")
  
  partial.binary=partialderivatives.binary(glm1,dat[,2],dat[,4])
  covariance=summary(glm1)$cov.unscaled
  
  mean.binaryOR=partial.binary[10]
  partials.binaryOR=partial.binary[1:3]
  var.binaryOR=t(partials.binaryOR)%*%covariance%*%partials.binaryOR
  
  mean.binaryRR=partial.binary[11]
  partials.binaryRR=partial.binary[4:6]
  var.binaryRR=t(partials.binaryRR)%*%covariance%*%partials.binaryRR
  
  mean.binaryRD=partial.binary[12]
  partials.binaryRD=partial.binary[7:9]
  var.binaryRD=t(partials.binaryRD)%*%covariance%*%partials.binaryRD
  
  CI.binaryOR=c(mean.binaryOR-1.96*sqrt(var.binaryOR),mean.binaryOR,mean.binaryOR+1.96*sqrt(var.binaryOR))
  CI.binaryRR=c(mean.binaryRR-1.96*sqrt(var.binaryRR),mean.binaryRR,mean.binaryRR+1.96*sqrt(var.binaryRR))
  CI.binaryRD=c(mean.binaryRD-1.96*sqrt(var.binaryRD),mean.binaryRD,mean.binaryRD+1.96*sqrt(var.binaryRD))
  
  result.bin<-c(CI.binaryOR,CI.binaryRR,CI.binaryRD,partial.binary[13:14])
  
  epshat1 <- dat[,3]-(mle[1]+mle[2]*dat[,2]+mle[4]*dat[,4])
  epshat <- as.matrix(epshat1)
  
  sig1hat <- exp(mle[3]) 
 
  modres <- (epshat^2)/sig1hat

  results<-c(result.latent,result.bin,modres,mle,se)
  return(results)
}


