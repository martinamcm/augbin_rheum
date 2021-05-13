

#Likelihood function

f_20<-function(X,dat)
{
 
  #parameters
  alpha0 <- X[1]
  alpha1 <- X[2]
  beta0 <- X[3]
  beta1 <- X[4]
  
  #covariance parameters
  sig1 <- exp(X[5]) 
  sig2 <- exp(X[6]) 
  rho12 <- 2*inv.logit(X[7])-1
  
  #addition baseline parameters
  alpha2 <- X[8]
  beta2 <- X[9]
  
  
  #model means
  muz1<-alpha0+alpha1*dat[,2]+alpha2*dat[,5]
  muz2<-beta0+beta1*dat[,2]+beta2*dat[,6]
  
  #continuous bivariate covariance
  matbiv11 <- (sig1)^2
  matbiv12 <- rho12*sig1*sig2
  matbiv22 <- (sig2)^2
  Sigbiv   <- matrix(c(matbiv11, matbiv12, matbiv12, matbiv22), nrow=2, ncol=2)#check
  
  ##Likelihood function
  
  #-log(likelihood)
  Tfinal<-sum(log(dmvnorm(cbind(dat[,3],dat[,4]), c(mean(muz1), mean(muz2)), Sigbiv)))
  
  return(-Tfinal)
}

##Probability of response
integrand_20<-function(Zint,meantreat,meanuntreat,mle)
{
  sigmahat=matrix(nrow=2,ncol=2)
  sigmahat[1,1]=(exp(mle[5]))^2
  sigmahat[2,1]=(2*inv.logit(mle[7])-1)*(exp(mle[5]))*exp(mle[6])
  sigmahat[1,2]=sigmahat[2,1]
  sigmahat[2,2]=(exp(mle[6]))^2
  
  xtreat<-cbind(-meantreat[,1]+Zint[1], -meantreat[,2]+Zint[2])
  xuntreat<-cbind(-meanuntreat[,1]+Zint[1],-meanuntreat[,2]+Zint[2])
  
  pdftreat=dmvnorm(xtreat, mean=c(0,0),sigma=sigmahat)
  pdfuntreat=dmvnorm(xuntreat, mean=c(0,0),sigma=sigmahat)
  
  return(c(mean(pdftreat),mean(pdfuntreat)))
}


probofsuccess_20<-function(mle,n,dat,eta)
{
  n=n
  
  meantreat=cbind(cbind(rep(1,n),rep(1,n),dat[,5])%*%c(mle[1:2],mle[8]),cbind(rep(1,n),rep(1,n),dat[,6])%*%c(mle[3:4],mle[9]))   
  meanuntreat=cbind(cbind(rep(1,n),rep(0,n),dat[,5])%*%c(mle[1:2],mle[8]),cbind(rep(1,n),rep(0,n),dat[,6])%*%c(mle[3:4],mle[9]))     
  
  #lower and upper bounds
  minmean1=min(c(meantreat[,1],meanuntreat[,1]))
  minmean2=min(c(meantreat[,2],meanuntreat[,2]))
  
  maxmean1=max(c(meantreat[,1],meanuntreat[,1]))
  maxmean2=max(c(meantreat[,2],meanuntreat[,2]))
  
  lower=c(qnorm(1e-15,minmean1,exp(mle[5])),qnorm(1e-15,minmean2,exp(mle[6])))
  upper=c(eta[1],eta[2])
  
  a=cuhre(f=integrand_20,nComp=2,lowerLimit=lower,upperLimit=upper,flags=list(verbose=0,final=1,pseudo.random=0,mersenne.seed=NULL),
          meantreat=meantreat,meanuntreat=meanuntreat,mle=mle)
  #return(c(a$value[1],a$value[2]))
  #return(c(a$value[1]-a$value[2],a$value[1],a$value[2]))
  return(c((log(a$integral[1]/(1-a$integral[1]))-log(a$integral[2]/(1-a$integral[2]))),log(a$integral[1]/a$integral[2]),
           a$integral[1]-a$integral[2],a$integral[1],a$integral[2]))
  #return(log(a$value[1]/a$value[2]))
}

#Partial derivatives

partials_20<-function(mle,n,dat,eta)
{
  p=length(mle)
  fit1<-probofsuccess_20(mle,n,dat,eta)
  fitOR<-fit1[1]
  fitRR<-fit1[2]
  fitRD<-fit1[3]
  partials.augbinOR<-as.vector(rep(0,p))
  partials.augbinRR<-as.vector(rep(0,p))
  partials.augbinRD<-as.vector(rep(0,p))
  
  for(i in 1:p){
    valueupdate=mle
    valueupdate[i]=valueupdate[i]+0.000001
    
    updateprobOR=probofsuccess_20(valueupdate,n,dat,eta)[1]
    updateprobRR=probofsuccess_20(valueupdate,n,dat,eta)[2]
    updateprobRD=probofsuccess_20(valueupdate,n,dat,eta)[3]
    
    partials.augbinOR[i]=(updateprobOR-fitOR)/0.000001
    partials.augbinRR[i]=(updateprobRR-fitRR)/0.000001
    partials.augbinRD[i]=(updateprobRD-fitRD)/0.000001
    
  }
  
  return(c(partials.augbinOR,partials.augbinRR,partials.augbinRD,fit1))
}


#Box-Cox transformation

boxcoxtransform=function(y,lambda)
{
  return((y^lambda-1)/lambda)
}


#Standard binary

differenceinprob.binary_20=function(glm1,t,x1,x2)
{
  #get fitted probs for each arm from model:
  
  fittedvalues.control=as.double(inv.logit(cbind(rep(1,length(t[t==0])),rep(0,length(t[t==0])),x1[t==0],x2[t==0])%*%glm1$coef))
  
  fittedvalues.exp=as.double(inv.logit(cbind(rep(1,length(t[t==1])),rep(1,length(t[t==1])),x1[t==1],x2[t==1])%*%glm1$coef))
  
  
  return(c(log(mean(fittedvalues.exp,na.rm=T)/(1-mean(fittedvalues.exp,na.rm=T)))-log(mean(fittedvalues.control,na.rm=T)/(1-mean(fittedvalues.control,na.rm=T))),
           log(mean(fittedvalues.exp,na.rm=T)/mean(fittedvalues.control,na.rm=T)),mean(fittedvalues.exp,na.rm=T)-mean(fittedvalues.control,na.rm=T),
           mean(fittedvalues.exp,na.rm=T),mean(fittedvalues.control,na.rm=T)))
}


partialderivatives.binary_20=function(glm1,t,x1,x2)
{
  
  value1=differenceinprob.binary_20(glm1,t,x1,x2)
  valueOR=value1[1]
  valueRR=value1[2]
  valueRD=value1[3]
  
  partialsOR=rep(0,4)
  partialsRR=rep(0,4)
  partialsRD=rep(0,4)
  
  for(i in 1:4){
    tempglm1=glm1
    tempglm1$coef[i]=tempglm1$coef[i]+0.00001
    
    partialsOR[i]=(differenceinprob.binary_20(tempglm1,t,x1,x2)[1]-valueOR)/0.00001
    partialsRR[i]=(differenceinprob.binary_20(tempglm1,t,x1,x2)[2]-valueRR)/0.00001
    partialsRD[i]=(differenceinprob.binary_20(tempglm1,t,x1,x2)[3]-valueRD)/0.00001
    
  }
  
  return(c(partialsOR,partialsRR,partialsRD,value1))
  
  
}


result.bin<-as.list(NULL)
result.latent<-as.list(NULL)
results<-as.list(NULL)

LatVarfunc_20<-function(dat,eta){
  n=dim(dat)[1]
  
  #Starting values
  lm1<-lm(dat[,3]~dat[,2]+dat[,5])
  lm2<-lm(dat[,4]~dat[,2]+dat[,6])
  sig1est<-log(var(dat[,3]))
  sig2est<-log(var(dat[,4]))
  rho12est<-log(((cor(dat[,3],dat[,4])+1)/2)/(1-(cor(dat[,3],dat[,4])+1)/2))
  
  X<-c(lm1$coef[1],lm1$coef[2],lm2$coef[1],lm2$coef[2],sig1est,sig2est,rho12est,lm1$coef[3],lm2$coef[3])
  X<-as.vector(X)
  
  #Latent variable method
  
  mlefit=optimx(X,f_20,dat=dat,lower=rep(-Inf,length(X)),upper=rep(+Inf,length(X)),method="nlminb",control=list(rel.tol=1e-12))
  mle<-coef(mlefit[1,])
  hess<-attr(mlefit,"details")["nlminb",]$nhatend
  mlecov=ginv(hess)
  mlecov<-nearPD(mlecov)$mat
  se<-sqrt(mlecov[col(mlecov)==row(mlecov)])
  
  part<-partials_20(mle,n,dat,eta)
  meanOR<-part[28]
  partsOR<-part[1:9]
  varianceOR=t(partsOR)%*%mlecov%*%partsOR
  varianceOR=varianceOR[1,1]
  
  meanRR<-part[29]
  partsRR<-part[10:18]
  varianceRR=t(partsRR)%*%mlecov%*%partsRR
  varianceRR=varianceRR[1,1]
  
  meanRD<-part[30]
  partsRD<-part[19:27]
  varianceRD=t(partsRD)%*%mlecov%*%partsRD
  varianceRD=varianceRD[1,1]
  
  CIOR<-c(meanOR-1.96*sqrt(varianceOR),meanOR,meanOR+1.96*sqrt(varianceOR))
  CIRR<-c(meanRR-1.96*sqrt(varianceRR),meanRR,meanRR+1.96*sqrt(varianceRR))
  CIRD<-c(meanRD-1.96*sqrt(varianceRD),meanRD,meanRD+1.96*sqrt(varianceRD))
  
  probresplat<-c(part[31],part[32])
  result.latent<-c(CIOR,CIRR,CIRD,probresplat)
  
  #Standard binary
  
  dat$resp<-ifelse(dat[,3]<=(eta[1]) & dat[,4]<=(eta[2]), 1,0)
  success.binary=dat$resp
  
  glm1=brglm(success.binary~dat[,2]+dat[,5]+dat[,6],family="binomial")
  
  partial.binary=partialderivatives.binary_20(glm1,dat[,2],dat[,5],dat[,6])
  covariance=summary(glm1)$cov.unscaled
  
  mean.binaryOR=partial.binary[13]
  partials.binaryOR=partial.binary[1:4]
  var.binaryOR=t(partials.binaryOR)%*%covariance%*%partials.binaryOR
  
  mean.binaryRR=partial.binary[14]
  partials.binaryRR=partial.binary[5:8]
  var.binaryRR=t(partials.binaryRR)%*%covariance%*%partials.binaryRR
  
  mean.binaryRD=partial.binary[15]
  partials.binaryRD=partial.binary[9:12]
  var.binaryRD=t(partials.binaryRD)%*%covariance%*%partials.binaryRD
  
  CI.binaryOR=c(mean.binaryOR-1.96*sqrt(var.binaryOR),mean.binaryOR,mean.binaryOR+1.96*sqrt(var.binaryOR))
  CI.binaryRR=c(mean.binaryRR-1.96*sqrt(var.binaryRR),mean.binaryRR,mean.binaryRR+1.96*sqrt(var.binaryRR))
  CI.binaryRD=c(mean.binaryRD-1.96*sqrt(var.binaryRD),mean.binaryRD,mean.binaryRD+1.96*sqrt(var.binaryRD))
  
  result.bin<-c(CI.binaryOR,CI.binaryRR,CI.binaryRD,partial.binary[16:17])
  
  epshat1 <- dat[,3]-(mle[1]+mle[2]*dat[,2]+mle[8]*dat[,5])
  epshat2 <- dat[,4]-(mle[3]+mle[4]*dat[,2]+mle[9]*dat[,6])
  epshat <- as.matrix(cbind(epshat1,epshat2))
  
  sig1hat <- exp(mle[5]) 
  sig2hat <- exp(mle[6]) 
  rho12hat <- 2*inv.logit(mle[7])-1
  
  SigHat<-matrix(data=c(sig1hat^2,rho12hat*sig1hat*sig2hat,rho12hat*sig1hat*sig2hat,sig2hat^2),ncol=2)
  modres <- diag((epshat)%*%ginv(SigHat)%*%t(epshat))
  
  results<-c(result.latent,result.bin,modres,mle,se)
  return(results)
}

