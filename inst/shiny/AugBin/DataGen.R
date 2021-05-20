
set.seed(seed)

n=300

#vars
Y10<-rnorm(n,0,1)
Y20<-rnorm(n,0,0.5)

treat0=rbinom(n/2,1,0)
treat1=rbinom(n/2,1,1)
treat=sample(c(treat0,treat1))

#covariance parameters #best scenario
sig1=1.2
sig2=1
rho12=0.5

#parameters - need to be rescaled!!
alpha0=-2
alpha1=-1
alpha2=-0.5
psi0=0.5
psi1=-0.8

Sigma1=matrix(c(sig1^2, rho12*sig1*sig2, rho12*sig1*sig2, 1), nrow=2)

eps<-mvrnorm(n=n,mu=c(0,0),Sigma=Sigma1)

Y1<-alpha0+alpha1*treat+alpha2*Y10+eps[,1]
Y2<-psi0+psi1*treat+eps[,2]

Y<-matrix(c(Y1, Y2),ncol=2)

Y2bin <-vector()

for(i in 1:n)
{0
  if(Y2[i]>0){Y2bin[i]=1}
  if(Y2[i]<0){Y2bin[i]=0}
}

id<-c(1:n)

dat<-data.frame(id,treat,Y1,Y2bin,Y10)





n=300

#vars
Z10<-rnorm(n,0,1)
Z20<-rnorm(n,0,0.5)

treat0=rbinom(n/2,1,0)
treat1=rbinom(n/2,1,1)
treat=sample(c(treat0,treat1))

#parameters - need to be rescaled!!
alpha0=-2
alpha1=-1#-0.4
alpha2=-0.5
beta0=-1
beta1=-1
beta2=-0.5

#covariance parameters #best scenario
sig1=2.5
sig2=2.5
rho12=0.5

Sigma1=matrix(c(sig1^2, rho12*sig1*sig2, rho12*sig1*sig2,
                sig2^2), nrow=2)

eps<-mvrnorm(n=n,mu=c(0,0),Sigma=Sigma1)

Z1<-alpha0+alpha1*treat+alpha2*Z10+eps[,1]
Z2<-beta0+beta1*treat+beta2*Z20+eps[,2]

Z<-matrix(c(Z1, Z2),ncol=2)

id<-c(1:n)

dat20<-data.frame(id,treat,Z1,Z2,Z10,Z20)


n=300

#vars
Z10<-rnorm(n,0,1)

treat0=rbinom(n/2,1,0)
treat1=rbinom(n/2,1,1)
treat=sample(c(treat0,treat1))

#parameters - need to be rescaled!!
alpha0=-2
alpha1=-1#-0.4
alpha2=-0.5

#covariance parameters #best scenario
sig1=2.5

eps<-rnorm(n=n,mean=0,sd=sig1)

Z1<-alpha0+alpha1*treat+alpha2*Z10+eps

id<-c(1:n)

dat10<-data.frame(id,treat,Z1,Z10)

