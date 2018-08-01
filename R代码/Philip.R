data1<-read.csv("C:\\Users\\chen\\Desktop\\wang\\all affected.csv",head =TRUE)
c.p<-data1[,1]
c.c<-data1[,3]
x.p<-data1[,2]
x.c<-data1[,4]
##shapiro.test(x.p)
##shapiro.test(x.c)
##bartlett.test(list(x.p,x.c))
t.test(x.p,x.c,alternative = "greater",paired = TRUE,var.equal = FALSE,conf.level = 0.95)
##wilcox.test(x.p,x.c,alternative = "greater",paired = TRUE,exact = FALSE,correct = FALSE)
require(mnormt)
#######################################################################################
#Returns parameter estimates which maximimze the conditional likelihood 
#given the truncation based on the method proposed by Huang and Vieland (1997)
#Computational details are given in Appendix A of Vieland and Huang (1998). 
#Currently, the methods maximize all parameters OR all parameters for a given correlation (rho)
#The function returns the estimated mle and a logical indicating whether or the estimated
#correlation was greater than 1 or the parameters estimated diverged to infinity.
#c.p = vector of PARENT ascertainment ages
#c.c = vector of CHILD asceratinment ages
#x.p = vector of PARENT AOOs
#x.c = vector of CHILD AOOs
#tol = convergence criterion (stop when the largest absolute difference of the vector theta 
#	[which contains mu.p, mu.c, sigma^2 and rho] between iterations is less than tol)
#maximize = if ==`all', maximize all parameters, else maximize all for a fixed rho
#rho = value of rho if not to be maximized
#######################################################################################
huang.vieland.mle <- function(c.p,c.c,x.p,x.c,tol=1e-6,maximize='all',rho=NULL) {
  n = length(x.p)
  if(maximize!='all'&is.null(rho)) {
    stop('A value for rho must be given if it is not to be maximized')
  }
  #Initialize guess
  current.theta=c(mean(x.p),mean(x.c),var(c(x.p,x.c)),ifelse(maximize=='all',cor(x.p,x.c),rho))
  prior.theta=c(rep(Inf,3),ifelse(maximize=='all',Inf,rho))
  iter=0;diverged=F
  #Iterate until convergence
  while(max(abs(current.theta-prior.theta))>=tol) {
    iter=iter+1;
    #calculate variables as defined on page 1224 of V&H (1998)
    k1=(c.p-current.theta[1])/sqrt(current.theta[3])
    k2=(c.c-current.theta[2])/sqrt(current.theta[3])
    denom=NULL;num=NULL	
    for(j in 1:n) {
      denom=c(denom,sadmvn(lower=rep(-Inf,2),upper=c(k1[j],k2[j]),mean=rep(0,2),varcov=diag(1,2)))
      num=c(num,dmnorm(c(k1[j],k2[j]),mean=rep(0,2),varcov=diag(1,2)))
    }
    m1=(1/n)*sum(dnorm(k1)*pnorm((k2-current.theta[4]*k1)/sqrt(1-(current.theta[4]^2)))/denom)
    m2=(1/n)*sum(dnorm(k2)*pnorm((k1-current.theta[4]*k2)/sqrt(1-(current.theta[4]^2)))/denom)
    m3=(1/n)*sum(k1*dnorm(k1)*pnorm((k2-current.theta[4]*k1)/sqrt(1-(current.theta[4]^2)))/denom)
    m4=(1/n)*sum(k2*dnorm(k2)*pnorm((k1-current.theta[4]*k2)/sqrt(1-(current.theta[4]^2)))/denom)
    m5=(1/n)*sum(num/denom)
    #Estimating equations
    muhat1=mean(x.p)+sqrt(current.theta[3])*(m1+current.theta[4]*m2)
    muhat2=mean(x.c)+sqrt(current.theta[3])*(m2+current.theta[4]*m1)
    sigmasqhat=(sum((x.p-current.theta[1])^2)-2*current.theta[4]*sum((x.p-current.theta[1])*(x.c-current.theta[2]))+sum((x.c-current.theta[2])^2))/(n*(1-(current.theta[4]^2))*(2-m3-m4))
    rhohat=ifelse(maximize=='all',(sum((x.p-current.theta[1])*(x.c-current.theta[2]))/(n*current.theta[3])-(1-(current.theta[4]^2))*m5)/(1-m3-m4),rho)
    #Update current, prior estimates
    prior.theta=current.theta
    current.theta=c(muhat1,muhat2,sigmasqhat,rhohat)
    #The following conditions specify when to break out of the loop
    #and instead estimate via a profiling method.
    #(i)Break if the parameters diverge
    #(ii)Break if nonsensical correlation is achieved
    #(iii)Break out of loop if convergence is not obtained after 1000 iterations
    if(any(is.na(current.theta))|any(current.theta%in%c(-Inf,Inf))|abs(rhohat)>1|iter>1000) {
      current.theta=prior.theta
      #set diverged=T as a flag to estimate via profiling
      diverged=T;
      break;
    }
  }	
  names(current.theta) = c("mu.p","mu.c","sigma.sq","rho")
  list(mle=current.theta,diverged=diverged);
}


#######################################################################################
#Returns parameter estimates which maximimze the conditional likelihood 
#given the truncation, where rho is estimated via profiling approach. This alternative 
# is described on page 1225 of Vieland and Huang (1998)
#######################################################################################
huang.vieland.profile.mle <- function(c.p,c.c,x.p,x.c,tol=1e-6) {
  #Coarse grid with larger error tolerance (to increase speed)
  rho.seq=seq(-.9,.9,.1)
  likelihoods = matrix(ncol=6,nrow=length(rho.seq))
  for(j in 1:length(rho.seq)) {
    foo = huang.vieland.mle(c.p,c.c,x.p,x.c,tol*10,maximize='not rho',rho.seq[j])
    current.theta = foo$mle
    likelihoods[j,] <- c(cond.loglike(c.p,c.c,x.p,x.c,current.theta[1],current.theta[2],current.theta[3],current.theta[4]),current.theta,foo$diverged)
  }
  #Home in with a finer grid and smaller error tolerance
  best.rho=likelihoods[which.max(likelihoods[,1]),5]
  rho.seq=seq(best.rho-.09,best.rho+.09,.01)
  likelihoods = matrix(ncol=6,nrow=length(rho.seq))
  for(j in 1:length(rho.seq)) {
    foo = huang.vieland.mle(c.p,c.c,x.p,x.c,tol,maximize='not rho',rho.seq[j])
    current.theta = foo$mle
    likelihoods[j,] <- c(cond.loglike(c.p,c.c,x.p,x.c,current.theta[1],current.theta[2],current.theta[3],current.theta[4]),current.theta,foo$diverged)
  }
  best.lik = which.max(likelihoods[,1])
  list(mle=likelihoods[best.lik,2:5],diverged=as.logical(likelihoods[best.lik,6]))
}

huang.vieland.profile.mle(c.p,c.c,x.p,x.c,tol)

current.theta1<-huang.vieland.profile.mle(c.p,c.c,x.p,x.c)
current.theta<-current.theta1$mle



#######################################################################################
#Numerically evaluate a second partial derivative with respect
#to the same variable
#func = (string or function) name of R function to differentiate
#value = (numeric) value at which to evaluate derivative (this also unambiguously 
#	identifies which argument the function will be differentiated with
#	respect to)
#eps = (numeric) epsilon
#... = other arguments to pass to func
#######################################################################################

second.partial.deriv <- function(func,value,eps=.Machine$double.eps,...) {
  temp=value+eps^(1/4)*ifelse(value==0,.1,value);
  h=temp-value;
  (do.call(func,list(value+h,...))+do.call(func,list(value-h,...))-2*+do.call(func,list(value,...)))/(h^2)
}

#######################################################################################
#Numerically evaluate a second partial derivative with respect 
#to two different variables
#func = (string or function) name of R function to differentiate
#value1 = (numeric) value at which to evaluate derivative (also identifies
#		1st argument which will be differentiated with respect to)
#value2 = (numeric) value at which to evaluate derivative (also identifies
#		2nd argument which will be differentiated with respect to)
#eps = (numeric) epsilon
#... = other arguments to pass to func
#######################################################################################

mixed.second.partial.deriv <- function(func,value1,value2,eps=.Machine$double.eps,...) {
  temp=value1+eps^(1/4)*ifelse(value1==0,.1,value1);
  h1=temp-value1;
  temp=value2+eps^(1/4)*ifelse(value2==0,.1,value2);
  h2=temp-value2;
  (do.call(func,list(value1+h1,value2+h2,...))-do.call(func,list(value1+h1,value2-h2,...))-do.call(func,list(value1-h1,value2+h2,...))+do.call(func,list(value1-h1,value2-h2,...)))/(4*h1*h2)
}

#######################################################################################
#Conditional log likelihood from Huang and Vieland (1997)
#######################################################################################
cond.loglike <- function(c.p,c.c,x.p,x.c,mu.p,mu.c,sigmasq,rho) {
  sigma <- matrix(c(sigmasq,rho*sigmasq,rho*sigmasq,sigmasq),nrow=2)
  means <- c(mu.p,mu.c)
  foo <- sum(log(dmnorm(matrix(c(x.p,x.c),ncol=2),means,sigma)))
  foo2 <- sum(log(apply(matrix(c(c.p,c.c),ncol=2),1,sadmvn,mean=means,varcov=sigma,lower=c(-Inf,-Inf))))
  ifelse(foo!=-Inf&foo2!=-Inf,foo-foo2,-Inf)
}

#######################################################################################
#Calculate observed information based on conditional loglikelihood
#######################################################################################
observed.info <- function(c.p,c.c,x.p,x.c,current.theta) {
  obs.info <- matrix(nrow=4,ncol=4)
  obs.info[1,1] = -second.partial.deriv(cond.loglike,current.theta[1],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.c=current.theta[2],sigmasq=current.theta[3],rho=current.theta[4])[1]
  obs.info[2,2] = -second.partial.deriv(cond.loglike,current.theta[2],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],sigmasq=current.theta[3],rho=current.theta[4])[1]
  obs.info[3,3] = -second.partial.deriv(cond.loglike,current.theta[3],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],mu.c=current.theta[2],rho=current.theta[4])[1]
  obs.info[4,4] = -second.partial.deriv(cond.loglike,current.theta[4],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],mu.c=current.theta[2],sigmasq=current.theta[3])[1]
  obs.info[1,2] = obs.info[2,1] = -mixed.second.partial.deriv(cond.loglike,current.theta[1],current.theta[2],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,sigmasq=current.theta[3],rho=current.theta[4])[1]
  obs.info[1,3] = obs.info[3,1] = -mixed.second.partial.deriv(cond.loglike,current.theta[1],current.theta[3],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.c=current.theta[2],rho=current.theta[4])[1]
  obs.info[1,4] = obs.info[4,1] = -mixed.second.partial.deriv(cond.loglike,current.theta[1],current.theta[4],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.c=current.theta[2],sigmasq=current.theta[3])[1]
  obs.info[2,3] = obs.info[3,2] = -mixed.second.partial.deriv(cond.loglike,current.theta[2],current.theta[3],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],rho=current.theta[4])[1]
  obs.info[2,4] = obs.info[4,2] = -mixed.second.partial.deriv(cond.loglike,current.theta[2],current.theta[4],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],sigmasq=current.theta[3])[1]
  obs.info[3,4] = obs.info[4,3] = -mixed.second.partial.deriv(cond.loglike,current.theta[3],current.theta[4],c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c,mu.p=current.theta[1],mu.c=current.theta[2])[1]
  obs.info
}

observed.info(c.p,c.c,x.p,x.c,current.theta)

#######################################################################################
#RY1 statistic -- Rabinowitz and Yang (1999)
#######################################################################################
rabinowitz.test1 <- function(c.p,c.c,x.p,x.c) {
  index <- which(apply(cbind(x.p,x.c),1,max)<=apply(cbind(c.p,c.c),1,min))
  estimate <- sum((as.numeric(x.c<x.p)-1/2)[index])
  variance <-	length(index)/4
  estimate/sqrt(variance)
}
rabinowitz.test1(c.p,c.c,x.p,x.c)


#######################################################################################
#Utility function for RY2-- Rabinowitz and Yang (1999)
#######################################################################################
eta <- function(i,c.p,c.c,x.p,x.c) {
  index <- which(apply(cbind(x.c[i],x.p),1,max)<apply(cbind(c.c[i],c.p),1,min)&
                   apply(cbind(x.p[i],x.c),1,max)<apply(cbind(c.p[i],c.c),1,min))
  if(i%in%index) {index=index[which(index!=i)]}
  sum(((as.numeric(x.c[i]<=x.p)-1/2)+(as.numeric(x.c<=x.p[i])-1/2))[index])+
    (as.numeric(x.c[i]<x.p[i])-1/2)*as.numeric(x.p[i]<=c.c[i])
}

#######################################################################################
#RY2 statistic -- Rabinowitz and Yang (1999)
#######################################################################################
rabinowitz.test2 <- function(c.p,c.c,x.p,x.c) {
  foo <- expand.grid(1:length(c.p),1:length(c.p))
  index <- which(apply(cbind(x.c[foo[,1]],x.p[foo[,2]]),1,max)<apply(cbind(c.c[foo[,1]],c.p[foo[,2]]),1,min)&
                   apply(cbind(x.p[foo[,1]],x.c[foo[,2]]),1,max)<apply(cbind(c.p[foo[,1]],c.c[foo[,2]]),1,min))
  estimate <- sum((as.numeric(x.c[foo[,1]]<=x.p[foo[,2]])-1/2)[index])
  variance <- length(c.p)*var(sapply(1:length(c.p),eta,c.p=c.p,c.c=c.c,x.p=x.p,x.c=x.c))
  estimate/sqrt(variance)
}

rabinowitz.test2(c.p,c.c,x.p,x.c)
#######################################################################################
#Code to generate a random ascertainment sample for a given parameter set. 
#######################################################################################
random.sample <- function(mu.p,mu.c,rho,sigmasq,n,asct.range,diff.range) {
  means=c(mu.p,mu.c);
  sigma=matrix(c(sigmasq,rho*sigmasq,rho*sigmasq,sigmasq),nrow=2)
  c.p = c.c = x.p = x.c = NULL;
  while(length(c.p)<n) {
    c1 = runif(1,asct.range[1],asct.range[2]);d=runif(1,diff.range[1],diff.range[2])
    c2 = c1 - d
    c = c(c1,c2)
    x = rmnorm(1,means,sigma)
    if(all(c>x)) {
      c.p <- c(c.p,c[1])
      c.c <- c(c.c,c[2])
      x.p <- c(x.p,x[1])
      x.c <- c(x.c,x[2])
    }
  }
  list(c.p,c.c,x.p,x.c,0)
}

#######################################################################################
#Code to generate a generalized single ascertainment (GSA) sample for a given parameter set. 
#######################################################################################
singleplex.sample <- function(mu.p,mu.c,rho,sigmasq,n,frac.young,asct.range,diff.range) {
  means=c(mu.p,mu.c);
  sigma=matrix(c(sigmasq,rho*sigmasq,rho*sigmasq,sigmasq),nrow=2)
  c.p = c.c = x.p = x.c = NULL;young=0
  while(length(c.p)<n) {
    select = 1+rbinom(1,1,frac.young)
    c1 = runif(1,asct.range[1],asct.range[2]);d=runif(2,diff.range[1],diff.range[2])
    c2 = c1 - d[1]; c3 = c2 - d[2];
    c = c(c1,c2,c3)
    x = rmnorm(1,means,sigma)
    if(all(c(c[select]>x[1],c[select+1]>x[2]))) {
      young=young+select-1
      c.p <- c(c.p,c[select])
      c.c <- c(c.c,c[select+1])
      x.p <- c(x.p,x[1])
      x.c <- c(x.c,x[2])
    }
  }
  list(c.p,c.c,x.p,x.c,young/n)
}

#######################################################################################
#Code to generate a multiplex sample for a given parameter set. 
#######################################################################################
multiplex.sample <- function(mu.p,mu.c,rho,sigmasq,n,frac.young,asct.range,diff.range) {
  means=c(mu.p,mu.c);
  sigma=matrix(c(sigmasq,rho*sigmasq,rho*sigmasq,sigmasq),nrow=2)
  c.p = c.c = x.p = x.c = NULL;young=0
  while(length(c.p)<n) {
    select <- 1+rbinom(1,1,frac.young)
    c1 = runif(1,asct.range[1],asct.range[2]);d=runif(2,diff.range[1],diff.range[2])
    c2 = c1 - d[1]; c3 = c2 - d[2];
    c = c(c1,c2,c3)
    c = c(c[select],c[select+1])
    x = rmnorm(1,means,sigma)
    while(!all(c>x)) {
      x = rmnorm(1,means,sigma)	
    }
    young=young+select-1
    c.p <- c(c.p,c[1])
    c.c <- c(c.c,c[2])
    x.p <- c(x.p,x[1])
    x.c <- c(x.c,x[2])
  }
  list(c.p,c.c,x.p,x.c,young/n)
}

#######################################################################################
#Code to generate a random sample from a mixture of two normal distributions for a given parameter set. 
#######################################################################################
mixture.sample <- function(first.mu.p,first.mu.c,second.mu.p,second.mu.c,rho,sigmasq,n,frac.young,asct.range,diff.range,frac.first.mixture) {
  means=c(first.mu.p,first.mu.c,second.mu.p,second.mu.c);
  sigma=matrix(c(sigmasq,rho*sigmasq,rho*sigmasq,sigmasq),nrow=2)
  c.p = c.c = x.p = x.c = NULL;young=0
  while(length(c.p)<n) {
    select = 1+rbinom(1,1,frac.young)
    which.means = 1+2*rbinom(1,1,frac.first.mixture)
    c1 = runif(1,asct.range[1],asct.range[2]);d=runif(2,diff.range[1],diff.range[2])
    c2 = c1 - d[1]; c3 = c2 - d[2];
    c = c(c1,c2,c3)
    x = rmnorm(1,means[c(which.means,which.means+1)],sigma)
    if(all(c(c[select]>x[1],c[select+1]>x[2]))) {
      young=young+select-1
      c.p <- c(c.p,c[select])
      c.c <- c(c.c,c[select+1])
      x.p <- c(x.p,x[1])
      x.c <- c(x.c,x[2])
    }
  }
  list(c.p,c.c,x.p,x.c,young/n)
}
#################################################################################
#Comparison of 4 test statistics to test for anticipation via a simulation study
#THIS IS THE MAIN SIMULATION FUNCTION TO USE
#mu.p = mean parent AOO
#mu.c = mean child AOO
#rho = cor(x.p,x.c)
#sigmasq = var(x.p) = var(x.c)
#n = sample size per simulation
#tol = convergence tolerance to pass on to huang.vieland.mle
#nsim = number of simulations
#scheme = sampling scheme %in% c('random','single','multi','mixture')
#frac.young = used in single and multiplex ascertainment, the probability that the pair
#	selected for ascertainment (out of a young pair or an old pair) is the young pair
#quiet = should the simulation progress be printed?
#verbose = should everything be returned or only the rejection rates
#asct.range = of the form c(r1,r2), where r1<r2, giving the range of ages over which to uniformly draw
#	the ascertainment age c.p
#diff.range = of the form c(r1,r2), where r1<r2, giving the range of age difference over which to uniformly
#	draw d = c.p - c.c, ie given c.p, draw d and therefore c.c = c.p - d. 
#second.mu.p,second.mu.c = the second set of means of AOO for parent and child when the draw is from a mixture
#frac.first.mixture = the mixture proportion when the draw is from a mixture
##################################################################################
anticipation.sim <- function(mu.p=55,mu.c=55,rho=0,sigmasq=100,n=50,tol=0.005,nsim,scheme='random',frac.young=.5,quiet=F,verbose=T,asct.range=c(80,90),diff.range=c(20,30),second.mu.p,second.mu.c,frac.first.mixture) {
  begin = Sys.time();
  if(verbose) {
    #return the number of young pairs (doesn't apply when scheme = 'random')
    young = 0; 
    #return the avg age of ascertainment 
    avg.age = 0;
  }
  naive = matrix(nrow=nsim,ncol=3);#ttests
  colnames(naive) = c("muhat.x.p","muhat.x.c","se.diff");
  hv.results = matrix(nrow=nsim,ncol=6);#hv
  colnames(hv.results) = c("diff.muhats","se.diff","muhat.x.p","muhat.x.c","sigma.sq","rho");
  rab.results = matrix(nrow=nsim,ncol=2,);#rabinowitz
  colnames(rab.results) = c("teststat1","teststat2");
  which.profiled = NULL;
  which.diverged = which.negdefinfo = which.negvar = which.failed = NULL; 
  for(i in 1:nsim) {
    if(i%%50==0&!quiet) {cat('i =',i,'\n')};
    #Generate a truncated sample
    if(scheme=='random') {
      samp = random.sample(mu.p=mu.p,mu.c=mu.c,rho=rho,sigmasq=sigmasq,n=n,asct.range=asct.range,diff.range=diff.range)
    } else if(scheme=='single') {
      samp = singleplex.sample(mu.p=mu.p,mu.c=mu.c,rho=rho,sigmasq=sigmasq,n=n,frac.young=frac.young,asct.range=asct.range,diff.range=diff.range)
    } else if(scheme=='multi') {
      samp = multiplex.sample(mu.p=mu.p,mu.c=mu.c,rho=rho,sigmasq=sigmasq,n=n,frac.young=frac.young,asct.range=asct.range,diff.range=diff.range)
    } else if(scheme=='mixture') {
      samp = mixture.sample(first.mu.p=mu.p,first.mu.c=mu.c,second.mu.p=second.mu.p,second.mu.c=second.mu.c,rho=rho,sigmasq=sigmasq,n=n,frac.young=frac.young,asct.range=asct.range,diff.range=diff.range,frac.first.mixture=frac.first.mixture)			
    } else stop('Scheme must be random, single, multi or mixture');
    c.p = samp[[1]]; c.c = samp[[2]]
    x.p = samp[[3]]; x.c = samp[[4]]
    naive[i,] = c(mean(x.p),mean(x.c),sd(x.p-x.c)/sqrt(n))
    if(verbose) {
      young = young + samp[[5]]/nsim
      avg.age = avg.age + mean(c(c.p,c.c))/nsim
    }
    #Compute MLEs
    foo = huang.vieland.mle(c.p,c.c,x.p,x.c,tol);
    current.theta = foo$mle;
    #If the MLES didn't diverge, then check to make sure that the 
    #observed information is acceptable: ie that it is invertible 
    #and that the estimated variance of the differences is positive.
    #If it is not acceptable, estimate instead via profiling.
    if(foo$diverged==F) {
      obs.info = observed.info(c.p,c.c,x.p,x.c,current.theta);
      if(class(var<-try(solve(obs.info),T))=='try-error') {
        foo$diverged=T;
      } else {
        if((mudiff.var<-var[1,1]+var[2,2]-2*var[1,2])>0) {
          hv.results[i,2] = sqrt(mudiff.var);
        } else {
          foo$diverged=T;
        }
      }
    }
    #If the algorithm diverged, the observed
    #information was not positive semi definite, or the
    #estimated variance of the difference in muhatss was negative (all indicated by diverged==T), then
    #compute mles based on the profiling approach suggested by authors
    if(foo$diverged==T) {
      which.profiled = c(which.profiled,i);
      foo = huang.vieland.profile.mle(c.p,c.c,x.p,x.c,tol);
      current.theta = foo$mle
      ##if parameter estimates still diverged despite profiling, the method failed
      if(foo$diverged==T) {
        hv.results[i,2]=0; which.diverged = c(which.diverged,i);
      } else {
        obs.info = observed.info(c.p,c.c,x.p,x.c,current.theta);
        if(class(var<-try(solve(obs.info),T))!='try-error') {
          if((mudiff.var<-var[1,1]+var[2,2]-2*var[1,2])>0) {
            hv.results[i,2] = sqrt(mudiff.var);
          } else {
            ##if estimated variance of difference in muhats is negative, the method failed 
            hv.results[i,2] = 0; which.negvar = c(which.negvar,i);
          }
        } else {
          ##if the observed information is nonpositive definite, the method failed 
          hv.results[i,2] = 0; which.negdefinfo = c(which.negdefinfo,i)
        }
      }
    }
    hv.results[i,1] = current.theta[1]-current.theta[2];
    hv.results[i,3:6] = current.theta
    rab.results[i,1] = rabinowitz.test1(c.p,c.c,x.p,x.c) 
    rab.results[i,2] = rabinowitz.test2(c.p,c.c,x.p,x.c)
  }
  which.failed = c(which.diverged,which.negvar,which.negdefinfo);
  ngood = nsim - length(which.failed);
  if(is.null(which.failed)) {which.failed = nsim+1;} else {which.failed=sort(which.failed);}
  foo = data.frame(row.names=c('HV','Rab1','Rab2','TTest'))
  foo[,1] = c(sum(hv.results[-which.failed,1]/hv.results[-which.failed,2]>qnorm(0.90))/ngood,
              sum(rab.results[,1]>qnorm(0.90))/nsim,
              sum(rab.results[,2]>qnorm(0.90))/nsim,
              sum((naive[,1]-naive[,2])/naive[,3]>qt(.90,df=n-1))/nsim)
  foo[,2] = c(sum(hv.results[-which.failed,1]/hv.results[-which.failed,2]>qnorm(0.95))/ngood,
              sum(rab.results[,1]>qnorm(0.95))/nsim,
              sum(rab.results[,2]>qnorm(0.95))/nsim,
              sum((naive[,1]-naive[,2])/naive[,3]>qt(.95,df=n-1))/nsim)
  foo[,3] = c(sum(hv.results[-which.failed,1]/hv.results[-which.failed,2]>qnorm(0.99))/ngood,
              sum(rab.results[,1]>qnorm(0.99))/nsim,			
              sum(rab.results[,2]>qnorm(0.99))/nsim,
              sum((naive[,1]-naive[,2])/naive[,3]>qt(.99,df=n-1))/nsim)
  colnames(foo)=c(.1,.05,.01)			
  if(is.null(which.diverged)) {which.diverged = 0;}
  if(is.null(which.negvar)) {which.negvar = 0;}
  if(is.null(which.negdefinfo)) {which.negdefinfo = 0;}
  if(scheme=='mixture') {
    params = data.frame(row.names=c('first.mu.p','first.mu.c','second.mu.p','second.mu.c','frac.first.mixture','rho','sigmasq','n','tol','nsim','scheme','frac.young','asct.lower','asct.upper','diff.lower','diff.upper'))
    params[,1] = c(mu.p,mu.c,second.mu.p,second.mu.c,frac.first.mixture,rho,sigmasq,n,tol,nsim,scheme,frac.young,asct.range,diff.range)
    if(verbose) {
      list(time.elapsed=Sys.time()-begin,parameters=params,alpha=foo,diverged=which.profiled,which.diverged=which.diverged,which.negvar=which.negvar,which.negdefinfo=which.negdefinfo,pct.young=young,avg.age=avg.age,naive.results=naive,hv.results=hv.results,rab.results=rab.results)
    } else {
      list(time.elapsed=Sys.time()-begin,parameters=params,alpha=foo)
    }
  } else {
    params = data.frame(row.names=c('mu.p','mu.c','rho','sigmasq','n','tol','nsim','scheme','frac.young','asct.lower','asct.upper','diff.lower','diff.upper'))
    params[,1] = c(mu.p,mu.c,rho,sigmasq,n,tol,nsim,scheme,frac.young,asct.range,diff.range)
    if(verbose) {
      list(time.elapsed=Sys.time()-begin,parameters=params,alpha=foo,diverged=which.profiled,which.diverged=which.diverged,which.negvar=which.negvar,which.negdefinfo=which.negdefinfo,pct.young=young,avg.age=avg.age,naive.results=naive,hv.results=hv.results,rab.results=rab.results)
    } else {
      list(time.elapsed=Sys.time()-begin,parameters=params,alpha=foo)
    }
  }
}


##Examples

if(0) {
  begin = Sys.time();
  sim1 <- anticipation.sim(nsim=100,n=50,scheme='random')
  sim2 <- anticipation.sim(nsim=100,n=50,scheme='random',rho=0.5)
  sim3 <- anticipation.sim(nsim=100,n=50,scheme='random',rho=0.7)
  sim4 <- anticipation.sim(nsim=100,mu.c=50,n=50,scheme='random')
  sim5 <- anticipation.sim(nsim=100,mu.c=50,n=50,scheme='random',rho=0.5)
  sim6 <- anticipation.sim(nsim=100,mu.c=50,n=50,scheme='random',rho=0.7)
  sim7 <- anticipation.sim(nsim=100,mu.c=45,n=50,scheme='random')
  sim8 <- anticipation.sim(nsim=100,mu.c=45,n=50,scheme='random',rho=0.5)
  sim9 <- anticipation.sim(nsim=100,mu.c=45,n=50,scheme='random',rho=0.7)
  Sys.time()-begin;
}

