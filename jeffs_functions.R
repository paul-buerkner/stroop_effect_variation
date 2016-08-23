myF=function(dat) {
  # Jeffs ANOVA
  K=table(dat$sub,dat$cond)
  N=sum(K)
  I=dim(K)[1]
  J=dim(K)[2]
  kstar=K[,1]*K[,2]/(K[,1]+K[,2])
  condMean=tapply(dat$y,list(dat$sub,dat$cond),mean)
  d=condMean[,2]-condMean[,1]
  dstar=d*sqrt(kstar)
  s1=var(dstar)
  ss=sum((K-1)*tapply(dat$y,list(dat$sub,dat$cond),var))
  s2=ss/(N-I*J)  
  return(s1/s2)
}