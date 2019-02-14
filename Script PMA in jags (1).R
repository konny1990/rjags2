install.packages("R2jags")
install.packages("jags")
library(R2jags)
library(jags)
help(jags)


#######################
#this is the data
#######################

#to prec to evala afou to ipologise i R, giati den to angnorize meta to jag
mydata = list(ns=3,m=cbind(c(187.5,193.4,161.3),c(165.6,171.9,180.5)),sd=cbind(c(12.3, 8.5,6.9),c(11.9, 10.8, 11.1)),n=cbind(c(12,13,14),c(14,15,16)))
#mydata
MD=c()
mydata$ns
#Ypologismos tou SMD
Swithinar=c()
Swithinpar= c()
Swithin=c()
SMD=c()
Var=c()
prec=c()
for (i in 1:mydata$ns) {
  #MD=M2-M1
  MD[i]=mydata$m[i,2]-mydata$m[i,1]
  #Arithmitis tou Swithin
  Swithinar[i]=(mydata$n[i,1]-1)*((mydata$sd[i,1])^2)+(mydata$n[i,2]-1)*((mydata$sd[i,2])^2)
  #paranomastis tou Swithin
  Swithinpar[i]=(mydata$n[i,1])+(mydata$n[i,2])-2
  #Swithin
  Swithin[i]=sqrt(Swithinar[i]/Swithinpar[i])
  #SMD
  SMD[i]=MD[i]/Swithin[i]
  #Var tou Smd
  Var[i]=((mydata$n[i,1]+mydata$n[i,2])/(mydata$n[i,1]*mydata$n[i,2]))+(SMD[i]^2)/(2*(mydata$n[i,1]+mydata$n[i,2]))
  prec[i]=1/Var[i]
}


mydata=list(ns=ns,m=m,sd=sd, n=n)
#######################
#then make the model
#

########################
PMAcontinuous=function() {
  
  for(i in 1:ns) { 
    #likelihood
    
    SMD[i]~ dnorm(theta[i],prec[i])
    theta[i]~dnorm(mean,prec.tau)
    
    }
  
    #prior distributions
      tau ~ dunif(0,1)   #dnorm(0,100)%_%T(0,)                                 
      prec.tau<- 1/pow(tau,2)
      mean ~ dnorm(0,0.01)
}#end of model#####################
# initial values
#######################

initialval = NULL
#initialval = list(list(tau=0.2,mean=0.3))

#######################
# run the model
#######################
PMAinJAGS<- jags(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 2, n.iter = 10000, n.burnin = 1000, DIC=F, model.file = PMAcontinuous)


#results
print(PMAinJAGS)

#///////////////////////////

#######################
# parallelising
#######################

start_time <- Sys.time()
PMAinJAGS<- jags(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 4, n.iter = 100000, n.burnin = 1000, DIC=F, model.file = PMAcontinuous)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
PMAinJAGS<- jags.parallel(mydata,initialval,parameters.to.save = c("tau","mean"), n.chains = 4, n.iter = 100000, n.burnin = 1000, DIC=F, model.file = PMAcontinuous)
end_time <- Sys.time()
end_time - start_time




