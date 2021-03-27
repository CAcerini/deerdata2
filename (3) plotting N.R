modelminN <- glm(survivedfirstwinter~log(Strongyles+1)+BirthYear,
                 family=binomial, data=Ndata2)

summary(modelminN)


NsEst<- coef(summary(modelminN))["log(Strongyles + 1)", "Estimate"]
NsErr<- coef(summary(modelminN))["log(Strongyles + 1)", "Std. Error"]


write.csv(summary(modelminN)$coefficients, file="survivalN.csv")

#getting main line with estimates 
Xn <- seq(from = 0, to = max(log(Ndata2$Strongyles + 1)), length.out = 100)  

mean(Xn)

Yn <- Xn*+NsEst

mn<- mean(Ndata2$survivedfirstwinter)

vn<- logit(mn)

sn <- mean(log(Ndata2$Strongyles+1))

An<- vn-sn*+NsEst

Zn <- An+ Xn*+NsEst 

Wn<- logistic(Zn) 

mainlinedfN <- data.frame(X=Xn, Y=Wn)



#creating uncertainty

linelistN<- list()
for(i in 1:100) {
  
  print(i)
  
  Bn<- rnorm(1, mean=NsEst , sd = NsErr)  #slopes
  
  Yn <- Xn*Bn
  
  An<- vn-sn*Bn
  
  Zn <- An+ Xn*Bn
  
  Wn<- logistic(Zn) 
  
  linedfN <- data.frame(X=Xn, Y=Wn)
  
  linelistN[[i]] <- linedfN 
}

#linelistN #100 different x and y values in 100 different groups  


bigdfN<- linelistN %>% bind_rows(.id = "group") #puts it in one group 



#plot without the uncertainty
ggplot(data=Ndata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=mainlinedfN, aes(x=X, y=Y), col="steelblue") +
  ylab("First winter survival") 


#plot with the uncertainty 
ggplot(data=Ndata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdfN, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfN, aes(x=X, y=Y), col="steelblue") +
  ylab("First winter survival") 



# plotting with uncertainty other... --------------------------------------


Np<- predict(modelminN, se.fit= T) %>% as.data.frame()

#Go through and use that as the standard deviation to randomly pick a value for each row of the data

#newvalue <- rnorm(1, mean=G$fit, sd=G$se.fit) #just gives one value

#G <- G %>% mutate(newvalue = rnorm(1, mean=fit, sd=se.fit)) #this just gives same value for each row - why?


#VALUE <- apply(G, 1,  rnorm(1, mean="fit", sd="se.fit")) #doesnt work 

#this seems to work.... correct expected range  
Npreds <- apply(Np, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))



#doing that stuff, but 100 times to get a distribution of grand means  
GMdist.N<- vector()
for(i in 1:100) {
  
  Npreds <- apply(Np, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm.N <- mean(Npreds)
  
  GMdist.N[[i]] <- gm.N 
}

gm.se.N<- sd(GMdist.N)


gm.N.f<-mean(GMdist.N) #this is on logistic scale  



#doing the main line .g 
X.N <- seq(from = 0, to = max(log(Ndata2$Strongyles + 1)), length.out = 100)  

Y.N <- X.N*+NsEst

#plot(Y~X) #- straight line

#lgm.g<- logit(gm.g) #dont need this as its on the logistic scale already  

s.N<- mean(log(Ndata2$Strongyles+1)) #i think this is wrong

A.N<- gm.N.f-s.N*+NsEst

Z.N <- A.N+ X.N*+NsEst

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W.N<- logistic(Z.N) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf.N <- data.frame(X=X.N, Y=W.N)

#creating uncertainty with method .2 


linelist.N<- list()
for(i in 1:100) {
  
  B.N<- rnorm(1, mean=NsEst, sd = NsErr)  #slopes
  
  Y.N <- X.N*B.N
  
  mgm.N<- rnorm(1, mean=gm.N.f, sd = gm.se.N)
  
  #lgm.g<- logit(mgm.g) #dont need this as its on the logistic scale already?
  
  s.N<- mean(log(Ndata2$Strongyles+1)) #i think this is wrong
  
  A.N<- mgm.N-s.N*B.N
  
  Z.N <- A.N+ X.N*B.N
  
  W.N<- logistic(Z.N) 
  
  linedf.N <- data.frame(X=X.N, Y=W.N)
  
  linelist.N[[i]] <- linedf.N 
}

#linelist #100 different x and y values in 100 different groups  


bigdf.N<- linelist.N %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty and main line
ggplot(data=Ndata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf.N, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf.N, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 
