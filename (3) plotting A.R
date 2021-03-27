modelminA <-  glm(survivedfirstwinter~log(Strongyles+1), family=binomial, data=Adata2)

tab_model(modelminA)
summary(modelminA)

write.csv(summary(modelminA)$coefficients, file="survivalAmin.csv")

AsEst<- coef(summary(modelminA))["log(Strongyles + 1)", "Estimate"]
AsErr<- coef(summary(modelminA))["log(Strongyles + 1)", "Std. Error"]


#getting main line with estimates 
Xa <- seq(from = 0, to = max(log(Adata2$Strongyles + 1)), length.out = 100)  

mean(Xa)

Ya<- Xa*+AsEst

ma<- mean(Adata2$survivedfirstwinter)

va<- logit(ma)

sa <- mean(log(Adata2$Strongyles+1))

Aa<- va-sa*+AsEst

Za <- Aa+ Xa*+AsEst

Wa<- logistic(Za) 

mainlinedfA <- data.frame(X=Xa, Y=Wa)

plot(Xa, Za)

#creating uncertainty

linelistA<- list()
for(i in 1:100) {
  
  print(i)
  
  Ba<- rnorm(1, mean=AsEst, sd = AsErr)  #slopes
  
  Ya <- Xa*Ba
  
  Aa<- va-sa*Ba
  
  Za <- Aa+ Xa*Ba
  
  Wa<- logistic(Za) 
  
  linedfA <- data.frame(X=Xa, Y=Wa)
  
  linelistA[[i]] <- linedfA 
}

linelistA #100 different x and y values in 100 different groups  


bigdfA<- linelistA %>% bind_rows(.id = "group") #puts it in one group 


#plot without the uncertainty
ggplot(data=Adata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=mainlinedfA, aes(x=X, y=Y), col="steelblue") +
  ylab("First winter survival") 


#plot with the uncertainty 
ggplot(data=Adata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdfA, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfA, aes(x=X, y=Y), col="steelblue") +
  ylab("First winter survival") 



# uncertainty . 2 ---------------------------------------------------------

Ap<- predict(modelminA, se.fit= T) %>% as.data.frame()

#Go through and use that as the standard deviation to randomly pick a value for each row of the data

#newvalue <- rnorm(1, mean=G$fit, sd=G$se.fit) #just gives one value

#G <- G %>% mutate(newvalue = rnorm(1, mean=fit, sd=se.fit)) #this just gives same value for each row - why?


#VALUE <- apply(G, 1,  rnorm(1, mean="fit", sd="se.fit")) #doesnt work 

#this seems to work.... correct expected range  
Apreds <- apply(Ap, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))



#doing that stuff, but 100 times to get a distribution of grand means  
GMdist.A<- vector()
for(i in 1:100) {
  
  Apreds <- apply(Ap, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm.A <- mean(Apreds)
  
  GMdist.A[[i]] <- gm.A 
}

gm.se.A<- sd(GMdist.A)
seA<- sd(Apreds)

gm.A.f<-mean(GMdist.A) #this is on logistic scale  



#doing the main line .g 
X.A <- seq(from = 0, to = max(log(Adata2$Strongyles + 1)), length.out = 100)  

Y.A <- X.A*+AsEst

#plot(Y~X) #- straight line

#lgm.g<- logit(gm.g) #dont need this as its on the logistic scale already  

s.A<- mean(log(Adata2$Strongyles+1)) #i think this is wrong

A.A<- gm.A.f-s.A*+AsEst

Z.A <- A.A+ X.A*+AsEst

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W.A<- logistic(Z.A) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf.A <- data.frame(X=X.A, Y=W.A)

#creating uncertainty with method .2 


linelist.A<- list()
for(i in 1:100) {
  
  B.A<- rnorm(1, mean=AsEst, sd = AsErr)  #slopes
  
  Y.A <- X.A*B.A
  
  mgm.A<- rnorm(1, mean=gm.A.f, sd = gm.se.A)
  
  #lgm.g<- logit(mgm.g) #dont need this as its on the logistic scale already?
  
  s.A<- mean(log(Adata2$Strongyles+1)) #i think this is wrong
  
  A.A<- mgm.A-s.A*B.A
  
  Z.A <- A.A+ X.A*B.A
  
  W.A<- logistic(Z.A) 
  
  linedf.A <- data.frame(X=X.A, Y=W.A)
  
  linelist.A[[i]] <- linedf.A 
}

#linelist #100 different x and y values in 100 different groups  


bigdf.A<- linelist.A %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty and main line
ggplot(data=Adata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  #geom_line(data=bigdf.A, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf.A, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 
