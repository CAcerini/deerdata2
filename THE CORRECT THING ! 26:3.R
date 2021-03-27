
# old stuff.. see bellow- please sort out and comment b4 u forget ---------


#using data2AN data frame to get predicted values and the  error 
G<- predict(minmodel, se.fit= T) %>% as.data.frame()
mean(G$fit)
 
Gpreds <- apply(G, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))

m<- mean(Gpreds) # the mean is on the logistic scale bound by minus infinity and infinity

#doing that stuff, but 100 times to get a distribution of grand means  
GMdist.G<- vector()
for(i in 1:100) {
  
  Gpreds <- apply(G, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm.g <- mean(Gpreds)
  
  GMdist.G[[i]] <- gm.g 
}

gm.se.g<- sd(GMdist.G)

se<- sd(logistic(Gpreds))

qplot(GMdist.G)

gm.g<-mean(GMdist.G) #this is on logistic scale  
sd9
sd(data2AN$survivedfirstwinter)

#doing the main line .g 
X.g <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y.g <- X.g*+sEst

#plot(Y~X) #- straight line

#lgm.g<- logit(gm.g) #dont need this as its on the logistic scale already  

s.g<- mean(log(data2AN$Strongyles+1)) #i think this is wrong

A.g<- gm.g-s.g*+sEst

Z.g <- A.g+ X.g*+sEst

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W.g<- logistic(Z.g) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf.g <- data.frame(X=X.g, Y=W.g)

#creating uncertainty with method .2 


linelist.g<- list()
for(i in 1:100) {
  
  B.g<- rnorm(1, mean=sEst, sd = sErr)  #slopes
  
  Y.g <- X.g*B.g
  
  mgm.g<- GMdist.G[[i]]
  
  #lgm.g<- logit(mgm.g) #dont need this as its on the logistic scale already?
  
  s.g<- mean(log(data2AN$Strongyles+1)) #i think this is wrong
  
  A.g<- mgm.g-s.g*B.g
  
  Z.g <- A.g+ X.g*B.g
  
  W.g<- logistic(Z.g) 
  
  linedf.g <- data.frame(X=X.g, Y=W.g)
  
  linelist.g[[i]] <- linedf.g 
}

#linelist #100 different x and y values in 100 different groups  


bigdf.g<- linelist.g %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



testmx <- mean(log(data2AN$Strongyles+1))
testmy <- mean(data2AN$survivedfirstwinter)

#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdf.g, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf.g, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 

#geom_point(aes(x=mean(log(predv$Strongyles+1)), y=gm), colour="orange") +
#geom_point(aes(x=mean(log(data2AN$Strongyles+1)), y=logistic(gm.g)), colour="pink") +
#geom_point(aes(x=testmx, y=testmy), colour="green")

#geom_point(aes(x=mean(log(data2AN$Strongyles+1)), y=logistic(gm.g)), colour="red")+


#logistic(gm.g)


#assessing model accuracy 
predicted.classes <- ifelse(G$fit > 0.5, "1", "0")

mean(predicted.classes == data2AN$survivedfirstwinter)


# this is the correct thing!!! --------------------------------------------


#model matrix... 

M1<-model.matrix(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                         BirthYear, data2AN)

M1 <- M1[,names(coef(minmodel))]

C<- summary(minmodel)$coefficients[,1:2] 

coefs <- apply(C, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))

preds<- M1 %*% coefs

GMdist.C<- vector()
for(i in 1:100) {
  
  coefs <- apply(C, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  preds<- M1 %*% coefs
  
  gm <- mean(preds)
  
  GMdist.C[[i]] <- gm
}


#qplot(GMdist.C)

gm.g<-mean(GMdist.C) 

#doing the main line  
X.g <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y.g <- X.g*+sEst

s.g<- mean(log(data2AN$Strongyles+1)) 

A.g<- gm.g-s.g*+sEst

Z.g <- A.g+ X.g*+sEst

W.g<- logistic(Z.g) 

mainlinedf.g <- data.frame(X=X.g, Y=W.g)

#creating uncertainty with method .2 

linelist.g<- list()
for(i in 1:100) {
  
  B.g<- rnorm(1, mean=sEst, sd = sErr)  #slopes
  
  Y.g <- X.g*B.g
  
  mgm.g<- GMdist.C[[i]] #random gm
  
  s.g<- mean(log(data2AN$Strongyles+1)) 
  
  A.g<- mgm.g-s.g*B.g #maybe change to sEst #no difference
  
  Z.g <- A.g+ X.g*B.g
  
  W.g<- logistic(Z.g) 
  
  linedf.g <- data.frame(X=X.g, Y=W.g)
  
  linelist.g[[i]] <- linedf.g 
}

#linelist #100 different x and y values in 100 different groups  


bigdf.g<- linelist.g %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdf.g, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf.g, aes(x=X, y=Y), col="black")+
  ylab("First winter survival") 


#assessing model accuracy with mean model predictions 

modelpreds <- predict(minmodel) 
predicted.classes <- ifelse(modelpreds > 0.5, "1", "0")
mean(predicted.classes == data2AN$survivedfirstwinter)
#0.6982249
