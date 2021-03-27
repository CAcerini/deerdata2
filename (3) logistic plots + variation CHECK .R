#G and standard deviation 

library(cowplot)
theme_set(theme_cowplot())

logit <- function(x){
  
  log(x/(1-x))
}
logistic <- function(x){
  exp(x)/(exp(x)+1)
}

#STOP WORKING. 
#YOU NEED TO FIGURE OUT WHAT IS CORRECT. .. LATER 

#remember the predicted values will be on the logistic scale ... bound by minus infinity and infiinty  

#minmodel 
minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)
#newmin model 
minmodel <- glm(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                  BirthYear, family=binomial, data=data2AN)


summary(minmodel)


#estimate and error of log strong in min model 
sEst<- coef(summary(minmodel))["log(Strongyles + 1)", "Estimate"]
sErr<- coef(summary(minmodel))["log(Strongyles + 1)", "Std. Error"]


# using a range of predicted values ---------------------------------------
predv <- expand.grid(Strongyles = seq(from=0, to = 194, by=1),
                     BirthYear = (2016:2019))


predv$BirthYear <- as.factor(predv$BirthYear)

predDF<- predict(minmodel , newdata=predv, se.fit = T) %>% as.data.frame()

#APPLY VS. LOOP - RANDOM PRED. VALUES 
#using the apply function
randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))


#using a loop 
output <- list()  
for (i in 1:nrow(predDF)) {          
  output[[i]] <- rnorm(1, mean=predDF$fit[i], sd =predDF$se.fit[i])    
}

#lets use the randpred stuff... 

m<- mean(randpred)
sd(randpred)

#GRAND MEAN DISTRIBUTION
#doing that stuff, but 100 times to get a distribution of grand means  
GMdist<- vector()
for(i in 1:100) {
  
  randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm <- mean(randpred)
  
  GMdist[[i]] <- gm 
}

gm.se<- sd(GMdist)


gm<-mean(GMdist)

#MAIN LINE
#using gm, to calculate intercept - and get main line.  
X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

#plot(Y~X) #- straight line


lgm<- logit(gm) #=v

s<- mean(log(predv$Strongyles+1)) #i think this is wrong

A<- lgm-s*-0.25168

Z <- A+ X*-0.25168

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W<- logistic(Z) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf <- data.frame(X=X, Y=W)

#plotting the line along with raw data points  
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=mainlinedf, aes(x=X, y=Y))  






# creating uncertainty - universe 1.0  ------------------------------------


#creating uncertainty 

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  Y <- X*B
  
  randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm <- mean(randpred)
  
  lgm<- logit(gm) #=v
  
  s<- mean(log(predv$Strongyles+1)) #i think this is wrong
  
  A<- lgm-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

#linelist #100 different x and y values in 100 different groups  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups


#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 







# creating uncertainty - an alternate universe - possibly this one yes --------------------------------------------------

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  Y <- X*B
  
  mgm<- rnorm(1, mean=gm, sd = gm.se) #using GMdist, to have variation in intercept. 

  lgm<- logit(mgm) 
  
  s<- mean(log(predv$Strongyles+1)) #i think this is wrong
  
  A<- lgm-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

#linelist #100 different x and y values in 100 different groups - 100 different lines  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty (100 grey lines) and main line (blue)
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") +
  geom_point(aes(x=mean(log(predv$Strongyles+1)), y=gm), colour="red") +
  geom_point(aes(x=mean(log(data2AN$Strongyles+1)), y=gm), colour="red")





mean(log(data2AN$Strongyles+1))











# trying to get the correct grand mean THIS IS CLOSER TO BEING CORRECT ? Y SO MUCH VARIATION IN INTERCEPT ? -----------------------------------
predv <- expand.grid(Strongyles = seq(from=0, to = 194, by=1),
                     BirthYear = (2016:2019))


predv$BirthYear <- as.factor(predv$BirthYear)

predDF<- predict(minmodel , newdata=predv, se.fit = T) %>% as.data.frame()

#APPLY VS. LOOP - RANDOM PRED. VALUES 
#using the apply function
randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))


#using a loop 
output <- list()  
for (i in 1:nrow(predDF)) {          
  output[[i]] <- rnorm(1, mean=predDF$fit[i], sd =predDF$se.fit[i])    
}

#lets use the apply// randpred stuff... 

m<- mean(randpred)
sd(randpred)

#GRAND MEAN DISTRIBUTION
#doing that stuff, but 100 times to get a distribution of grand means  
GMdist<- vector()
for(i in 1:100) {
  
  randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm <- mean(randpred)
  
  GMdist[[i]] <- gm 
}

gm.se<- sd(GMdist)


gm<-mean(GMdist)

#MAIN LINE
#using gm, to calculate intercept - and get main line.  
X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

#plot(Y~X) #- straight line


lgm<- logit(gm) # i think doing this could be wrong 

s<- mean(log(data2AN$Strongyles+1)) #alternate - this is bit u have changed 

A<- lgm-s*-0.25168

Z <- A+ X*-0.25168

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W<- logistic(Z) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf <- data.frame(X=X, Y=W)

#plotting the line along with raw data points  
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=mainlinedf, aes(x=X, y=Y))  

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  Y <- X*B
  
  mgm<- rnorm(1, mean=gm, sd = gm.se) #using GMdist, to have variation in intercept. 
  
  lgm<- logit(mgm) 
  
  s<- mean(log(data2AN$Strongyles+1)) #alternate - this is bit u have changed 
  
  A<- lgm-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

#linelist #100 different x and y values in 100 different groups - 100 different lines  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty (100 grey lines) and main line (blue)
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") +
  geom_point(aes(x=mean(log(predv$Strongyles+1)), y=gm), colour="red") +
  geom_point(aes(x=mean(log(data2AN$Strongyles+1)), y=gm), colour="red")




#THIS IS WHAT YOU WANT VVVV

# THIS ONE using predicted values form data2AN values ------------------------------
#NB// THIS WENT WRONG NB//bc u were transforming ?

#using data2AN data frame to get predicted values and the  error 
G<- predict(minmodel, se.fit= T) %>% as.data.frame()
mean(G$fit)
#Go through and use that as the standard deviation to randomly pick a value for each row of the data

#newvalue <- rnorm(1, mean=G$fit, sd=G$se.fit) #just gives one value

#G <- G %>% mutate(newvalue = rnorm(1, mean=fit, sd=se.fit)) #this just gives same value for each row - why?


#VALUE <- apply(G, 1,  rnorm(1, mean="fit", sd="se.fit")) #doesnt work 

#this seems to work.... correct expected range  
Gpreds <- apply(G, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))


#GLpred <- numeric()  
#for (i in 1:nrow(G)) {GLpred[[i]] <- rnorm(1, mean=G$fit[i], sd =G$se.fit[i])}

mean(GLpred)
range(Gpreds)
range(GLpred)

m<- mean(Gpreds) # the mean is on the logistic scale bound by minus infinity and infinity

logistic(m) 

#logit(m) #you cant do this,,, does this mean the other stuff above is wrong too - changed below 

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
  
  A.g<- mgm.g-s.g*sEst
  
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

model.matrix


# rand preds not transforming ---------------------------------------------


#so if what you have done there (not logit transforming mean cause its already on logistic scale, 
#then lets re-do the predv version and see what that gives)
#I STILL THINK U WANT TO BE USING THE PREDICT VALUES OF ACTUAL DATA NOT THE RANDOM  STUFF 

# not transforming  -------------------------------------------------------
predv <- expand.grid(Strongyles = seq(from=0, to = 194, by=1),
                     BirthYear = (2016:2019))


predv$BirthYear <- as.factor(predv$BirthYear)

predDF<- predict(minmodel , newdata=predv, se.fit = T) %>% as.data.frame()

#APPLY VS. LOOP - RANDOM PRED. VALUES 
#using the apply function
randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))


#using a loop 
output <- list()  
for (i in 1:nrow(predDF)) {          
  output[[i]] <- rnorm(1, mean=predDF$fit[i], sd =predDF$se.fit[i])    
}

#lets use the apply// randpred stuff... 
m<- mean(randpred)
sd(randpred)

#GRAND MEAN DISTRIBUTION
#doing that stuff, but 100 times to get a distribution of grand means  
GMdist<- vector()
for(i in 1:100) {
  
  randpred <- apply(predDF, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  gm <- mean(randpred)
  
  GMdist[[i]] <- gm 
}

gm.se<- sd(GMdist)


gm<-mean(GMdist)

#MAIN LINE
#using gm, to calculate intercept - and get main line.  
X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

#plot(Y~X) #- straight line


#lgm<- logit(gm) #dont need bc i think gm is already on logistic scale

s<- mean(log(data2AN$Strongyles+1)) #alternate - this is bit u have changed 

A<- gm-s*-0.25168

Z <- A+ X*-0.25168

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

W<- logistic(Z) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf <- data.frame(X=X, Y=W)

#plotting the line along with raw data points  
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=mainlinedf, aes(x=X, y=Y))  

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  Y <- X*B
  
  mgm<- rnorm(1, mean=gm, sd = gm.se) #using GMdist, to have variation in intercept. 
  
  #lgm<- logit(mgm) # dont need bc i think gm is already on logistic scale
  
  s<- mean(log(data2AN$Strongyles+1)) #alternate - this is bit u have changed 
  
  A<- mgm-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

#linelist #100 different x and y values in 100 different groups - 100 different lines  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty (100 grey lines) and main line (blue)
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") +
  geom_point(aes(x=mean(log(predv$Strongyles+1)), y=logistic(gm)), colour="red") +
  geom_point(aes(x=mean(log(data2AN$Strongyles+1)), y=logistic(gm)), colour="red") +
  geom_point(aes(x=testmx, y=testmy), colour="green")


# other method - lmao no  ------------------------------------------------------------

minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)
summary(minmodel)
#mean survival from data 

X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

#plot(Y~X) - straight line

logit <- function(x){
  
  log(x/(1-x))
}


m<- mean(data2AN$survivedfirstwinter)

v<- logit(m)

s <- mean(log(data2AN$Strongyles+1))

A<- v-s*-0.25168

Z <- A+ X*-0.25168

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

logistic <- function(x){
  exp(x)/(exp(x)+1)
}

W<- logistic(Z) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf <- data.frame(X=X, Y=W)

#plotting the line along with raw data points  
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=mainlinedf, aes(x=X, y=Y))  


#creating uncertainty - this is just showing the uncertainity in the gradient- it will still all be plot to be through the intercept

#X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

#m<- mean(data2AN$survivedfirstwinter)

#v<- logit(m)

#s <- mean(log(data2AN$Strongyles+1))

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  
  m<- rnorm(1, mean=mean(data2AN$survivedfirstwinter), sd= sd(data2AN$survivedfirstwinter))
  
  v<- logit(m)
  
  s <- mean(log(data2AN$Strongyles+1))
  
  Y <- X*B
  
  A<- v-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}
#warnings wihth not being able to log(x... ) transform the thign . 
#linelist #100 different x and y values in 100 different groups  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100

#plot with just uncertainty
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3) 

#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 




