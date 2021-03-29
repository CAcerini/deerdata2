
library(cowplot)
theme_set(theme_cowplot())

#functions
logit <- function(x){
  
  log(x/(1-x))
}

logistic <- function(x){
  exp(x)/(exp(x)+1)
}

# combined model 1st winter survival plot --------------------------------------------
#this is the min model for combined first winter. see (3) 1st winter survival models.R for full break down
minmodel <- glm(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                  BirthYear, family=binomial, data=data2AN)


summary(minmodel)


#estimate and error of log strong in min model 
sEst<- coef(summary(minmodel))["log(Strongyles + 1)", "Estimate"]
sErr<- coef(summary(minmodel))["log(Strongyles + 1)", "Std. Error"]


#create variation in the coefficients to create a distribution of grandmeans for the observed data
#this is to be used to show variation in the model 
#using model matrix multiplication with the coefficients for the model and their associated standard error

M1<-model.matrix(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                         BirthYear, data2AN) #this creates the model matrix - model matrix shows us the observed data for each row in the data set 

M1 <- M1[,names(coef(minmodel))] #this reduces the model matrix to only include the columns observed in coeffs table ie. get rid of april

C<- summary(minmodel)$coefficients[,1:2]  #saving the model mean coefficients and their standard error 

#coefs <- apply(C, 1,  function(x) rnorm(1, mean=x[1], sd=x[2])) #this function generates a random coeff. from the mean and standard error 

#preds<- M1 %*% coefs  #creating predicted survival values for each row using matrix multiplication with coeffs. (this is = to the inner workings of the predict() function)
#mean of preds= grandmean
#using loop to create a distribution of grand means - this will be used to show model variation 

GMdist<- vector()
for(i in 1:100) {
  
  coefs <- apply(C, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  preds<- M1 %*% coefs
  
  gm <- mean(preds)
  
  GMdist[[i]] <- gm
}


#qplot(GMdist) #quick plot to show us distribution of grandmeans 

gm.g<-mean(GMdist) #the grand mean 

#doing the main line  
X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*+sEst #sEst = mean coeff for strongyles from the model. 

s<- mean(log(data2AN$Strongyles+1)) 

A<- gm.g-s*+sEst #using mean pred and mean SFEC (=grand mean) to calculate intercept 

Z <- A+ X*+sEst 

W<- logistic(Z) #converting back to data scale  

mainlinedf <- data.frame(X=X, Y=W)

#creating uncertainty 

linelist<- list()
for(i in 1:100) {
  
  B<- rnorm(1, mean=sEst, sd = sErr)  #slope uncertainty- getting a random slope using SFEC coeff and its error 
  
  Y <- X*B
  
  rgm<- GMdist[[i]] #random grand mean from the distribution of grandmeans
  
  A<- rgm-s*B #maybe change to sEst #no difference #random intercept
  
  Z <- A+ X*B #Yaxis values for random line
  
  W<- logistic(Z)  #convert to data scale
  
  linedf <- data.frame(X=X, Y=W) 
  
  linelist[[i]] <- linedf 
}

#linelist #100 different lines ('groups') each with 100 different x and y values.   


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100 groups



#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="black")+
  ylab("First winter survival") +
  ggtitle("combined 1st winter")


#assessing model accuracy with mean model predictions 

modelpreds <- predict(minmodel) 
predicted.classes <- ifelse(modelpreds > 0.5, "1", "0")
mean(predicted.classes == data2AN$survivedfirstwinter) #true/false -> mean. 
#0.6982249



# August model 1st winter survival plot -----------------------------------
modelminA <-  glm(survivedfirstwinter~log(Strongyles+1), family=binomial, data=Adata2)

summary(modelminA)

AsEst<- coef(summary(modelminA))["log(Strongyles + 1)", "Estimate"]
AsErr<- coef(summary(modelminA))["log(Strongyles + 1)", "Std. Error"]

#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MA<-model.matrix(survivedfirstwinter~log(Strongyles+1), Adata2) 
MA <- MA[,names(coef(modelminA))] 
CA<- summary(modelminA)$coefficients[,1:2] 

GMdist.A<- vector()
for(i in 1:100) {
  
  coefsA <- apply(CA, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsA<- MA %*% coefsA
  
  gmA <- mean(predsA)
  
  GMdist.A[[i]] <- gmA
}


gm.a<-mean(GMdist.A) #the grand mean 


#getting main line with estimates 
Xa <- seq(from = 0, to = max(log(Adata2$Strongyles + 1)), length.out = 100)  

Ya<- Xa*+AsEst

sa <- mean(log(Adata2$Strongyles+1))

Aa<- gm.a-sa*+AsEst

Za <- Aa+ Xa*+AsEst

Wa<- logistic(Za) 

mainlinedfA <- data.frame(X=Xa, Y=Wa)


#creating uncertainty

linelist.A<- list()
for(i in 1:100) {
  
  Ba<- rnorm(1, mean=AsEst, sd = AsErr)  #slopes
  
  Ya <- Xa*Ba
  
  rgmA<- GMdist.A[[i]] #random grand mean from the distribution of grandmeans
  
  Aa<- rgmA-sa*Ba
  
  Za <- Aa+ Xa*Ba
  
  Wa<- logistic(Za) 
  
  linedf.A <- data.frame(X=Xa, Y=Wa)
  
  linelist.A[[i]] <- linedf.A 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfA<- linelist.A %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=Adata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdfA, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfA, aes(x=X, y=Y), col="black") +
  ylab("First winter survival") +
  ggtitle("August firstwinter")



#assessing model accuracy with mean model predictions 

modelpredsA <- predict(modelminA) 
predicted.classes.A <- ifelse(modelpredsA > 0.5, "1", "0")
mean(predicted.classes.A == Adata2$survivedfirstwinter) #true/false -> mean. 
#0.7304965




# November model 1st winter survival plot ---------------------------------

modelminN <- glm(survivedfirstwinter~log(Strongyles+1)+BirthYear,
                 family=binomial, data=Ndata2)

summary(modelminN)


NsEst<- coef(summary(modelminN))["log(Strongyles + 1)", "Estimate"]
NsErr<- coef(summary(modelminN))["log(Strongyles + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MN<-model.matrix(survivedfirstwinter~log(Strongyles+1)+BirthYear, Ndata2) 
MN <- MN[,names(coef(modelminN))] 
CN<- summary(modelminN)$coefficients[,1:2] 

GMdist.N<- vector()
for(i in 1:100) {
  
  coefsN <- apply(CN, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsN<- MN %*% coefsN
  
  gmN <- mean(predsN)
  
  GMdist.N[[i]] <- gmN
}


gm.n<-mean(GMdist.N) #the grand mean 


#getting main line with estimates 
Xn <- seq(from = 0, to = max(log(Ndata2$Strongyles + 1)), length.out = 100)  

Yn<- Xn*+NsEst

sn <- mean(log(Ndata2$Strongyles+1))

An<- gm.n-sn*+NsEst

Zn <- An+ Xn*+NsEst

Wn<- logistic(Zn) 

mainlinedfN <- data.frame(X=Xn, Y=Wn)


#creating uncertainty

linelist.N<- list()
for(i in 1:100) {
  
  Bn<- rnorm(1, mean=NsEst, sd = NsErr)  #slopes
  
  Yn <- Xn*Bn
  
  rgmN<- GMdist.N[[i]] #random grand mean from the distribution of grandmeans
  
  An<- rgmN-sn*Bn
  
  Zn <- An+ Xn*Bn
  
  Wn<- logistic(Zn) 
  
  linedf.N <- data.frame(X=Xn, Y=Wn)

  linelist.N[[i]] <- linedf.N 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfN<- linelist.N %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=Ndata2, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdfN, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfN, aes(x=X, y=Y), col="black") +
  ylab("First winter survival") +
  ggtitle("November firstwinter")



#assessing model accuracy with mean model predictions 

modelpredsN <- predict(modelminN) 
predicted.classes.N <- ifelse(modelpredsN > 0.5, "1", "0")
mean(predicted.classes.N == Ndata2$survivedfirstwinter) #true/false -> mean. 
#0.6792453

