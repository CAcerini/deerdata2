library(cowplot)
theme_set(theme_cowplot())

#functions
logit <- function(x){
  
  log(x/(1-x))
}

logistic <- function(x){
  exp(x)/(exp(x)+1)
}

#first wint 

# August model 1st winter survival plot -----------------------------------
modelA <-  glm(survivedfirstwinter~log(Strongyles+1)+MassPerPellet+Sex+BirthYear, family=binomial, data=Adata2)
#modelA <- glm(survivedfirstwinter~log(Strongyles+1), family=binomial, data = Adata2) #when u run w/ this looks better, 
#same applys for ones below- work with ur simple models, doing something wrong at gmdist loop?

summary(modelA)

AsEst<- coef(summary(modelA))["log(Strongyles + 1)", "Estimate"]
AsErr<- coef(summary(modelA))["log(Strongyles + 1)", "Std. Error"]

#Adata2 %>% write.csv("greg.csv")

#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MA<-model.matrix(survivedfirstwinter~log(Strongyles+1)+MassPerPellet+Sex+BirthYear, Adata2) 
MA <- MA[,names(coef(modelA))] 
CA<- summary(modelA)$coefficients[,1:2] 

GMdist.A<- vector()
for(i in 1:100) {
  
  coefsA <- apply(CA, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsA<- MA %*% coefsA
  
  gmA <- mean(predsA)
  
  GMdist.A[[i]] <- gmA
}
#plot(unlist(GMdist.A))

gm.a<-mean(GMdist.A) #the grand mean 
#Adata2$survivedfirstwinter %>% qplot(predsA)

#getting main line with estimates 
Xa <- seq(from = 0, to = max(log(Adata2$Strongyles + 1)), length.out = 100)  

#Ya<- Xa*+AsEst

sa <- mean(log(Adata2$Strongyles+1))

Aa<- gm.a-sa*+AsEst

Za <- Aa+ Xa*+AsEst

Wa<- logistic(Za) 

mainlinedfA <- data.frame(X=Xa, Y=Wa)


#creating uncertainty

linelist.A<- list()
for(i in 1:100) {
  
  Ba<- rnorm(1, mean=AsEst, sd = AsErr)  #slopes
  
  rgmA<- GMdist.A[[i]] #random grand mean from the distribution of grandmeans
  
  rAa<- rgmA-sa*Ba
  
  rZa <- rAa+ Xa*Ba
  
  rWa<- logistic(rZa) 
  
  linedf.A <- data.frame(X=Xa, Y=rWa)
  
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


#assessing model accuracy with mean model predictions . 

modelpredsA <- predict(modelA) 
predicted.classes.A <- ifelse(modelA > 0.5, "1", "0")
mean(predicted.classes.A == Adata2$survivedfirstwinter) #true/false -> mean. 

#4 latr -changes
y <- Aa+ log(40+1)*+AsEst
logistic(y)
#when calves had 0 Strongyle EPG, they had a >93.5% chance of survival. 
#calves with >40EPG (25.5% of samples), had a <59.3% chance of survival. 
sum(Adata2$Strongyles>40)
(36/141)*100 


# November model 1st winter survival plot ---------------------------------

modelN <- glm(survivedfirstwinter~log(Strongyles+1)+log(Flukes+1)+MassPerPellet+Sex+BirthYear,
                 family=binomial, data=Ndata2)

summary(modelN)


NsEst<- coef(summary(modelN))["log(Strongyles + 1)", "Estimate"]
NsErr<- coef(summary(modelN))["log(Strongyles + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MN<-model.matrix(survivedfirstwinter~log(Strongyles+1)+log(Flukes+1)+MassPerPellet+Sex+BirthYear, Ndata2) 
MN <- MN[,names(coef(modelN))] 
CN<- summary(modelN)$coefficients[,1:2] 

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

#Yn<- Xn*+NsEst

sn <- mean(log(Ndata2$Strongyles+1))

An<- gm.n-sn*+NsEst

Zn <- An+ Xn*+NsEst

Wn<- logistic(Zn) 

mainlinedfN <- data.frame(X=Xn, Y=Wn)


#creating uncertainty

linelist.N<- list()
for(i in 1:100) {
  
  Bn<- rnorm(1, mean=NsEst, sd = NsErr)  #slopes
  
  rgmN<- GMdist.N[[i]] #random grand mean from the distribution of grandmeans
  
  rAn<- rgmN-sn*Bn
  
  rZn <- rAn+ Xn*Bn
  
  rWn<- logistic(rZn) 
  
  linedf.N <- data.frame(X=Xn, Y=rWn)

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

modelpredsN <- predict(modelN) 
predicted.classes.N <- ifelse(modelpredsN > 0.5, "1", "0")
mean(predicted.classes.N == Ndata2$survivedfirstwinter) #true/false -> mean. 
#0.6792453


y <- An+ log(10+1)*+NsEst
logistic(y)
#when calves had 0 Strongyle EPG, they had a >90.1% chance of survival. 
#calves with >10EPG (13% of samples), had a <50% chance of survival. 

sum(Ndata2$Strongyles>10)
(21/159)*100 



#sec wint 

# April sec wint strong plot ----------------------------------------------


modelAP2 <- glm(survivedsecondwinter~log(Strongyles+1)+log(Elaphostrongylus+1)+log(Flukes+1)+
                      MassPerPellet+Sex+deeryear, family=binomial, data=APdata22W)
summary(modelAP2)


APsEst2<- coef(summary(modelAP2))["log(Strongyles + 1)", "Estimate"]
APsErr2<- coef(summary(modelAP2))["log(Strongyles + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MAP2<-model.matrix(survivedsecondwinter~log(Strongyles+1)+log(Elaphostrongylus+1)+log(Flukes+1)+
                     MassPerPellet+Sex+deeryear, APdata22W) 
MAP2 <- MAP2[,names(coef(modelAP2))] 
CAP2<- summary(modelAP2)$coefficients[,1:2] 

GMdist.AP2<- vector()
for(i in 1:100) {
  
  coefsAP2 <- apply(CAP2, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsAP2<- MAP2 %*% coefsAP2
  
  gmAP2 <- mean(predsAP2)
  
  GMdist.AP2[[i]] <- gmAP2
}


gm.ap2<-mean(GMdist.AP2) #the grand mean 


#getting main line with estimates 
Xap2 <- seq(from = 0, to = max(log(APdata22W$Strongyles + 1)), length.out = 100)  

sap2 <- mean(log(APdata22W$Strongyles+1))

Aap2<- gm.ap2-sap2*+APsEst2

Zap2 <- Aap2+ Xap2*+APsEst2

Wap2<- logistic(Zap2) 

mainlinedfAP2 <- data.frame(X=Xap2, Y=Wap2)


#creating uncertainty

linelist.AP2<- list()
for(i in 1:100) {
  
  Bap2<- rnorm(1, mean=APsEst2, sd = APsErr2)  #slopes
  
  rgmAP2<- GMdist.AP2[[i]] #random grand mean from the distribution of grandmeans
  
  rAap2<- rgmAP2-sap2*Bap2
  
  rZap2 <- rAap2+ Xap2*Bap2
  
  rWap2<- logistic(rZap2) 
  
  linedf.AP2 <- data.frame(X=Xap2, Y=rWap2)
  
  linelist.AP2[[i]] <- linedf.AP2 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfAP2<- linelist.AP2 %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=APdata22W, aes(x=log(Strongyles+1), y=survivedsecondwinter)) + geom_point(alpha=0.3, size=3) +
  geom_line(data=bigdfAP2, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfAP2, aes(x=X, y=Y), col="black") +
  ylab("second winter survival") +
  ggtitle("april second winter")



#assessing model accuracy with mean model predictions 

modelpredsAP2 <- predict(modelAP2) 
predicted.classes.AP2 <- ifelse(modelpredsAP2 > 0.5, "1", "0")
mean(predicted.classes.AP2 == APdata22W$survivedsecondwinter) #true/false -> mean. 
#0.8222222



y <- Aap2+ log(0+1)*+APsEst2
logistic(y)
#when yearlings had 0 strongyles EPG in april, they had a >99.3% chance of survival. (only 4.44% of samples).. 
#calves with >90EPG (25.55% of samples), had a <82.1% chance of survival. 

sum(APdata22W$Strongyles==0)
(4/90)*100 




#things r going wrong look at later
# aug sec wint strongyle plot  -------------------------------------------------------


modelA2 <- glm(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1)+log(Elaphostrongylus+1)+
                  MassPerPellet+Sex+deeryear, family=binomial, data=Adata22W)
summary(modelA2)


AsEst2<- coef(summary(modelA2))["log(Strongyles + 1)", "Estimate"]
AsErr2<- coef(summary(modelA2))["log(Strongyles + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MA2<-model.matrix(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1)+log(Elaphostrongylus+1)+
                    MassPerPellet+Sex+deeryear, Adata22W) 
MA2 <- MA2[,names(coef(modelA2))] 
CA2<- summary(modelA2)$coefficients[,1:2] 

GMdist.A2<- vector()
for(i in 1:100) {
  
  coefsA2 <- apply(CA2, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsA2<- MA2 %*% coefsA2
  
  gmA2 <- mean(predsA2)
  
  GMdist.A2[[i]] <- gmA2
}


gm.a2<-mean(GMdist.A2) #the grand mean 


#getting main line with estimates 
Xa2 <- seq(from = 0, to = max(log(Adata22W$Strongyles + 1)), length.out = 100)  

Ya2 <- Xa2*+AsEst2

sa2 <- mean(log(Adata22W$Strongyles+1))

Aa2<- gm.a2-sa2*+AsEst2

Za2 <- Aa2+ Xa2*+AsEst2

Wa2<- logistic(Za2) 

mainlinedfA2 <- data.frame(X=Xa2, Y=Wa2)


#creating uncertainty

linelist.A2<- list()
for(i in 1:100) {
  
  Ba2<- rnorm(1, mean=AsEst2, sd = AsErr2)  #slopes
  
  rgmA2<- GMdist.A2[[i]] #random grand mean from the distribution of grandmeans
  
  rAa2<- rgmA2-sa2*Ba2
  
  rZa2 <- rAa2+ Xa2*Ba2
  
  rWa2<- logistic(rZa2) 
  
  linedf.A2 <- data.frame(X=Xa2, Y=rWa2)
  
  linelist.A2[[i]] <- linedf.A2 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfA2<- linelist.A2 %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=Adata22W, aes(x=log(Strongyles+1), y=survivedsecondwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdfA2, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfA2, aes(x=X, y=Y), col="black") +
  ylab("second winter survival") +
  ggtitle("August second winter")



#assessing model accuracy with mean model predictions 

modelpredsA2 <- predict(Asecwint) 
predicted.classes.A2 <- ifelse(modelpredsA2 > 0.5, "1", "0")
mean(predicted.classes.A2 == Adata22W$survivedsecondwinter) #true/false -> mean. 
#0.8351648




y <- Aa2+ log(30+1)*+AsEst2
logistic(y)
#when yearlings had 0 Strongyle EPG in AUG, they had a >99.8% chance of survival. 
#yearlings with >30EPG (15.38% of samples), had a <77.2% chance of survival. 

sum(Adata22W$Strongyles>30)
(14/91)*100 



# aug sec wint fluke plot  ------------------------------------------------

#u havent changed names of things change names 

Asecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1)), family=binomial, data=Adata22W)
summary(Asecwint)

AfEst2<- coef(summary(Asecwint))["log(Flukes + 1)", "Estimate"]
AfErr2<- coef(summary(Asecwint))["log(Flukes + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MA2<-model.matrix(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1), Adata22W) 
MA2 <- MA2[,names(coef(Asecwint))] 
CA2<- summary(Asecwint)$coefficients[,1:2]

GMdist.A2<- vector()
for(i in 1:100) {
  
  coefsA2 <- apply(CA2, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsA2<- MA2 %*% coefsA2
  
  gmA2 <- mean(predsA2)
  
  GMdist.A2[[i]] <- gmA2
}


gm.a2<-mean(GMdist.A2) #the grand mean 


#getting main line with estimates 
Xa2 <- seq(from = 0, to = max(log(Adata22W$Flukes + 1)), length.out = 100)  

Ya2 <- Xa2*+AfEst2

sa2 <- mean(log(Adata22W$Flukes+1))

Aa2<- gm.a2-sa2*+AfEst2

Za2 <- Aa2+ Xa2*+AfEst2

Wa2<- logistic(Za2) 

mainlinedfA2 <- data.frame(X=Xa2, Y=Wa2)


#creating uncertainty

linelist.A2<- list()
for(i in 1:100) {
  
  Ba2<- rnorm(1, mean=AfEst2, sd = AfErr2)  #slopes
  
  rgmA2<- GMdist.A2[[i]]
  
  rAa2<- rgmA2-sa2*Ba2
  
  rZa2 <- rAa2+ Xa2*Ba2
  
  rWa2<- logistic(rZa2) 
  
  linedf.A2 <- data.frame(X=Xa2, Y=rWa2)
  
  linelist.A2[[i]] <- linedf.A2 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfA2<- linelist.A2 %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=Adata22W, aes(x=log(Flukes+1), y=survivedsecondwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdfA2, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfA2, aes(x=X, y=Y), col="black") +
  ylab("second winter survival") +
  ggtitle("August second winter flukes")



#assessing model accuracy with mean model predictions 

modelpredsA2 <- predict(Asecwint) 
predicted.classes.A2 <- ifelse(modelpredsA2 > 0.5, "1", "0")
mean(predicted.classes.A2 == Adata22W$survivedsecondwinter) #true/false -> mean. 
#0.8351648


y <- Aa2+ log(30+1)*+AfEst2
logistic(y)
#when yearlings had 0 Fluke EPG in Aug, they had a >98.5% chance of survival. 
#calves with >30EPG (14.28% of samples), had a <90.67% chance of survival. 

sum(Adata22W$Flukes>30)
(13/91)*100 



# November model 2nd winter survival plot ---------------------------------


Nminsecwint <- glm(survivedsecondwinter~log(Strongyles+1)+ 
                     deeryear, family=binomial, data=Ndata22W)
summary(Nminsecwint)


N2sEst<- coef(summary(Nminsecwint))["log(Strongyles + 1)", "Estimate"]
N2sErr<- coef(summary(Nminsecwint))["log(Strongyles + 1)", "Std. Error"]


#getting the grand mean, and a distribution of grandmeans to show uncertainty 

MN2<-model.matrix(survivedsecondwinter~log(Strongyles+1)+deeryear, Ndata22W) 
MN2 <- MN2[,names(coef(Nminsecwint))] 
CN2<- summary(Nminsecwint)$coefficients[,1:2] 

GMdist.N2<- vector()
for(i in 1:100) {
  
  coefsN2 <- apply(CN2, 1,  function(x) rnorm(1, mean=x[1], sd=x[2]))
  
  predsN2<- MN2 %*% coefsN2
  
  gmN2 <- mean(predsN2)
  
  GMdist.N2[[i]] <- gmN2
}


gm.n2<-mean(GMdist.N2) #the grand mean 


#getting main line with estimates 
Xn2 <- seq(from = 0, to = max(log(Ndata22W$Strongyles + 1)), length.out = 100)  

Yn2<- Xn2*+N2sEst

sn2 <- mean(log(Ndata22W$Strongyles+1))

An2<- gm.n2-sn2*+N2sEst

Zn2 <- An2+ Xn2*+N2sEst

Wn2<- logistic(Zn2) 

mainlinedfN2 <- data.frame(X=Xn2, Y=Wn2)


#creating uncertainty

linelist.N2<- list()
for(i in 1:100) {
  
  Bn2 <- rnorm(1, mean=N2sEst, sd = N2sErr)  #slopes
  
  rgmN2 <- GMdist.N2[[i]] #random grand mean from the distribution of grandmeans
  
  rAn2 <- rgmN2-sn2*Bn2
  
  rZn2 <- rAn2+ Xn2*Bn2
  
  rWn2 <- logistic(rZn2) 
  
  linedf.N2 <- data.frame(X=Xn2, Y=rWn2)
  
  linelist.N2[[i]] <- linedf.N2 
}

#linelist.A #100 different x and y values in 100 different groups  - 100 diff lines 


bigdfN2<- linelist.N2 %>% bind_rows(.id = "group") #puts it in one df, groups  bound.  


#plot with the uncertainty and main line 
ggplot(data=Ndata22W, aes(x=log(Strongyles+1), y=survivedsecondwinter)) + geom_point(alpha=0.3, size=2) +
  geom_line(data=bigdfN2, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedfN2, aes(x=X, y=Y), col="black") +
  ylab("second winter survival") +
  ggtitle("November secondwinter")


#assessing model accuracy with mean model predictions 

modelpredsN2 <- predict(Nminsecwint) 
predicted.classes.N2 <- ifelse(modelpredsN2 > 0.5, "1", "0")
mean(predicted.classes.N2 == Ndata22W$survivedsecondwinter) #true/false -> mean. 
#0.8571429


y <- An2+ log(10+1)*+N2sEst
logistic(y)
#when yearlings had 0 Strongyle EPG in NOV, they had a >94.6% chance of survival. 
#calves with >10EPG (10.9% of samples), had a <68.8% chance of survival. 

sum(Ndata22W$Strongyles>10)
(10/91)*100 






#wont include 
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



