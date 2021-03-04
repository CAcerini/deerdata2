#First model looking at the November data
model1N <-  glm(survivedfirstwinter~MassPerPellet+Strongyles+Flukes+Elaphostrongylus+BirthYear, family=binomial, data=Ndata2)

par(mfrow=c(2,2)) 
plot(model1N)
par(mfrow=c(1,1))

anova(modelNB, test="Chisq")
summary(modelNB)



#plotting logistic regression models for model1N
par(mfrow=c(2,2))

xvm <- seq(0,2.5,0.001)
yvm <- predict(model1N,list(MassPerPellet=xvm),type="response")
plot(survivedfirstwinter~MassPerPellet, data=Ndata2)
lines(xvm,yvm, col="red")

xvs <- seq(0,33,1)
yvs <- predict(model1N,list(Strongyles=xvs),type="response")
plot(survivedfirstwinter~Strongyles, data=Ndata2)
lines(xvs,yvs, col="red")

xvf <- seq(0,134,0.1)
yvf <- predict(model1N,list(Flukes=xvf),type="response")
plot(survivedfirstwinter~Flukes, data=Ndata2)
lines(xvf,yvf, col="red")

xve <- seq(0,43.66,0.1)
yve <- predict(model1N,list(Elaphostrongylus=xve),type="response")
plot(survivedfirstwinter~Elaphostrongylus, data=Ndata2)
lines(xve,yve, col="red")

par(mfrow=c(1,1)) 






#model looking at the August data


model1A <-  glm(survivedfirstwinter~MassPerPellet+Strongyles+Flukes+Elaphostrongylus+BirthYear, family=binomial, data=Adata2)


par(mfrow=c(2,2)) 
plot(model1N)
par(mfrow=c(1,1))


anova(model1A, test="Chisq")
summary(model1A)


#plotting logistic regression from model1A
par(mfrow=c(2,2))

xvm <- seq(0,2.5,0.001)
yvm <- predict(model1A,list(MassPerPellet=xvm),type="response")
plot(survivedfirstwinter~MassPerPellet, data=Adata2)
lines(xvm,yvm, col="red")

xvs <- seq(0,33,1)
yvs <- predict(model1A,list(Strongyles=xvs),type="response")
plot(survivedfirstwinter~Strongyles, data=Adata2)
lines(xvs,yvs, col="red")

xvf <- seq(0,134,0.1)
yvf <- predict(model1A,list(Flukes=xvf),type="response")
plot(survivedfirstwinter~Flukes, data=Adata2)
lines(xvf,yvf, col="red")

xve <- seq(0,43.66,0.1)
yve <- predict(model1A,list(Elaphostrongylus=xve),type="response")
plot(survivedfirstwinter~Elaphostrongylus, data=Adata2)
lines(xve,yve, col="red")

par(mfrow=c(1,1)) 


#second model- including data of both months, and fitting interactions. 
model2 <- glm(survivedfirstwinter~MassPerPellet+Strongyles+Flukes+Elaphostrongylus+BirthYear+`Sample month`*MassPerPellet+`Sample month`*Strongyles+`Sample month`*Flukes+`Sample month`*Elaphostrongylus, family=binomial, data=dataset2)


par(mfrow=c(2,2)) 
plot(model2)
par(mfrow=c(1,1))

anova(model1, test="Chisq")
summary(model1)

