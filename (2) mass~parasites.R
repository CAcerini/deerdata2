#(2) - parasites and pellet mass 

library(lme4); library(lmerTest); library(sjPlot)

#Calves only. 

# logging parasites -------------------------------------------------------


#Strongyles
modelS <- lmer(log(Strongyles+1)~ MassPerPellet + Sex+ BirthYear +`Sample month` + (1|Code), data=data2AN)

anova(modelS)
summary(modelS)

par(mfrow=c(2,2))
plot(modelS)
par(mfrow=c(1,1))

drop1(modelS, test = "T")

lsmeans(modelS, pairwise~BirthYear*`Sample month`)
#Flukes - fit flukes only in November  
modelF <- lm(log(Flukes+1)~MassPerPellet + Sex  + BirthYear, data=Ndata2)

summary(modelF)
anova(modelF)

tab_model(modelE, show.fstat = T, show.ci = F, show.se = T)


par(mfrow=c(2,2))
plot(modelF)
par(mfrow=c(1,1))

#Flukes - Both - no should only fit flukes in Nov 
#modelF <- lmer(Flukes~MassPerPellet + Sex + BirthYear*`Sample month` + (1|Code), data=data2AN)

#summary(modelF)
#anova(modelF)

#E.cervi
modelE <- lmer(log(Elaphostrongylus+1)~MassPerPellet + Sex + BirthYear*`Sample month` + (1|Code), data=data2AN)

anova(modelE)
summary(modelE)



write.csv(summary(modelS)$coefficients, file="2S.summ.csv")
write.csv(summary(modelE)$coefficients, file="2E.summ.csv")
write.csv(summary(modelF)$coefficients, file="2F.summ.csv")

#tabE<- tab_model(modelE)

tab_model(modelS, modelE, modelF, show.fstat = T, show.ci = F, show.se = T, show.re.var = T)





























# no interaction.  --------------------------------------------------------
#strongyles
modelS2 <- lmer(log(Strongyles+1)~ MassPerPellet + Sex+ BirthYear+`Sample month` + (1|Code), data=data2AN)

anova(modelS2)
summary(modelS2)

par(mfrow=c(2,2))
plot(modelS2)
par(mfrow=c(1,1))

#flukes
modelF2 <- lm(log(Flukes+1)~MassPerPellet + Sex  + BirthYear, data=Ndata2)

summary(modelF2)
anova(modelF2)


par(mfrow=c(2,2))
plot(modelF2)
par(mfrow=c(1,1))

#E.cervi
modelE2 <- lmer(log(Elaphostrongylus+1)~MassPerPellet + Sex + BirthYear + `Sample month` + (1|Code), data=data2AN)

anova(modelE2)
summary(modelE2)


# not logging parasites -nope---------------------------------------------------
#strongyles 
modelS <- lmer(Strongyles~ MassPerPellet + Sex+ BirthYear*`Sample month` + (1|Code), data=data2AN)

anova(modelS)
summary(modelS)

#flukes

#Flukes - fit flukes only in November  
modelF <- lm(Flukes+~MassPerPellet + Sex  + BirthYear, data=Ndata2)

summary(modelF)
anova(modelF)

#E.cervi
modelE <- lmer(Elaphostrongylus~MassPerPellet + Sex + BirthYear*`Sample month` + (1|Code), data=data2AN)

anova(modelE)
summary(modelE)





# juv deer - nope  -------------------------------------------------------------

#juvenile deer 
modelSA <- lmer(Strongyles~MassPerPellet + Sex  + BirthYear+ `Sample month`  + sampleage + MassPerPellet*sampleage + 
                  MassPerPellet*`Sample month` + (1|Code), data=dataset2AA)
anova(modelSA)
summary(modelSA)

plot(Strongyles~MassPerPellet, data=dataset2AA)


