library(sjPlot)

# November subset 1st winter (Ndata2)-------------------------------


model1N <-  glm(survivedfirstwinter~MassPerPellet+log(Strongyles+1)+
                  log(Flukes+1)+log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Ndata2)
par(mfrow=c(2,2)) 
plot(model1N)
par(mfrow=c(1,1))

write.csv(summary(model1N)$coefficients, file="maxmodelN.csv")

anova(model1N, test="Chisq")
summary(model1N)

#simplifying November model
#order of removal
#flukes, pellet mass, sex, elaphostrongylus 
modelminN <- glm(survivedfirstwinter~log(Strongyles+1)+BirthYear,
                 family=binomial, data=Ndata2)

summary(modelminN)


# August subset 1st winter (Adata2) ------------------------------------------------

#model looking at the August data 


model1A <-  glm(survivedfirstwinter~log(Strongyles+1)+
                  BirthYear, family=binomial, data=Adata2)
maxmodel1A <-  glm(survivedfirstwinter~MassPerPellet+log(Strongyles+1)+
                     log(Flukes+1)+log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Adata2)

model1A <-  glm(survivedfirstwinter~log(Strongyles+1)+
                  BirthYear, family=binomial, data=Adata2)
#Birth year is not significant in August  

modelminA <-  glm(survivedfirstwinter~log(Strongyles+1), family=binomial, data=Adata2)

par(mfrow=c(2,2)) 
plot(modelminA)
par(mfrow=c(1,1))


anova(model1A, test="Chisq")
summary(modelminA)


tab_model(modelminA)# - an sjplot output, shows ' Odds ratios' 

write.csv(summary(maxmodel1A)$coefficients, file="maxmodelA.csv")

write.csv(summary(modelminA)$coefficients, file="survivalAmin.csv")




# Combined model 1st winter (data2AN) -------------------------------------------------------------



maxmodel <- glm(survivedfirstwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+log(Elaphostrongylus+1))*`Sample month`+
                  BirthYear+Sex, family=binomial, data=data2AN)


summary(maxmodel)
   
par(mfrow=c(2,2)) 
plot(maxmodel)
par(mfrow=c(1,1))

#tab_model(maxmodel) #an sjplot output - 'Odds ratios'

#simplification, order of taking things out..
#sample month interaction, flukes, sex, massperpellet, elaphostrongylus
minmodel <- glm(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                  BirthYear, family=binomial, data=data2AN)


summary(minmodel)


#estimate and error of log strong in min model 
sEst<- coef(summary(minmodel))["log(Strongyles + 1)", "Estimate"]
sErr<- coef(summary(minmodel))["log(Strongyles + 1)", "Std. Error"]

# simple model previous only strongyles. why  -----------------------------------------------------------

#whywhywhywhywhywhywhywhywhyyyyyyy 
minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)

par(mfrow=c(2,2))
plot(minmodel)
par(mfrow=c(1,1))

summary(minmodel)
  #strongyles signficant **, birth year significant ****


tab_model(minmodel)


ggplot(data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() +
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE, col='steel blue') +
  labs(x="Log(Strongyle FEC + 1)", y="Survived first winter") +
  scale_x_continuous(breaks=seq(0,5.5, by=2)) +
  theme(panel.border = element_rect(colour = "white", fill=NA),
        panel.background = element_rect(fill="white"),
        axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4),
        axis.text=element_text(size=12),
        axis.title=element_text(size=16),
        legend.position = "bottom")










# repeated measures removed data2AN.2 -------------------------------------


RMRmaxmodel <- glm(survivedfirstwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+log(Elaphostrongylus+1))*`Sample month`+BirthYear+Sex,
                   family=binomial, data=data2AN.2)
summary(RMRmaxmodel)

#simplifying model
#taking things out in this order:
#flukes, interaction, sex, elaphostrongylus
#weird, pelletmass significant until sex taken out then not significant, but then is significant again when elapho taken out... 
RMRminmodel <- glm(survivedfirstwinter~MassPerPellet+log(Strongyles+1)+`Sample month`+BirthYear,
                     family=binomial, data=data2AN.2)
summary(RMRminmodel)
