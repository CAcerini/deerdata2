
### first winter 

# November subset 1st winter (Ndata2)-------------------------------


maxmodel1N <-  glm(survivedfirstwinter~MassPerPellet+log(Strongyles+1)+
                  log(Flukes+1)+log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Ndata2)
par(mfrow=c(2,2)) 
plot(maxmodel1N)
par(mfrow=c(1,1))


Anova(maxmodel1N, test.statistic = "Wald", type = 3) 
summary(maxmodel1N) #AIC = 168.88

#simplifying November model
#rem(flukes, pellet mass, sex, elaphostrongylus) 
modelminN <- glm(survivedfirstwinter~log(Strongyles+1)+BirthYear,
                 family=binomial, data=Ndata2)
summary(modelminN)
#AIC = 165.83

anova(maxmodel1N, modelminN, test = "Chisq")

# August subset 1st winter (Adata2) ------------------------------------------------

#model looking at the August data 

maxmodel1A <-  glm(survivedfirstwinter~MassPerPellet+log(Strongyles+1)+
                     log(Flukes+1)+log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Adata2)

model1A <-  glm(survivedfirstwinter~log(Strongyles+1)+
                  BirthYear, family=binomial, data=Adata2)
#Birth year is not significant in August  

modelminA <-  glm(survivedfirstwinter~log(Strongyles+1), family=binomial, data=Adata2)

par(mfrow=c(2,2)) 
plot(modelminA)
par(mfrow=c(1,1))

anova(modelminA, test="Chisq")
Anova(modelminA, test.statistic = "Wald", type = 3)
summary(modelminA)


tab_model(modelminA)# -  sjplot output gives me ' Odds ratios' 


# Combined model 1st winter (data2AN) -------------------------------------------------------------

maxmodel <- glm(survivedfirstwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+log(Elaphostrongylus+1))*`Sample month`+
                  BirthYear+Sex, family=binomial, data=data2AN)


summary(maxmodel)
   
par(mfrow=c(2,2)) 
plot(maxmodel)
par(mfrow=c(1,1))

#tab_model(maxmodel) #an sjplot output - 'Odds ratios'

#simplification
#sample month interaction, flukes, sex, massperpellet, elaphostrongylus
minmodel <- glm(survivedfirstwinter~log(Strongyles+1)+`Sample month`+
                  BirthYear, family=binomial, data=data2AN)


summary(minmodel)


#estimate and error of log strong in min model 
sEst<- coef(summary(minmodel))["log(Strongyles + 1)", "Estimate"]
sErr<- coef(summary(minmodel))["log(Strongyles + 1)", "Std. Error"]



### second winter 

# 2W Nov, Aug, Apr subsets  --------------------------------------------------



Nsecwint <- glm(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1)+
                  log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Ndata22W)
summary(Nsecwint)
Anova(Nsecwint, test.statistic = "Wald", type = 3) 
anova(Nsecwint)

Asecwint <- glm(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1)+
                  log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=Adata22W)
summary(Asecwint)

APsecwint <- glm(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1)+
                   log(Elaphostrongylus+1)+BirthYear+Sex, family=binomial, data=APdata22W)
summary(APsecwint)




# 2W subset minimal models ---------------------------------------------------

#November- remove flukes

Nminsecwint <- glm(survivedsecondwinter~log(Strongyles+1)+ 
                     deeryear, family=binomial, data=Ndata22W)
summary(Nminsecwint)

#august remove year 
Aminsecwint <- glm(survivedsecondwinter~log(Strongyles+1)+log(Flukes+1),
                   family=binomial, data=Adata22W)
summary(Aminsecwint)

#april remove flukes, and year
APminsecwint <- glm(survivedsecondwinter~log(Strongyles+1),
                    family=binomial, data=APdata22W)
summary(APminsecwint)









# combined model (dataset22W) ----------------------------------------------------------


#combined models - looking at spring, summer and autumn samples before their second winter  

maxsecwint <- glm(survivedsecondwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+
                                          log(Elaphostrongylus+1))*`Sample month`+ Sex+ deeryear, family=binomial, data=dataset22W)


summary(maxsecwint)


minsecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))*`Sample month`+ 
                    deeryear, family=binomial, data=dataset22W)
summary(minsecwint)



#NOW R WE REALLY SEEING VARIATION IN SURVIVAL IN DIFFERENT YEARS OR IS IT JUST A REFLECTION OF THE SAMPLES OF OUR DATA
#DIFFERENT NUMBERS OF INDIVIDUALS SAMPLED EACH YEAR  




# combined model repeated measures removed (dataset22W.2) -----------------


#dataset22W.2 no repeated measures, but still all months

rrmsecwint <- glm(survivedsecondwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+
                                          log(Elaphostrongylus+1))*`Sample month`+ Sex+ deeryear,family=binomial, data=dataset22W.2)

summary(rrmsecwint)

#simplifying model


rrmminsecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))*`Sample month`+ 
                       deeryear, family=binomial, data=dataset22W)
summary(rrmminsecwint)








#other 

# ARGHH simple model previous only strongyles. why  -----------------------------------------------------------

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










# rmr data2AN.2 -------------------------------------


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

