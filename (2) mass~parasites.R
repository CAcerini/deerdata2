#(2) - parasites and pellet mass 

library(lme4); library(lmerTest); library(sjPlot); library(emmeans)

#Calves only. 

### Strongyles--------------
modelS2 <- lmer(log(Strongyles+1)~ MassPerPellet + Sex + BirthYear + `Sample month` + (1|Code), data=data2AN)

anova(modelS2) ; summary(modelS2)

par(mfrow=c(2,2))
plot(modelS2)
par(mfrow=c(1,1))

#Male and female calves did not differ significantly in faecal Strongyle count (GLMM, F1,170 = 0.2807, p=0.5969), 
#The average pellet mass of an individual did not explain significant variation in Strongyle count (GLMM, F1,329.57 = 0.6187, p=4321).
# removed both sex and pellet mass from the analysis  

modelS2min <- lmer(log(Strongyles+1)~ BirthYear + `Sample month` + (1|Code), data=data2AN)
anova(modelS2min) ; summary(modelS2min)

#The average calf strongyle count differed between years (GLM, F3,176.35= 23.608, p<0.001). 
#Calves in 2016 had significantly  lower strongyle counts than in 2017 and significantly higher strongyle counts than calves in 2019(table X).
#Calves in 2018 did not differ significantly  in strongyle count from calves in 2016 (table X). 
#Calves had significantly  lower Strongyle counts in November than in August (GLM, F1,231.04 = 216.452, P<0.001). 

r.squaredGLMM(modelS2min)
#the minimal model explains 0.455 in calf feacal Strongyle count. 


### flukes-------------------------------------
modelF2 <- lm(log(Flukes+1)~MassPerPellet + Sex + BirthYear, data=Ndata2)

Anova(modelF2, type = 3) ; summary(modelF2)

par(mfrow=c(2,2))
plot(modelF2)
par(mfrow=c(1,1))

#Male and female calves did not differ significantly in faecal F. hepatica count (GLM, F1,159 = 1.4436, p=0.2314).
#The average pellet mass of an individual did not explain significant variation in F. hepatica count (GLM, F1,159 = 1.3908, p=0.2401).
#pellet mass and sex were removed from the model 

modelF2min <- lm(log(Flukes+1)~ BirthYear, data=Ndata2)
Anova(modelF2min, type = 3) ; summary(modelF2min)

#the F. hepatica count of calves differed between years (GLM, F1,159 = 8.0923, p<0.001).
#Calves in 2018 and 2019 had significantly  lower faecal F. hepatica counts than in 2016 (table X). 
#Calves in 2017 did not differ significantly  in faecal F. hepatica count than in 2016 (table X).

r.squaredGLMM(modelF2min)

#the minimal model explains 0.133 variation in calf faecal F.hepatica count  

### E.cervi--------------------------
modelE2 <- lmer(log(Elaphostrongylus+1)~MassPerPellet + Sex + BirthYear+`Sample month` + (1|Code), data=data2AN)
anova(modelE2) ; summary(modelE2)
#Male and female calves did not differ significantly in faecal E.cervi count (GLMM, F1,186.45 = 2.1613, p=0.143).
#The average faecal pellet mass of an individual did not explain significant variation in E. cervi count (GLM, F1,330.70 = 2.2617, p=0.133).
#pellet mass and sex were removed from the model 

modelE2min <- lmer(log(Elaphostrongylus+1)~ BirthYear+`Sample month` + (1|Code), data=data2AN)
anova(modelE2min) ; summary(modelE2min)
#The E. cervi count of calves differed between years (GLMM, F1,192.45 = 11.811, p<0.001)
#Calves in 2016 had significantly   higher E.cervi counts than calves in 2017 and significantly  lower counts than calves in 2018(tableX).
#Calves in 2016 and 2019 did not differ significantly   in E. cervi count (table X). 
#Calves had significantly  lower E. cervi counts in November than in August (GlMM, F1,264.54, p=0.008)
 
r.squaredGLMM(modelE2min)
#min model explains 0.111 variation in calf faecal E.cervi count 



###### other 
#difflsmeans(modelE2min, test.effs = "BirthYear")
#emmeans(modelE2min, list(Pairwise~BirthYear), adjust = "tukey")

#lsmeansLT(modelE2min, pairwise = "BirthYear", adjust = "Tukey")

m_eff = emmeans(modelE2min, spec='BirthYear') #estimated birth year means
summary(m_eff)
m_tukey = contrast(m_eff, method='pairwise')
summary(m_tukey)
plot(m_tukey) #cant really remember the bars. bars confidence inter? dots is the estimate difference.  
#if bars DONT cross zero then the contrast IS significant. 
confint(m_tukey) #confidence intervals 



plot_summs(modelE2)




#viewer ------------
#max mods
tab_model(modelS2, modelE2, modelF2, show.se = T, show.ci = F,
          show.ngroups = F, show.icc = F, digits = 3)
#min mods
tab_model(modelS2min, modelF2min, modelE2min, show.se = T, show.ci = F,
          show.ngroups = F, show.icc = F, digits = 3)

