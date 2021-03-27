
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



# Nov, Aug, Apr subsets  --------------------------------------------------



Nsecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))+ 
                    deeryear, family=binomial, data=Ndata22W)
summary(Nsecwint)


Asecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))+ 
                     deeryear, family=binomial, data=Adata22W)
summary(Asecwint)

APsecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))+ 
                     deeryear, family=binomial, data=APdata22W)
summary(APsecwint)




# subset minimal models ---------------------------------------------------

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









# combined model repeated measures removed (dataset22W.2) -----------------


#dataset22W.2 no repeated measures, but still all months

rrmsecwint <- glm(survivedsecondwinter~(MassPerPellet+log(Strongyles+1)+log(Flukes+1)+
                                          log(Elaphostrongylus+1))*`Sample month`+ Sex+ deeryear,family=binomial, data=dataset22W.2)

summary(rrmsecwint)

#simplifying model


rrmminsecwint <- glm(survivedsecondwinter~(log(Strongyles+1)+log(Flukes+1))*`Sample month`+ 
                    deeryear, family=binomial, data=dataset22W)
summary(rrmminsecwint)


