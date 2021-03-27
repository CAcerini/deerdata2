#clear objects 
rm(list=ls())

library(readxl); library(tidyverse)


#import original dataset 2 - unchanged  
Origional_dataset_2 <- read_excel("Origional dataset 2.xlsx")

#new copy of dataset2 for you to add to - calf survival
dataset2 <- Origional_dataset_2

# dataset2 - survival data changes ----------------------------------------

#removing deer that were shot (S), or died of calving issue as moth (G), accident (A) or query (Q)
dataset2 <-  dataset2 %>% filter(!(DeathType %in% c("A", "G", "S", "Q")))


#change sex from numbers to male and female 
dataset2$Sex <- ifelse(dataset2$Sex == 1, "Female", "Male") %>% as.factor()


#changing sample month years to the deer year 
dataset2$deeryear <- dataset2$`Sample YearSample Month`
dataset2$deeryear[dataset2$`Sample month` < 5] <- dataset2$deeryear[dataset2$`Sample month` <5 ]-1

#sample age - this is for checks.
sampleage2 <- (dataset2$deeryear-dataset2$BirthYear) %>% as.double()
dataset2$sampleage <- sampleage2

#calculating death year as its deer year  
dataset2 <- dataset2 %>% separate(Deathdate, sep = "-", into = c("DeathYear", "DeathMonth", "DeathDay")) %>% 
  mutate_at(c("DeathYear", "DeathMonth", "DeathDay"), as.numeric)

dataset2$deathdeeryear <- dataset2$`DeathYear`
dataset2$deathdeeryear[(dataset2$`DeathMonth` <5) & !is.na(dataset2$DeathMonth)] <- 
  dataset2$deathdeeryear[(dataset2$`DeathMonth` <5) & !is.na(dataset2$DeathMonth)]-1

#calculating death age 
deathage2 <- (dataset2$deathdeeryear-dataset2$BirthYear) %>% as.double()  
dataset2$deathage <- deathage2


#create new column with survived (1) or died (0) - overall (ie. up until 2019 deer year)
dataset2$survived <- dataset2$DeathType %>% is.na() %>% as.numeric()
#is.na(dataset2$DeathType) -- this returns all NA death types  as True 


#removing deer that died but don't have a death year 
dataset2 <- dataset2 %>% filter((survived == 0 & !is.na(deathage)) | (survived==1))


# new mass per pellet column 
dataset2 <- dataset2 %>% mutate(MassPerPellet= SampleMass/PelletCount) %>% drop_na(MassPerPellet)

#removing NAs for parasites 
dataset2 <- dataset2 %>% drop_na(Strongyles, Flukes, Elaphostrongylus)

#new dataset for their second winter (run at this position)  
dataset22W<- dataset2

#create new column with first winter survived (1) or died (0) (in dataset2)
firstwinterdeath <- (dataset2$deathage == 0) %>% as.numeric()   #this creates a vector 'firstwinterdeath' , listing  deer that die in first winter  as the number 1 and deer that deer that don't as 0 

firstwinterdeath[is.na(firstwinterdeath)] <- 0  #this assigns all NAs in firstwinterdeath vector as the number 0 

dataset2$survivedfirstwinter <- 1-firstwinterdeath #this creates column in dataset2 with deer that died in first winter as 0 and deer that survived first winter as 1


#filtered for samples taken in same year calf was born. - all samples will be from when they were calves
dataset2 <- dataset2%>% filter(BirthYear == `deeryear`)

#removing that September sample  
dataset2 <- dataset2 %>% filter(!(`Sample month` == 9))


#changing sample month to factor and reordering levels to fit with deer year  
dataset2$`Sample month` <- factor(dataset2$`Sample month` , levels=c("8", "11", "4"))


#changing birth year and sample deer year to  factors 
dataset2$BirthYear <- dataset2$BirthYear %>% factor()
dataset2$deeryear <- dataset2$deeryear %>% factor()


#if you want to add columns of log transforming your parasites  - maybe do later. not important rn as would need to change everything else  
#logS <- log(dataset2$Strongyles +1)
#dataset2$logStrong <- logS

# Ndata2 and Adata2 -------------------------------------------------------


#New data sets with November or August samples (taken from same year as born) 
Ndata2 <- dataset2 %>% filter(`Sample month` == 11)

Ndata2<- Ndata2 %>%  group_by(Code) %>% sample_n(1) #randomly removing repeated samples of individuals 


Adata2 <- dataset2 %>% filter(`Sample month` == 8)

Adata2<- Adata2 %>%  group_by(Code) %>% sample_n(1) #randomly removing repeated samples of individuals 




# data2AN -----------------------------------------------------------------


#new data set that looks at just august and Nov samples 

data2AN <- dataset2 %>% filter(`Sample month` == (8) | `Sample month` == (11))



# data2AN repeated measures removed randomly ------------------------------
#data2AN %>% distinct(Code) %>% count() #191 unique individuals, but 343 samples

data2AN.2 <- data2AN %>% group_by(Code) %>% sample_n(1)

#data2AN.2 %>% group_by(`Sample month`) %>% count(survivedfirstwinter)
#data2AN.2 %>% group_by(survivedfirstwinter) %>% count() #57 die, 134 survive 






# dataset22W --------------------------------------------------------------

#create new column with second winter survived (1) or died (0)

dataset22W<- dataset22W %>% filter(!(deathage == 0) | is.na(deathage)) #removing deer that die age 0 (in first winter), but keeping the NA death ages

secondwinterdeath <- (dataset22W$deathage == 1) %>% as.numeric()   #this creates a vector 'secondwinterdeath' , listing  deer that die in 2nd winter  as the number 1 and deer that deer that don't as NA 

secondwinterdeath[is.na(secondwinterdeath)] <- 0  #this assigns all NAs in secondwinterdeath vector as the number 0 

dataset22W$survivedsecondwinter <- 1-secondwinterdeath #this creates column in dataset2 with deer that died in first winter as 0 and deer that survived first winter as 1

dataset22W <- dataset22W %>% filter(!(BirthYear== 2019)) #removing deer born in 2019 bc we dont know if they have survived/died second winter(winter of 2021, deer yr 2020)

#to be revised... trying to filter for samples taken in the april,aug,nov before their second winter. so april samples will be as calves. and aug and nov as yearlings  
dataset22W <- dataset22W%>% 
  filter((sampleage == 0 & `Sample month`==4) | (sampleage==1 & `Sample month`== 11) | (sampleage==1 & `Sample month`== 8))


#changing sample month to factor and reordering levels to fit with deer year  
dataset22W$`Sample month` <- factor(dataset22W$`Sample month` , levels=c("8", "11", "4"))


#changing birth year and sample deer year to ordered factors 
dataset22W$BirthYear <- dataset22W$BirthYear %>% factor()
dataset22W$deeryear <- dataset22W$deeryear %>% factor()


#dataset22W %>% group_by(survivedsecondwinter) %>% count() #nb. repeated deer measures though.

#dataset22W %>% group_by(`Sample month`) %>% count(survivedsecondwinter)

#dataset22W %>% distinct(Code) %>% count() #we have 134 unique individuals.. repeated samples.. 304 samples 




# dataset22W.2 - removing repeated samples --------------------------------

#this is a trial to make sure we dont have repeated measures of calves, but can have samples from different months in the dataset
#should gt 134 samples now. 
#we have 17 deer that die in their second winter, and 117 that survive  

dataset22W.2 <- dataset22W %>% group_by(Code) %>% sample_n(1)

#dataset22W.2 %>% group_by(`Sample month`) %>% count(survivedsecondwinter)
#dataset22W.2 %>% group_by(survivedsecondwinter) %>% count()



# Nov, Aug & April subsets of 2W survival data ----------------------------


#november subset of second winter survival data set   

Ndata22W<- dataset22W %>% filter(`Sample month` == 11)

Ndata22W<- Ndata22W %>%  group_by(Code) %>% sample_n(1) #randomly removing repeated sample 

Ndata22W %>% group_by(survivedsecondwinter) %>% count() #14 deer have died, 77 survived  

#august subset of second winter 

Adata22W<- dataset22W %>% filter(`Sample month` == 8)

Adata22W<- Adata22W %>%  group_by(Code) %>% sample_n(1) 

Adata22W %>% group_by(survivedsecondwinter) %>% count() #13 deer have died, 79 survived  

#april subset of second winter

APdata22W <- dataset22W %>% filter(`Sample month` == 4)
APdata22W <- APdata22W %>% group_by(Code) %>% sample_n(1)

APdata22W %>% group_by(survivedsecondwinter) %>% count() #13 deer have died, 80 survived  








# other -------------------------------------------------------------------


#know how, but dont do this unless you have good reason!!!
#replacing outlier mass per pellet values with NA 
data2MOR <- data2AN %>% mutate(MassPerPellet = ifelse(MassPerPellet > 1.5, NA, MassPerPellet))

# dataset2AA - for all juveniles - No ----------------------------------------------------


#and another dataset to look at pellet mass and strongyle counts 
dataset2AA <- Origional_dataset_2

#change sex from numbers to male and female 
dataset2AA$Sex <- ifelse(dataset2AA$Sex == 1, "Female", "Male") %>% as.factor()


#changing sample month years to the deer year 
dataset2AA$deeryear <- dataset2AA$`Sample YearSample Month`
dataset2AA$deeryear[dataset2AA$`Sample month` < 5] <- dataset2AA$deeryear[dataset2AA$`Sample month` <5 ]-1



#calculating sample age 

sampleage <- (dataset2AA$deeryear-dataset2AA$BirthYear) %>% as.double()  
dataset2AA$sampleage <- sampleage


# new mass per pellet column 
dataset2AA <- dataset2AA %>% mutate(MassPerPellet= SampleMass/PelletCount) %>% drop_na(MassPerPellet)


#removing that September sample  
dataset2AA <- dataset2AA %>% filter(!(`Sample month` == 9))


#changing sample month to factor and reordering levels to fit with deer year  
#dataset2AA$`Sample month` <- factor(dataset2AA$`Sample month` , levels=c("8", "11", "4"))

dataset2AA$`Sample month` <- as.factor(dataset2AA$`Sample month`)

#changing birth year and sample deer year to  factors 
dataset2AA$BirthYear <- dataset2AA$BirthYear %>% factor()
dataset2AA$deeryear <- dataset2AA$deeryear %>% factor()


dataset2AA <- dataset2AA %>%select(Code, Sex, BirthYear, BirthWt, `Sample month`, Strongyles, Flukes, Elaphostrongylus, deeryear, sampleage, MassPerPellet)







