#clear objects 
rm(list=ls())

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)


#import original dataset 2 - unchanged  
Origional_dataset_2 <- read_excel("Origional dataset 2.xlsx")
View(Origional_dataset_2)





#new copy of dataset2 for you to add to 
dataset2 <- Origional_dataset_2


#removing deer that were shot (S), or died of calving issue as moth (G), accident (A) or query (Q)
dataset2 <-  dataset2 %>% filter(!(DeathType %in% c("A", "G", "S", "Q")))



#change sex from numbers to male and female 
dataset2$Sex <- ifelse(dataset2$Sex == 1, "Female", "Male") %>% as.factor()


#changing sample month years to the deer year 
dataset2$deeryear <- dataset2$`Sample YearSample Month`
dataset2$deeryear[dataset2$`Sample month` < 5] <- dataset2$deeryear[dataset2$`Sample month` <5 ]-1



#calculating death age 
dataset2 <- dataset2 %>% separate(Deathdate, sep = "-", into = c("DeathYear", "DeathMonth", "DeathDay")) %>% 
  mutate_at(c("DeathYear", "DeathMonth", "DeathDay"), as.numeric)

dataset2$deathdeeryear <- dataset2$`DeathYear`
dataset2$deathdeeryear[(dataset2$`DeathMonth` <5) & !is.na(dataset2$DeathMonth)] <- 
  dataset2$deathdeeryear[(dataset2$`DeathMonth` <5) & !is.na(dataset2$DeathMonth)]-1

deathage2 <- (dataset2$deathdeeryear-dataset2$BirthYear) %>% as.double()  
dataset2$deathage <- deathage2



#create new column with survived (1) or died (0)
dataset2$survived <- dataset2$DeathType %>% is.na() %>% as.numeric()
#is.na(dataset2$DeathType) -- this returns all NA death types  as True 



#removing deer that died but don't have a death year 
dataset2 <- dataset2 %>% filter((survived == 0 & !is.na(deathage)) | (survived==1))



#create new column with first winter survived (1) or died (0)
firstwinterdeath <- (dataset2$deathage == 0) %>% as.numeric()   #this creates a vector 'firstwinterdeath' , listing  deer that die in first winter  as the number 1 and deer that deer that don't as 0 

firstwinterdeath[is.na(firstwinterdeath)] <- 0  #this assigns all NAs in firstwinterdeath vector as the number 0 

dataset2$survivedfirstwinter <- 1-firstwinterdeath #this creates column in dataset2 with deer that died in first winter as 0 and deer that survived first winter as 1



#filtered for samples taken in same year calf was born. - all deer will be less than 1 now 
dataset2 <- dataset2%>% filter(BirthYear == `deeryear`)


# new mass per pellet column 
dataset2 <- dataset2 %>% mutate(MassPerPellet= SampleMass/PelletCount) %>% drop_na(MassPerPellet)



#New data sets with November or August samples (taken from same year as born) 
Ndata2 <- dataset2 %>% filter(`Sample month` == 11)

Adata2 <- dataset2 %>% filter(`Sample month` == 8)










