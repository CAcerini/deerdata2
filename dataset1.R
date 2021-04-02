rm(list=ls())

library(readxl); library(tidyverse)


#import dataset1 and view -unchanged copy 
Origional_dataset_1 <- read_excel("Origional_dataset_1.xlsx")


#new copy of dataset1 for you to add to 
dataset1 <- Origional_dataset_1


# dataset1 edits ----------------------------------------------------------

#creating a new column of mass per pellet 
dataset1 <- dataset1 %>% mutate(MassPerPellet= LastOfSampleMass/LastOfPelletCount) %>% drop_na(MassPerPellet)

#changing numbers to male and female
dataset1$Sex <- ifelse(test=dataset1$Sex == 1, yes="Female", no="Male") %>%  as.factor()

#removing all NAs
dataset1 <- dataset1 %>% drop_na()

#making sample deer year
dataset1 <- dataset1 %>% separate(LastOfSampleDate, sep = "-", into = c("SampYear", "SampMonth", "SampDay")) %>% 
  mutate_at(c("SampYear", "SampMonth", "SampDay"), as.numeric)

dataset1$deeryear <- dataset1$`SampYear`
dataset1$deeryear[dataset1$`SampMonth` == 4] <- dataset1$deeryear[dataset1$`SampMonth` == 4]-1

#making death deer year
dataset1 <- dataset1 %>% separate(ExactDoD, sep = "-", into = c("DeathYear", "DeathMonth", "DeathDay")) %>% 
  mutate_at(c("DeathYear", "DeathMonth", "DeathDay"), as.numeric)

dataset1$deathdeeryear <- dataset1$`DeathYear`
dataset1$deathdeeryear[(dataset1$`DeathMonth` <5) & !is.na(dataset1$DeathMonth)] <- 
  dataset1$deathdeeryear[(dataset1$`DeathMonth` <5) & !is.na(dataset1$DeathMonth)]-1

#calculating age 
deathage1 <- (dataset1$deathdeeryear-dataset1$BirthYear) %>% as.double()  
dataset1$deathage <- deathage1


#years and sample month as factors  
dataset1$deathdeeryear <- dataset1$deathdeeryear %>% as.factor()

dataset1$SampMonth <- dataset1$SampMonth %>% as.factor()




# November subset (allages) - Ndata1 --------------------------------------


#create new dataset with just samples of November before death  
Ndata1 <- dataset1 %>% filter(`SampMonth` == 11)

Ndata1 <- Ndata1 %>% filter(deeryear == deathdeeryear) # removing deer that have a long time between sample and death. 
                                                      #should just be deaths in december-april (same deer year)








# Calf subsets - NdataCalf ------------------------------------------------


#creating calf subset with both august and November data 

dataCalf <- dataset1 %>% filter( `deathage` == 0 )

dataCalf<- dataCalf %>% filter(!(SampMonth == 4))  #removing that one april sample 


#creating calf subset with only november samples 
NdataCalf <- dataCalf %>%  filter(SampMonth == 11)





