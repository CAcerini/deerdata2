

# greg tutorial 16/3/21 -------------------------------------------------------------------
library(cowplot)
theme_set(theme_cowplot())


minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)
summary(minmodel)
#mean survival from data 

X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

#plot(Y~X) - straight line

logit <- function(x){
  
  log(x/(1-x))
}


m<- mean(data2AN$survivedfirstwinter)

v<- logit(m)

s <- mean(log(data2AN$Strongyles+1))

A<- v-s*-0.25168

Z <- A+ X*-0.25168

#plot(Z~X) #this shows the straight line that is plot on the logistic scale bound between infinity and -infinity 

logistic <- function(x){
  exp(x)/(exp(x)+1)
}

W<- logistic(Z) #convert to data scale

#plot(W~X) #line will be plot on data scale - bound at 1 and 0 

mainlinedf <- data.frame(X=X, Y=W)

#plotting the line along with raw data points  
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=mainlinedf, aes(x=X, y=Y))  


#creating uncertainty - this is just showing the uncertainity in the gradient- it will still all be plot to be through the intercept

#X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

#m<- mean(data2AN$survivedfirstwinter)

#v<- logit(m)

#s <- mean(log(data2AN$Strongyles+1))

linelist<- list()
for(i in 1:100) {
  
  print(i)
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)  #slopes
  
  Y <- X*B
  
  A<- v-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

#linelist #100 different x and y values in 100 different groups  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group #each of the line dfs have its own group, 100

#plot with just uncertainty
#ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() + geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3) 

#plot with uncertainty and main line
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3, col="grey") +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 


#plot with main line only 
ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point(alpha=0.3) +
  geom_line(data=mainlinedf, aes(x=X, y=Y), col="steelblue")+
  ylab("First winter survival") 









#checks - alpha 
Bdist<- rnorm(10000, mean=-0.25168, sd = 0.08224)

qplot(Bdist)
  

table(Bdist>0)/10000*2



minmodel %>% str
summary(minmodel) %>% str

write.csv(summary(minmodel)$coefficients, file="outputtable.csv")

?rnorm


rnorm(100, mean=-0.25168, sd = 0.08224) %>% qplot



