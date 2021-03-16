
# no ----------------------------------------------------------------------


minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)

plotting_dfm <- expand.grid(Strongyles = seq(from=0, to = 194, by=0.01),
                            BirthYear = (2016:2019))
plotting_dfm$BirthYear <- as.factor(plotting_dfm$BirthYear)

plotting_dfm <- plotting_dfm %>% mutate(logstrong = log(Strongyles+1))

plotting_dfm$survivalpreds <- predict(minmodel , newdata=plotting_dfm, type='response', re.form=NA)

mean(plotting_dfm$survivalpreds)

logistic_mean <- log((mean(plotting_dfm$survivalpreds)/(1-mean(plotting_dfm$survivalpreds))))


plotting_dfm <- plotting_dfm %>% mutate(slopevalues = logstrong*-0.25168)

plot(plotting_dfm$survivalpreds~plotting_dfm$slopevalues)

#add to the mean
plotting_dfm <- plotting_dfm %>% mutate(slopemean = slopevalues+logistic_mean)

#convert back 
plotting_dfm <- plotting_dfm %>% mutate(convert = (exp(slopemean)/(exp(slopemean)+1)))

plot(plotting_dfm$survivalpreds~plotting_dfm$convert)



#  -------------------------------------------------------------------
install.packages("cowplot")
library(cowplot)
theme_set(theme_cowplot())


minmodel<- glm(survivedfirstwinter~log(Strongyles+1) + BirthYear,
               family=binomial, data=data2AN)

#mean survival from data 
mean(data2AN$survivedfirstwinter)

X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

Y <- X*-0.25168

plot(Y~X)

logit <- function(x){
  
  log(x/(1-x))
}

logit(0)

m<- mean(data2AN$survivedfirstwinter)

v<- logit(m)

s <- mean(log(data2AN$Strongyles+1))

A<- v-s*-0.25168

Z <- A+ X*-0.25168

points(Z~X)
plot(Z~X)

logistic <- function(x){
  exp(x)/(exp(x)+1)
}
W<- logistic(Z) #convert to data scale

plot(W~X)

linedf <- data.frame(X=X, Y=W)

ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() +
  geom_line(data=linedf, aes(x=X, y=Y))  
  
#creating uncertainty

X <- seq(from = 0, to = max(log(data2AN$Strongyles + 1)), length.out = 100)  

m<- mean(data2AN$survivedfirstwinter)

v<- logit(m)

s <- mean(log(data2AN$Strongyles+1))

linelist<- list()
for(i in 1:100) {
  
  print(i)
  
  B<- rnorm(1, mean=-0.25168, sd = 0.08224)
  
  Y <- X*B
  
  A<- v-s*B
  
  Z <- A+ X*B
  
  W<- logistic(Z) 
  
  linedf <- data.frame(X=X, Y=W)
  
  linelist[[i]] <- linedf 
}

linelist #100 different x and y values in 100 different groups  


bigdf<- linelist %>% bind_rows(.id = "group") #puts it in one group 


ggplot(data=data2AN, aes(x=log(Strongyles+1), y=survivedfirstwinter)) + geom_point() +
  geom_line(data=bigdf, aes(x=X, y=Y, group=group), alpha=0.3) 
  


#checks - alpha 
Bdist<- rnorm(10000, mean=-0.25168, sd = 0.08224)

qplot(Bdist)
  

table(Bdist>0)/10000*2


minmodel %>% str
summary(minmodel) %>% str


write.csv(summary(minmodel)$coefficients, file="outputtable.csv")

?rnorm


rnorm(100, mean=-0.25168, sd = 0.08224) %>% qplot





