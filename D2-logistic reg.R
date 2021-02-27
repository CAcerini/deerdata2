model1 <- glm(survivedfirstwinter ~ Strongyles, family = binomial, data=Ndata2)
model1

summary(model1, test="chisq")




range(Ndata2$Strongyles)


xStrongyles <-seq (0, 33, 1)

YStrongyles <- predict(model1, list(Strongyles=xStrongyles),type="response")
plot(survivedfirstwinter~Strongyles, data=Ndata2, col="red4")
lines(xStrongyles, YStrongyles, col = "red", lwd = 2)



#maybe...
library(ggplot2)

ggplot(Ndata2, aes(x=Strongyles, y=survivedfirstwinter)) + 
  geom_point(shape=1, position=position_jitter(width=.05,height=.05)) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

