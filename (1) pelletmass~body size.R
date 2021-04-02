library(cowplot) ; library(car)
theme_set(theme_cowplot())

# 1A: all ages November --------------------------------------------------------


#1A : deer of all ages


#max model


maxmodel1A <- lm(MassPerPellet~Sex+JawLengthA+deathage, data=Ndata1)

par(mfrow=c(2,2)) 
plot(maxmodel1A)
par(mfrow=c(1,1))

Anova(maxmodel1A, type=3)
summary(maxmodel1A)
#model simplification
#order of removing terms (remove least significant each time, until final model all terms significant)

#In deer of all ages (model 1A), sex did not have a significant effect on faecal pellet mass (GLM, F1,64=0.223, p=0.639). 
#The age of an individual also did not have a significant effect on the faecal pellet mass (GLM, F1,64=1.304, p=0.258). 
#Both sex and age were removed from the analysis.

# minimal model 
minmodel1A <- lm(MassPerPellet~JawLengthA, data=Ndata1)

par(mfrow=c(2,2)) 
plot(minmodel1A)
par(mfrow=c(1,1))

anova(minmodel1A)
summary(minmodel1A)

#df- 63?
#Deer with longer jaw bones, produced significantly heavier faecal pellets (GLM, F1,63=100.840, p<0.001)
#For every 1 cm increase in the jaw length, the faecal pellet mass of produced increases by 0.19g (Figure X)

#plotting (minmodel 1A)
plot1A<- ggplot(Ndata1, aes(x=JawLengthA, y=MassPerPellet)) +
              geom_point()+
              geom_smooth(method=lm, se=FALSE, colour="steel blue") +
              xlab("Post-mortem jaw length (mm)") + ylab(" Faecal pellet mass (g)") +
              scale_y_continuous(expand = c(0,0), limits = c(0.0,4.0), breaks=seq(0.0,4.0, by=1.0))  +
              labs(tag="A") +
              theme(panel.border = element_rect(colour = "white", fill=NA),
                    panel.background = element_rect(fill="white"),
                    axis.line.x = element_line(color="black", size = 0.4),
                    axis.line.y = element_line(color="black", size = 0.4),
                    axis.text=element_text(size=15),
                    axis.title=element_text(size=20),
                    plot.tag = element_text(size=30))

 
ggplot(Ndata1, aes(x=JawLengthA, y=MassPerPellet)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, colour = "Black") +
  xlab("Post-mortem jaw length (mm)") + ylab(" Faecal pellet mass (g)") +
  scale_y_continuous(expand = c(0,0), limits = c(0.0,4.0), breaks=seq(0.0,4.0, by=1.0))  +
  labs(tag="A")
# 1B: calves November ---------------------------------------------------------

#no need to fit year we don't expect differences in body size across years - stop forgetting this omg...

#max model
maxmodel1B <- lm(MassPerPellet~Sex+JawLengthA, data=NdataCalf)

par(mfrow=c(2,2)) 
plot(maxmodel1B)
par(mfrow=c(1,1))

Anova(maxmodel1B, type=3) 

#For calves only (model 1B), sex also did not have a significant effect on faecal pellet mass 
#and was removed from the analysis (GLM, F1,37=0.029, p=0.866).

#model simplification

# minimal model 
minmodel1B <- lm(MassPerPellet~JawLengthA, data=NdataCalf)

par(mfrow=c(2,2)) 
plot(minmodel1B)
par(mfrow=c(1,1))

anova(minmodel1B)
summary(minmodel1B)

#calf jaw length did not explain significant variation in faecal pellet mass in calves alone (GLM, F1,37=3.933, p=0.055). 

#plotting(minmodel1B)
plot1B<- ggplot(NdataCalf, aes(x=JawLengthA, y=MassPerPellet)) +
              geom_point()+
              geom_smooth(method=lm, se=FALSE, colour="steel blue") +
              xlab("Post-mortem jaw length (mm)") + ylab(" Faecal pellet mass (g)") +
              scale_y_continuous(expand = c(0,0), limits = c(0,2.25), breaks=seq(0,2.25, by=0.5))  +
              labs(tag="B") +
              theme(panel.border = element_rect(colour = "white", fill=NA),
                    panel.background = element_rect(fill="white"),
                    axis.line.x = element_line(color="black", size = 0.4),
                    axis.line.y = element_line(color="black", size = 0.4),
                    axis.text=element_text(size=15),
                    axis.title=element_text(size=20),
                    plot.tag = element_text(size=30))






# plotting ----------------------------------------------------------------


plot_grid(plot1A, plot1B) #why the fuckisnt this working now #ur pkgs probs

plot1A+plot1B + ggsave("plot.jpeg", units="mm", width = 250, height = 120)





