# 1) ANVOA dry ED vs tissue type
# 2) ANOVA wet ED vs tissue type
# 3) Two-way ANOVA wet ED vs tissue type + bomb used
# 4) Test for differences in ED between dorsal/ventral and wing/trunk muscle - two way ANOVA?
# 5) test (ANOVA?) for ED difference between 'red' and 'white' muscle? (see labbook) - probably unimportant, and could be difficult to analyse (have to make three categories: white, red and mixed)


#Written by Chris Lawson started 31/10/18
#Looking at mobula kuhlii cf. eregoodootenkee bomb calorimetry data

library(dplyr)
library(tidyr)
library(ggplot2)

dat <- read.csv("Energy content datav2_ADULTS.csv")
head(dat)
dat$EDwet<- dat$ED.wet..kJ.g.
dat$EDdry <- dat$ED.dry..MJ.kg.
dat <- dat %>% drop_na(Bomb...used) #remove any rows with NA in 'bomb used' column (i.e. any samples that were not run)
str(dat)


#-----------------
#1) ANOVA dry ED vs tissue type
anova1 <- aov(EDdry ~ Tissue.type, data = dat)
summary(anova1)
anova1
TukeyHSD(anova1)
plot(dat$Tissue.type, dat$EDdry, las = 2)


#------------------
#-------------------
#2) ANOVA wet ED
anova2 <- aov(EDwet ~ Tissue.type, data = dat)
summary(anova2)
TukeyHSD(anova2) #liver and skin sig different to all others, except skin-ovary
#plot(dat$Tissue.type, dat$EDwet, las = 2)
#http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
ggplot(dat, aes(x=Tissue.type, y = EDwet)) + geom_boxplot() + theme_classic() + 
  labs(x = "Tissue Type", y = "Wet Energy Density (kJ/g)") + 
  scale_y_continuous(breaks = pretty(dat$EDwet, n = 6)) #https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks


#from beginner R workshop course with Ant et al:

datgrouped <- group_by(dat, Tissue.type)
summarise(datgrouped, mean_ED = mean(EDwet)) #gives mean of each tissue type





#---------------------------------------
#2 part 2 (added 3 June 2019) - after co-author feedback, calculate mean of tissue types by getting mean per individual (so instead of n = 5 *2, n = 5 (each calculated by 2 replicates))

by_animal <- dat %>% 
  group_by(Animal.ID, Tissue.type) %>%
  summarise(meanED = mean(EDwet, na.rm = TRUE)) #gives mean of each tissue type per individual

#Remove ovary and unknown (ovary n = 1, unknown is silly to include)

by_animal <- by_animal[!(by_animal$Tissue.type == "Ovary" | by_animal$Tissue.type == "Unknown"),]

#plot, with larger text, bold box plots and flipped so axis labels are easier to read:
ggplot(by_animal, aes(x=Tissue.type, y = meanED)) +
  geom_boxplot(lwd = 0.5) + 
  theme_bw() + 
  labs(x = "Tissue Type", y = "Wet Energy Density (kJ/g)", size = 12) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14),
        axis.title.y = element_text(vjust = 3), 
        axis.text.x = element_text(colour = "black"), #https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors
        axis.text.y = element_text(colour = "black")) +      
  coord_flip() +
  scale_y_continuous(breaks = pretty(by_animal$meanED, n = 6),
                     name = bquote('Energy Density'~('kJ g'^-1))) #https://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks

ggsave("Figure 1_ED_Tissues.eps", width = 7, height = 6, units = c("in"), dpi = 600)


#Pull out means and SEM for each tissue type ----
#write functon for SEM:
sem <- function (x){
  sd(x)/sqrt(length(x))
} 


by_animal_grouped <- by_animal %>%
  group_by(Tissue.type) %>% 
  summarise(ED = mean(meanED), ED_sem = sem(meanED))
by_animal_grouped



#Check if any significant difference between muscle and skeleton tissue
str(by_animal)
ttdatMuscle <- by_animal %>% 
  filter(Tissue.type == "Muscle")
  
ttdatSkel <- by_animal %>% 
  filter(Tissue.type == "Skeleton")

ttdat2 <- data.frame(matrix(nrow = 5, ncol = 2))
ttdat2$Muscle <- ttdatMuscle$meanED
ttdat2$Skeleton <- ttdatSkel$meanED
t.test(ttdat2$Muscle, ttdat2$Skeleton) #p = 0.2808



#-------------------
#-------------------
#3) Two-way ANOVA wet ED vs tissue type + bomb used
twanova <- aov(EDwet ~ Tissue.type + Bomb...used, data = dat)
summary(twanova) #no dig diff in bomb used (good!)


#-------------------
#-------------------
#4) Differences in dorsal/ventral muscle (and skin)
datDvV <- read.csv("Energy content datav2_ADULTS.csv", header=T, na.strings=c("","NA")) #https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
datDvV$EDwet<- datDvV$ED.wet..kJ.g.
datDvV$EDdry <- datDvV$ED.dry..MJ.kg.
datDvV <- datDvV %>% drop_na(Bomb...used)
datDvV <- datDvV %>% drop_na(Dorsal.ventral)
datDvV <- datDvV[!grepl("Skin", datDvV$Tissue.type),] #https://stackoverflow.com/questions/22249702/delete-rows-containing-specific-strings-in-r

plot(EDwet ~ Dorsal.ventral, data = datDvV)
anova5 <- aov(EDwet ~ Dorsal.ventral, data = datDvV)
summary(anova5)
anova5 #no sig difference between dorsal and ventral muscle

#-----
#Quick check of dorsal v ventral skin (but only 9 data points so unlikely to be different)

datDvVS <- read.csv("Energy content datav2_ADULTS.csv", header=T, na.strings=c("","NA")) #https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
datDvVS$EDwet<- datDvVS$ED.wet..kJ.g.
datDvVS$EDdry <- datDvVS$ED.dry..MJ.kg.
datDvVS <- datDvVS %>% drop_na(Bomb...used)
datDvVS <- datDvVS %>% drop_na(Dorsal.ventral)
datDvVS <- datDvVS[!grepl("Muscle", datDvVS$Tissue.type),]

t.test(EDwet ~ Dorsal.ventral, data = datDvVS) 
#NOTE: MAYBE THIS WOULD BE MORE ACCURATELY DONE WITH A PAIRED T-TEST
anova6 <- aov(EDwet ~ Dorsal.ventral, data = datDvVS)
summary(anova6) #SIGNIFICANT difference between dorsal and ventral skin (low sample size though)
anova6 
plot(EDwet ~ Dorsal.ventral, data = datDvVS)


#---------------------
#Differences in wing/trunk muscle (and skin + skeleton)
datwing <- read.csv("Energy content datav2_ADULTS.csv", header=T, na.strings=c("","NA")) #https://stackoverflow.com/questions/24172111/change-the-blank-cells-to-na
datwing$EDwet<- datwing$ED.wet..kJ.g.
datwing$EDdry <- datwing$ED.dry..MJ.kg.
datwing <- datwing %>% drop_na(Bomb...used)
datwing <- datwing %>% drop_na(Body.area)

datwingM <- datwing[!grepl("Skin", datwing$Tissue.type),]
datwingM <- datwingM[!grepl("Skeleton", datwingM$Tissue.type),]

plot(EDwet ~ Body.area, data = datwingM)
summary(aov(EDwet ~ Body.area, data = datwingM)) #no sig diff in wing vs trunk muscle ED

#--
datwingSkin <- datwing[!grepl("Muscle", datwing$Tissue.type),]
datwingSkin <- datwingSkin[!grepl("Skeleton", datwingSkin$Tissue.type),]

plot(EDwet ~ Body.area, data = datwingSkin)
summary(aov(EDwet ~ Body.area, data = datwingSkin)) #no sig diff in wing vs trunk skin ED, but n = 1 for trunk

#---
datwingS <- datwing[!grepl("Muscle", datwing$Tissue.type),]
datwingS <- datwingS[!grepl("Skin", datwingS$Tissue.type),]

plot(EDwet ~ Body.area, data = datwingS)
summary(aov(EDwet ~ Body.area, data = datwingS)) #no sig diff between trunk and wing skeleton ED

#---------------------
#---------------------
#5)ANOVA of muscle 'colour' (see lab book)

datMusc <- dat[grepl("Muscle", dat$Tissue.type),]
datMusc <- datMusc %>% drop_na(Muscle.colour)
plot(EDwet ~ Muscle.colour, data = datMusc)
summary(aov(EDwet ~ Muscle.colour, data = datMusc)) #no sig diff in ED between muscle 'colours'
TukeyHSD(aov(EDwet ~ Muscle.colour, data = datMusc))
