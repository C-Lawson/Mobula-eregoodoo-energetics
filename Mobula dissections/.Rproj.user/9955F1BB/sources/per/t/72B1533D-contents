#Written Chris Lawson 19/2/19
#Looking at differences in HSI from total dataset (including DPI)
#To test: - Individual variation
        # - male vs female
        # - pregnant vs non-pregnant
        # - vs gestation stage (foetus size?)
        # - HSI vs mass (possibly a GAM for this...? Don't think so...)

#Load packages ----
library(ggplot2)
library(tidyverse)
library(mgcv)
#install.packages('corrplot')
library(corrplot)
#install.packages("mctest")
library(mctest)
#install.packages("car")
library(car)
library(ggpubr)
library(gtable)
library(gridExtra)
library(cowplot)
library(visreg)
library(patchwork)
library(svglite)

dat1 <- read.csv("Mobula_eregoodootenkee reproduction data- Latest update MBB_CL_CL EDITED COLUMNS.csv")
head(dat1)
str(dat1)

#clean up and look at adults (embryo HSI not measured):
HSI <- dat1
HSI <- HSI[,1:41]
HSI <- subset(HSI, H_S_I != 'NA') #https://stackoverflow.com/questions/6650510/remove-rows-from-data-frame-where-a-row-match-a-string/6650564
str(HSI)
HSI[,] <- lapply(HSI, function(x) if(is.factor(x)) factor(x) else x) #https://stackoverflow.com/questions/1195826/drop-factor-levels-in-a-subsetted-data-frame
str(HSI) #removes non-used factor levels
head(HSI)

boxplot(HSI$H_S_I)
range(HSI$H_S_I)
mean(HSI$H_S_I)
median(HSI$H_S_I)
mean(HSI$Total.mass..kg.) #17.13 kg
HSI$Disc.width..cm. <- as.numeric(as.character(HSI$Disc.width..cm.))
mean(HSI$Disc.width..cm.) #111.26 cm 
summarise(HSI$Sex)

#---



#---
#Reproductive stage

ggplot(HSI, aes(x=Stage, y = H_S_I)) + geom_boxplot() + theme_classic() +
  labs(y = "HSI")

anova4 <- aov(H_S_I ~ Stage, data = HSI)
summary(anova4) #significant (note adult == male, all females are in the 4 other categories)
TukeyHSD(anova4) #only significant is adult and gravid, p = 0.0005
#should "mature ovulating" and "Maturing ovulating" be the same category? Don't think so.
aggregate(HSI[,"H_S_I"], list(HSI$Stage), mean) #https://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame

#---




#---
#Vs Gestation stage
#First try HSI vs embryo size


dat1E <- HSI
dat1E <- dat1E %>% drop_na(Embryo.mass..kg.)

ggplot(dat1E, aes(x=Embryo.mass..kg., y = H_S_I)) + geom_point() +
  geom_smooth(method = "lm", se = F, colour = "Black") + #https://stackoverflow.com/questions/42897269/remove-grey-background-confidence-interval-from-forecasting-plot
  theme_classic() + 
  labs(x ="Embryo Mass (kg)", y = "HSI") 

lm1 <- lm(dat1E$`H_S_I` ~ dat1E$`Embryo.mass..kg.`)
summary(lm1) #no sig relationship

#---
#Then make new column for relative embryo size to mother and compare that
#[update 13/8/19 - I don't know where this column went,the .csv file has gone weird]
ggplot(dat1E, aes(x=Relative.embryo.mass...., y = H_S_I)) + geom_point() + theme_classic() +
  labs(y = "HSI")

lm2 <- lm(dat1E$H_S_I ~ dat1E$Relative.embryo.mass....)
summary(lm2) #no sig relationship

#--
#---
#Combine plots required for manuscript into one image----
#https://stackoverflow.com/questions/31319942/change-the-size-of-a-plot-when-plotting-multiple-plots-in-r
# #https://bookdown.org/ndphillips/YaRrr/arranging-plots-with-parmfrow-and-layout.html
# 
# layout.matrix <- matrix(c(1,3,2,3), nrow = 2, ncol = 2)
# layout.matrix
# 
# layout(mat = layout.matrix,
#        heights = c(1,1.5),
#        widths = c(1,1))
# 
# layout.show(3)
# 
# #---Plot 1
#Male vs female


ggMF <- ggplot(HSI, aes(x=Sex, y = H_S_I)) + geom_boxplot() + theme_classic() +
  labs(y = "HSI")
ggMF

anova3 <- aov(H_S_I ~ Sex, data = HSI)
summary(anova3)
TukeyHSD(anova3)

t.test(H_S_I ~ Sex, data = HSI) #significantly different p = 0.002423


#-------Plot 2
#Gravid vs not-gravid (only females)

dat1G <- HSI
dat1G <- dat1G %>% drop_na(Gravid)

ggGRAV <- ggplot(dat1G, aes(x=Gravid, y = H_S_I)) + geom_boxplot() + theme_classic() +
  labs(y = "HSI")
ggGRAV

t.test(H_S_I ~ Gravid, data = dat1G) #sig, p < 0.01, no mean = 4.51, yes mean = 3.80



#-------Plot 3
#HSI v foetus mass
ggFOET <- ggplot(dat1E, aes(x=Embryo.mass..kg., y = H_S_I)) + geom_point() +
  # geom_smooth(method = "lm", se = F, colour = "Black") + #https://stackoverflow.com/questions/42897269/remove-grey-background-confidence-interval-from-forecasting-plot
  theme_classic() +
  labs(x ="Embryo Mass (kg)", y = "HSI")

#Do in visreg to add 95% CI----
# 
# lm1 <- lm(H_S_I ~ `Embryo.mass..kg.`, data = dat1E)
# ggFOET <- visreg(lm1)




summary(lm(`H_S_I` ~ `Embryo.mass..kg.`, data = dat1E))

#-- arrange all 3 HSI plots
#UPDATE 13/12/19 USE PATCHWORK INSTEAD OF ARRANGEGROB, WAY EASIER


#ggarrange(ggMF, ggGRAV, ggFOET, widths = c(1, 1))
#https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html [IGNORE]
#https://stackoverflow.com/questions/21529926/arrange-ggplots-together-in-custom-ratios-and-spacing [IGNOR]
#http://www.sthda.com/english/wiki/print.php?id=177 

# ggALL <- arrangeGrob(ggMF, ggGRAV, ggFOET,
#              layout_matrix = matrix(c(1,3,2,3), nrow = 2, ncol = 2))
# 
# ggHSI <- as_ggplot(ggALL) +
#   draw_plot_label(label = c("A", "B", "C"), size = 15,
#                   x = c(0,0.5,0), y = c(1,1,0.5))
# 
# ggHSI


  # annotate_figure(top = text_grob("(A)", hjust = 19, vjust = 2),
  #                 bottom = text_grob("(B)", hjust = -2, vjust = -47),
  #                 left = text_grob("(C)",  hjust = -2.5, vjust = 1.5))



#13/12/19 USE PATCHWORK INSTEAD OF ARRANGEGROB, WAY EASIER ---
#Remake plots:
#Derive SE http://environmentalcomputing.net/plotting-with-ggplot-bar-plots-with-error-bars/
HSI_sex <- HSI %>%
  group_by(Sex) %>%
  summarise(mean_HSI = mean(H_S_I),
            SE_HSI = sd(H_S_I)/sqrt(n()))

#UPDATE 29/4/2020 Mike B wants to use SD not SE
# HSI_sex <- HSI %>%
#   group_by(Sex) %>%
#   summarise(mean_HSI = mean(H_S_I),
#             SD_HSI = sd(H_S_I))
#UPDATE AJR AND CD DONT LIKE SD, GO BACK TO SE (above)

HSI_sex

#Reorder to it flows from male to female to then panel B gravid vs non-gravid
HSI_sex$Sex <- factor(HSI_sex$Sex, levels = c("Male", "Female"))


#plot 1:
p1 <- ggplot(HSI_sex, aes(x = Sex, y = mean_HSI)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean_HSI - SE_HSI, ymax = mean_HSI + SE_HSI), width = 0.4) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, by = 1)) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(y = "HSI (%)") 
p1


#plot 2
HSI_gravid <- HSI %>%
  drop_na(Gravid) %>% 
  group_by(Gravid) %>% 
  summarise(mean_HSI = mean(H_S_I),
            SE_HSI = sd(H_S_I)/sqrt(n()))

#Quick aside:
#Determine difference in kJ of HSI between gravid and non-gravid females (mean diff, with mean mass of mothers 19.2kg)
HSI_gravid
HSI_Grav_dif <- as.numeric(HSI_gravid[1, 2] - HSI_gravid[2, 2])
HSI_Grav_dif
HSI_mass_dif <- (HSI_Grav_dif/100)*19200 #works out to be a difference of 137.2971 g of liver between gravid and no gravid
#Now if liver tissue has ED of 23.2 kJ/g:
HSI_mass_dif*23.2 #3185.3 kJ
#If total cost of repro on average is 20,222 kJ,
3185.3/20222*100
#Difference equals just 15.75% of total reproductive costs.

#But check maximum difference in HSI:
#min HSI for female (gravid) = 2.4%, max HSI for female (mature ovulating) = 5.5%
HSI_dif_max <- ((5.5-2.4)/100)*19200
HSI_dif_max*23.2
13808.64/20222*100
#Max difference in HSI accounts for ~68.28523% of mean reproductive costs
  
p2 <- ggplot(HSI_gravid, aes(x=Gravid, y = mean_HSI)) +
  geom_point(size = 2)  +
  geom_errorbar(aes(ymin = mean_HSI - SE_HSI, ymax = mean_HSI + SE_HSI), width = 0.4) +
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1)) +
  theme(axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
  labs(y = "HSI (%)")
p2

#plot 3
#HSI v foetus mass
p3 <- ggplot(dat1E, aes(x=Embryo.mass..kg., y = H_S_I)) + geom_point() +
  # geom_smooth(method = "lm", se = F, colour = "Black") + #https://stackoverflow.com/questions/42897269/remove-grey-background-confidence-interval-from-forecasting-plot
  theme_classic() +
  theme(axis.title.x = element_text(size = 2),
        axis.title.y = element_text(size = 2),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black")) +
    labs(x ="Foetus Mass (kg)", y = "HSI (%)")
p3

#Combine

plot1 <- (p1 + p2)/p3 + plot_annotation(tag_levels = ("a"), tag_prefix = "(",
                                        tag_suffix = ")")  &
  theme_bw()
plot1

ggsave("Figure 3_HSI_3plots.eps", width = 6, height = 6, units = c("in"), dpi = 900)









# UNUSED/TESTING/UNKNOWN BELOW ----

#---
#HSI vs mass


#---
#Quick check to see if season is significant for HSI
ggplot(HSI, aes(x=Season, y = H_S_I, colour = Sex)) + geom_boxplot() + theme_classic() +
  labs(y = "HSI")

summary(aov(H_S_I~Season, dat = HSI)) #nope

#---

#---
#Trying to make linear model for HSi ~ sex + stage + season + mass, and see what comes out as significant


#Following Ants R course instructions for general linear models: test with Sex, Total Mass, Stage, Season
str(HSI)
datHSIlm <- HSI[c('Season','Total.mass..kg.', 'Sex', 'Stage', 'H_S_I')]
pairs(datHSIlm)
hist(datHSIlm$H_S_I)

model1 <- lm(H_S_I ~ Total.mass..kg. + Sex + Stage + Season, data = datHSIlm)
summary(model1)

par(mfrow =c(2,2))
termplot(model1, se = TRUE)
plot(model1)
#Model selection
summary(model1)
model2 <- update(model1, ~ . - Total.mass..kg.)
summary(model2)
anova(model2, model1, test = "F") #no diff by removing mass
drop1(model2)
model3 <- step(model2)
summary(model3) #I think this removed Sex because it is essentially nested within stage (all "adult" are all the males)

par(mfrow =c(1,1))
termplot(model3, se = TRUE)

#note that HSI ~ mass produces significance but mass is cut from mulitple linear model, maybe multi-colinearility? Explore further. 
#---

#-

ggplot(HSI, aes(x=Total.mass..kg., y = H_S_I, colour = Sex)) + geom_point() + theme_classic() +
  labs(y = "HSI")

summary(lm(H_S_I ~ Total.mass..kg. + Sex, data = datHSIlm))
step(lm(H_S_I ~ Total.mass..kg. + Sex, data = datHSIlm))

summary(lm(H_S_I ~ Total.mass..kg., data = datHSIlm))

summary(lm(H_S_I ~ Total.mass..kg. + Sex + Total.mass..kg.*Sex, data = datHSIlm))

step(lm(H_S_I ~ Total.mass..kg. + Sex + Total.mass..kg.*Sex, data = datHSIlm))

summary(lm(H_S_I ~ Total.mass..kg.*Sex + Sex*Stage + 
             Total.mass..kg.*Stage, data = datHSIlm))

step((lm(H_S_I ~ Total.mass..kg.*Sex + Sex*Stage + 
           Total.mass..kg.*Stage, data = datHSIlm)))


#---
#From here to next break is taken from  #https://www.r-bloggers.com/dealing-with-the-problem-of-multicollinearity-in-r/
plot(lm(H_S_I ~ Total.mass..kg.*Sex + Sex*Stage + 
          Total.mass..kg.*Stage, data = datHSIlm))


cor1 = cor(datHSIlm) #doest work, needs numeric variables apparently

omcdiag(datHSIlm[,c(1:4)], datHSIlm$H_S_I) #also needs numeric...

imcdiag(datHSIlm[,c(1:4)],datHSIlm$H_S_I) #again, needs numeric (continuous) variables

#---
#From https://stats.stackexchange.com/questions/294678/multicollinearity-between-two-categorical-variables

vif(lm(H_S_I ~ Total.mass..kg.*Sex + Sex*Stage + 
         Total.mass..kg.*Stage, data = datHSIlm))

alias(lm(H_S_I ~ Total.mass..kg.*Sex + Sex*Stage + 
           Total.mass..kg.*Stage, data = datHSIlm)) #https://stats.stackexchange.com/questions/112442/what-are-aliased-coefficients 
#I think need to remove sex because male is perfectly colinear (?) with 'adult' level in stage
#also see https://stat.ethz.ch/pipermail/r-help/2015-March/427244.html

datHSIlm2 <- datHSIlm[c('Season','Total.mass..kg.', 'Stage', 'H_S_I')]
head(datHSIlm2)
str(datHSIlm2)
  
#try vif again
vif(lm(H_S_I ~ Total.mass..kg. + Stage + Season, data = datHSIlm)) #maybe vif just doens't like interaction terms, try again with above (including sex) but cut interaction terms:

vif(lm(H_S_I ~ Total.mass..kg. + Sex + Stage + Season, data = datHSIlm)) #nope,must actually be Sex causing the problem (perfect colinearlit with 'adult' in "stage' because they are the same data)

vif(lm(H_S_I ~ Total.mass..kg. + Stage + Season, data = datHSIlm2)) #all low VIF (maybe stage a little high ~ 5), therefore it's probably fine
#see https://stats.stackexchange.com/questions/70679/which-variance-inflation-factor-should-i-be-using-textgvif-or-textgvif


  
  
#---

lm3 <- lm(HSI$H_S_I ~ HSI$Total.mass..kg.)
summary(lm3)

summary(glm(H_S_I ~ Total.mass..kg. + Sex + Stage, dat = HSI))

summary(gam(H_S_I ~ Total.mass..kg. + Sex + Stage, dat = HSI))


