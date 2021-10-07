#Started 5/6/19 Chris Lawson
#Looking at metabolic costs of foetus development - essentially creating bioenergetics model for foetus

#Parameters:
#Mean mass of mother: 19.2 kg
#Ra = 0.02 g(O2)/g/d (Schindler et al 2002)
#Rb = -0.15 (Schindler et al 2002)  (How is this negative? ANS: THIS IS MASS-SPECIFIC. IF NOT MASS-SPECIFIC, this is equivalent to rb = 0.85), proof:
    #####x = 1:100
    #####plot(0.02*x^-0.15)
    #####plot((0.02*x^0.85)/x)
#Rq/Q10 = Q10 = 2.2 (Schindler et al 2002), use Brodie et al 2016 for using Q10 in RMR equation (as opposed to RQ)
#Temp = 22.16 C
#Foetus mass at birth: 1.1 kg
#Gestation period: 12 months (Broadhurst et al 2019)
#Foetus growth curve shape: Power curve: 0.1*dat_E$`Age(d)`^1.578 #See "mass-age of embryo" spreadsheet (I made this curve up to reach 1.1kg in 365 days)

library(tidyr)
library(dplyr)


ACT <- 1.5
Assim <- 0.33 #mean A = (37 + 37 +29 +29)/4 = 33. bamboo shark (Chen et al 2008) A = (28.2 + 6.7 + 2.1) = 37, Rhinobatus annulatus & Myliobatus aquila (Du Preez et al 1990) A = (11 + 2 + 16) = 29, blue shark (Schindler et al 2002) = 37
Temp <- 25.1

#----
#Calculate mean mother mass
Foet_MR <- read.csv("Mobula_eregoodootenkee reproduction data- Latest update MBB_CL_CL EDITED COLUMNS.csv")

Foet_MR %>% group_by(Sex, Stage) %>% 
  summarise(meanMass = mean(Total.mass..kg.))

#Mean mass of pregnant females: 19.2 kg

#----
# Mean water temp at catch
Foet_MR %>% 
summarise(meanTemp = mean(Sea.surface.temperature..degrees.C., na.rm = T))
 #22.1575 degrees C


#---------------------------------

#Bioenergetics model
dat_E <- data.frame(matrix(ncol = 4, nrow = 365)) 
colnames(dat_E) <- c("Age(d)", "Temp", "Mass(g)", "Respiration(J/d)")

dat_E$`Age(d)` <- c(1:365)
dat_E$Temp <- Temp
dat_E$`Mass(g)` <- 0.1*dat_E$`Age(d)`^1.578 #See "mass-age of embryo" spreadsheet (I made this curve up to reach 1.1kg in 365 days)
dat_E$`Respiration(J/d)` <- (dat_E$`Mass(g)`*14140*(0.02*(dat_E$`Mass(g)`^-0.15)*exp((log(2.2)/10)*(dat_E$Temp-28)))) #-28 because of error in tailor paper (Ra and RB calculated at a specific temp, which was 28C for the refernce used here - Schindler et al 2002 (see Carlson et al 1999 for actual experiments)) 

#Total MR costs through gestation:
foet_energy <- 6633 #kJ, based on mass of 1.1 kg and mean ED of 6.03 kJ
sum(dat_E$`Respiration(J/d)`)/1000 #13,589.63 kJ
foet_total <- (sum(dat_E$`Respiration(J/d)`)/1000) + foet_energy #total 20,222.63 kJ

#---------------------------------
#Cost of gestation as percentage of mother SMR (for year)
#Create another bioenergetics model, using same parameters but for mother

dat_M <-  data.frame(matrix(ncol = 4, nrow = 365))
colnames(dat_M) <- c("Age(d)", "Temp", "Mass(g)", "Respiration(J/d)")

dat_M$`Age(d)` <- c(1:365)
dat_M$Temp <- Temp
dat_M$`Mass(g)` <- 19200 #average mass of mothers caught in this trial
dat_M$`Respiration(J/d)` <- (dat_M$`Mass(g)`*14140*(0.02*(dat_M$`Mass(g)`^-0.15)*exp((log(2.2)/10)*(dat_M$Temp-28))))

#Total annual SMR costs of mother:
sum(dat_M$`Respiration(J/d)`)/1000 #359,149.9 kJ

#Percentage cost of reproducton in terms of SMR:
foet_total*1000/sum(dat_M$`Respiration(J/d)`)*100
#5.63%

#-----------------
#Cost of gestation as percentage of mother total MR (for year)
#Assumptions:
  ## ACT = 1.2 (Schindler et al 2002) #UPDATE 17/9/19 THIS WAS UPDATED PREVIOUSLY TO ACT = 1.5, I THINK FROM PAPASTAMATIOU ET AL 2018 SCI REPORTS BUT LOOKING AT THE PAPER THIS IS WRONG, SO JUST SAY ASSUMING ACTIVITY COST OF 1.5
  ## No growth


ACT <- 1.5
Assim <- 0.33 #mean A = (37 + 37 +29 +29)/4 = 33. bamboo shark (Chen et al 2008) A = (28.2 + 6.7 + 2.1) = 37, Rhinobatus annulatus & Myliobatus aquila (Du Preez et al 1990) A = (11 + 2 + 16) = 29, blue shark (Schindler et al 2002) = 37
Temp <- 25.1  

dat_MR <-  data.frame(matrix(ncol = 4, nrow = 365))
colnames(dat_MR) <- c("Age(d)", "Temp", "Mass(g)", "Respiration(J/d)")


dat_MR$`Age(d)` <- c(1:365)
dat_MR$Temp <- Temp
dat_MR$`Mass(g)` <- 19200 #average mass of mothers caught in this trial
dat_MR$`Respiration(J/d)` <- ((dat_MR$`Mass(g)`*14140*(0.02*(dat_MR$`Mass(g)`^-0.15)*
                                                         exp((log(2.2)/10)*(dat_MR$Temp-28))))*ACT)/(1-Assim)

#Total annual MR costs of mother (including COT and SDA):
sum(dat_MR$`Respiration(J/d)`)/1000 #804,066.9 kJ


#Percentage cost of reproducton in terms of SMR:
foet_total*1000/sum(dat_MR$`Respiration(J/d)`)*100
#2.52%

