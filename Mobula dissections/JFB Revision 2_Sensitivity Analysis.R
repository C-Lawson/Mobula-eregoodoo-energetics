#Setup ----
#Started Chris Lawson 08/10/2021
#In response to Reviewer #1 at Journal of Fish Biology second round of comments who really wanted a proper sensitivity analysis
#Plan: half and double each parameter (as suggested by Anthony Richardson) and output total energy requirements from BioE model
# Given we already have a range of values based on min and max of all parameters simultaneously (unlikely to actually happen),
      #the sensitivity analysis will simply show which parameter has the largest influence (likely SMR Rb)
      #This will be mentioned in the text and sensitivity analysis added to the supplementary material, i.e.: 
      #"we provide a range of values given min/max values of each parameter, and a sensitivity analysis is provided in the supplementary material,
            #to show the relative sensitivity of the model to variation in each parameter. 


#Detailed plan:
# Copy code from "Sensitivity Analysis_JFB Revision 1.R" but automate it so don't need to copy and paste a million times.
# Make BioE model a single function
# Make a list of lists of parameters for each model iteration, e.g.:
    # 1) Model 1: half Ra, all other parameters mean.
    # 2) MOdel 2: double Ra, all other parameters mean.
    # 3) Model 3: half Rb, all other parameters mean.
    # 4) Model X: etc....
# Then, output total energy requirements for each model and see which has the biggest effect. Outputs:
    #1) Total energy expenditure. 
    #2) Cost of growth as % of total energy expenditure for:  
          #2a) one-year old
          #2b) average mother (age 12)
    #3) Cost of reproduction:
          #3a) as % of yearly energy expenditure
          #3b) as % of maximum difference in HSI

#Housekeeping----
library(tidyverse)

#BioE Model Function ----
#Create the bioenergetics model as a function



#create BioE model as a function
BioE <- function(DWinf, to, k, Ma, Mb, Temp_env, oxy, Q10, Ra, Rb, Temp_exp, ACT, Assim){
  temp_Bio_Model <- data.frame(matrix(ncol = 6, nrow = 365*20))
  colnames(temp_Bio_Model) <- c("Age (y)", "Disc Width (cm)", "Mass (kg)", "Delta Mass (kg)", "Growth cost (kJ)", "Temp")
  temp_Bio_Model$`Age (y)` <- c((1:(365*20))/365)
  temp_Bio_Model$`Disc Width (cm)` <- DWinf*(1-exp(-k*(temp_Bio_Model$`Age (y)`-to)))
  temp_Bio_Model$`Mass (kg)`<- Ma*temp_Bio_Model$`Disc Width (cm)`^Mb
  temp_Bio_Model$`Delta Mass (kg)` <- (c(0.001, (temp_Bio_Model[2:nrow(temp_Bio_Model),3]-temp_Bio_Model[1:(nrow(temp_Bio_Model)-1),3]))) #Taken from bream bioE model
  temp_Bio_Model$`Growth cost (kJ)` <- temp_Bio_Model$`Delta Mass (kg)`*6700 #see excel file "All rays component masses" for calculation of mean ED
  temp_Bio_Model$`Growth cost (kJ)`[1] <- temp_Bio_Model$`Growth cost (kJ)`[2] #Change first value to equal second value, first was wrong and stuffing up the graph
  temp_Bio_Model$`Temp` <- Temp_env
  temp_Bio_Model$`SMR (kJ/d)` <- (temp_Bio_Model$`Mass (kg)`*oxy*(Ra*(temp_Bio_Model$`Mass (kg)`^Rb)*
                                                            exp((log(Q10)/10)*(temp_Bio_Model$Temp-Temp_exp))))
  temp_Bio_Model$`Respiration (kJ/d)` <- ((temp_Bio_Model$`SMR (kJ/d)`*ACT)+temp_Bio_Model$`Growth cost (kJ)`)/(1-Assim) 
  print(sum(temp_Bio_Model$`Respiration (kJ/d)`))
  }

#test
Model <- BioE(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
              Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)
summary(Model)

#Create other functions for each required output
#cost of growth age 1
BioE_grow1 <- function(DWinf, to, k, Ma, Mb, Temp_env, oxy, Q10, Ra, Rb, Temp_exp, ACT, Assim){
  temp_Bio_Model <- data.frame(matrix(ncol = 6, nrow = 365*20))
  colnames(temp_Bio_Model) <- c("Age (y)", "Disc Width (cm)", "Mass (kg)", "Delta Mass (kg)", "Growth cost (kJ)", "Temp")
  temp_Bio_Model$`Age (y)` <- c((1:(365*20))/365)
  temp_Bio_Model$`Disc Width (cm)` <- DWinf*(1-exp(-k*(temp_Bio_Model$`Age (y)`-to)))
  temp_Bio_Model$`Mass (kg)`<- Ma*temp_Bio_Model$`Disc Width (cm)`^Mb
  temp_Bio_Model$`Delta Mass (kg)` <- (c(0.001, (temp_Bio_Model[2:nrow(temp_Bio_Model),3]-temp_Bio_Model[1:(nrow(temp_Bio_Model)-1),3]))) #Taken from bream bioE model
  temp_Bio_Model$`Growth cost (kJ)` <- temp_Bio_Model$`Delta Mass (kg)`*6700 #see excel file "All rays component masses" for calculation of mean ED
  temp_Bio_Model$`Growth cost (kJ)`[1] <- temp_Bio_Model$`Growth cost (kJ)`[2] #Change first value to equal second value, first was wrong and stuffing up the graph
  temp_Bio_Model$`Temp` <- Temp_env
  temp_Bio_Model$`SMR (kJ/d)` <- (temp_Bio_Model$`Mass (kg)`*oxy*(Ra*(temp_Bio_Model$`Mass (kg)`^Rb)*
                                                                    exp((log(Q10)/10)*(temp_Bio_Model$Temp-Temp_exp))))
  temp_Bio_Model$`Respiration (kJ/d)` <- ((temp_Bio_Model$`SMR (kJ/d)`*ACT)+temp_Bio_Model$`Growth cost (kJ)`)/(1-Assim) 
  temp_Bio_Model$`growth/respiration_mean (%)` <- temp_Bio_Model$`Growth cost (kJ)`/temp_Bio_Model$`Respiration (kJ/d)`*100
  print(temp_Bio_Model$`growth/respiration_mean (%)`[365]) 
}

#test
BioE_grow1(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
              Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)


#Cost of growth age 
BioE_growAdult <- function(DWinf, to, k, Ma, Mb, Temp_env, oxy, Q10, Ra, Rb, Temp_exp, ACT, Assim){
  temp_Bio_Model <- data.frame(matrix(ncol = 6, nrow = 365*20))
  colnames(temp_Bio_Model) <- c("Age (y)", "Disc Width (cm)", "Mass (kg)", "Delta Mass (kg)", "Growth cost (kJ)", "Temp")
  temp_Bio_Model$`Age (y)` <- c((1:(365*20))/365)
  temp_Bio_Model$`Disc Width (cm)` <- DWinf*(1-exp(-k*(temp_Bio_Model$`Age (y)`-to)))
  temp_Bio_Model$`Mass (kg)`<- Ma*temp_Bio_Model$`Disc Width (cm)`^Mb
  temp_Bio_Model$`Delta Mass (kg)` <- (c(0.001, (temp_Bio_Model[2:nrow(temp_Bio_Model),3]-temp_Bio_Model[1:(nrow(temp_Bio_Model)-1),3]))) #Taken from bream bioE model
  temp_Bio_Model$`Growth cost (kJ)` <- temp_Bio_Model$`Delta Mass (kg)`*6700 #see excel file "All rays component masses" for calculation of mean ED
  temp_Bio_Model$`Growth cost (kJ)`[1] <- temp_Bio_Model$`Growth cost (kJ)`[2] #Change first value to equal second value, first was wrong and stuffing up the graph
  temp_Bio_Model$`Temp` <- Temp_env
  temp_Bio_Model$`SMR (kJ/d)` <- (temp_Bio_Model$`Mass (kg)`*oxy*(Ra*(temp_Bio_Model$`Mass (kg)`^Rb)*
                                                                    exp((log(Q10)/10)*(temp_Bio_Model$Temp-Temp_exp))))
  temp_Bio_Model$`Respiration (kJ/d)` <- ((temp_Bio_Model$`SMR (kJ/d)`*ACT)+temp_Bio_Model$`Growth cost (kJ)`)/(1-Assim) 
  temp_Bio_Model$`growth/respiration_mean (%)` <- temp_Bio_Model$`Growth cost (kJ)`/temp_Bio_Model$`Respiration (kJ/d)`*100
  print(temp_Bio_Model$`growth/respiration_mean (%)`[365*12]) 
}

#test
BioE_growAdult(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
           Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)




#Cost of reproduction (yearly energy expenditure):
BioE_repro_annual <- function(DWinf, to, k, Ma, Mb, Temp_env, oxy, Q10, Ra, Rb, Temp_exp, ACT, Assim){
    #embryo costs:
  dat_E <- data.frame(matrix(ncol = 4, nrow = 365)) 
  colnames(dat_E) <- c("Age(d)", "Temp", "Mass(g)", "Respiration(J/d)")
  
  dat_E$`Age(d)` <- c(1:365)
  dat_E$Temp <- Temp_env
  dat_E$`Mass(g)` <- 0.1*dat_E$`Age(d)`^1.578 #See "mass-age of embryo" spreadsheet (I made this curve up to reach 1.1kg in 365 days)
  dat_E$`Mass(kg)` <- dat_E$`Mass(g)`/1000
  dat_E$`SMR (kJ/d)` <- (dat_E$`Mass(kg)`*oxy*(Ra*(dat_E$`Mass(kg)`^Rb)*
                                                     exp((log(Q10)/10)*(dat_E$Temp-Temp_exp))))

  foet_total <- sum(dat_E$`SMR (kJ/d)`) + 6633 #foet_energy <- 6633 #kJ, based on mass of 1.1 kg and mean ED of 6.03 kJ
  
  #Cost of reproduction as percentage of mother total MR (for year)
    #Assumes no growth
  dat_MR <-  data.frame(matrix(ncol = 2, nrow = 365))
  colnames(dat_MR) <- c("Age(d)", "Temp")
  
  dat_MR$`Age(d)` <- c(1:365)
  dat_MR$Temp <- Temp_env
  dat_MR$`Mass(kg)` <- 19.2 #average mass of mothers caught in this trial
  
  dat_MR$`Respiration (kJ/d)` <- 
    (dat_MR$`Mass(kg)`*oxy*(Ra*(dat_MR$`Mass(kg)`^Rb)*
                              exp((log(Q10)/10)*(dat_MR$Temp-Temp_exp))))*ACT/(1-Assim)
 
  print(foet_total/sum(dat_MR$`Respiration (kJ/d)`)*100)
}

#test
BioE_repro_annual(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
               Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)



BioE_repro_HSI <- function(DWinf, to, k, Ma, Mb, Temp_env, oxy, Q10, Ra, Rb, Temp_exp, ACT, Assim){
  #embryo costs:
  dat_E <- data.frame(matrix(ncol = 4, nrow = 365)) 
  colnames(dat_E) <- c("Age(d)", "Temp", "Mass(g)", "Respiration(J/d)")
  
  dat_E$`Age(d)` <- c(1:365)
  dat_E$Temp <- Temp_env
  dat_E$`Mass(g)` <- 0.1*dat_E$`Age(d)`^1.578 #See "mass-age of embryo" spreadsheet (I made this curve up to reach 1.1kg in 365 days)
  dat_E$`Mass(kg)` <- dat_E$`Mass(g)`/1000
  dat_E$`SMR (kJ/d)` <- (dat_E$`Mass(kg)`*oxy*(Ra*(dat_E$`Mass(kg)`^Rb)*
                                                 exp((log(Q10)/10)*(dat_E$Temp-Temp_exp))))
  
  foet_total <- sum(dat_E$`SMR (kJ/d)`) + 6633 #foet_energy <- 6633 #kJ, based on mass of 1.1 kg and mean ED of 6.03 kJ
  
  
  # check maximum difference in HSI:
    #min HSI for female (gravid) = 2.4%, max HSI for female (mature ovulating) = 5.5%
    ((5.5-2.4)/100)*19200
  #works out to be a difference of 595.2 g of liver between gravid and no gravid
  #Now if liver tissue has ED of 23.2 kJ/g:
  max_HSI_diff <- 595.2*23.2 #13,808.64 kJ
  
  print(max_HSI_diff/foet_total*100) #Difference accounts for X% of total reproductive costs.
    
  }


#test
BioE_repro_HSI(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
                  Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)



#Apply BioE function to a list of parameters----
# constant parameters:
DWinf <- 130
to <- -3.25
k <- 0.12
Ma <- 0.00003
Mb <- 2.84915
Temp_exp <- 24 #24 #temperature Ra was derived at

DWinf_list <- rep(list(DWinf), 15)
to_list <- rep(list(to), 15)
k_list <- rep(list(k), 15)
Ma_list <- rep(list(Ma), 15)
Mb_list <- rep(list(Mb), 15)
Temp_exp_list <- rep(list(Temp_exp), 15)

# parameters to vary in the sensitivity analysis (7 parameters, vary each by +/- 25%, so 14 models total to check effect + original = 15 MODELS):
Temp_env <- 25.1
oxy <- 14.140 #kJ/g
Q10 <- 2.435
Ra <- 11.44  #(gO2/kg/d)
Rb <- -0.18
ACT <- 1.5
Assim <- 0.33


#Model run-list:
#1: all normal (control/to compare to)
#2: oxy + 25%
#3: oxy - 25%
#4: Q10 + 25%
#5: Q10 - 25%
#6: Ra + 25%
#7: Ra - 25%
#8: Rb + 25%
#9: Rb - 25%
#10: Temp_env +25%
#11: Temp_env -25%
#12: ACT + 25%
#13: ACT - 25%
#14: Assim + 25%
#15: Assim - 25%


oxy_list <- list(oxy, oxy*1.25, oxy*0.75, oxy, oxy, oxy, oxy, oxy, oxy, oxy, oxy, oxy, oxy, oxy, oxy) #there must be a better way but out of time
Q10_list <- list(Q10, Q10, Q10, Q10*1.25, Q10*0.75, Q10, Q10, Q10, Q10, Q10, Q10, Q10, Q10, Q10, Q10)
Ra_list <- list(Ra, Ra, Ra, Ra, Ra, Ra*1.25, Ra*0.75, Ra, Ra, Ra, Ra, Ra, Ra, Ra, Ra)
Rb_list <- list(Rb, Rb, Rb, Rb, Rb, Rb, Rb, Rb*1.25, Rb*0.75, Rb, Rb, Rb, Rb, Rb, Rb)
Temp_env_list <- list(Temp_env, Temp_env, Temp_env, Temp_env, Temp_env, Temp_env, Temp_env, Temp_env, Temp_env, 
                      Temp_env*1.25, Temp_env*0.75, Temp_env, Temp_env, Temp_env, Temp_env)
ACT_list <- list(ACT, ACT, ACT, ACT, ACT, ACT, ACT, ACT, ACT, ACT, ACT, ACT*1.25, ACT*0.75, ACT, ACT)
Assim_list <- list(Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim, Assim*1.25, Assim*0.75)

param_list <- list(DWinf_list, to_list, k_list, Ma_list, Mb_list, Temp_env_list, oxy_list, Q10_list, Ra_list, Rb_list,
                   Temp_exp_list, ACT_list, Assim_list) 


#Run Model ----
param_model_list <- list()

for(i in 1:15){
   param_model_list[[i]] <- list(DWinf_list[[i]], to_list[[i]], k_list[[i]], Ma_list[[i]], Mb_list[[i]], Temp_env_list[[i]], oxy_list[[i]],
                         Q10_list[[i]], Ra_list[[i]], Rb_list[[i]], Temp_exp_list[[i]], ACT_list[[i]], Assim_list[[i]])
  print(i)
}

#Create for loop to run all models at once
model_outputs <- data.frame(matrix(ncol = 10, nrow = 15))
colnames(model_outputs) <- c("Lifetime Energy (MJ)", "Difference % (lifetime energy)", "Growth cost % (age 1)",
                             "Difference % (growth age 1)","Growth cost % (age 12)", "Difference % (growth age 12)",
                             "Annual Reproduction Cost %", "Difference % (annual reproduction cost)", 
                             "Reproduction Cost (% liver stores)", "Difference % (reproductive use of liver stores)")
rownames(model_outputs) <- c("Control", "oxy + 25%", "oxy - 25%", "Q10 + 25%", "Q10 - 25%", "Ra + 25%", "Ra - 25%",
                             "Rb + 25%", "Rb - 25%", "Temperature + 25%","Temperature -25%", "ACT + 25%", "ACT - 25%",
                             "Assim + 25%", "Assim - 25%")

#calculate lifetime costs
for(i in 1:15){
  model_outputs[i,1] <- pmap(param_model_list[[i]], BioE)
}

model_outputs$`Lifetime Energy (MJ)` <- model_outputs$`Lifetime Energy (MJ)`/1000

# Compare lifetime costs
for(i in 1:15){
  model_outputs[i,2] <- model_outputs[i,1]/model_outputs[1,1]*100 - 100
}

#Calculate growth costs at age 1
for(i in 1:15){
  model_outputs[i,3] <- pmap(param_model_list[[i]], BioE_grow1)
}

#Compare growth costs at age 1
for(i in 1:15){
  model_outputs[i,4] <- model_outputs[i,3]/model_outputs[1,3]*100 - 100
}

#Calculate growth costs of adult
for(i in 1:15){
  model_outputs[i,5] <- pmap(param_model_list[[i]], BioE_growAdult)
}

#Compare growth costs of adult
for(i in 1:15){
  model_outputs[i,6] <- model_outputs[i,5]/model_outputs[1,5]*100 - 100
}

#Calculate repro costs as proportion of annual energy cost of mum
for(i in 1:15){
  model_outputs[i,7] <- pmap(param_model_list[[i]], BioE_repro_annual)
}

#Compare repro costs as proportion of annual energy cost of mum
for(i in 1:15){
  model_outputs[i,8] <- model_outputs[i,7]/model_outputs[1,7]*100 - 100
}

#Calculate repro costs as maximum difference in HSI
for(i in 1:15){
  model_outputs[i,9] <- pmap(param_model_list[[i]], BioE_repro_HSI)
}

#Compare repro costs as maximum difference in HSI
for(i in 1:15){
  model_outputs[i,10] <- model_outputs[i,9]/model_outputs[1,9]*100 - 100
}

write_csv(model_outputs, "Sensitivity Analysis 2 Outputs.csv")

##IGNORE BELOW ----
#AHHH ----
model_test <- pmap(DWinf = DWinf_list, to = to_list, k = k_list,Ma = Ma_list, Mb = Mb_list, Temp_env = Temp_env_list, 
                   oxy = oxy_list, Q10 = Q10_list, Ra = Ra_list, Rb = Rb_list,
                   Temp_exp = Temp_exp_list, ACT = ACT_list, Assim = Assim_list, .f = BioE())

Modeltest3 <- BioE(130, 0.12, -3.25,  0.00003,  2.84915,  25.1,  14.140,  2.435,  11.44,
               -0.18,  24, 1.5, 0.33)

Model <- BioE(DWinf = 130, k = 0.12, to = -3.25, Ma = 0.00003, Mb = 2.84915, Temp_env = 25.1, oxy = 14.140, Q10 = 2.435, Ra = 11.44,
              Rb = -0.18, Temp_exp = 24, ACT = 1.5, Assim = 0.33)


#THIS IS HOW THE STRUCTURE NEEDS TO BE FOR pmap TO DO ITS THING - NOPE NOT ANYMORE
#i.e. each parameter it's own list, where the order is absolutely critical
test_func <- function(x,y,z){
  return(x + y + z)
  }

test_func(1,2,3)

pmap(.l=list(1,2,3), test_func)

test_x <- list(1,2,3)
test_y <- list(4,5,6)
test_z <- list(7,8,9)
test5_output <- pmap(.l = list(test_x, test_y, test_z), test_func)





# test_func(test_x, test_y, test_z)
# 
# 
# test4_outpout <- pmap(test_list4[1],
#                            test_list4[2],
#                            test_list4[3], test_func())
# 

# test_list4 <- list(list(1,2,3),
                   # list(4,5,6),
                   # list(7,8,9))

# test_list3 <- list(list(x = 1, y = 2, z = 3), list(x = 4, y = 5, z = 6))
# 
# test3_output <- pmap(test_list3,test_func) #nope
# test3_output <- pmap(test_list3,test_func) 

# test_list <- list(list(5,10), list(7,6))
# pmap(test_list,test_func) 
# 
# test_list2 <- list(y = c(6, 10), z = c(5,7), x = c(2,4)) #nope


