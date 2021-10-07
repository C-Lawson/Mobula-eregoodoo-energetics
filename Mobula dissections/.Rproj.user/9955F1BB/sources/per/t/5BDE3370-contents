#Started 17/6/19 Chris Lawson
#Looking at cost of growth for Mobula kuhlii

library(ggplot2)
#Von bert Growth rate parameters taken for Mobula japanica (Pardo et al 2016, Cuevas-Zimbron et al 2012)
  ##k = 0.12, DW0 (disc width at birth = 42cm, Broadhurst et al 2019) yields t0 = -1.68 y [UPDATE 22/6/19 - no it doesn't, not sure where this came from, t0 = 3.25 for birth size = 42cm] (Cuevas-Zimbron et al 2013)
  ##and adjusted for Mboula kuhlii max length 130cm (Broadhurst et al 2018)

#therefore von bert growth equation:

#DWt = 130(1-exp(-0.12(t+3.25)))

DWinf = 130
to = -3.25 #this was changed so size at birth matches what was found in largest embryo here (~42cm)
k = 0.12

#Length-weight regression (calculated from individuals in this study):

#mass(kg) = 0.00003*DiscWidth(cm)^2.84915
Ma = 0.00003
Mb = 2.84915

ACT <- 1.5
Assim <- 0.33 #mean A = (37 + 37 +29 +29)/4 = 33. bamboo shark (Chen et al 2008) A = (28.2 + 6.7 + 2.1) = 37, Rhinobatus annulatus & Myliobatus aquila (Du Preez et al 1990) A = (11 + 2 + 16) = 29, blue shark (Schindler et al 2002) = 37
Temp <- 25.1


dat_grow <- data.frame(matrix(ncol = 7, nrow = 365*20))
colnames(dat_grow) <- c("Age (y)", "Disc Width (cm)", "Mass (kg)", "Delta Mass (kg)", "Growth cost (kJ)", "Temp", "SMR (kJ/d)")

dat_grow$`Age (y)` <- c((1:(365*20))/365)
dat_grow$`Disc Width (cm)` <- DWinf*(1-exp(-k*(dat_grow$`Age (y)`-to)))
dat_grow$`Mass (kg)`<- Ma*dat_grow$`Disc Width (cm)`^Mb
dat_grow$`Delta Mass (kg)` <- (c(0.001, (dat_grow[2:nrow(dat_grow),3]-dat_grow[1:(nrow(dat_grow)-1),3]))) #Taken from bream bioE model
dat_grow$`Growth cost (kJ)` <- dat_grow$`Delta Mass (kg)`*6700 #see excel file "All rays component masses" for calculation of mean ED
dat_grow$`Growth cost (kJ)`[1] <- dat_grow$`Growth cost (kJ)`[2] #Change first value to equal second value, first was wrong and stuffing up the graph
dat_grow$`Temp` <- Temp
dat_grow$`SMR (kJ/d)` <- (dat_grow$`Mass (kg)`*14140*(0.002*(dat_grow$`Mass (kg)`^-0.15)*exp((log(2.2)/10)*(dat_grow$Temp-28))))
dat_grow$`growth/SMR (%)` <- dat_grow$`Growth cost (kJ)`/dat_grow$`SMR (kJ/d)`*100
dat_grow$`Respiration (kJ/d)` <- (dat_grow$`Mass (kg)`*14140*(0.002*(dat_grow$`Mass (kg)`^-0.15)*exp((log(2.2)/10)*(dat_grow$Temp-28))))*ACT/(1-Assim) #Taken from "metabolic cost of foetus" model
dat_grow$`growth/respiration (%)` <- dat_grow$`Growth cost (kJ)`/dat_grow$`Respiration (kJ/d)`*100

adult_growCost <- dat_grow %>% 
  filter(`Age (y)` == 12)
adult_growCost

juv_growCost <- dat_grow %>% 
  filter(`Age (y)` == 1)
juv_growCost

plot(dat_grow$`Age (y)`, dat_grow$`Disc Width (cm)`)
plot(dat_grow$`Age (y)`, dat_grow$`Mass (kg)`)
plot(dat_grow$`Disc Width (cm)`, dat_grow$`Mass (kg)`)
plot(dat_grow$`Age (y)`, dat_grow$`Growth cost (kJ)`)
plot(dat_grow$`Age (y)`, dat_grow$`growth/SMR (%)`)
plot(dat_grow$`Age (y)`, dat_grow$`growth/respiration (%)`)

#To get second axis, need to know the relative difference between the two axes #https://rpubs.com/MarkusLoew/226759
dat_grow$test <- dat_grow$`growth/SMR (%)`/dat_grow$`growth/respiration (%)` #2.238806 is the relative difference
grow_resp_dif <- dat_grow$test[1] #2.238806


ggplot() + 
  geom_line(data = dat_grow, aes(x = `Age (y)`, y = `growth/SMR (%)`))+
  labs(x= "Age (year)") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 18, colour = "black"), 
        axis.text.y = element_text(size = 18, colour = "black"),
        axis.line = element_line(colour = "black", 
                                 size = 0.5, linetype = "solid"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, margin = margin(r = 15)),
        plot.margin = unit(c(00, 0, 0, 0), "cm")) +
  scale_x_continuous(breaks = seq(0, 19.5,2), expand=c(0.0005,0)) + #http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
  scale_y_continuous(name = "Growth (% of SMR)", breaks = seq(0, 60,10), 
                     expand = c(0.052,0),
                     sec.axis = sec_axis(~./grow_resp_dif, #https://rpubs.com/MarkusLoew/226759
                                         name =('Growth (% of total energy use)\n'),
                                         breaks = seq(0,30,5)))   #https://stackoverflow.com/questions/22945651/how-to-remove-space-between-axis-area-plot-in-ggplot2


ggsave("Figure 2_growth.eps", width = 9, height = 7, units = c("in"), dpi = 600, )


#Now, estimates of SMR (without COT or SDA, same as foetus cost)

  
