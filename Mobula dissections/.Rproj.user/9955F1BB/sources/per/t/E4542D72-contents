#Started 19/12/19 Chris lawson
#Creating plot of disc-width v mass regression from animals collected in Ballina shark nets

#Packages ----
library(tidyverse)
library(visreg)

#Import and tidy data ----
dat <- read_csv("Mobula_eregoodootenkee reproduction data- Latest update MBB_CL_CL EDITED COLUMNS.csv")
str(dat)

#One entry is "??" instead of NA
dat$`Disc width (cm)` <- ifelse(dat$`Disc width (cm)` == "??", NA, dat$`Disc width (cm)`) %>% 
  as.numeric(dat$`Disc width (cm)`)
str(dat)

dat <- dat %>% 
  drop_na(`Disc width (cm)`)



#Create data for adding line because ggplot sucks shit at plotting curves 

df <- data.frame(matrix(ncol = 2, nrow = 1250))
colnames(df) <- c("x","y")
df$x <- seq(0.1,125, 0.1)
df$y <- 0.000025095623077*(df$x)^(2.849150254809490) #generated from excel because I'm lazy and frustrated with R right now

ggplot() + 
  geom_point(data = dat, aes(x = `Disc width (cm)`, y = `Total mass (kg)`)) +
  geom_line(data = df, aes(x = x, y = y)) +
  theme_bw()


###
#Updates 24/11/2020 J Fish Biology Review #1
#Generate R2 and p value for length-weight regression (for reviewer comments) #----

lm1 <- lm(`Total mass (kg)` ~ `Disc width (cm)`, data = dat)
summary(lm1)
plot(lm1) #crap but whatever, p value is very small

visreg(lm1)

lm2  <- lm(log(`Total mass (kg)`) ~ `Disc width (cm)`, data = dat)
plot(lm2)

