#5 Dec 2019
#Chris Lawson 
#To estimate % cost of reproduction as calculated in Mourier et al 2016 Cell Bio (Reef sharks and inverted trophic pyramid)
#Results of this will be used in discussion

#Libraries ----


#Build BioE Models----
#Taken from formulas in sup material of paper
#General structure of models:

W <- a*L^(b)

M <- 4184*10^((0.7993*W + 2.5756)*24*13.598)

g <- a*(L + K(Linf - L))^(b) - a*L^(b)
r <- Wb * Ls 

G <- 5414.096*(g + r)/365.25

DR <- 1.37*(M+G) #DR = daily energy requirement

R <- 5414.096*(r)/365.25 #Energy cost of reproduction


#C. amblyrhynchos ----
#Set parameters:
a <- 0.00000136
b <- 3.34
Wb <- 1200
Ls <- 4
Linf <- 229.2
K <- 0.05
L <- mean(c(150,149,143, 155,169, 144, 152, 151,158,154,161,161,159))

#Make species-specifc BioE model:
W <- a*L^(b)

M <- 4184*W^((0.7993 + 2.5756)*24)*13.598

g <- a*(L + K*(Linf - L))^(b) - a*L^(b)
r <- Wb * Ls 

G <- 5414.096*(g + r)/365.25

DR <- 1.37*(M+G) #DR = daily energy requirement
DR
R <- 5414.096*(r)/365.25 #Energy cost of reproduction
R/DR*100


####No idea what they have actually done, these equations don't make sense. Not going to bother. 