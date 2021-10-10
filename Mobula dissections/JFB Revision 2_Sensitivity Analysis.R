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
          #2b) average mother
    #3) Cost of reproduction:
          #3a) as % of yearly energy expenditure
          #3b) as % of maximum difference in HSI

#Housekeeping----
library(tidyverse)
library(ggplot2)

#BioE Model Function ----
#Create the bioenergetics model as a function

test_func2 <- function(x,y,z){
  print(x + y + z)
  print(x + y)
}

test_list <- list(list(5,10), list(7,6))
pmap(test_list,test_func) 

test_list2 <- list(y = c(6, 10), z = c(5,7), x = c(2,4))
pmap(test_list2,test_func2) 



test_func(1,2)  
test_func(test_list)



