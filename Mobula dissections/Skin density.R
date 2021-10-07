#Started 27/2/19 by Chris Lawson
#To look at differences in skin density (g/cm2)

library(ggplot2)

skin <- read.csv("Skin area and density calcs.csv")
head(skin)
str(skin)

ggplot(skin, aes(x = Dorsal.ventral, y = Density..g.cm2.)) + geom_boxplot() + theme_classic()

t.test(Density..g.cm2.~Dorsal.ventral, data = skin) #no sig diff

