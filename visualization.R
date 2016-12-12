# Import packages
library(gdata)      # Reading .csv
library(ggplot2)    # Visualization

# Change the working directory
setwd( "Projects/DataScience/Animales")
getwd()

# Open the database
data <- read.csv("train.csv")

# Get some info
summary(data)

# Animal type
ggplot( data, aes( x = AnimalType) ) + geom_bar()

# Outcomes
ggplot( data, aes( x = OutcomeType) ) + geom_bar()

# SexUponOutcome
ggplot( data, aes( x = SexuponOutcome) ) + geom_bar()

# Sex
ggplot( data, aes( x = OutcomeType) ) + geom_bar()

# Color
#unique(data$Color)
color_data = as.data.frame( table(data$Color) )

# Ordenar más frecuentes
color_data = color_data[ order(Freq) ]

# Gráfico de barras
ggplot( color_data, aes( x=))

ggplot( color_data, aes( x = Var1 ) )
