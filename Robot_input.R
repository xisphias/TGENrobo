###############
# ROBO SCRIPT #
###############

setwd("~/Documents/R/Robo_csv")

source("Robot_function.R")

# Where we run the function
robotProgram(sourceFile='./input/RDMLtoTSVResults.tsv',
             outDir='./output/',
             firstSourceWell='A01',
             firstDestinWell='B11',
             destinationPlate_prefix='160-p',
             destinationPlate_first=359
)

# Could add another parameter if we wanted a date other than the system date