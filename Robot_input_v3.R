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
             destinationPlates=c('160-p435','160-p436','160-p437','160-p438','160-p439','160-p440','160-p441','160-p442') # Could add another parameter if we wanted a date other than the system date
)
