# Draws together covariate data for for the cluster analysis
# The covariates are used to calculate the seizure and reporting rate for all countries
# This is a larger set of countries than that used for the TI modelling
# Because the bias adjustment requires information on all countries that made a seizure
#__________________________________________________________________________________
# INPUTS
#  covars_Final_ALL_part1.csv  - covariates for all countries and years 
#  Ivory_Route_Ctry_Year un disc_TRUE_y1_y2_ALL.csv - Trade Chain Index for all countries and years
#  R package:
#   tidyverse
#__________________________________________________________________________________
# OUTPUTS
# csv file: covars_use_Final_ALL.csv  
#===================================================================================

library(tidyverse)
path.wkg <- 'C:/ETIS/analysis/Processed Data'
setwd(path.wkg)

year.from <- 2008
year.to <- 2017

file.cov <- 'covars_Final_ALL_part1.csv'

file.add.covars <- paste('Ivory Route Ctry_Year un disc_TRUE_', 
                         year.from, '_', year.to, '_ALL.csv', sep = '')

df.covars <- read.csv(file.cov, header=T)
df.add <- read.csv(file.add.covars, header = T)

#===========================================================
# Get correct set of data for df.covars
# Restrict the years

df.covars <- df.covars %>%
  filter(year >= year.from, year<= year.to)

# Revise South Sudan data to be from 2011 only

df.covars <- df.covars[!(df.covars$ctry == "ss" & df.covars$year < 2011),]               

#====================================================
# Trade chain variables
# Only variable that want is log.iv.rte2

df.add.use <- df.add %>%
  select(ctry, year, log.iv.tr.ch2 = log.iv.rte2)

#===========================================================
# Add in extra variables to df.covars

df.covars <- df.covars %>%
  left_join(df.add.use)# %>%

#===========================================================
# Write to file

filename.covars <- paste('covars_use_Final_ALL.csv', sep = '')
write.csv(df.covars, file = filename.covars, row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
