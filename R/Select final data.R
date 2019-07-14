# This file draws together all the relevant data and covariates for 
# the seizures model analysis. 
#___________________________________________________________________________________
# INPUTS
#   R package: tidyverse
#   csv file: szs_ALL_ddmmyyyy.csv - set of seizure records
#   csv file: covars_use_ddmmyyyy.csv - main set of covariates for use
#   csv file: Trade Route Ctry_Year un disc_TRUE_y1_y2.csv - Trade Chain Index
#___________________________________________________________________________________
# OUTPUTS
#   Seizures data szs_use_Final.csv 
#   Covariates data covars_use_Final.csv
#
# This file should always be carefully reviewed prior to any new analysis
# because it may include additional exclusions or final adaptations to the data 
# (country and/or year) 
#
#====================================================================================
library(tidyverse)
path.data <- 'C:/ETIS/analysis/Processed Data'
setwd(path.data)

year.from <- 2007
year.to <- 2017

file.cov <- 'covars_19072018.csv'
file.szs <- 'szs_ALL_17072018.csv'

file.add.covars <- paste('Trade Route Ctry_Year un disc_TRUE_', 
                         year.from-1, '_', year.to, '.csv', sep = '')

df.covars <- read.csv(file.cov, header=T)
df.szs <- read.csv(file.szs, header=T)
df.add <- read.csv(file.add.covars, header = T)

#===========================================================
# Get correct set of data for df.szs and df.covars
# Restrict the years
df.szs <- df.szs %>%
  filter(year >= year.from, year<= year.to)

df.covars <- df.covars %>%
  filter(year >= year.from, year<= year.to)

# Revise South Sudan data to be from 2011 only - as was not a country prior to that
id <- 1:nrow(df.szs)
ii <- with(df.szs, id[ctry == "ss" & year < 2011])
df.szs <- df.szs[-ii, ]

df.covars <- df.covars[!(df.covars$ctry == "ss" & df.covars$year < 2011),]               

#====================================================
# Trade Route
# Include with and without lag
# So with lag add a .L at end

# Replace names of iv.rte2 with iv.tr.ch2 and same for others
names(df.add) <- gsub('rte', 'tr.ch', names(df.add))

df.lag <- df.add
df.lag$year <- df.add$year + 1

# Add ".L" to all names in df.lag other than ctry and year
names(df.lag)[-(1:2)] <- paste(names(df.lag)[-(1:2)], 'L', sep = '_')


#===========================================================
# Add in extra variable to df.covars
df.covars <- df.covars %>%
  left_join(df.add, by = c("ctry", "year")) %>%
  left_join(df.lag, by = c("ctry", "year")) 

# Remove 2007 - as only use 10 years for analysis
df.szs <- df.szs %>%
    filter(year > 2007)
  
df.covars <- df.covars %>%
    filter(year > 2007)
  
# Remove  Portugal - covariates currently under review
ctry.excl <- c("pt")

df.szs <- df.szs[!is.element(df.szs$ctry,ctry.excl),]
df.szs$ctry <- as.factor(as.character(df.szs$ctry))

df.covars <- df.covars[!is.element(df.covars$ctry,ctry.excl),]
df.covars$ctry <- as.factor(as.character(df.covars$ctry))

# Make sure everything matches
# Are the files the same size
nrow(df.szs) - nrow(df.covars)

# Are the same countries in each
table(is.element(levels(df.szs$ctry), levels(df.covars$ctry)))
table(is.element(levels(df.covars$ctry), levels(df.szs$ctry)))

# Are there the same number of observations for each year
table(df.szs$year) - table(df.covars$year)

# Are there the same number of observations for each country
table(df.szs$ctry) - table(df.covars$ctry)

# Make sure everything is in the same order
df.szs$sz.index <- 100*df.szs$year + as.numeric(df.szs$ctry)
df.covars$covar.index <- 100 * df.covars$year + as.numeric(df.covars$ctry)

diff.index <- df.szs$sz.index - df.covars$covar.index

df.szs[diff.index != 0,]

# If not - reorder here
# Reorder here
df.szs <- arrange(df.szs, sz.index)
df.covars <- arrange(df.covars, covar.index)

# Check again
diff.index <- df.szs$sz.index - df.covars$covar.index
df.szs[diff.index != 0,]

# Write out files to use in analysis

filename.szs <- 'szs_use_Final.csv'
write.csv(df.szs, file = filename.szs, row.names = F)
          
filename.covars <- 'covars_use_Final.csv'
write.csv(df.covars, file = filename.covars, row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
