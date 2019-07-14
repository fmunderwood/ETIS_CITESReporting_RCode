#  Sets up covariates for countries x years for the cluster analysis.
#
#  Covariates for all countries in the database are extracted
#
#  Extracts subsidiary vars from the database 
#  and applies patches for incomplete series.
#
#  This file needs to be modified for each analysis 
#  depending on which variables are needed
#______________________________________________________________________________________
#  INPUTS
#     R package: RPostgreSQL
#     R package: tidyverse
#     R file: PG settings.R
#     R file: get subsid vars.R  Functions to extract data and var names from database
#______________________________________________________________________________________
#  OUTPUTS
#      covars_Final_ALL_part1.csv 
#      File contains relevant covariates summarised for each country in each year 
#=======================================================================================
#
#  Set range of years of seizure to include;
#   leave as 1900 - 2100 to include ALL years in database:
#
year.from <- 2008
year.to <- 2017
#______________________________________________________________________

#  Set path name, in quotes, for working folder
#
path.code <- 'C:/ETIS/analysis/R Code'
path.data <- 'C:/ETIS/analysis/Processed Data'
# 

setwd(path.code)
source('PG settings.R')
setwd(path.data)
#______________________________________________________________________


load.pkgs <- function() {
  library(RPostgreSQL)
  library(tidyverse)
}
suppressPackageStartupMessages(load.pkgs())
#
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, host = host.name, port = pg.port, user = user.name,
                 password = passwd, dbname = db.name)

setwd(path.code)
source('get subsid vars_pg.R')

# To inspect the currently available subsid vars in the DB, execute get.subsid.names()
get.subsid.names()

# id                          name                             source
# 1   1  corruption_perceptions_index         Transparency International
# 2   2          voice&accountability                     World Bank WGI
# 3   3           political_stability                     World Bank WGI
# 4   4      government_effectiveness                     World Bank WGI
# 5   5            regulatory_quality                     World Bank WGI
# 6   6                   rule_of_law                     World Bank WGI
# 7   7         control_of_corruption                     World Bank WGI
# 8   8               DC_mode_passive                               TESA
# 9   9              DC_mode_prompted                               TESA
# 10 10              DC_mode_targeted                               TESA
# 11 11             legislation_score                  CITES Secretariat
# 12 12 CITES_reporting_score_reports                  CITES Secretariat
# 13 13   CITES_reporting_score_years                  CITES Secretariat
# 14 14       human_development_index             UN Statistics Division
# 15 15                per_capita_gdp IMF/WEO (& UN Statistics Division)
# 16 16              gini_coefficient      World Bank poverty indicators
# 17 17                      LE_ratio                               TESA
# 18 18               LE_ratio_lagged                               TESA

# Variables that are needed are:
# DC variables 8 - 10
# Reporting score variables 12 - 13
# Lagged LE ratio 18
var.id <- c(8:10, 12:13, 18)

# this is slow so WAIT ...

df.all <- get.subsid.data(var.id = var.id, year.from = year.from, year.to = year.to)
df.all <- df.all[order(df.all$year, df.all$ctry),]

# List variables with missing data
num.miss <- array(data = NA, dim = c(length(unique(df.all$ctry)), length(unique(df.all$year)),
                                length(var.id)))
for (i in 1:length(var.id))
  num.miss[,,i] <- (with(df.all, tapply(is.na(df.all[ ,(i+2)]), list(ctry, year), sum)))

var.yr.miss <- apply(num.miss, c(2, 3), sum)
colnames(var.yr.miss) <- names(df.all)[-(1:2)]
rownames(var.yr.miss) <- year.from:year.to
t(var.yr.miss)

# Lots of missing data for all variables - mainly countries that haven't reported anything
# Make all of these values zero

# compute DC mode proportions prompted & targeted
df.all$all.sz <- df.all$DC_mode_passive + df.all$DC_mode_prompted + df.all$DC_mode_targeted
df.all$pr.prompted <- df.all$DC_mode_prompted/df.all$all.sz
df.all$pr.targeted <- df.all$DC_mode_targeted/df.all$all.sz
# assumed DC scores where missing
df.all$pr.prompted[is.na(df.all$pr.prompted)] <-0
df.all$pr.targeted[is.na(df.all$pr.targeted)] <-0

# assign LE ratio = 0 for 0/0 cases
df.all$LE_ratio_lagged[is.na(df.all$LE_ratio_lagged)] <- 0

# CITES reporting rate
df.all$rep.sc <- df.all$CITES_reporting_score_reports/df.all$CITES_reporting_score_years
df.all$rep.sc[is.na(df.all$rep.sc)] <- 0

# CITES reporting rate:  emp. logit
emp.logit <- function(y,n) log((y+0.5)/(n-y+0.5))
rep.log <-
  emp.logit(df.all$CITES_reporting_score_reports, df.all$CITES_reporting_score_years)
rep.log[is.na(rep.log)] <- emp.logit(0,1)

# prepare data frame for use in analysis
# Only those variables required for this repeat analysis are calculated
df.covars <- data.frame(ctry    = df.all$ctry,
                        year    = df.all$year,
                        dc.pr   = df.all$pr.prompted,
                        dc.ta   = df.all$pr.targeted,
                        LE1     = df.all$LE_ratio_lagged,
                        rep.log = rep.log)

# Write to file
setwd(path.data)

filename <- paste('covars_Final_ALL_part1.csv', sep='')
write.csv(df.covars, file=filename, row.names=F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
