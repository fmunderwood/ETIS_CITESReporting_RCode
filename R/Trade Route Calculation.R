# Calculates the Trade Chain Index
# For inclusion as potential covariate in seizures model
#
# This file is a modified version of a file that was used
# to explore possible ways of calculating a trade chain index
# So the method of calculation and final variable name is not 
# how one would do this from scratch
# But given extensive checking of the calculations carried out here it is retained 
# in this format for now
#
# TCI = log(dest.score + 1) - log(oet.score + 1)
# dest.score for country x in year y is number of times
# country x is listed as a country of destination divided by
# the number of seizures that list a country of destination
# Dest.score is calculated separately for raw and worked ivory and then averaged
#
# The oet.score is the average of the origin, export and transit scores
# these are each calculated in the same way as the dest.score
# Transit countries with no seizure opportunity are not included
#__________________________________________________________________________
# INPUTS
#  R file: PG settings.R to link to database
#  R file: SQL Select countries OETD.R 
#          These are SQL commands to select countries of origin, export, transit or destination
#  R package: RPostgreSQL
#  R package: tidyverse
#__________________________________________________________________________
# OUTPUTS
#  csv file: Ivory Route Ctry_Year un disc_TRUE_2008_2017.csv
#  Dataframe with year, country and trade chain index - here labelled as log.iv.rte2
#==========================================================================
#
#  Set range of years of seizure to include;
#   leave as 1900 - 2100 to include ALL years in database:
#
year.from <- 2008
year.to <- 2017
#_______________________________________________________________________
#
#  Do we count the country if it is also the country of discovery
#
incl.disc <- T
#_______________________________________________________________________
#
#  Set minimum Status of seizure records to include (normally 3):
#
statusMin <- 3
#_______________________________________________________________________
#
#  Set path name, in quotes, for working folder
#
path.code <- 'C:/ETIS/analysis/R code'
path.data <- 'C:/ETIS/analysis/Processed Data'
# 
#_______________________________________________________________________

setwd(path.code)
source('PG settings.R')
source('SQL Select countries OETD.R')
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

df.r.orig <- dbGetQuery(con, ifelse(incl.disc, SQLstr.r.orig.d, SQLstr.r.orig))
df.r.expt <- dbGetQuery(con, ifelse(incl.disc, SQLstr.r.expt.d, SQLstr.r.expt))  
df.r.tran <- dbGetQuery(con, ifelse(incl.disc, SQLstr.r.tran.d, SQLstr.r.tran))  
df.r.dest <- dbGetQuery(con, ifelse(incl.disc, SQLstr.r.dest.d, SQLstr.r.dest))  

df.w.orig <- dbGetQuery(con, ifelse(incl.disc, SQLstr.w.orig.d, SQLstr.w.orig))
df.w.expt <- dbGetQuery(con, ifelse(incl.disc, SQLstr.w.expt.d, SQLstr.w.expt))  
df.w.tran <- dbGetQuery(con, ifelse(incl.disc, SQLstr.w.tran.d, SQLstr.w.tran))  
df.w.dest <- dbGetQuery(con, ifelse(incl.disc, SQLstr.w.dest.d, SQLstr.w.dest))  

df.ctry <- dbGetQuery(con, SQLstr.country, stringsAsFactors = F)
df.sz.info <- dbGetQuery(con, SQLstr.0, stringsAsFactors = F)

list.r <- list(df.r.orig, df.r.expt, df.r.tran, df.r.dest)
list.w <- list(df.w.orig, df.w.expt, df.w.tran, df.w.dest)
ctry.un <- numeric()

df.raw <- df.sz.info %>%
  filter(!is.na(raw_pieces)|!is.na(raw_weight)) %>%
  select(id:raw_weight) %>%
  mutate(raw.est = is.na(raw_weight))

#________________________________________________________________________________
# Specifies ivory class for seizures where only number of pieces is provided
# Based on cut-offs from the estimating weights from pieces calculations
# This might need modifying in future years if the calculation changed
# Note that for the final calculations the ivory classes are not needed
# But original code tested out different strategies
# So this is retained for now

df.raw <- df.raw %>%
  mutate(raw_wt_est = ifelse(raw_pieces < 3, 1, 
                             ifelse(raw_pieces < 34, 2,
                                    ifelse(raw_pieces < 164, 3, 
                                           ifelse(raw_pieces < 288, 4, 5)))),
         raw.wt = ifelse(raw_weight < 10, 1,
                         ifelse(raw_weight < 100, 2, 
                                ifelse(raw_weight < 500, 3,
                                       ifelse(raw_weight < 1000, 4, 5)))),
         raw_wt = ifelse(raw.est, raw_wt_est, raw.wt)
  ) %>%
  select(id:discovered_country_id, r.gp = raw_wt)

df.wkd <- df.sz.info %>%
  filter(!is.na(worked_pieces)|!is.na(worked_weight)) %>%
  select(id:discovered_country_id, wkd_pieces = worked_pieces, wkd_weight = worked_weight) %>%
  mutate(wkd.est = is.na(wkd_weight))

df.wkd <- df.wkd %>%
  mutate(wkd_wt_est = ifelse(wkd_pieces < 126, 1, 
                             ifelse(wkd_pieces < 3194, 2, 3)),
         wkd.wt = ifelse(wkd_weight < 10, 1,
                         ifelse(wkd_weight < 100, 2, 
                                ifelse(wkd_weight < 500, 3,
                                       ifelse(wkd_weight < 1000, 4, 5)))),
         wkd_wt = ifelse(wkd.est, wkd_wt_est, wkd.wt)
  ) %>%
  select(id:discovered_country_id, w.gp = wkd_wt)


# Select relevant years and status
# Identify list of countries
list.rw <- list(NULL)

for (i in 1:4){
  list.rw[[i]] <- 
    left_join(list.r[[i]], df.raw, by = "id")
}

for (i in 1:4){
  list.rw[[i + 4]] <- 
    left_join(list.w[[i]], df.wkd, by = "id")
}


for (i in 1:8){
  list.rw[[i]] <- list.rw[[i]] %>%
    filter((seizure_year.x >=  year.from),
           (seizure_year.x <=  year.to),
           status_id.x >= statusMin) 
  list.rw[[i]] <- list.rw[[i]][,-c(2, 5, 6, 7)]
  names(list.rw[[i]]) <- c('id', 'year', 'ctry', 'iv.gp')
  ctry.un <- unique(c(ctry.un, list.rw[[i]]$ctry))
}

nyr <- year.to - year.from + 1
# ids for all ctries 
ct.id.used.all <- sort(ctry.un)

df.ctry.used <-  df.ctry %>%
  filter(is.element(id, ct.id.used.all)) %>%
  mutate(code.low = tolower(code)) %>%
  select(id, code = code.low) %>%
  arrange(code)

for (i in 1:8){
  list.rw[[i]] <- list.rw[[i]] %>%
    filter(!is.na(ctry)) %>%
    mutate(ctry = factor(as.numeric(ctry), levels = df.ctry.used$id, labels = df.ctry.used$code))
}

list.rw.un <- list(NULL)
for (i in 1:8){
  list.rw.un[[i]] <- list.rw[[i]] %>%
    distinct(id, .keep_all = T) %>%
    select(id:ctry, iv.gp)
}

tab.tot.iv.un  <- list(NULL) 

cols.ind <- 1:5

for (i in 1:8){

  tab.tot.iv.un[[i]] <- as.matrix(with(list.rw.un[[i]], table(year, iv.gp)))
  if(ncol(tab.tot.iv.un[[i]]) < 5){
    col.exist <- as.numeric(colnames(tab.tot.iv.un[[i]]))
    base.mod <- matrix(nrow = nyr, ncol = 5, data = 0)
    x <- 0
    for (z in col.exist){
      x <- x + 1
      base.mod[,z] <- tab.tot.iv.un[[i]][, x]
    }
    tab.tot.iv.un[[i]] <- base.mod
  }
}
# Now only select countries to be used in the ETIS analysis
ctry.use <- read.csv('CTS_ALL_17072018_tidy.csv')

ctry.name <- ctry.use$ctry


nm.yr.all <- nm.yr <-  list(NULL)
for (i in 1:8){
  nm.yr[[i]] <- list.rw[[i]][ , -4] %>%
    group_by(ctry, year) %>%
    summarise(y = n())
  nm.yr.all[[i]] <- expand.grid(ctry = as.character(ctry.name), year = year.from:year.to) %>%
    left_join(nm.yr[[i]], by  = c("ctry", "year")) %>%
    replace_na(list(y =0))
}

nyr <- year.to - year.from + 1

tab.num <- list(NULL)
prop.un <- list(NULL)
tab.tot.un <- matrix(nrow = nyr, ncol = 8, data = 0)
tot.un <- matrix(ncol = 8, data = 0)

for (i in 1:8){
  tab.num[[i]] <- with(nm.yr.all[[i]], tapply(y, list(ctry, year), sum))
  tab.tot.un[,i] <- apply(tab.tot.iv.un[[i]], 1, sum)
  prop.un[[i]] <- matrix(nrow = nrow(tab.num[[i]]), ncol = ncol(tab.num[[i]]))
  prop.un[[i]] <- t(100 * t(tab.num[[i]])/tab.tot.un[,i])
}

# Now combine raw and worked origin etc
rw.yr.un <-  list(NULL)
for (i in 1:4){
  rw.yr.un[[i]] <- 0.5 * (prop.un[[i]] + prop.un[[i + 4]])
}

 oet.yr.un <- (rw.yr.un[[1]] + rw.yr.un[[2]] + rw.yr.un[[3]])/3
log.iv.rte2.yr.un <- log(rw.yr.un[[4]] + 1) - log(oet.yr.un + 1)


# Convert to data.frame

iv.rte.yr.un.df <- expand.grid(ctry = ctry.name, year = year.from:year.to)
iv.rte.yr.un.df <- iv.rte.yr.un.df %>%
  mutate(
         log.iv.rte2 = as.numeric(log.iv.rte2.yr.un)
  )

# Write to file

setwd(path.data)
file.name <- paste("Trade Route Ctry_Year un disc_", incl.disc, "_", year.from, "_", year.to, ".csv", sep = "")
write.csv(iv.rte.yr.un.df, file = file.name, row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
