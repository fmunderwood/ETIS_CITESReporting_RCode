#  Extracts seizure records for analysis from ETIS database
#    with or without non-ivory cases
#___________________________________________________________________________
#  INPUTS
#    R file: PG settings.R
#___________________________________________________________________________
#  OUTPUTS
#    Saves results in 'ivory sz recs yrfrom_yrto.csv' - if only consider ivory cases
#    or             'all sz recs yrfrom_yrto.csv' - if include non-ivory cases
#===========================================================================
#
#  Set range of years of seizure either;
#   leave as 1900 - 2100 to include ALL years in database & a subset of this 
#      used to estimate weights from pieces
#   set specific range for COP or SC analysis
#
year.from <- 2007
year.to <- 2017
#______________________________________________________________________
#
#  Set minimum Status of seizure records to include (normally 3):
#
statusMin <- 3
#______________________________________________________________________
#
#  Set path name, in quotes, for working folder
#
path.Rcode <- 'C:/ETIS/analysis/R Code'
path.data <- 'C:/ETIS/analysis/Processed Data'

#______________________________________________________________________
#
#  Set indicator to exclude non-ivory seizures or not
#
exclude.nonivory <- TRUE
#
#______________________________________________________________________
# Link to ETIS database

setwd(path.Rcode)
source('PG settings.R')

#____________________________________________________________________________________________________

setwd(path.data)

load.pkgs <- function() {
  library(RPostgreSQL)
  library(tidyverse)
}
suppressPackageStartupMessages(load.pkgs())
#
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, host=host.name, port = pg.port, user = user.name,
                 password = passwd, dbname = db.name)

# get sz records from database
SQLstr <- '
SELECT  s.id AS "sz.id",
        s.seizure_year AS "sz.yr",
        LOWER(c.code) AS "disc.ct",
        s.raw_pieces AS "raw.pcs",
        s.raw_weight AS "raw.wgt",
        s.raw_present_amount_unknown AS "raw.pres",
        s.worked_pieces AS "wkd.pcs",
        s.worked_weight AS "wkd.wgt",
        s.worked_present_amount_unknown AS "wkd.pres",
        s.status_id AS "status.id"
FROM    public.seizures s
  JOIN  public.countries c
  ON    s.discovered_country_id = c.id
ORDER BY s.id
'
df.szrecs <- dbGetQuery(con, SQLstr)

#select years & status
y0 <- max(min(df.szrecs$sz.yr), year.from)
y1 <- min(max(df.szrecs$sz.yr), year.to)

df.szrecs.tidy <- df.szrecs %>%
  filter(sz.yr >= y0, sz.yr <= y1) %>%
  filter(status.id >= statusMin) %>%
  select(-status.id)

# set flags 'raw' & 'wkd' for each sz
# Sets NAs to zero or FALSE to make selection easier

 df.szrecs.tidy <- df.szrecs.tidy %>%
  mutate(i.raw.pcs = raw.pcs > 0 &  !is.na(raw.pcs)) %>%
  mutate(i.raw.wgt = raw.wgt > 0 &  !is.na(raw.wgt)) %>%
  mutate(i.raw.pres = raw.pres) %>%
  mutate(i.wkd.pcs = wkd.pcs > 0 &  !is.na(wkd.pcs)) %>%
  mutate(i.wkd.wgt = wkd.wgt > 0 &  !is.na(wkd.wgt)) %>%
  mutate(i.wkd.pres = wkd.pres) %>%
  mutate(raw = i.raw.pcs | i.raw.wgt | i.raw.pres) %>%
  mutate(wkd = i.wkd.pcs | i.wkd.wgt | i.wkd.pres) 
  
  

if(exclude.nonivory) {
  df.szrecs.tidy <- df.szrecs.tidy %>%
    filter(raw | wkd)
}


df.szrecs.tidy <- df.szrecs.tidy %>%
  mutate(raw.pcs = replace(raw.pcs, raw.pcs == 0, NA)) %>%
  mutate(raw.wgt = replace(raw.wgt, raw.wgt == 0, NA)) %>%
  mutate(raw.pres = replace(raw.pres, raw.pres == F, NA)) %>%
  mutate(wkd.pcs = replace(wkd.pcs, wkd.pcs == 0, NA)) %>%
  mutate(wkd.wgt = replace(wkd.wgt, wkd.wgt == 0, NA)) %>%
  mutate(wkd.pres = replace(wkd.pres, wkd.pres == F, NA)) %>%
  select(-(i.raw.pcs:i.wkd.pres))

# output
filename <- paste('sz recs ', year.from, '_', year.to, '_tidy.csv', sep='')
if(exclude.nonivory) {
  filename <- paste('ivory', filename, sep=' ')
} else {
  filename <- paste('all', filename, sep=' ')
}
write.csv(df.szrecs.tidy, file=filename, row.names=F)

rm(SQLstr, con, filename,y0, y1)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
