# Identifies seizures with multiple countries of origin
# and the whole of the trade chain
#
# This is so that weights out (*) can be changed 
# if the country only role in the trade chain is  as a country of origin 
# and only a proportion of the shipment comes from that country
#
# (*) Also need to correct seizures out when working with seizures of at least 500kg
#
#____________________________________________________________________________________________________
# INPUTS
# (a) PG settings.R
# (b)df_quantities_RIE_separate.R (calculates RIE for seizures with missing weights)
# (c)sz recs with estd wgts 1900_2100.csv (from sz data setup.R)
# (d) R packages:
#      RPostgreSQL
#      XLConnect
#      tidyverse
#____________________________________________________________________________________________________
# OUTPUTS
# A series of  files that list the:
#  countries of origin - multiple_origins_yr1_yr2.xlsx
#  country of discovery - multiple_disc_yr1_yr2.csv 
#  countries of export, transit and destination - multiple_rtes_yr1_yr2.csv
# for all seizures with more than one country of origin
# 
# These are used in Multiple mentions all wtlimit sims.R
#=====================================================================================================
# 
# input parameters from user:

year.from <- 1900 
year.to <- 2100

statusMin <- 3  # minimum level of seizure status field for inclusion in analysis
#
#____________________________________________________________________________________________________
#
library(RPostgreSQL)
library(XLConnect)
library(tidyverse)

# set working directory:

code.dir <- 'C:/ETIS/analysis/R Code'
data.dir <- 'C:/ETIS/analysis/Processed Data'

setwd(code.dir)

# these are to be set according to the postgresql installation settings:

source('PG settings.R')

drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, host = host.name, port = pg.port, user = user.name, dbname = db.name, password = passwd)

# get weights which are then saved in df.1
source('df_quantities_RIE_separate.R')  

df.wts <- df.1 %>%
  select(seizure_id = id, RIE.raw, RIE.wkd, RIE)

setwd(data.dir)

df.sz <- read.csv('sz recs with estd wgts 1900_2100.csv') # get seizure records
df.sz <- df.sz %>%
  select(seizure_id = sz.id, RIE.raw = raw.wgt, RIE.wkd = wkd.wgt) %>%
  mutate(RIE.raw = replace_na(RIE.raw, 0),
         RIE.wkd = replace_na(RIE.wkd, 0),
         RIE = RIE.raw + RIE.wkd)

df.wts <- df.wts %>%
  left_join(df.sz, by = "seizure_id") %>%
  select(seizure_id, 
         RIE.raw = RIE.raw.y, 
         RIE.wkd = RIE.wkd.y, 
         RIE = RIE.y)

df.1 <- df.1 %>%
  left_join(df.sz, by = c("id" = "seizure_id")) %>%
  select(id:worked_weight, 
         RIE.raw = RIE.raw.y, 
         RIE.wkd = RIE.wkd.y, 
         RIE = RIE.y)

remove(df.sz)

setwd(code.dir)

year.lab <- paste('_', year.from, '_', year.to, sep = '')

SQLstr.2 <- '
WITH  
tran_junc AS (
SELECT  seizure_id, 
CASE WHEN seizure_opportunity THEN country_id
ELSE NULL
END AS ct_id            -- set to NULL if no seizure opportunity
FROM public.seizure_transit_countries ), 
tmp AS (
SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
j1r.country_id AS ct_raw_orig_id, 
j1w.country_id AS ct_wkd_orig_id, 
j2.country_id AS ct_expt_id, 
j3.ct_id AS ct_tran_id, 
s.destination_country_id
FROM      public.seizures s
LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
WHERE     s.raw_pieces>0 OR s.raw_weight>0 OR s.worked_pieces>0 OR s.worked_weight>0
ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod, 
NULLIF(ct_wkd_orig_id, discovered_country_id) AS ct_wkd_orig_mod, 
NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod, 
NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod, 
NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'

df.countries <- dbReadTable(con, 'countries')
df.countries <- df.countries %>%
  select(id, code)

df.1 <- left_join(df.1, df.countries, by = c('discovered_country_id' = 'id')) %>%
  mutate(disc_code = code) %>% select(-code)

# df.2 has multiple rows for szs where multiple countries occur
df.2 <- dbGetQuery(con, SQLstr.2)  
y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.2 <- df.2 %>%
  filter(seizure_year >= y0 & seizure_year <= y1) %>%
  filter(status_id >= statusMin) %>%
  select(-status_id)

df.2 <- left_join(df.2, df.countries, by = c('ct_raw_orig_mod' = 'id')) %>%
  mutate(raw_orig_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_wkd_orig_mod' = 'id')) %>%
  mutate(wkd_orig_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_expt_mod' = 'id')) %>%
  mutate(expt_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_tran_mod' = 'id')) %>%
  mutate(tran_code = code) %>% select(-code)
df.2 <- left_join(df.2, df.countries, by = c('ct_dest_mod' = 'id')) %>%
  mutate(dest_code = code) %>% select(-code)


# find szs with multiple ctries of origin:

# raw
SQLstr.r <- 'SELECT j.id, j.seizure_id, j.country_id AS ct_orig_raw_id, 
             j.proportion, s.status_id, s.seizure_year
             FROM seizure_raw_origin_countries j
             JOIN seizures s ON j.seizure_id = s.id
             WHERE j.proportion < 100
             ORDER BY j.seizure_id'

df.raw <- dbGetQuery(con, SQLstr.r)
df.raw <- df.raw %>%
  filter(status_id >=  statusMin) %>%
  filter(seizure_year >=  year.from, seizure_year <=  year.to) %>%
  select( -status_id)

df.raw <- inner_join(df.raw, df.wts, by  = 'seizure_id')
df.raw <- df.raw %>%
  mutate(prop_raw_wt = RIE.raw*proportion/100)

df.raw <- left_join(df.raw, df.countries, by = c('ct_orig_raw_id' = 'id')) %>%
  mutate(raw_orig_code = code) %>% select(-code)

# wkd
SQLstr.w <- 'SELECT j.id, j.seizure_id, j.country_id AS ct_orig_wkd_id, 
             j.proportion, s.status_id, s.seizure_year
             FROM seizure_worked_origin_countries j
             JOIN seizures s ON j.seizure_id = s.id
             WHERE j.proportion < 100
             ORDER BY j.seizure_id'

df.wkd <- dbGetQuery(con, SQLstr.w)
df.wkd <- df.wkd %>%
  filter(status_id >=  statusMin) %>%
  filter(seizure_year >=  year.from, seizure_year <=  year.to) %>%
  select( -status_id)

df.wkd <- inner_join(df.wkd, df.wts, by  = 'seizure_id')
df.wkd <- df.wkd %>%
  mutate(prop_wkd_wt = RIE.wkd*proportion/100)

df.wkd <- left_join(df.wkd, df.countries, by = c('ct_orig_wkd_id' = 'id')) %>%
  mutate(wkd_orig_code = code) %>% select(-code)

# all seizures with mult cts of orig:
sz.r <- sort(unique(df.raw$seizure_id))
sz.w <- sort(unique(df.wkd$seizure_id))
sz.rw <- sort(unique(c(sz.r, sz.w)))

# szs with mult cts of orig:
df.1m <- df.1 %>%
  filter(id %in% sz.rw)
df.2m <- df.2 %>%
  filter(id %in% sz.rw)

#______________________________________________________________________
# Write to file

setwd(data.dir)

outfile <- paste("multiple_disc", year.lab, ".csv", sep = "")
write.csv(df.1m, file = outfile)
outfile <- paste("multiple_rtes", year.lab, ".csv", sep = "")
write.csv(df.2m, file = outfile)

outfile <- paste("multiple_origins", year.lab, ".xlsx", sep = "")
if(file.exists(outfile)) file.remove(outfile)
wb <- loadWorkbook(outfile, create = T)
createSheet(wb, name = 'raw')
createSheet(wb, name = 'worked')
writeWorksheet(wb, df.raw, 'raw')
writeWorksheet(wb, df.wkd, 'worked')
saveWorkbook(wb)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
