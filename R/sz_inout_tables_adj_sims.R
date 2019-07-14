# Creates posterior distributions of adjusted seizures and weights in and out 
# using posterior predictive distributions of bias adjustment factor
#
# Similar to other dfinout_*.R file  used to calculate szs in and out
# for example for the LE ratio in database
# But with many modifications to deal with the specific requirements here
#
# Szs and weights in and out for each country and year are required for:
#  (i)  Calculating LE ratio - when country of destination is excluded from seizures out
#  (ii) Calculating Trade Flows - when country of destination is included in seizures out
# Both (i) and (ii) are needed for 
#  (I) For all seizures
#  (II)For seizures over 500kg
# So this file needs to be run four times with options for:
# (i) and (ii) determined by whether ctry_dest_included is FALSE (i) or TRUE (ii) 
# (I) and (II) determined by whether size.min is 0 (I) or 500 (II)
# 
# Note that there is a modification to seizure 109462 
# One country (188)  is marked as a transit country
# And on the chain of custody but it occured after the point of seizure
# So shipment should count for 188 as a country of destination (rather than transit)
# And therefore it is excluded from LE ratio calculation but not Trade Flow calculation
#_____________________________________________________________________________
# INPUTS
#  (a) PG settings.R
#  (b) sz recs with estd wgts yr1_yr2.csv - seizure records (from sz data setup.R)
#  (c) sz_adjusted_modnm_Final.csv - posterior distributions of bias adjustment factor (from Sz x rep rate for all countries for cluster analysis Final.R)
#  (d) covars_adj_ref.csv  - reference file for (c) above from same file
#  (e) R PACKAGEs
#      tidyverse
#____________________________________________________________________________________________________
# OUTPUTS:
# Series of .Rdata files with draws from the posterior predictive distributions of 
# seizures and weights in and out
# 
# Basic file name is:
#     Sims_ specify number of simulations
#     Adj_ variable calculated - sz_in, sz_out, wt_in, wt_out
#     modnm _ variable name
#     dest_  whether country of destination is included or not
#     size_  used if only for seizures of at least 500kg or not
#     
# For example the setup here would give the following file name:
#    Sims_10000Adj_sz_in_sz_Final_dest_FALSE_size_500.Rdata
#=========================================================================================

library(tidyverse)

#____________________________________________________________________________________________________
# INPUTS:
#  Set range of years of seizure to include;
#   leave as 1900 - 2100 to include ALL years in database:
#

year.from <- 1900
year.to <- 2100

#_______________________________________________________________________
# Size cutoff

size.min <- 500
#_______________________________________________________________________
#
#  Set minimum Status of seizure records to include (normally 3):
#
statusMin <- 3
#
#_______________________________________________________________________
#  Set path name, in quotes, for analysis folder
#
path.code <- 'C:/ETIS/analysis/R Code'
path.data <- 'C:/ETIS/analysis/Processed Data'
path.jags.data <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs '
#_______________________________________________________________________
#
#  Set flag for including ctry of destination in computing szs out
#
ctry_dest_included <- FALSE

setwd(path.code)
source('PG settings.R')

#______________________________________________________________________
# Identify model name

mod.nm <- 'sz_Final' # SET model name

# File where seizure records are
sz_recs_data <- 'sz recs with estd wgts 2007_2017.csv'

#______________________________________________________________________

load.pkgs <- function() {
  library(RPostgreSQL)
}
suppressPackageStartupMessages(load.pkgs())
#
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, host = host.name, port = pg.port, user = user.name, 
                 password = passwd, dbname = db.name)

#____________________________________________________________________________________________________

setwd(path.data)

df.1 <- read.csv(sz_recs_data)
pcs.wgts <- df.1 %>%
  select(sz.id, raw.pcs, raw.wgt, wkd.pcs, wkd.wgt) %>%
  filter(is.na(raw.pcs), is.na(raw.wgt), is.na(wkd.pcs), is.na(wkd.wgt))

df.1 <- df.1 %>%
  filter(raw.wgt >= size.min | wkd.wgt >= size.min)

df.1 <- df.1 %>%
  filter(sz.yr >= year.from, sz.yr <= year.to) %>%
  filter(!is.element(sz.id, pcs.wgts$sz.id)) %>%
  select(id = sz.id, 
         seizure_year = sz.yr, 
         ctry = disc.ct,
         raw_pieces = raw.pcs,
         raw_weight = raw.wgt,
         worked_pieces = wkd.pcs,
         worked_weight = wkd.wgt
         ) %>%
  mutate(raw_weight = replace_na(raw_weight,0),
         worked_weight = replace_na(worked_weight,0))


df.1 <- df.1 %>%
  mutate(RIE = raw_weight + worked_weight)
         
df.1$raw_weight[df.1$raw_weight ==0] <- NA
df.1$raw_weight[df.1$raw_weight ==0] <- NA


# Need to get country number for each code to be able to use later on
SQLstr.c <- '
 SELECT    c.id, c.code
 FROM      public.countries c 
 ORDER BY c.id;'
df.ctry <- dbGetQuery(con, SQLstr.c)
df.ctry$ctry <- tolower(df.ctry$code)
df.ctry <- df.ctry[,-2]

# Add country code number of df.1
df.1 <- df.1 %>%
  left_join(df.ctry, by = 'ctry') %>%
  select(id = id.x, seizure_year, ctry, ctry.code = id.y, raw_pieces:RIE)

# ids for all countries "in"
ct.id.used.in <- sort(unique(df.1$ctry.code))


# df for ctries of orig, expt, tran & dest each distinct from ctry of disc (non-ivory szs removed)
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
            j1.country_id AS ct_orig_id, 
            j1r.country_id AS ct_raw_orig_id, 
            j1w.country_id AS ct_wkd_orig_id, 
            j2.country_id AS ct_expt_id, 
            j3.ct_id AS ct_tran_id, 
            s.destination_country_id
  FROM      public.seizures s
  LEFT JOIN      public.seizure_origin_countries j1 ON s.id  =  j1.seizure_id
  LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id  =  j1r.seizure_id
  LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id  =  j1w.seizure_id
  LEFT JOIN      public.seizure_export_countries j2 ON s.id  =  j2.seizure_id
  LEFT JOIN      tran_junc j3 ON s.id  =  j3.seizure_id
  WHERE     s.raw_pieces>0 OR s.raw_weight>0 OR s.worked_pieces>0 OR s.worked_weight>0
  ORDER BY  s.id)
--  remove country (set to NULL) if same as country of discovery ...
SELECT    id, status_id, seizure_year, 
          NULLIF(ct_orig_id, discovered_country_id) AS ct_orig_mod, 
          NULLIF(ct_raw_orig_id, discovered_country_id) AS ct_raw_orig_mod, 
          NULLIF(ct_wkd_orig_id, discovered_country_id) AS ct_wkd_orig_mod, 
          NULLIF(ct_expt_id, discovered_country_id) AS ct_expt_mod, 
          NULLIF(ct_tran_id, discovered_country_id) AS ct_tran_mod, 
          NULLIF(destination_country_id, discovered_country_id) AS ct_dest_mod
FROM      tmp
ORDER BY  id
;'

df.2 <- dbGetQuery(con, SQLstr.2)  # has multiple rows for szs where multiple countries occur
# y0 <- max(min(df.1$seizure_year), year.from)
# y1 <- min(max(df.1$seizure_year), year.to)
# df.2 <- df.2[(df.2$seizure_year >= y0) & (df.2$seizure_year <= y1), ]
# df.2 <- df.2[df.2$status_id >= statusMin, ]
# df.2 <- df.2[, -2]  # finished with status field, so remove it

df.2 <- df.2 %>%
  filter(seizure_year >= year.from, seizure_year <= year.to, status_id >= statusMin)

df.2 <- df.2 %>%
  select(-status_id)
    
df.2 <- df.2 %>%
  filter(is.element(id, df.1$id))

#=================================================================================
# Correcting for error in data recording for seizure 109462 
# Country 188 is marked as having an opportunity to seize as a transit country, 
# But it sent seizure back. 
# Therefore when ctry_dest_included is FALSE 
# it needs to exclude this from the seizures out.
#=================================================================================
if(!ctry_dest_included){
  df.2$ct_tran_mod[df.2$id==109462] <- NA
}
#=================================================================================

# create list cts - each component (one for each seizure) is a vector of unique
# countries (ids) recorded outside the country of discovery

ID <- unique(df.2$id)
cts <- list(NULL)
yrs <- numeric(length(ID))

tt <- list(NULL)
for (i in 1:6)
  tt[[i]]<-tapply(df.2[, i + 2], df.2$id, unique, simplify = F)
# yr.use <- tapply(df.2[, 2], df.2[, 1], mn.fn)
yr.use <- tapply(df.2[, 2], df.2[, 1], first)

tt.id <- list(NULL)
yr.id <- list(NULL)
for (i in 1:6){
  lgth <- unlist(lapply(tt[[i]], length))
  tt.id[[i]] <- rep(names(tt[[i]]), lgth)
  yr.id[[i]] <- rep(yr.use, lgth)
}

if (ctry_dest_included){
  yr.id <- c(yr.id[[1]], yr.id[[2]], yr.id[[3]], yr.id[[4]], yr.id[[5]], yr.id[[6]])
  res <- cbind(as.numeric(c(tt.id[[1]], tt.id[[2]], tt.id[[3]], tt.id[[4]], tt.id[[5]], tt.id[[6]])), 
  as.numeric(c(unlist(tt[[1]]), unlist(tt[[2]]), unlist(tt[[3]]), unlist(tt[[4]]), unlist(tt[[5]]), unlist(tt[[6]]))), 
  yr.id)
} else {
  yr.id <- c(yr.id[[1]], yr.id[[2]], yr.id[[3]], yr.id[[4]], yr.id[[5]])
  res <- cbind(as.numeric(c(tt.id[[1]], tt.id[[2]], tt.id[[3]], tt.id[[4]], tt.id[[5]])), 
  as.numeric(c(unlist(tt[[1]]), unlist(tt[[2]]), unlist(tt[[3]]), unlist(tt[[4]]), unlist(tt[[5]]))), 
  yr.id)
}

ii <- is.na(res[, 2])
res <- res[!ii, ]
cts <-tapply(res[, 2], res[, 1], unique)

yr <- tapply(res[, 3], res[, 1], first)


#=================================================================================
# create years & RIE vectors to match (unlisted) countries in cts
len.cts <- unlist(lapply(cts, length))
yr.rep <- rep(yr, len.cts)
yr.val <- sort(unique(df.2$seizure_year))        # years that occur in DB
yr.out.fac <- factor(yr.rep, levels = yr.val)      # years as factor

# ids for all ctries "in" & "out"
ct.id.used.out <- sort(unique(unlist(df.2[, 3:8])))
ct.id.used.all <- sort(union(ct.id.used.in, ct.id.used.out))

# create countries factor for table of sz out
df.used.all <- subset(df.ctry, id %in% ct.id.used.all)
code.used.all <- df.used.all$ctry                # country codes used ("in" and "out")
cts.vec <- unlist(cts)
ct.out.fac <- factor(as.numeric(cts.vec), levels = ct.id.used.all, labels = code.used.all)

ct.in.fac <- factor(df.1$ctry.code, levels = ct.id.used.all, labels = code.used.all)
yr.in.fac <- factor(df.1$seizure_year, levels = yr.val)
sz.in.tbl <- table(ct.in.fac, yr.in.fac)
wt.in.tbl <- tapply(df.1$RIE, list(ct.in.fac, yr.in.fac), sum, na.rm = T)
wt.in.tbl[is.na(wt.in.tbl)] <- 0

#________________________________________________________
# Now need posterior distributions of bias adjusment
# Read in sz and rep rate    
setwd(path.jags.data)
sz.rep <- read.csv(paste('sz_adjusted_', mod.nm, '_ALL_Final.csv', sep = '')) 
sz.rep.cov <- read.csv('covars_adj_ref_Final.csv')

sz.rep <- sz.rep[,-1]
sz.rep.cov <- sz.rep.cov[,-1]
sz.rep.df <- cbind(sz.rep.cov, sz.rep)
num.sim <- ncol(sz.rep)

df.1.orig <- df.1

sz.out.tbl <- table(ct.out.fac, yr.out.fac)

sz.in.mat <- sz.out.mat <- wt.in.mat <- wt.out.mat <-
 array(dim = c( dim(sz.out.tbl)[1], num.sim, length(yr.val)), data = 0,
       dimnames = list(dimnames(sz.out.tbl)$ct.out.fac,NULL,dimnames(sz.out.tbl)$yr.out.fac))


for (i in 1:num.sim){
  df.1 <- df.1.orig %>%
  left_join(sz.rep.df[,c(1:2,(i + 2))], by = c('ctry', 'seizure_year' = 'year'), )
  names(df.1) <- c(names(df.1.orig), 'sz.adj')

# adjusted RIE
  df.1$RIE.adj <- df.1$RIE * df.1$sz.adj

  df.RIE <- df.1[is.element(df.1$id, as.numeric(names(cts))), ]
  df.RIE <- df.RIE[order(df.RIE$id), ]
  RIE.out <- rep(df.RIE$RIE, len.cts)


  df.szadj <- df.1[is.element(df.1$id, as.numeric(names(cts))), ]
  df.szadj <- df.szadj[order(df.szadj$id), ]
  szadj.out <- rep(df.szadj$sz.adj, len.cts)

  df.RIEadj <- df.1[is.element(df.1$id, as.numeric(names(cts))), ]
  df.RIEadj <- df.RIEadj[order(df.RIEadj$id), ]
  RIEadj.out <- rep(df.RIEadj$RIE.adj, len.cts)
    

# seizures & wgts "out" tables
  wt.out.tbl <- tapply(RIE.out, list(ct.out.fac, yr.out.fac), sum, na.rm = T)
  wt.out.tbl[is.na(wt.out.tbl)] <- 0

  szadj.out.tbl <- tapply(szadj.out, list(ct.out.fac, yr.out.fac), sum, na.rm = T)
  szadj.out.tbl[is.na(szadj.out.tbl)] <- 0

  wtadj.out.tbl <- tapply(RIEadj.out, list(ct.out.fac, yr.out.fac), sum, na.rm = T)
  wtadj.out.tbl[is.na(wtadj.out.tbl)] <- 0
    
  szadj.in.tbl <- tapply(df.1$sz.adj, list(ct.in.fac, yr.in.fac), sum, na.rm = T)
  szadj.in.tbl[is.na(szadj.in.tbl)] <- 0
 
  wtadj.in.tbl <- tapply(df.1$RIE.adj, list(ct.in.fac, yr.in.fac), sum, na.rm = T)
  wtadj.in.tbl[is.na(wtadj.in.tbl)] <- 0
   
  sz.in.mat[,i,] <- szadj.in.tbl
  sz.out.mat[,i,] <- szadj.out.tbl
  wt.in.mat[,i,] <- wtadj.in.tbl
  wt.out.mat[,i,] <- wtadj.out.tbl
  remove(df.1)
}  

# save as csv files
#sz.in
file.name <-paste('Sims_',num.sim,'Adj_sz_in_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  file.name <- paste(file.name, '_size_', size.min, sep = '')
}
saveRDS(sz.in.mat, file = paste(file.name, '.Rdata', sep = ''))

#sz.out
file.name <-paste('Sims_',num.sim,'Adj_sz_out_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  file.name <- paste(file.name, '_size_', size.min, sep = '')
}
saveRDS(sz.out.mat, file = paste(file.name, '.Rdata', sep = ''))

##wt.in
file.name <-paste('Sims_',num.sim,'Adj_wt_in_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  file.name <- paste(file.name, '_size_', size.min, sep = '')
}
saveRDS(wt.in.mat, file = paste(file.name, '.Rdata', sep = ''))

#wt.out
file.name <-paste('Sims_',num.sim,'Adj_wt_out_', mod.nm,'_dest_',ctry_dest_included, 
                  sep = '')
if(size.min > 0){
  file.name <- paste(file.name, '_size_', size.min, sep = '')
}
saveRDS(wt.out.mat, file = paste(file.name, '.Rdata', sep = ''))

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #




    
