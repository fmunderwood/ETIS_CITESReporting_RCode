#  Extracts data used to calculate inclusion score  for selecting countries 
#  to be included in analysis. Score is calculated in 'sz datasets_ALL gps.R'
#
#  Computes szs in/out for each country in each category group 
#  The three category groups (small, medium, large) are used in calculations
#  to assess whether a country meets the inclusion criteria or not
#
#  This file allows for the following to be specified:
#     years to include
#     seizure opportunity to be included or not
#     country of destination to be included or not 
#_______________________________________________________________________
#  INPUTS 
#        (1) R file: PG Settings.R
#        (2) R workspace: 'wt est.Rdata' (update if necessary)
#_______________________________________________________________________
#  OUTPUTS 
#      csv file with root of name as 'dfinout_gps_
#      transallop_',  ctry_noszopp_included :  whether countries with no seizure opportunity are included
#      '_ctrydest_', ctry_dest_included :  whether country of destination included
#      '_', year.from, '_', year.to,'.csv' : years to consider
#
#     eg dfinout_gps_transallop_TRUE_ctrydest_TRUE_2008_2017.csv
#
#     File contains number of seizures in and out for each country in each group
#     Groups are small, medium and large seizures
#
#  NOTE: this is not quite the same as the script 'sz_inout_tables.R' or files with other similar names
#    used on the ETIS DB on the server (because they have different output options)
#    But the basis of the code is similar to other files with 'sz_inout_' in the name
#=======================================================================================
#
#  Set range of years of seizure to include;
#   leave as 1900 - 2100 to include ALL years in database:
#
year.from <- 2008
year.to <- 2017
#_______________________________________________________________________
#
#  Set minimum Status of seizure records to include (normally 3):
#
statusMin <- 3
#_______________________________________________________________________
#
#  Set path name, in quotes, for working folder
#
path.Rcode <- 'C:/ETIS/analysis/R code'
path.data <- 'C:/ETIS/analysis/Processed Data'
# 
#_______________________________________________________________________
#
#  Set flag for including ctry of destination in computing szs out
#
ctry_dest_included <- TRUE

# Set flag for including countries where seizure opportunity not possible in transit
ctry_noszopp_included <- TRUE

setwd(path.Rcode)
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


# produce dataframe of ivory quantities (& RIE) : df.1
SQLstr.1 <- '
SELECT    id, status_id, seizure_year, discovered_country_id, 
raw_pieces, raw_weight, worked_pieces, worked_weight
FROM      public.seizures
WHERE     (raw_pieces>0 OR raw_weight>0 OR worked_pieces>0 OR worked_weight>0)
ORDER BY id;'
df.1 <- dbGetQuery(con, SQLstr.1)
y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.1 <- df.1[(df.1$seizure_year >=  y0) & (df.1$seizure_year <=  y1), ]
df.1 <- df.1[df.1$status_id >=  statusMin, ]
df.1 <- df.1[, -2]  # finished with status field, so remove it

# compute predicted weights & RIEs:
load('wt est.Rdata') #get regression models

# Raw ivory
jj <- !is.na(df.1$raw_pieces) & is.na(df.1$raw_weight)
df.new <- data.frame(x = log(df.1$raw_pieces[jj] + 1))
df.1$raw_weight[jj] <- predict(lm.r, newdata = df.new)^(1/lambda.r)

# Worked ivory 
kk <- !is.na(df.1$worked_pieces) & is.na(df.1$worked_weight)
df.new <- data.frame(x = log(df.1$worked_pieces[kk] + 1))
df.1$worked_weight[kk] <- predict(lm.w, newdata = df.new)^(1/lambda.w)

# Combined  
df.1$raw_weight[is.na(df.1$raw_weight)] <- 0
df.1$worked_weight[is.na(df.1$worked_weight)] <- 0
df.1$RIE <- df.1$raw_weight + df.1$worked_weight/0.7

# Identify which group each seizure belongs to - small, medium or large
df.1$gp <- ifelse(df.1$RIE < 10, 1, ifelse(df.1$RIE < 100, 2, 3))
df.1$raw_weight[df.1$raw_weight ==  0] <- NA
df.1$worked_weight[df.1$worked_weight == 0] <- NA

#
rm(SQLstr.1, jj, kk, df.new)
#
#
# ids for all countries "in"
ct.id.used.in <- sort(unique(df.1$discovered_country_id))

# df for ctries of orig, expt, tran & dest each distinct from ctry of disc (non-ivory szs removed)
if (ctry_noszopp_included ==  TRUE){
SQLstr.2 <- '
WITH  
  /*tran_junc AS (
  SELECT  seizure_id, 
  CASE WHEN seizure_opportunity THEN country_id
  ELSE NULL
  END AS ct_id            -- set to NULL if no seizure opportunity
  FROM public.seizure_transit_countries ), 
  */
tmp AS (
  SELECT    s.id, s.status_id, s.seizure_year, s.discovered_country_id, 
    j1.country_id AS ct_orig_id, 
   j1r.country_id AS ct_raw_orig_id, 
   j1w.country_id AS ct_wkd_orig_id, 
    j2.country_id AS ct_expt_id, 
    /*j3.ct_id AS ct_tran_id, */
    j3.country_id AS ct_tran_id, 
    s.destination_country_id
  FROM      public.seizures s
    LEFT JOIN      public.seizure_origin_countries j1 ON s.id = j1.seizure_id
    LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
    LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
    LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
    /*LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id*/
    LEFT JOIN      public.seizure_transit_countries j3 ON s.id = j3.seizure_id
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
} else {
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
    /*j3.country_id AS ct_tran_id, */
   s.destination_country_id
  FROM      public.seizures s
    LEFT JOIN      public.seizure_origin_countries j1 ON s.id = j1.seizure_id
    LEFT JOIN      public.seizure_raw_origin_countries j1r ON s.id = j1r.seizure_id
    LEFT JOIN      public.seizure_worked_origin_countries j1w ON s.id = j1w.seizure_id
    LEFT JOIN      public.seizure_export_countries j2 ON s.id = j2.seizure_id
    LEFT JOIN      tran_junc j3 ON s.id = j3.seizure_id
   /* LEFT JOIN      public.seizure_transit_countries j3 ON s.id = j3.seizure_id*/
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
}

df.2 <- dbGetQuery(con, SQLstr.2)  # has multiple rows for szs where multiple countries occur
y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.2 <- filter(df.2, (seizure_year >=  y0), (seizure_year <=  y1))
df.2 <- filter(df.2, status_id >=  statusMin)
df.2 <- df.2[, -2]  # finished with status field, so remove it

df.2 <- filter(df.2, is.element(id, df.1$id))

# create list cts - each component (one for each seizure) is a vector of unique
# countries (ids) recorded outside the country of discovery

ID <- unique(df.2$id)
cts <- list(NULL)
yrs <- numeric(length(ID))

first <- function(x) x[1]

tt <- list(NULL)
for (i in 1:6)
  tt[[i]] <- tapply(df.2[, i+2], df.2$id, unique, simplify = F)
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

#_______________________________________________________________________
# create years & RIE vectors to match (unlisted) countries in cts

len.cts <- unlist(lapply(cts, length))
yr.rep <- rep(yr, len.cts)
yr.val <- sort(unique(df.2$seizure_year))        # years that occur in DB
yr.out.fac <- factor(yr.rep, levels = yr.val)      # years as factor
df.RIE <- filter(df.1, is.element(id, as.numeric(names(cts))))
df.RIE <- arrange(df.RIE, id)
RIE.out <- rep(df.RIE$RIE, len.cts)
gp.out <- rep(df.RIE$gp, len.cts)

# ids for all ctries "in" & "out"
ct.id.used.out <- unique(unlist(df.2[, 3:8]))
ct.id.used.all <- sort(union(ct.id.used.in, ct.id.used.out))


# create countries factor for table of sz out
SQLstr.c <- 'SELECT id, code FROM public.countries ORDER BY id;'
df.ctry <- dbGetQuery(con, SQLstr.c, stringsAsFactors = F)
df.used.all <- subset(df.ctry, id %in% ct.id.used.all)
code.used.all <- df.used.all$code                # country codes used ("in" and "out")
code.used.all[is.na(code.used.all)] <- "NA"      # put Namibia (NA) back in!
cts.vec <- unlist(cts)
ct.out.fac <- factor(as.numeric(cts.vec), levels = ct.id.used.all, labels = code.used.all)

# Identify the groups for the out table
gp.tbl.out <- table(ct.out.fac, gp.out)#, yr.out.fac)

# dataframes for analysis
df.gp.out <- as.data.frame.matrix(gp.tbl.out)
df.gp.out$ctry <- rownames(df.gp.out)
df.gp.out <- df.gp.out[, c(4, 1:3)]
names(df.gp.out) <- c('ctry', 'gp1.out', 'gp2.out', 'gp3.out')

# seizures & wgts "in" tables
# first make country & year factors with same levels & labels as in the "out" table
ct.in.fac <- factor(df.1$discovered_country_id, levels = ct.id.used.all, labels = code.used.all)

gp1.in.tbl <- tapply((df.1$gp == 1), ct.in.fac, sum, na.rm = T)
gp2.in.tbl <- tapply((df.1$gp == 2), ct.in.fac, sum, na.rm = T)
gp3.in.tbl <- tapply((df.1$gp == 3), ct.in.fac, sum, na.rm = T)
gp1.in.tbl[is.na(gp1.in.tbl)] <- 0
gp2.in.tbl[is.na(gp2.in.tbl)] <- 0
gp3.in.tbl[is.na(gp3.in.tbl)] <- 0


# combined df for analysis
df.inout <- df.gp.out %>%
  mutate(gp1.in = gp1.in.tbl, 
         gp2.in  = gp2.in.tbl,
          gp3.in = gp3.in.tbl)

df.inout <- df.inout[, c(1, 5:7, 2:4)]

setwd(path.data)

filename <- paste('dfinout_gps_transallop_', ctry_noszopp_included, '_ctrydest_', ctry_dest_included, '_', year.from, '_', year.to, '_tidy.csv', sep = '')
write.csv(df.inout, file = filename, row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
