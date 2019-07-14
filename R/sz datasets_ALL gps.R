# Identifies countries that meet criteria for inclusion in analysis
# Score for each country is calculated
#
#______________________________________________________________________
# INPUTS 
#     csv file dfinout_gps_transallop_TRUE_ctrydest_TRUE_yrfrom_yrto.csv
#      (created by sz_inout_tables_df_gps_transit.R)
#
# Category multiplier is specified for each group
# Threshold for inclusion is specified
#______________________________________________________________________
# OUTPUTS 
#  A series of csv files
#   1. List of countries to be included in the analysis 
#   2. Number of seizures of each ivory class for each country and year
#      Saved separately for raw and worked ivory and also combined
#
# All file names includes date that this program is run
#======================================================================
#
#  Set range of years of seizure to include for criteria assessment;
year.crit.from <- 2008
year.crit.to <- 2017

#  Set range of years of seizure to consider for analysis;
year.from <- 2007
year.to <- 2017
#______________________________________________________________________
#
#  Set numbers in each of the three gps
 gp1.mult <- 1
 gp2.mult <- 10
 gp3.mult <- 100
 crit.use <- 100
#______________________________________________________________________
#
#  Set path name, in quotes, for working folder
#
path.data <- 'C:/ETIS/analysis/Processed Data'
#______________________________________________________________________
#
#
library(tidyverse)
library(gdata)

setwd(path.data)

# select ctries for analysis 
# read szwt_inout table as d.f.
file.inout <-  paste('dfinout_gps_transallop_TRUE_ctrydest_TRUE_', year.crit.from, '_', year.crit.to, '.csv', sep = '')
df.inout <- read.csv(file.inout, header = T) 

df.inout$ctry <- tolower(df.inout$ctry)
df.inout$ctry[is.na(df.inout$ctry)] <- 'na' #Namibia back in
df.inout <- df.inout[substr(df.inout$ctry, 1, 1) != 'x', ]  #remove pseudo-ctries
cts.all <- df.inout$ctry

# Calculate weighted sum 
crit <- with(df.inout, 
             gp1.mult*(gp1.in + gp1.out)  +  
               gp2.mult*(gp2.in + gp2.out) +  
               gp3.mult*(gp3.in + gp3.out))

# Select countries to use in the analysis
cts.use <- cts.all[crit >= crit.use] # use these ctries for analysis
# Rearrange so in alphabetical order
cts.use <- cts.use[order(cts.use)]
# Excluded countries
# List of countries not in analysis
cts.not.use <- cts.all[crit < crit.use] # exclude these ctries for analysis

# Write out table for all countries with criteria
df.inout <- mutate(df.inout, crit = crit)
write.csv(df.inout, file = 'Criteria Countries.csv', row.names = F)

# stub for dataframes for analysis ctries
df.stub <- expand.grid(ctry = cts.use, year = year.from:year.to)

# get sz records with est'd wgts
df.szrecs <- read.csv(paste('sz recs with estd wgts ', year.from, '_', year.to, '_tidy.csv', sep = ''), header = T)

# Separate raw & wkd into separate data files
# Select relevant variables from df.szrecs

#names(df.szrecs)
#[1] "sz.id"    "sz.yr"    "disc.ct"  "raw.pcs"  "raw.wgt"  "raw.pres" "wkd.pcs"  "wkd.wgt" 
#[9] "wkd.pres" "raw"      "wkd"      "raw_l"    "raw_u"    "wkd_l"    "wkd_u"    "raw.grp" 
#[17] "wkd.grp" 

# And rearrange group levels so that in size order - 1, 2, 3, 5, 4, 6

#levels(df.szrecs$raw.grp)
#[1] "[0,1)"       "[1,10)"      "[10,50)"     "[100,500)"   "[50,100)"    "[500,1e+04)"

df.raw <- df.szrecs %>%
  filter(raw) %>%
  select(sz.id = sz.id, sz.yr = sz.yr, ctry = disc.ct, 
          pcs = raw.pcs, wgt = raw.wgt, lo.cl = raw_l, up.cl = raw_u, grp = raw.grp) %>%
  mutate(grp = reorder(grp, new.order = c(1, 2, 3, 5, 4, 6)))

df.wkd <- df.szrecs %>%
  filter(wkd) %>%
  select(sz.id = sz.id, sz.yr = sz.yr, ctry = disc.ct, 
         pcs = wkd.pcs, wgt = wkd.wgt, lo.cl = wkd_l, up.cl = wkd_u, grp = wkd.grp) %>%
  mutate(grp = reorder(grp, new.order = c(1, 2, 3, 5, 4, 6)))

# RAW:
# Count number in small, medium and large ivory classes
# szs with wgts in [0, 10):
df.raw.lo <- filter(df.raw, as.numeric(grp) <= 2)
df.r1 <- df.raw.lo %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 

# szs with wgts in [10, 100):
df.raw.mid <- filter(df.raw, as.numeric(grp) > 2, as.numeric(grp) < 5)
df.r2 <- df.raw.mid %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 

# szs with wgts in [100, 1e + 04):
df.raw.hi <- filter(df.raw, as.numeric(grp) > 4)
df.r3 <- df.raw.hi %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 

df.raw.szs <- data.frame(df.stub, sz.r1 = 0, sz.r2 = 0, sz.r3 = 0)
for(i in 1:dim(df.r1)[1]) {
  cc <- as.character(df.r1$ctry[i])
  yy <- df.r1$sz.yr[i]
  df.raw.szs$sz.r1[df.raw.szs$ctry == cc & df.raw.szs$year == yy] <- df.r1$Freq[i]
}
for(i in 1:dim(df.r2)[1]) {
  cc <- as.character(df.r2$ctry[i])
  yy <- df.r2$sz.yr[i]
  df.raw.szs$sz.r2[df.raw.szs$ctry == cc & df.raw.szs$year == yy] <- df.r2$Freq[i]
}
for(i in 1:dim(df.r3)[1]) {
  cc <- as.character(df.r3$ctry[i])
  yy <- df.r3$sz.yr[i]
  df.raw.szs$sz.r3[df.raw.szs$ctry == cc & df.raw.szs$year == yy] <- df.r3$Freq[i]
}

# WORKED:

# szs with wgts in [0, 10):

df.wkd.lo <- filter(df.wkd, as.numeric(grp) <= 2)
df.w1 <- df.wkd.lo %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 


df.wkd.mid <- filter(df.wkd, as.numeric(grp) > 2, as.numeric(grp) < 5)
df.w2 <- df.wkd.mid %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 

# szs with wgts in [100, 1e + 04):
df.wkd.hi <- filter(df.wkd, as.numeric(grp) > 4)
df.w3 <- df.wkd.hi %>%
  group_by(ctry, sz.yr) %>%
  summarise(Freq = n()) 

df.wkd.szs <- data.frame(df.stub, sz.w1 = 0, sz.w2 = 0, sz.w3 = 0)
for(i in 1:dim(df.w1)[1]) {
  cc <- as.character(df.w1$ctry[i])
  yy <- df.w1$sz.yr[i]
  df.wkd.szs$sz.w1[df.wkd.szs$ctry == cc & df.wkd.szs$year == yy] <- df.w1$Freq[i]
}
for(i in 1:dim(df.w2)[1]) {
  cc <- as.character(df.w2$ctry[i])
  yy <- df.w2$sz.yr[i]
  df.wkd.szs$sz.w2[df.wkd.szs$ctry == cc & df.wkd.szs$year == yy] <- df.w2$Freq[i]
}
for(i in 1:dim(df.w3)[1]) {
  cc <- as.character(df.w3$ctry[i])
  yy <- df.w3$sz.yr[i]
  df.wkd.szs$sz.w3[df.wkd.szs$ctry == cc & df.wkd.szs$year == yy] <- df.w3$Freq[i]
}


df.both.szs <- cbind(df.raw.szs, df.wkd.szs[, -(1:2)])
df.cts <- data.frame(ctry = sort(cts.use))

# Save outputs

filename <- paste('raw_szs_ALL_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
write.csv(df.raw.szs, file = filename, row.names = F)

filename <- paste('wkd_szs_ALL_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
write.csv(df.wkd.szs, file = filename, row.names = F)

filename <- paste('szs_ALL_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
write.csv(df.both.szs, file = filename, row.names = F)

filename <- paste('CTS_ALL_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
write.csv(df.cts, file = filename, row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #

