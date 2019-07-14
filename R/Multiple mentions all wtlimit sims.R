# Corrects seizures weights out when there are multiple countries of origin
# The file calculates how much needs to be subtracted 
# from the bias-adjusted weights or seizures out and then does the subtraction
#
# The basic correction is as follows:
#
# If a country's role in the trade chain is only as a country of origin
# And the seizures consist of ivory from more than one country of origin
# The weights out for this country needs to be adjusted so that only 
# the ivory that comes from this country is counted in the weights out
#
# For example a shipment of 750kg may contain 200kg from country A and 550kg from country B
# and neither country appears anywhere else in the trade chain
# Then country A total weight out needs to be reduced by 550kg
# And country B total weight needs to be reduced by 200kg
#
# Seizures out also need to be corrected when considering seizures of at least 500kg
# Thus the shipment in the above example would no longer count as a seizure out over 500kg for country A 
#
# The correction also takes account of the (posterior distribution of the) bias-adjustment factor for that seizure
#
# Szs and weights in and out for each country and year are required for:
#  (i)  Calculating LE ratio - when country of destination is excluded from seizures out
#  (ii) Calculating Trade Flows - when country of destination is included in seizures out
# Both (i) and (ii) are needed  
#  (I) For all seizures
#  (II)For seizures over 500kg
# So this file needs to be run four times where the file 
# Sims_10000Adj_sz_in_sz_Final_dest_*_size_.R name varies for:
# (i) and (ii) dest_* and * is either FALSE (i) or TRUE (ii) 
# (I) and (II) there is no size_ part - ie all seizures (I) or size_500 (II)
#
# NOTE this file is adapted from a file which calculated multiple weights out
# for other circumstances when there is no need for bias adjustment
# For example simple summaries of the data
# Hence the option for setting the sz.rep.adjust flag
#________________________________________________________________________________
# INPUTS
# (a) Posterior predictive distributions of bias adjustment factors - sz_adjusted_modnm_ALL_Final.csv 
#     (from Sz x rep rate for all countries for cluster analysis Final.R)
# (b) Reference file for (a) - covars_adj_ref.csv (from same file as (a))
# (c) Posterior distributions of bias-adjusted szs & wgts in/out which are not corrected for multiple countries of origin
#     - Sims_10000Adj_sz_in_sz_Final_dest_FALSE_size_500.Rdata (from sz_inout_tables_adj_sims.R)
# (d) Information for each seizure with multiple countries of origin (from mult_ctries.R)
#     (i) countries of origin - multiple_origins_yr1_yr2.xlsx
#     (ii) trade route - multiple_rtes_yr1_yr2.csv
#     (iii) countries of discovery - multiple_disc_yr1_yr2.csv
# (e) R packages:
#     tidyverse
#     XLConnect
#________________________________________________________________________________
# OUTPUTS
# .Rdata files for seizures and weights in and out
# Adj_sz_in_ - or sz_out, wt_in, wt_out
# dfinout_ model name
# dest_  whether country of destination is included or not
# size_  when only seizures of a particular size are considered - eg 500kg

# For example the options listed here could give the filename
# Adj_sz_in_dfinout_sz_Final_dest_TRUE_size_500.Rdata
#================================================================================

library(XLConnect)
library(tidyverse)

path.data <- 'C:/ETIS/analysis/Processed Data'
path.sims.data <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

year.from.all <- 1900
year.to.all <- 2100

year.from.use <- 2015 
year.to.use <- 2017 

ctry_dest_included <-  TRUE #FALSE

size.min <- 500

sz.rep.adjust <- TRUE
if (sz.rep.adjust)
  mod.nm <- 'sz_Final' # SET model name - only necessary if sz.rep.adjust is TRUE

num.sim <- 10000

year_dest.all <- paste('_', year.from.all, '_', year.to.all,  sep = '')
year_dest.use <- paste('_', year.from.use, '_', year.to.use,  sep = '')

transit.name <- paste('multiple_rtes', year_dest.all, '.csv', sep = '')
disc.name <- paste('multiple_disc', year_dest.all, '.csv', sep = '')
orig.name <- paste('multiple_origins', year_dest.all, '.xlsx', sep= '')

# File with adjusted sz in and out
# Use a different file if doing seizure and reporting rate adjustments

sz.in.name <- paste('Sims_',num.sim,'Adj_sz_in_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  sz.in.name <- paste(sz.in.name, '_size_', size.min, sep = '')
}
sz.out.name <- paste('Sims_',num.sim,'Adj_sz_out_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  sz.out.name <- paste(sz.out.name, '_size_', size.min, sep = '')
}
wt.in.name <- paste('Sims_',num.sim,'Adj_wt_in_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  wt.in.name <- paste(wt.in.name, '_size_', size.min, sep = '')
}
wt.out.name <- paste('Sims_',num.sim,'Adj_wt_out_', mod.nm,'_dest_',ctry_dest_included, sep = '')
if(size.min > 0){
  wt.out.name <- paste(wt.out.name, '_size_', size.min, sep = '')
}

# Data on seizure and reporting rates
sz.rep.rates.name <- paste('sz_adjusted_', mod.nm, '_ALL_Final.csv', sep = '')

# Read in data
setwd(path.data)
transit <- read.csv(transit.name)
disc <- read.csv(disc.name)

orig_raw <- readWorksheetFromFile(orig.name, sheet = "raw")
orig_wkd <- readWorksheetFromFile(orig.name, sheet = "worked")

setwd(path.sims.data)
sz.rep.cov <- read.csv('covars_adj_ref_Final.csv')

sz.in <- readRDS(paste(sz.in.name,'.Rdata', sep = ''))
sz.out <- readRDS(paste(sz.out.name,'.Rdata', sep = ''))
wt.in <- readRDS(paste(wt.in.name,'.Rdata', sep = ''))
wt.out <- readRDS(paste(wt.out.name,'.Rdata', sep = ''))

df.szrep.rates <- read.csv(sz.rep.rates.name)

df.szrep.rates <- df.szrep.rates[,-1]
df.szrep.rates <- cbind(sz.rep.cov[,-1], df.szrep.rates)

#setwd(path.out)

# Years to use
yr.use <- year.from.use:year.to.use

# Tidying up all dataframes so that 
# Only relevant years are used
# Country code is in lower case - and deal with NA issue

# Original number and weight in and out
# Produce key for year and country

df.inout_tidy <- df.szrep.rates %>%
  filter(is.element(year, yr.use)) %>%
#  mutate(ctry = tolower(ctry)) %>%
#  mutate(ctry = replace_na(ctry, "na")) %>%
  mutate(year.ctry = paste(ctry, year, sep = ':')) 
df.inout_tidy <- df.inout_tidy[,c(1,2,ncol(df.inout_tidy),3:(ncol(df.inout_tidy)-1))]


# Country of origin
orig_raw_tidy <- orig_raw %>%
  filter(is.element(seizure_year, yr.use))  %>%
  mutate(raw_orig_code = tolower(raw_orig_code)) %>%
  mutate(raw_orig_code = replace_na(raw_orig_code, "na"))

orig_wkd_tidy <- orig_wkd %>%
  filter(is.element(seizure_year, yr.use))  %>%
  mutate(wkd_orig_code = tolower(wkd_orig_code)) %>%
  mutate(wkd_orig_code = replace_na(wkd_orig_code, "na"))

# Country of discovery 
# Only one country of discovery for each seizure
disc_tidy <- disc %>%
  filter(!is.na(discovered_country_id)) %>%
  select(id, discovered_country_id, disc_code) %>%
  mutate(disc_code = tolower(disc_code)) %>%
  mutate(disc_code = replace_na(disc_code, "na"))

# Select only rows with export countries listed
export_tidy <- transit %>%
  filter(!is.na(ct_expt_mod)) %>%
  select(id, ct_expt_mod, expt_code) %>%
  distinct() %>%
  mutate(expt_code = tolower(expt_code)) %>%
  mutate(expt_code = replace_na(expt_code, "na"))

# Select only rows with transit countries listed
transit_tidy <- transit %>%
  filter(!is.na(ct_tran_mod)) %>%
  select(id, ct_tran_mod, tran_code) %>%
  distinct() %>%
  mutate(tran_code = tolower(tran_code)) %>%
  mutate(tran_code = replace_na(tran_code, "na"))

# Obtain country of destination for each seizure
# Only necessary if dest TRUE
if (ctry_dest_included){
  dest_tidy <- transit %>%
    filter(!is.na(ct_dest_mod)) %>%
    select(id, ct_dest_mod, dest_code) %>%
    distinct() %>%
    mutate(dest_code = tolower(dest_code)) %>%
    mutate(dest_code = replace_na(dest_code, "na"))
}

# Now only select seizures where either raw or worked RIE is more than 1 tonne
orig_raw_tidy <- orig_raw_tidy %>%
  filter(RIE.raw >= size.min) 

orig_wkd_tidy <- orig_wkd_tidy %>%
  filter(RIE.wkd >= size.min) 

# Select seizures with these values only
disc_tidy <- disc_tidy %>%
  filter(is.element(id, c(orig_raw_tidy$seizure_id, orig_wkd_tidy$seizure_id)))

#==================================================================
# Indicators to mark 1 if country is exclusively an origin country
# Or zero if also a country of discovery, export or transit
# And calculate what reduction is necessary
#==================================================================

# RAW SEIZURES

# Combine discovery information with origin information
# And create a year x country of discovery key
mutual_raw <- orig_raw_tidy %>%
  left_join(disc_tidy, by = c("seizure_id" = "id"))  %>%
  mutate(disc = ifelse(ct_orig_raw_id == discovered_country_id, 0, 1)) %>%
  mutate(year.ctry.disc = paste(disc_code, seizure_year, sep = ':')) 
  
# Combine export information with origin information
# And indicate if country of export is also a country of origin
mutual_raw <- mutual_raw %>%
  left_join(export_tidy, by = c("seizure_id" = "id")) %>%
  mutate(export = ifelse(ct_orig_raw_id == ct_expt_mod, 0, 1)) %>%
  mutate(export = replace_na(export, 1))

# Combine transit information with origin information
# And indicate if country of transit is also a country of origin
mutual_raw <- mutual_raw %>%
  left_join(transit_tidy, by = c("seizure_id" = "id")) %>%
  mutate(transit = ifelse(ct_orig_raw_id == ct_tran_mod, 0, 1)) %>%
  mutate(transit = replace_na(transit, 1))

# Combine destination information with origin information
# And indicate if country ofdestination is also a country of origin 
  if (ctry_dest_included){
    mutual_raw <- mutual_raw %>%
      left_join(dest_tidy, by = c("seizure_id" = "id")) %>%
#    mutate(dest = ifelse(ctry_dest_included, ifelse(ct_orig_raw_id == ct_dest_mod, 0, 1), 1)) %>%
      mutate(dest = ifelse(ct_orig_raw_id == ct_dest_mod, 0, 1)) %>%
      mutate(dest = replace_na(dest, 1))
  } else {
     mutual_raw <- mutual_raw %>%
      mutate(dest = 1)
  }
    

#==================================================================
# Calculate how much of RIE should be removed from initial calculation
# If country is only listed as one of several countries of origin 
# and appears nowhere else on the trade chain then 
# only count proportion of ivory that came from the country
# Join with information on seizure and reporting rates for each country of discovery
# Calculate amount to reduce estimate by - multiple proportion by RIE.raw and sz.adj
#==================================================================

mutual_raw_reduce <- mutual_raw %>%
  mutate(reduce = disc * export * transit * dest) %>%
  mutate(orig_id = paste(raw_orig_code, seizure_id, sep=":"))

# If it is not the right size (when considering seizures over 500kg for example)
# then a complete reduction is required
mutual_raw_reduce <- mutual_raw_reduce %>%
  mutate(complete_red = prop_raw_wt < size.min)
         
raw_product <- mutual_raw_reduce %>%
  group_by(seizure_id, raw_orig_code, year.ctry.disc) %>%
  summarise(red = prod(reduce)) %>%
  filter(red == 1) %>%
  mutate(orig_id = paste(raw_orig_code, seizure_id, sep=":"))

#_______________________________________________________________________________
# Same for WORKED
# Combine discovery information with origin information
mutual_wkd <- orig_wkd_tidy %>%
  left_join(disc_tidy, by = c("seizure_id" = "id"))  %>%
  mutate(disc = ifelse(ct_orig_wkd_id == discovered_country_id, 0, 1)) %>%
  mutate(year.ctry.disc = paste(disc_code, seizure_year, sep = ':')) 

# Combine export information with origin information
# And indicate if country of export is also a country of origin
mutual_wkd <- mutual_wkd %>%
  left_join(export_tidy, by = c("seizure_id" = "id")) %>%
  mutate(export = ifelse(ct_orig_wkd_id == ct_expt_mod, 0, 1)) %>%
  mutate(export = replace_na(export, 1))

# Combine transit information with origin information
# And indicate if country of transit is also a country of origin
mutual_wkd <- mutual_wkd %>%
  left_join(transit_tidy, by = c("seizure_id" = "id")) %>%
  mutate(transit = ifelse(ct_orig_wkd_id == ct_tran_mod, 0, 1)) %>%
  mutate(transit = replace_na(transit, 1))

# Combine destination information with origin information
# And indicate if country of destination is also a country of origin
if (ctry_dest_included){
  mutual_wkd <- mutual_wkd %>%
    left_join(dest_tidy, by = c("seizure_id" = "id")) %>%
    #    mutate(dest = ifelse(ctry_dest_included, ifelse(ct_orig_wkd_id == ct_dest_mod, 0, 1), 1)) %>%
    mutate(dest = ifelse(ct_orig_wkd_id == ct_dest_mod, 0, 1)) %>%
    mutate(dest = replace_na(dest, 1))
} else {
  mutual_wkd <- mutual_wkd %>%
    mutate(dest = 1)
}

#==================================================================
# Calculate how much of RIE should be removed from initial calculation
# If country is only listed as one of several countries of origin 
# and appears nowhere else on the trade chain then 
# Join with information on seizure and reporting rates for each country of discovery
# Calculate amount to reduce estimate by - multiple proportion by RIE.wkd and sz.adj
#==================================================================

mutual_wkd_reduce <- mutual_wkd %>%
  mutate(reduce = disc * export * transit * dest) %>%
  mutate(orig_id = paste(wkd_orig_code, seizure_id, sep=":"))

# If it is not the right size (when considering seizures over 500kg for example)
# then a complete reduction is required

mutual_wkd_reduce <- mutual_wkd_reduce %>%
  mutate(complete_red = prop_wkd_wt < size.min)

wkd_product <- mutual_wkd_reduce %>%
  group_by(seizure_id, wkd_orig_code, year.ctry.disc) %>%
  summarise(red = prod(reduce)) %>%
  filter(red == 1) %>%
  mutate(orig_id = paste(wkd_orig_code, seizure_id, sep=":"))

#_______________________________________________________________________________
# Loop to obtain posterior predictive distributions

# Seizure and reporting rate adjustment
nyr <- length(yr.use)

sz.in.rn <- rownames(sz.in[,1,])
sz.out.rn <- rownames(sz.out[,1,])
wt.in.rn <- rownames(wt.in[,1,])
wt.out.rn <- rownames(wt.out[,1,])

sz.in.cn <- colnames(sz.in[,1,])
sz.out.cn <- colnames(sz.out[,1,])
wt.in.cn <- colnames(wt.in[,1,])
wt.out.cn <- colnames(wt.out[,1,])

sz.in.dm1 <- dim(sz.in)[1]
sz.in.dm3 <- dim(sz.in)[3]
sz.out.dm1 <- dim(sz.out)[1]
sz.out.dm3 <- dim(sz.out)[3]
wt.in.dm1 <- dim(wt.in)[1]
wt.in.dm3 <- dim(wt.in)[3]
wt.out.dm1 <- dim(wt.out)[1]
wt.out.dm3 <- dim(wt.out)[3]

yr.ctry.dfinout <- paste(df.inout_tidy$ctry, df.inout_tidy$year, sep = ':')

sz.in.adj.fin <- wt.in.adj.fin <- wt.out.adj.fin <- sz.out.adj.fin <- 
  matrix(nrow = nrow(df.inout_tidy), ncol = num.sim, data = 0,
         dimnames = list(yr.ctry.dfinout))

df.inout_tidy.orig <- df.inout_tidy

for (i in 1:num.sim){
  df.inout_tidy.1 <- df.inout_tidy[, c(1:3, (i + 3))]
  names(df.inout_tidy.1)[4] <- 'sz.adj'
  
  sz.in.1 <- data.frame(ctry = rep(sz.in.rn, sz.in.dm3),
                        year = as.numeric(rep(sz.in.cn, each = sz.in.dm1)),
                        sz.in.adj = as.numeric(sz.in[,i,])
  )
  sz.out.1 <- data.frame(ctry = rep(sz.out.rn, sz.out.dm3),
                         year = as.numeric(rep(sz.out.cn, each = sz.out.dm1)),
                         sz.out.adj = as.numeric(sz.out[,i,])
  )
                         
  wt.in.1 <- data.frame(ctry = rep(wt.in.rn, wt.in.dm3),
                        year = as.numeric(rep(wt.in.cn, each = wt.in.dm1)),
                        wt.in.adj = as.numeric( wt.in[,i,])
  )
                        
  wt.out.1 <- data.frame(ctry = rep(wt.out.rn, wt.out.dm3),
                         year = as.numeric(rep(wt.out.cn, each = wt.out.dm1)),
                         wt.out.adj = as.numeric(wt.out[,i,])
  )

  df.inout_tidy.1 <- df.inout_tidy.1 %>%
    left_join(sz.in.1) %>%
    left_join(sz.out.1) %>%
    left_join(wt.in.1)  %>%
    left_join(wt.out.1) %>%
    mutate(sz.in.adj = replace_na(sz.in.adj, 0),
           sz.out.adj = replace_na(sz.out.adj, 0),
           wt.in.adj = replace_na(wt.in.adj, 0),
           wt.out.adj = replace_na(wt.out.adj, 0))

# RAW SEIZURES

    mutual_raw_use <- mutual_raw_reduce %>%
    filter(is.element(orig_id, raw_product$orig_id)) %>%
    left_join(df.inout_tidy.1, by = c("year.ctry.disc" = "year.ctry")) %>%
     mutate(amount.reduce = ifelse(complete_red, 
                                   RIE.raw * sz.adj,
                                   reduce * (100 - proportion) * RIE.raw * sz.adj / 100),
            sz.reduce = ifelse(complete_red, sz.adj, 0)
     )

mutual_raw_sum <- mutual_raw_use %>%
  select(seizure_id, seizure_year, raw_orig_code, amount.reduce, sz.reduce) %>%
  distinct()

mutual_raw_sum_chk <- mutual_raw_use %>%
  group_by(seizure_id, seizure_year, raw_orig_code) %>%
  summarise(am.reduce = first(amount.reduce),
            sz.reduce = first(sz.reduce))

# WORKED SEIZURES

mutual_wkd_use <- mutual_wkd_reduce %>%
    filter(is.element(orig_id, wkd_product$orig_id)) %>%
    left_join(df.inout_tidy.1, by = c("year.ctry.disc" = "year.ctry")) %>%
    mutate(amount.reduce = ifelse(complete_red,
                                  RIE.wkd * sz.adj,
                                  reduce * (100 - proportion) * RIE.wkd * sz.adj / 100),
           sz.reduce = ifelse(complete_red, sz.adj, 0)
    )

mutual_wkd_sum <- mutual_wkd_use %>%
  select(seizure_id, seizure_year, wkd_orig_code, amount.reduce, sz.reduce) %>%
  distinct()

mutual_wkd_sum_chk <- mutual_wkd_use %>%
  group_by(seizure_id, seizure_year, wkd_orig_code) %>%
  summarise(am.reduce = first(amount.reduce),
            sz.reduce = first(sz.reduce))

#__________________________________________________________________________________
# Combine raw and worked summaries with original information 

# Summarise amount to reduce for  each year and country 
# And create a common key
# RAW
raw_summ <- mutual_raw_sum %>%
  group_by(raw_orig_code, seizure_year) %>%
  summarise(raw_reduce = sum(amount.reduce, na.rm = T),
            raw_sz_reduce = sum(sz.reduce, na.rm = T)) %>%
  mutate(year.ctry = paste(raw_orig_code, seizure_year, sep = ':'))

# WORKED
wkd_summ <- mutual_wkd_sum %>%
  group_by(wkd_orig_code, seizure_year) %>%
  summarise(wkd_reduce = sum(amount.reduce, na.rm = T),
            wkd_sz_reduce = sum(sz.reduce, na.rm= T)) %>%
  mutate(year.ctry = paste(wkd_orig_code, seizure_year, sep = ':'))

#==================================================
# Save reduction (if any) here
#==================================================
# Join with raw
df.inout_raw <- df.inout_tidy.1 %>%
  left_join(raw_summ, by = "year.ctry" ) %>%
  mutate(raw_reduce = replace_na(raw_reduce, 0),
         raw_sz_reduce = replace_na(raw_sz_reduce, 0)) 
  
# Join with worked
df.inout_raw_wkd <- df.inout_raw %>%
  left_join(wkd_summ, by = "year.ctry" ) %>%
  mutate(wkd_reduce = replace_na(wkd_reduce, 0),
         wkd_sz_reduce = replace_na(wkd_sz_reduce, 0)) 

# Calculate overall reduction

df.inout_reduce <- df.inout_raw_wkd %>%
  mutate(total_reduce = raw_reduce + wkd_reduce,
         total_sz_reduce = raw_sz_reduce + wkd_sz_reduce)

  df.inout_reduce <- df.inout_reduce %>%
  mutate(wt.out.adj.fin = wt.out.adj - total_reduce,
         sz.out.adj.fin = sz.out.adj - total_sz_reduce)
  
  sz.in.adj.fin[,i] <- df.inout_reduce$sz.in.adj
  wt.in.adj.fin[,i] <- df.inout_reduce$wt.in.adj
  sz.out.adj.fin[,i] <- df.inout_reduce$sz.out.adj.fin
  wt.out.adj.fin[,i] <- df.inout_reduce$wt.out.adj.fin
  
  remove(df.inout_tidy.1)
}

# The above loop takes a while ....
#______________________________________________________________________________________
# Write data to .Rdata files

  begin.name <- paste('dfinout_', mod.nm, sep = '')
  end.name <- paste('_dest_', ctry_dest_included, sep = '')
  if(size.min > 0)
    end.name <-  paste(end.name, '_min_', size.min, sep = '')
 file.name <- paste(begin.name, end.name, sep = '')

saveRDS(sz.in.adj.fin, file = paste('Adj_sz_in_',file.name, 'rev.Rdata', sep = ''))
saveRDS(sz.out.adj.fin, file = paste('Adj_sz_out_',file.name, 'rev.Rdata', sep = ''))
saveRDS(wt.in.adj.fin, file = paste('Adj_wt_in_',file.name, 'rev.Rdata', sep = ''))
saveRDS(wt.out.adj.fin, file = paste('Adj_wt_out_',file.name, 'rev.Rdata', sep = ''))

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
