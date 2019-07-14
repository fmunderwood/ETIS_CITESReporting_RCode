# Calculates weights for seizures with no recorded weights
# And records RIE separately for raw and worked ivory
#
# This script is used within mult_ctries.R
#
# So assumes connection to ETIS database 
# and that year.from, year.to and statusMin = minimum status value have been specified
# and that the workspace  'wt est.Rdata' is in the directory noted below 
#
# computes df for ctry of discovery & ivory quantities:
# (szs removed:  all non-ivory; ivory szs with no info on quantities)
#======================================================================================
orig.dir <- getwd()
SQLstr.1 <- '
SELECT    id, status_id, seizure_year, discovered_country_id, 
          raw_pieces, raw_weight, worked_pieces, worked_weight
FROM      public.seizures
WHERE     (raw_pieces > 0 OR raw_weight > 0 OR worked_pieces > 0 OR worked_weight > 0)
ORDER BY id;'

df.1 <- dbGetQuery(con, SQLstr.1)

y0 <- max(min(df.1$seizure_year), year.from)
y1 <- min(max(df.1$seizure_year), year.to)
df.1 <- df.1[(df.1$seizure_year  >=  y0) & (df.1$seizure_year <= y1), ]
df.1 <- df.1[df.1$status_id  >=  statusMin, ]
df.1 <- df.1[, -2]  # finished with status field, so remove it

# compute predicted weights & RIEs:
setwd('C:/ETIS/analysis/Processed Data')
load('wt est.Rdata') #get regression models
jj <- !is.na(df.1$raw_pieces)&is.na(df.1$raw_weight)
df.new <- data.frame(x = log(df.1$raw_pieces[jj] + 1), sz.yr = df.1$seizure_year[jj])
df.1$raw_weight[jj] <- predict(lm.r, newdata = df.new)^(1/lambda.r)
# 
kk <- !is.na(df.1$worked_pieces)&is.na(df.1$worked_weight)
df.new <- data.frame(x = log(df.1$worked_pieces[kk] + 1), sz.yr = df.1$seizure_year[kk])
df.1$worked_weight[kk] <- predict(lm.w, newdata = df.new)^(1/lambda.w)
# 
df.1$raw_weight[is.na(df.1$raw_weight)] <- 0
df.1$worked_weight[is.na(df.1$worked_weight)] <- 0
df.1$RIE.raw <- df.1$raw_weight
df.1$RIE.wkd <- df.1$worked_weight/0.7
df.1$RIE <- df.1$RIE.raw + df.1$RIE.wkd

df.1$raw_weight[df.1$raw_weight == 0] <- NA
df.1$worked_weight[df.1$worked_weight == 0] <- NA
#
rm(SQLstr.1, jj, kk, df.new)

setwd(orig.dir)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
