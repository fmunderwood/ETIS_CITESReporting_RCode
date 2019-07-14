#  Replaces unknown weights of seizures with estimates
#    and allocates seizures to weight groups
# _____________________________________________________________________
#  INPUTS
#     (1) csv file 'ivory sz recs yrfrom_yrto.csv' (created by
#             'sz data setup.R')
#     (2) R workspace 'wt est.Rdata' (updated if necessary, using
#               'wgt est models.R')
#     (3) R file 'sz id weights changed.R' (seizures where weights need correction) 
# _____________________________________________________________________
#  OUTPUTS
#   'sz recs with estd wgts ', year.from, '_', year.to, '.csv' Relevant seizure records
# ======================================================================
#
#  Set range of years of seizure to include;
#   leave as 1900 - 2100 to include ALL years in database:
#
year.from <- 2007
year.to <- 2017
#______________________________________________________________________
#
#  Set path name, in quotes, for working folder
#
path.data <- 'C:/ETIS/analysis/Processed Data'
path.Rcode <- 'C:/ETIS/analysis/R Code'
#______________________________________________________________________
#

library(tidyverse)

setwd(path.data)

file.in <- paste('ivory sz recs ', year.from, '_', year.to, '_tidy.csv', sep='')

df.szrecs <- read.csv(file.in, header=T)
df.szrecs <- arrange(df.szrecs, sz.id)

df.raw <- df.szrecs %>%
  filter(raw) %>%
  select(sz.id:raw.wgt)

names(df.raw)[4:5] <- c('pcs', 'wgt')

df.wkd <- df.szrecs %>%
  filter(wkd) %>%
  select(sz.id, sz.yr, disc.ct, wkd.pcs, wkd.wgt)

names(df.wkd)[4:5] <- c('pcs', 'wgt')

#estimate wgt from pcs where known:

load('wt est.Rdata')

#Raw:
#df.r.est = cases with known raw pcs & unknown raw wgts ...
df.raw <- mutate(df.raw, x = log(pcs + 1))

df.r.pred <- df.raw %>%
  filter(!is.na(pcs) & is.na(wgt)) %>%
  select(sz.id, sz.yr, pcs, wgt, x) 

pred.r <- predict(lm.r, newdata = df.r.pred, se.fit = T)

# Calculate new weights
df.r.pred <- df.r.pred %>%
  mutate(
    wgt = pred.r$fit^(1/lambda.r),
    wgt_l = (pred.r$fit - 1.96*pred.r$se.fit)^(1/lambda.r),
    wgt_u = (pred.r$fit + 1.96*pred.r$se.fit)^(1/lambda.r)
  )

#and then to df.szrecs:
df.szrecs$raw.wgt[is.element(df.szrecs$sz.id, df.r.pred$sz.id)] <- df.r.pred$wgt
df.szrecs$raw_l[is.element(df.szrecs$sz.id, df.r.pred$sz.id)] <- df.r.pred$wgt_l
df.szrecs$raw_u[is.element(df.szrecs$sz.id, df.r.pred$sz.id)] <- df.r.pred$wgt_u

#Worked:
# Same for worked weights
df.wkd <- mutate(df.wkd, x = log(pcs + 1))

df.w.pred <- df.wkd %>%
  filter(!is.na(pcs) & is.na(wgt)) %>%
  select(sz.id, sz.yr, pcs, wgt, x)

pred.w <- predict(lm.w, newdata = df.w.pred, se.fit = T)

# Calculate new weights
df.w.pred <- df.w.pred %>%
  mutate(
    wgt = pred.w$fit^(1/lambda.w),
    wgt_l = (pred.w$fit - 1.96*pred.w$se.fit)^(1/lambda.w),
    wgt_u = (pred.w$fit + 1.96*pred.w$se.fit)^(1/lambda.w)
  )

#and then to df.szrecs:
df.szrecs$wkd.wgt[is.element(df.szrecs$sz.id, df.w.pred$sz.id)] <- df.w.pred$wgt
df.szrecs$wkd_l[is.element(df.szrecs$sz.id, df.w.pred$sz.id)] <- df.w.pred$wgt_l
df.szrecs$wkd_u[is.element(df.szrecs$sz.id, df.w.pred$sz.id)] <- df.w.pred$wgt_u



# Corrections to some seizures
setwd(path.Rcode)
source('sz id weights changed.R')
setwd(path.data)

chg.df <- arrange(chg.df,sz.id)

# Those with a correction
correct <- filter(chg.df, method == "correction")
df.szrecs$raw.wgt[is.element(df.szrecs$sz.id, correct$sz.id)] <- correct$new.raw.wgt
df.szrecs$wkd.wgt[is.element(df.szrecs$sz.id, correct$sz.id)] <- correct$new.wkd.wgt

# Those with estimates to be made
# Need to do each one separately given particular circumstances
filter(chg.df, method != "correction")
# For 101114
pred.df1 <- filter(df.wkd, sz.id == 101114)
new.pred <- predict(lm.w, newdata = pred.df1, se.fit = T)
pred.df1$wgt <- (new.pred$fit)^(1/lambda.w)

pred.df1 <- pred.df1 %>%
  mutate(
    wgt = new.pred$fit^(1/lambda.w),
    wgt_l = (new.pred$fit - 1.96 * new.pred$se.fit)^(1/lambda.w),
    wgt_u = (new.pred$fit + 1.96  *new.pred$se.fit)^(1/lambda.w)
  )

ii <- df.szrecs$sz.id == pred.df1$sz.id

df.szrecs$wkd.wgt[ii] <- pred.df1$wgt
df.szrecs$wkd_l[ii] <- pred.df1$wgt_l
df.szrecs$wkd_u[ii] <- pred.df1$wgt_u

raw.old <- df.szrecs$raw.wgt[ii]
df.szrecs$raw.wgt[ii] <- raw.old - df.szrecs$wkd.wgt[ii]
df.szrecs$raw_l[ii] <- raw.old - pred.df1$wgt_l
df.szrecs$raw_u[ii] <- raw.old - pred.df1$wgt_u


# For 107677
pred.df2 <- filter(df.wkd, sz.id == 107677)
wkd.wgt.old <- pred.df2$wgt
wkd.pcs.old <- pred.df2$pcs

pred.df2$pcs <- pred.df2$pcs - 1
new.pred <- predict(lm.w, newdata = pred.df2, se.fit = T)


pred.df2 <- pred.df2 %>%
  mutate(
    wgt = new.pred$fit^(1/lambda.w) ,
    wgt_l = (new.pred$fit - 1.96 * new.pred$se.fit)^(1/lambda.w) ,
    wgt_u = (new.pred$fit + 1.96  *new.pred$se.fit)^(1/lambda.w) 
  )


ii <- df.szrecs$sz.id == pred.df2$sz.id

df.szrecs$wkd.pcs[ii] <- wkd.pcs.old
df.szrecs$wkd.wgt[ii] <- pred.df2$wgt + wkd.wgt.old
df.szrecs$wkd_l[ii] <- pred.df1$wgt_l + wkd.wgt.old
df.szrecs$wkd_u[ii] <- pred.df1$wgt_u + wkd.wgt.old


# Final manipulations to all estimates

#convert wkd wgts to RIE:
df.szrecs <- df.szrecs %>%
  mutate(wkd.wgt = wkd.wgt/0.7,
         wkd_l = wkd_l/0.7,
         wkd_u = wkd_u/0.7)

#grouping (6 classes for each of raw, wkd):
df.szrecs$raw.grp <- cut(df.szrecs$raw.wgt, c(0,1,10,50,100,500,10000), right=F)
df.szrecs$wkd.grp <- cut(df.szrecs$wkd.wgt, c(0,1,10,50,100,500,10000), right=F)
df.out <- df.szrecs 

#output:
file.out <- paste('sz recs with estd wgts ', year.from, '_', year.to, '.csv', sep='')
write.csv(df.out, file=file.out, row.names=F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #

