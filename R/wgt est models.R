#  Estimates weights of seizures from no. of pieces using all ivory
#    seizures in the database where both weights and no. of pieces
#    are known.
#
#  A number of different models are tried and tested before the final models
#   described in this file are given. This development can be found in the file
#   wgt est models development.R 
#
#  Change the values of lambda.r and lambda.w after inspecting the
#    profile likelihoods from the Box-Cox method.  Then refit the
#    models, with new polynomial orders as necessary.
#______________________________________________________________________
#  INPUTS
#    csv file ivory sz recs 1900_2100.csv (created by sz data setup.R)
#    The following files created from wgt ets models development.R
#       R file: sz id remove weight pieces model.R  
#       .Rdata file: Raw weight from pieces.Rdata  
#       .Rdata file: Worked weight from pieces.Rdata
#       The .Rdata files contain specific seizures to be omitted from weight estimation
#       Due to strong influence or extreme values
#______________________________________________________________________
#  OUTPUTS
#   R workspace 'wt est.Rdata' with models for worked and raw ivory
#======================================================================
#
# Set path name, in quotes, for working folder
#
path.wkg <- 'C:/ETIS/analysis/Processed Data'
path.code <- 'C:/ETIS/analysis/R Code'
#______________________________________________________________________

# NOTE WE ARE NOW ONLY USING DATA FROM 1996 onwards

library(MASS)
library(tidyverse)

setwd(path.code)
source('sz id remove weight pieces model.R')

setwd(path.wkg)
df.szrecs <- read.csv('ivory sz recs 1900_2100.csv', header=T)

load("Raw wt from pieces.Rdata")
load("Worked wt from pieces.Rdata")

df.szrecs <- df.szrecs %>%
  filter(!is.element(sz.id, sz.rm))

df.szrecs <- df.szrecs %>%
  filter(sz.yr > 1995)

df.raw <- df.szrecs %>%
  filter(raw) %>%
  select(sz.id:raw.wgt)

names(df.raw)[4:5] <- c('pcs', 'wgt')

df.wkd <- df.szrecs %>%
  filter(wkd) %>%
  select(sz.id, sz.yr, disc.ct, wkd.pcs, wkd.wgt)

names(df.wkd)[4:5] <- c('pcs', 'wgt')

df.r <- df.raw %>%
  filter(!is.na(pcs) & !is.na(wgt)) %>%
  mutate(wt.pc = wgt/pcs)


df.w <- df.wkd %>%
  filter(!is.na(pcs) & !is.na(wgt)) %>%
  mutate(wt.pc = wgt/pcs)


raw.name <- paste('RawWeightPcs_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
wkd.name <- paste('WkdWeightPcs_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
#write.csv(df.r, file = raw.name)
#write.csv(df.w, file = wkd.name)

# Transform number of pieces onto log scale
df.r <- df.r %>%
  mutate(x = log(pcs + 1))

# Raw seizures to be omitted from estimation
raw.omit <- raw.save$omit 

df.r1 <- filter(df.r, !is.element(sz.id, raw.omit))

# Fit basic model to determine lambda 
lm.r1.0 <- lm(wgt ~ poly(x, degree = 10), data = df.r1)

boxcox(lm.r1.0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx <- boxcox(lm.r1.0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx$x[xx$y == max(xx$y)]

#box-cox suggests ...
# A value of 0.09747475  
# Use a rounded value of 0.1
lambda.r <- 0.1               # <<- CHANGE AS APPROPRIATE
df.r1$y <- (df.r1$wgt) ^ lambda.r

lambda.r <- 0.1
df.r1$y <- (df.r1$wgt) ^ lambda.r

lm.r.orig <- lm(y ~ poly(x, 10), data = df.r1)

# Remove seizures with standardised residual of less than -3
resid.r.orig <- residuals(lm.r.orig)
stand.res <- (resid.r.orig - mean(resid.r.orig))/sd(resid.r.orig)

df.r2 <- filter(df.r1, stand.res > (-3))

# Fit model with 10 degrees of freedom (as suggested by detailed modelling)
lm.r <- lm(y ~ poly(x, 10), data = df.r2)

#=======================================================================
# Worked ivory

df.w.large <- filter(df.w, pcs >= 10000)

# Omit all seizures of more than 10000 pieces
# Because no worked seizures require weights estimated for more than 10000 pieces
df.w1 <- filter(df.w, pcs < 10000)
df.w1$x <- log(df.w1$pcs + 1)

# Worked seizures to be omited from estimation process
wkd.omit <- wkd.save$omit 
df.w2 <- df.w1[!is.element(df.w1$sz.id, wkd.omit),]

lm.w0 <- lm(wgt ~ poly(x, degree = 10), data = df.w2)
boxcox(lm.w0, lambda=seq(0.0, 0.1, length = 50), plotit = T)
bb <- boxcox(lm.w0, lambda = seq(0.0, 0.1, length=100), plotit = T)
bb$x[bb$y == max(bb$y)]
#box-cox suggests ...0.068
# Use value of 0.07 for rounding and consistency
lambda.w <- 0.07 # <<- CHANGE AS APPROPRIATE 

df.w2$y <- (df.w2$wgt)^lambda.w

# Fit model with 5 degrees of freedom (as suggested by detailed modelling)
lm.w <- lm(y ~ poly(x, 5), data = df.w2)

# Save results for predicting weights for seizures with pieces and no weights
save(lambda.r, lambda.w, lm.r, lm.w, file='wt est.Rdata')

# Save list of additional seizures omited from raw analysis
df.r2.omit <- df.r1 
df.r2.omit$stand.resid <- stand.res
df.r2.omit <- filter(df.r2.omit, stand.res < (-3)) 
write.csv(df.r2.omit, file = "Raw sz small resid omit.csv", row.names = F)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
