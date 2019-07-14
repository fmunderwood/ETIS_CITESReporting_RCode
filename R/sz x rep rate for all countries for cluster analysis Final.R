# Calculates the posterior predictive distribution 
# of the bias adjustment factor:  1/(theta * phi) 
# for all countries that made or were implicated in seizures 
# and for years relevant for the cluster analysis
#
# The covariates used for the seizures analysis are required because they are standardised
# The mean and variance are calculated and used when 
# calculating seizure and reporting rates for all countries and years
#
# This file requires editing each analysis to update relevant years and covariates
#_____________________________________________________________________________
# INPUTS
# (a) covars_use_Final.csv - Covariates for countries used in the seizures analysis (from Select final data.R)
# (b) covars_use_Final_ALL.csv - File with covariates for all countries in all years (from Select covars cluster ALL.R)
# (c) pms_modnm_jags_Rdata - posterior distributions of parameters used 
#                            to estimate seizure rate and reporting rate (from szs_JAGS_model Final.R)
# (d) sel_it_modnm_jags.Rdata - iterations selected for calculating the TI (from szs_JAGS_model Final.R)
# (e) R Packages:
#       XLConnect
#       tidyverse
#_____________________________________________________________________________
# OUTPUTS
# (a) sz_adjusted_modnm_ALL_Final.csv - posterior distribution of the bias adjustment rate 
# (b) covars_adj_ref.csv - indicates which country and year each row in the above file relates to
#===============================================================================

# Identify path names
# Path for data
path.data <- 'C:/ETIS/analysis/Processed Data'
path.jags.sims <- 'C:/ETIS/2018 JAGS MCMCoutputs'
path.jags.sims.proc <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'
#_____________________________________________________________________________
# Identify model name

mod.nm <- 'sz_Final' # SET model name

#_____________________________________________________________________________
# Identify data files

# Data used in the model
dat.covars.mod <- 'covars_use_Final.csv'

# All countries data
dat.covars.all <- 'covars_use_Final_ALL.csv'


# years to use
year.from <- 2008
year.to <- 2017


# Cluster years to use
year.from.clus <- 2015
year.to.clus <- 2017
#=====================================================================

library(XLConnect)
library(tidyverse)

# Get data and obtain mean and sd used for standardising

setwd(path.data)

df.covars <- read.csv(dat.covars.mod, header=T)

yrs.use <-year.from:year.to
df.covars$dc <- df.covars$dc.pr+df.covars$dc.ta

rep.l.mn <- mean(df.covars$rep.log)
dc.mn <- mean(df.covars$dc)
LE1.mn <- mean(df.covars$LE1)
tr.ch.mn<- mean(df.covars$log.iv.tr.ch2)

rep.l.sd <- sd(df.covars$rep.log)
dc.sd <- sd(df.covars$dc)
LE1.sd <- sd(df.covars$LE1)
tr.ch.sd <- sd(df.covars$log.iv.tr.ch2)

# Get data for all countries
setwd(path.data)
df.covars.all <- read.csv(dat.covars.all,header=T)

df.covars.all <- df.covars.all %>%
  filter(year >= year.from, year <= year.to)

df.covars.all$dc <- df.covars.all$dc.pr + df.covars.all$dc.ta

# Get model coefficients
setwd(path.jags.sims)

load(paste('b1_', mod.nm, '_jags.Rdata', sep = ''))
load(paste('b2_', mod.nm, '_jags.Rdata', sep = ''))
load(paste('g1_', mod.nm, '_jags.Rdata', sep = ''))
load(paste('g2_', mod.nm, '_jags.Rdata', sep = ''))

setwd(path.jags.sims.proc)
load(paste('sel_it_', mod.nm, '_jags.Rdata', sep = ''))

num.sims <- length(sel.it)

b1 <- b1[sel.it]
b2 <- b2[sel.it]
g1 <- g1[sel.it]
g2 <- g2[sel.it]

# Now create coefficients for model
# Get coefficients
beta0 <- beta1 <- beta2  <- gamma0 <- gamma1 <- gamma2 <- numeric(num.sims)
# Create theta and phi
phi.all <- theta.all <- matrix(nrow = nrow(df.covars.all), ncol = num.sims, data = 0)

for (i in 1:length(sel.it)){
  beta0[i] <- -1*(b1[i] * LE1.mn/LE1.sd + 
                    b2[i] * tr.ch.mn/tr.ch.sd)
  beta1[i] <- b1[i] / LE1.sd
  beta2[i] <- b2[i] / tr.ch.sd

  gamma0[i] <- -1*(g1[i] * dc.mn/dc.sd + g2[i] * rep.l.mn/rep.l.sd)
  gamma1[i] <- g1[i]/dc.sd
  gamma2[i] <- g2[i]/rep.l.sd

   phi.all[,i] <- plogis(beta0[i] + 
                           beta1[i] * df.covars.all$LE1 + 
                           beta2[i] * df.covars.all$log.iv.tr.ch2
   )
  
  
  theta.all[,i] <- plogis(gamma0[i] + 
                            gamma1[i] * df.covars.all$dc + 
                            gamma2[i] * df.covars.all$rep.l)
}
# Calculate adjustment for seizures
sz.adj.all <- 1/(phi.all * theta.all)

# Select for years that are required for the cluster analysis
sel.all <- 1:nrow(df.covars.all)
yrs.sel <- df.covars.all$year >= year.from.clus & 
           df.covars.all$year <= year.to.clus
sel.yrs <- sel.all[yrs.sel]
sz.adj.sel <- sz.adj.all[sel.yrs,]
df.covars.all.sel <- df.covars.all[sel.yrs,]


# Write to file
setwd(path.jags.sims.proc)
write.csv(sz.adj.sel, file = paste('sz_adjusted_', mod.nm, '_ALL_Final.csv', sep = ""))

# Covariate information
write.csv(data.frame(ctry = df.covars.all.sel$ctry, year = df.covars.all.sel$year),
          file = 'covars_adj_ref_Final.csv')

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
