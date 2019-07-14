# Calculates posterior distribution of lambda, theta and phi
# For each year, country and ivory class
# For a specific seizures model (fitted in sz_JAGS_model Final.R)
#
#  This file was used to fit many different models
#  Differences might include:
#    The number of polynomial terms
#    Covariates included in the seizure rate
#    Covariates included in the reporting rate
#  And so relevant parts of the R code including model name would change to reflect this
#  This script describes the Final model used for CoP18 analysis
#__________________________________________________________________
#  INPUTS
#    (a) covariates data - covars_use_Final.csv (from Select final data.R)
#    (b) seizures data - szs_use_Final.csv (from Select final data.R)
#    (c) R code to deal with South Sudan only having data from 2011 onwards - df_ctry_add_fn.R
#    (d) MCMC parameter chains for model saved in R workspace - eg a2_modnm.Rdata (from sz_JAGS_model Final.R)

# NOTE: There is a section in this program which is a copy and paste 
# of (most of) the DATA section in file - sz_JAGS_model_Final.R
# For this program to work you must ensure it is the same here
# Other components of this script need to change depending on the seizures model being used

# Also needs R packages 
# gdata
# tidyverse
#__________________________________________________________________
#  OUTPUTS
#    (a) Lambdas for each country, year and ivory class - lambda_modname.Rdata
#    (b) Thetas for each country, year and ivory class - theta_modname.Rdata
#    (c) Phis for each country, year and ivory class - phi_modname.Rdata
#    (d) The ids of the draws taken from the posterior distribution - sel_it_modname.Rdata
# These are all saved as .Rdata files.
#================================================================================================

# Set path names, in quotes, for working folder and sub-folder for results
path.code <- 'C:/ETIS/analysis/R Code'
path.dat <- 'C:/ETIS/analysis/Processed Data'
path.sims <- 'C:/ETIS/2018 JAGS MCMCoutputs'
path.outputs <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

#_______________________________________________________________
# Set model name

mod.nm <- 'sz_Final_jags' # SET model name

#================================================================================================

# Extract relevant data used in the modelling

library(gdata)
library(tidyverse)

# get data
setwd(path.dat)
file.cov <- 'covars_use_Final.csv'
file.szs <- 'szs_use_Final.csv'
df.covars <- read.csv(file.cov, header=T)
df.szs<- read.csv(file.szs, header=T)

year.from <- min(df.covars$year)
year.to <- max(df.covars$year)

n.ctry <- nlevels(df.szs$ctry)
ctry.lab <- levels(df.szs$ctry)
ctry.index <- 1:length(ctry.lab)

#_______________________________________________________________

# Get posterior distribution of model parameters

setwd(path.sims)

pms <- c('a0', 'a1','a2', 'a3','a4')
num.par <- length(pms)
for (i in 1:num.par)
    load(paste(pms[i],'_',mod.nm,'.Rdata',sep=''))


# Standardise all of the variables

zsc.fn <- function(x){
  zz <- (x - mean(x, na.rm = T))/sd(x, na.rm = T)
  return(zz)
}

# Create polynomial terms
Y.p <- poly(df.covars$year, 7)
Y.p <- as.data.frame(Y.p)
for (i in 1:7)
  names(Y.p)[i] <- paste('Y.p.',i, sep = '')
  
df.covars <- cbind(df.covars, Y.p)

# Adjustments for ss
# Need to add in values for ss for 2008 - 2010
# ie years in which the country did not exist
# Set polynomial terms to be same as for other years
# And for all other covariates set them to be NA
# And then when calculating values set them to be NA

setwd(path.code)
source('df_ctry_add_fn.R')

df.covars.orig <- df.covars
df.szs.orig <- df.szs

df.covars <- df.ctry.add.fn(df.covars.orig, ctry = 'ss')
df.szs <- df.ctry.add.fn(df.szs.orig, ctry = 'ss')

ss.info <- df.ctry.add.fn(df.szs.orig, ind = T)
ss.ind <- ss.info$ind
ss.diff.yr <- ss.info$diff.yr

Y.p.orig <- Y.p
Y.p <- df.covars[,grep('Y.p.', names(df.covars))]

# Create data collection score
dc <- df.covars$dc.pr + df.covars$dc.ta

#_______________________________________________________________
# Create model file with the data - to be comparable with that used in the modelling
# Copied into the DATA file

mod.dat <- 
  list(N      = dim(df.szs)[1], 
       N.c    = n.ctry,
       n.sz   = as.matrix(df.szs[,3:7]),
       ctry   = as.numeric(df.szs$ctry),
       Y.1    = Y.p[,1],
       Y.2    = Y.p[,2],
       Y.3    = Y.p[,3],
       Y.4    = Y.p[,4],
       LE1    = zsc.fn(df.covars$LE1),
       dc     = zsc.fn(dc),
       log.tr.ch2 =  zsc.fn(df.covars$log.iv.tr.ch2), 
       rep.l  = zsc.fn(df.covars$rep.log)
  )

n.ctry <- mod.dat$N.c
n.yr <- length(unique(df.covars$year))

#================================================================================================

# CALCULATE LAMBDAS  and save to file
#
num.iter <- 10000  
num.each.ch <- num.iter/2
n.sim <- dim(a0)[1]
ch.len <- n.sim/2

sel.it <- c((ch.len-num.each.ch+1):ch.len,(n.sim-num.each.ch+1):n.sim)

nsim <- length(sel.it)

lambda <- list(NULL)

for (k in 1:5){
  a.fix <- cbind(a2[, k], a3[, k], a4[, k])
  a.fix <- a.fix[sel.it, ]

  a0.sel <- a0[sel.it, , k]
  a1.sel <- a1[sel.it, , k]

  lg.lambda.fix <- (a.fix) %*%t (Y.p[, 2:4]) # Change for relevant number of polynomials

  lg.lambda.data <- data.frame(ctry = as.numeric(df.covars$ctry), Y.1 = Y.p[, 1])
  lg.lambda.data.arr <- as.array(as.matrix(lg.lambda.data))
  dim(lg.lambda.data.arr) <- c(n.ctry,n.yr,ncol(lg.lambda.data))
  lg.lambda.fix.arr <- as.array(as.matrix(t(lg.lambda.fix)))
   dim(lg.lambda.fix.arr) <- c(n.ctry,n.yr,nrow(lg.lambda.fix))

# Random intercept 
  lg.lambda.0 <- lg.lambda.fix.arr
  lg.lambda.1 <- lg.lambda.0
  for (i in 1:n.yr)
    lg.lambda.1[,i,]<- lg.lambda.0[,i,]+t(a0.sel)
  
# Random year effect
  lg.lambda.2 <- lg.lambda.1
  for (i in 1:n.yr)
    lg.lambda.2[,i,]<- lg.lambda.1[,i,]+t(a1.sel)*lg.lambda.data.arr[,i,2]

  lambda[[k]] <- exp(lg.lambda.2)
}

# Correct for SS values up until 2011
for (k in 1:5){
  lambda[[k]][ss.ind, (1:ss.diff.yr) , ] <- NA
}

setwd(path.outputs)
save(lambda, file=paste('lambda_', mod.nm, '.Rdata', sep=''))


####################################################################

#CALCULATE PHIs and THETAs and save to file
#setwd(path.wkg)
setwd(path.sims)
theta <- phi <- matrix(nrow = n.ctry * n.yr, ncol = nsim, data = 0)

pms <- c('b1', 'b2', 'g1', 'g2')
num.par <- length(pms)

for (i in 1:num.par)
  load(paste(pms[i],'_',mod.nm,'.Rdata',sep=''))

g1.sel <- g1[sel.it]
g2.sel <- g2[sel.it]
b1.sel <- b1[sel.it]
b2.sel <- b2[sel.it]

phi <- t(plogis(b1.sel %*% t(mod.dat$LE1)  + 
                  b2.sel %*% t(mod.dat$log.tr.ch2)  
                ))
theta <- t(plogis(g1.sel %*% t(mod.dat$dc)  + 
                    g2.sel %*% t(mod.dat$rep.l)
                  ))

setwd(path.outputs)
save(phi, file=paste('phi_', mod.nm, '.Rdata', sep=''))
save(theta, file=paste('theta_', mod.nm, '.Rdata', sep=''))

phi.arr <- phi
dim(phi.arr) <- c(n.ctry, n.yr, nsim)
theta.arr <- theta
dim(theta.arr) <- c(n.ctry, n.yr, nsim)

save(phi.arr, file=paste('phi_arr_', mod.nm, '.Rdata', sep=''))
save(theta.arr, file=paste('theta_arr_', mod.nm, '.Rdata', sep=''))

save(sel.it,file=paste('sel_it_',mod.nm,'.Rdata', sep=''))

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
