# Model checking of the model for number of seizures
# Looks at posterior predictive distributions of the y's
#   And compares to the data
#   This can be done for each ivory class, year or country
#
# Here I have used 50% intervals (as suggested by Gelman in talk)
#  As a way of clearly seeing how the model is working
#________________________________________________________________________
# INPUTS
#  (1) Seizures data - szs_use_Final.csv
#  (2) Covariate data -  covars_use_Final.csv
#  (3) R file to deal with South Sudan only having data from 2011 onwards - df_ctry_add_fn.R 
#  (4) Posterior distributions of the parameters of the seizures model
#      File names are parameter_modelname.Rdata where parameters are
#     (a) Lambda - calculated in Posterior distributions for lambda theta phi.R
#     (b) Theta - calculated in Posterior distributions for lambda theta phi.R
#     (c) Phi - calculated in Posterior distributions for lambda theta phi.R
#     (d) r - original draws saved in sz_JAGS_model Final.R
#     (e) sel_it - identify of draws used to create posterior distributions of lambda, theta and phi 
#   and saved in Posterior distributions for lambda theta phi.R
#
#  R packages: 
#    tidyverse
#________________________________________________________________________
# OUTPUTS
#   A set of y's could be saved so that the first part of the program does not
#   need to be repeated
#   Main outputs are a series of graphs that give observed data
#   and posterior medians or meansand credible intervals from posterior predictive distributions
#   These are summarised by ivory class, year or country
#=======================================================================

library(tidyverse)
#________________________________________________________________________
# Path for R code
path.code <- 'C:/ETIS/analysis/R Code'

# Path for model parameters - lambda, theta, phi
path.param <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

# Path for model parameters r
path.param.r <- 'C:/ETIS/2018 JAGS MCMCoutputs/'

# Path for data - the number of seizures
path.dat <- 'C:/ETIS/analysis/Processed Data'

# Path for graphs
path.graph <- 'C:/ETIS/2018 JAGS MCMCoutputs/Graphs'

#________________________________________________________________________

# Model name
mod.nm <- 'sz_Final_jags' # SET model name

#________________________________________________________________________

# Identify files with data
file.cov <- 'covars_use_Final.csv'
file.szs <- 'szs_use_Final.csv'

#________________________________________________________________________
# Set years 

yrs <- 2008:2017

#========================================================================
# Get functions for adjusting for ss
setwd(path.code)
source('df_ctry_add_fn.R')

# get data
setwd(path.dat)
df.covars <- read.csv(file.cov, header=T)
df.szs<- read.csv(file.szs, header=T)

# Adjust for South Sudan with missing years
df.covars.orig <- df.covars
df.szs.orig <- df.szs

df.szs <- df.ctry.add.fn(df.szs.orig)
df.covars <- df.ctry.add.fn(df.covars.orig)

# get model parameters
setwd(path.param)

load(file=paste('lambda_', mod.nm, '.Rdata', sep=''))
load(file=paste('theta_arr_', mod.nm, '.Rdata', sep=''))
load(file=paste('phi_arr_', mod.nm, '.Rdata', sep=''))
load(file=paste(paste('sel_it_', mod.nm, '.Rdata', sep='')))

setwd(path.param.r)
load(file=paste('r.cont_', mod.nm, '.Rdata', sep=''))
r <- r.cont

nsim <- dim(lambda[[1]])[3]
rlen <- dim(r)[1]

r.use <- r[sel.it, ]

ngps <- length(lambda)

n.ctry <- length(unique(df.covars$ctry))
n.yr <- length(unique(df.covars$year))

# Calculate the mu - mean
 mu <- list(NULL)
 for (k in 1:ngps){
    mu[[k]] <- lambda[[k]] * theta.arr * phi.arr
 }

# Generate the y's
 y <- list(NULL)

 for (k in 1:ngps){
 y[[k]] <- numeric(nsim * n.ctry * n.yr)
 dim(y[[k]]) <- dim(mu[[k]])
 for (i in 1:nsim){
  y[[k]][, , i] <- rnbinom(n = n.ctry * n.yr, size = r.use[i, k], mu = mu[[k]][, , i])
 }
}

# Save posterior predictive distribution of y's if required
#setwd(path.param)
#save(y, file=paste('y_', mod.nm, '.Rdata', sep=''))

# Load output if desired
#load(file=paste('y_', mod.nm, '.Rdata', sep=''))

#_________________________________________________________________________
# Calculate relevant summaries of the posterior predictive distributions
# Summarise across countries and years
y.mn.lst <- list(NULL)
y.lo.lst <- list(NULL)
y.hi.lst <- list(NULL)
for (k in 1:ngps){
 y.mn.lst[[k]] <- apply(y[[k]], c(1, 2), mean, na.rm = T)
 y.lo.lst[[k]] <- apply(y[[k]], c(1, 2), quantile, 0.25, na.rm = T)
 y.hi.lst[[k]] <- apply(y[[k]], c(1, 2), mean, 0.75)
} 

# Sum across years or across countries
y.yrs.sum <- list(NULL)
y.ctry.sum <- list(NULL)
for (k in 1:ngps){
 y.yrs.sum[[k]] <- apply(y[[k]], c(2, 3), sum, na.rm = T)
 y.ctry.sum[[k]] <- apply(y[[k]], c(1, 3), sum, na.rm = T)
}

y.yrs.mn <- matrix(nrow=n.yr, ncol=ngps, data=0)
y.yrs.md <- matrix(nrow=n.yr, ncol=ngps, data=0)
y.yrs.lo <- matrix(nrow=n.yr, ncol=ngps, data=0)
y.yrs.hi <- matrix(nrow=n.yr, ncol=ngps, data=0)
y.ctry.mn <- matrix(nrow=n.ctry, ncol=ngps, data=0)
y.ctry.md <- matrix(nrow=n.ctry, ncol=ngps, data=0)
y.ctry.lo <- matrix(nrow=n.ctry, ncol=ngps, data=0)
y.ctry.hi <- matrix(nrow=n.ctry, ncol=ngps, data=0)
for (k in 1:ngps){
 y.yrs.mn[, k] <- apply(y.yrs.sum[[k]], 1, mean, na.rm = T)
 y.yrs.md[, k] <- apply(y.yrs.sum[[k]], 1, median, na.rm = T)
 y.yrs.lo[, k] <- apply(y.yrs.sum[[k]], 1, quantile, 0.25, na.rm = T)
 y.yrs.hi[, k] <- apply(y.yrs.sum[[k]], 1, quantile, 0.75, na.rm = T)
 y.ctry.mn[, k] <- apply(y.ctry.sum[[k]], 1, mean, na.rm = T)
 y.ctry.md[, k] <- apply(y.ctry.sum[[k]], 1, median, na.rm = T)
 y.ctry.lo[, k] <- apply(y.ctry.sum[[k]], 1, quantile, 0.25, na.rm = T)
 y.ctry.hi[, k] <- apply(y.ctry.sum[[k]], 1, quantile, 0.75, na.rm = T)
}

# Calculate relevant summary values for observed data

obs.yrs.sum <- matrix(nrow=n.yr, ncol=ngps, data=0)
for (k in 1:ngps){
 obs.yrs.sum[, k] <- tapply(df.szs[, (k+2)], df.szs$year, sum, na.rm = T)
}

obs.ctry.sum <- matrix(nrow=n.ctry, ncol=ngps, data=0)
for (k in 1:ngps){
 obs.ctry.sum[, k] <- tapply(df.szs[, (k+2)], df.szs$ctry, sum, na.rm = T)
}

# Sum across countries in each year for an ivory class
y.ic <- y.ic.md <- y.ic.mn <- y.ic.lo <- y.ic.hi <- list(NULL)
for (i in 1:5){
 y.ic[[i]] <- apply(y[[i]], c(2, 3), sum, na.rm = T)
 y.ic.mn[[i]] <- apply(y.ic[[i]], 1, mean, na.rm = T) 
 y.ic.md[[i]] <- apply(y.ic[[i]], 1, median, na.rm = T) 
 y.ic.lo[[i]] <- apply(y.ic[[i]], 1, quantile, 0.25, na.rm = T) 
 y.ic.hi[[i]] <- apply(y.ic[[i]], 1, quantile, 0.75, na.rm = T) 
}
# Do same for data
obs.ic.sum <- matrix(nrow=n.yr, ncol=ngps, data=0)
for (k in 1:ngps){
 obs.ic.sum[, k] <- tapply(df.szs[, (k+2)], df.szs$year, sum, na.rm = T)
}

#=============================================================
#Plots
wt.gp <- c('Raw <10kg', 'Raw 10-100kg', 'Raw 100kg+', 'Worked <10kg', 'Worked 10kg+')
setwd(path.graph)

# Each year 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(paste("CheckYears_", mod.nm, ".pdf", sep=""))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfcol=c(3, 2))
for (k in 1:ngps){
 plot(y.yrs.md[, k]~yrs, type='l', ylim=c(0, max(c(y.yrs.hi[, k], obs.yrs.sum[, k]))), 
    , main=wt.gp[k], xlab='Year', ylab='Seizures')
 lines(y.yrs.lo[, k]~yrs, lty=2)
 lines(y.yrs.hi[, k]~yrs, lty=2)
 points(obs.yrs.sum[, k]~yrs, pch=19, col=2)   
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Each country
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(paste("CheckCtry_", mod.nm, ".pdf", sep=""))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfcol=c(3, 2), mar=c(3, 2, 4, 1))
ctry.num <- 1:n.ctry
ctry.id <- levels(df.szs$ctry)
for (k in 1:ngps){
 ord.ctry <- order(obs.ctry.sum[, k])
 plot(y.ctry.md[, k]~ctry.num, 
    ylim=c(0, 10+max(c(y.ctry.hi[, k], obs.ctry.sum[, k]))), 
    type='n')
 segments(ctry.num, y.ctry.lo[, k], ctry.num, y.ctry.hi, col=1)
 points(obs.ctry.sum[, k]~ctry.num, pch=19, col=2)   
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 # Each ivory class
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(paste("CheckIC_", mod.nm, ".pdf", sep=""))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
par(mfcol=c(3, 2))
for (k in 1:ngps){
 plot(y.ic.md[[k]]~yrs, type='l', ylim=c(0, max(c(y.ic.hi[[k]], obs.ic.sum[, k]))), 
    , main=wt.gp[k], xlab='Year', ylab='Seizures')
 lines(y.ic.lo[[k]]~yrs, lty=2)
 lines(y.ic.hi[[k]]~yrs, lty=2)
 points(obs.ic.sum[, k]~yrs, pch=19, col=2)   
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#_________________________________________________________________

# Plots for model checking for each country
# 
y.ctry.yr.md <- list(NULL)
y.ctry.yr.lo <- list(NULL)
y.ctry.yr.hi <- list(NULL)
y.ctry.yr.mn <- list(NULL)
for (k in 1:ngps){
 y.ctry.yr.md[[k]] <- apply(y[[k]], c(1, 2), median, na.rm = T)
 y.ctry.yr.lo[[k]] <- apply(y[[k]], c(1, 2), quantile, 0.25, na.rm = T)
 y.ctry.yr.hi[[k]] <- apply(y[[k]], c(1, 2), quantile, 0.75, na.rm = T)
 y.ctry.yr.mn[[k]] <- apply(y[[k]], c(1, 2), mean, na.rm = T)
}

 
#Select country desired for plotting
# List of countries to check
ctry.use.list <- levels(df.szs$ctry)  # Could use everything
ctry.use.list <- c("gb") # Or an individual country
for (i in 1:length(ctry.use.list)){
ctry.use <- ctry.use.list[i]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf(paste("Check_", ctry.use, "_", mod.nm, "50int.pdf", sep=""))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ctry.num <- 1:length(ctry.id)
ctry.code <- ctry.num[ctry.id==ctry.use]
par(mfcol=c(3, 2))
for (k in 1:ngps){
 obs.val <- df.szs[df.szs$ctry==ctry.use, (k+2)]
 plot(y.ctry.yr.mn[[k]][ctry.code, ]~yrs, type='l', ylim=c(0, max(c(y.ctry.yr.hi[[k]][ctry.code, ], obs.val), na.rm = T)), main=ctry.use)
 lines(y.ctry.yr.lo[[k]][ctry.code, ]~yrs, lty=2)
 lines(y.ctry.yr.hi[[k]][ctry.code, ]~yrs, lty=2)
 points(obs.val~yrs, pch=19, col=2)   
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
}

# Covariates for that country to help understand data
#df.covars[df.covars$ctry==ctry.use, ]

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
