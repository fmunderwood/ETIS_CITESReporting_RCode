# Creates the data used in the cluster analysis for CoP18
# The data are
# (i) for the countries used in the main analysis 
# (ii)totalled over the years used in the CoP report - in this case 2015 t0 2017
#
# Variables are:
#   (a) Transactions Index for each ivory class
#   (b) Bias adjusted seizures out 
#   (c) Bias adjusted seizures in
#   (d) Bias adjusted weights in
#   (e) Bias adjusted weights out
#
# (b-e) are calculated for all seizures
# and for those of at least the size limit - for CoP18 it is 500kg
# Note here that the labels say 1T rather than 500kg to denote change in size
# Originally a value of 1 Tonne was used (as for the last CoP report) 
# but there were only a  few seizures in this group

# This program needs to be run twice when: 
# country of destination is TRUE - to calculate trade flows for weights out
# country of destination is FALSE - to calculate seizures out
#
# Draws from the posterior predictive distribution of each variable are stored as outputs
#__________________________________________________________________________________
# INPUTS
# (a) covars_use_Final.csv (from Select final data.R)
# (b) lambda_modnm.Rdata - the posterior predictive distribution of lambda's 
#                          the individual country x year Transaction Index values
#                          (from Posterior distributions for lambda theta phi.R)
# (c) Variables for cluster analysis (from Multiple mentions all wtlimit sims.R)
#     These are adjusted seizures and weights in and out
#     For a particular model name
#     With inclusion or not of country of destination 
#     And for all seizures or those over the size limit - here 500kg
#     For example:Adj_sz_in_modnm_dest_TRUE.Rdata
#     And Adj_wt_out_modnm_dest_TRUE_min1T_500.Rdata
#  (d) R package: tidyverse
#__________________________________________________________________________________
# OUTPUTS
# .Rdata files of posterior predictive distribution of variables for cluster analysis
# Root file name is: ClusterVars_modnm_dest_*_size_*.Rdata
# Where dest_* can be dest_TRUE or dest_FALSE
# And size (for CoP18) size_500
#==================================================================================

library(tidyverse)

# Years:
yrs.base <- 2008:2017
yrs.use <- 2015:2017
yrs.lambda <- 2008:2017
#___________________________________________________________

# Model name
mod.nm <- 'sz_Final'

# Do we include country of destination
ct_dest <- TRUE

# Size class
size.use <- 500
#___________________________________________________________

# Path name for basic data
path.data <- 'C:/ETIS/analysis/Processed Data'
path.jags.data <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

#___________________________________________________________

# File name for covariates
file.covars <- 'covars_use_Final.csv'

# File names for adjusted seizures in and out
begin.name <- paste('dfinout_', mod.nm, sep = '')
end.name <- paste('_dest_', ct_dest, sep = '')
file.name <- paste(begin.name, end.name, sep = '')


# File name for adjusted seizures in and out over 1 tonne or other specified size
end.name.1T <-  paste(end.name, '_min_', size.use, sep = '')
file.name.1T <- paste(begin.name, end.name.1T, sep = '')

#==========================================================

#Reading in data
setwd(path.data)
df.covars <- read.csv(file.covars)

setwd(path.jags.data)
load(file=paste('lambda_', mod.nm, '_jags.Rdata', sep=''))

sz.in <- readRDS(file = paste('Adj_sz_in_',file.name, '.Rdata', sep = ''))
sz.out <- readRDS(file = paste('Adj_sz_out_',file.name, '.Rdata', sep = ''))
wt.in <- readRDS(file = paste('Adj_wt_in_',file.name, '.Rdata', sep = ''))
wt.out <- readRDS(file = paste('Adj_wt_out_',file.name, '.Rdata', sep = ''))


sz.in.1T <- readRDS(file = paste('Adj_sz_in_',file.name.1T, '.Rdata', sep = ''))
sz.out.1T <- readRDS(file = paste('Adj_sz_out_',file.name.1T, '.Rdata', sep = ''))
wt.in.1T <- readRDS(file = paste('Adj_wt_in_',file.name.1T, '.Rdata', sep = ''))
wt.out.1T <- readRDS(file = paste('Adj_wt_out_',file.name.1T, '.Rdata', sep = ''))

#___________________________________________________________
# Selecting the correct countries
ctry.use <- levels(df.covars$ctry)
n.ctry <- nlevels(df.covars$ctry)
ctry.lab <- sort(ctry.use)
n.yr <- length(yrs.use)
#___________________________________________________________
# Obtain ctry and year for each of the sz in and out etc
ctry.yr.inout <- rownames(sz.in)
ctry.yr.spl <- strsplit(ctry.yr.inout, ":")

ctry.inout.all <- unlist(lapply(ctry.yr.spl, first))
ind.ctry <- 1:length(ctry.inout.all)
ctry.inout.sel <- ind.ctry[is.element(ctry.inout.all,ctry.use)]


sz.in.sel <- sz.in[ctry.inout.sel,]
sz.out.sel <- sz.out[ctry.inout.sel,]
wt.in.sel <- wt.in[ctry.inout.sel,]
wt.out.sel <- wt.out[ctry.inout.sel,]

sz.in.arr <- as.array(sz.in.sel) 
dim(sz.in.arr) <- c(nrow(sz.in.sel)/n.yr, n.yr, ncol(sz.in.sel))
sz.out.arr <- as.array(sz.out.sel)
dim(sz.out.arr) <- c(nrow(sz.out.sel)/n.yr, n.yr, ncol(sz.out.sel))
wt.in.arr <- as.array(wt.in.sel)
dim(wt.in.arr) <- c(nrow(wt.in.sel)/n.yr, n.yr, ncol(wt.in.sel))
wt.out.arr <- as.array(wt.out.sel)
dim(wt.out.arr) <- c(nrow(wt.out.sel)/n.yr, n.yr, ncol(wt.out.sel))

sz.in.summ <- apply(sz.in.arr, c(1,3), sum)
sz.out.summ <- apply(sz.out.arr, c(1,3), sum)
wt.in.summ <- apply(wt.in.arr, c(1,3), sum)
wt.out.summ <- apply(wt.out.arr, c(1,3), sum)

ctry.yr.inout.1T <- rownames(sz.in.1T)
ctry.yr.spl.1T <- strsplit(ctry.yr.inout.1T, ":")

ctry.inout.1T.all <- unlist(lapply(ctry.yr.spl.1T, first))
ind.ctry.1T <- 1:length(ctry.inout.1T.all)
ctry.inout.sel.1T <- ind.ctry.1T[is.element(ctry.inout.1T.all,ctry.use)]

sz.in.1T.sel <- sz.in.1T[ctry.inout.sel.1T,]
sz.out.1T.sel <- sz.out.1T[ctry.inout.sel.1T,]
wt.in.1T.sel <- wt.in.1T[ctry.inout.sel.1T,]
wt.out.1T.sel <- wt.out.1T[ctry.inout.sel.1T,]

sz.in.1T.arr <- as.array(sz.in.1T.sel)
dim(sz.in.1T.arr) <- c(nrow(sz.in.1T.sel)/n.yr, n.yr, ncol(sz.in.1T.sel))
sz.out.1T.arr <- as.array(sz.out.1T.sel)
dim(sz.out.1T.arr) <- c(nrow(sz.out.1T.sel)/n.yr, n.yr, ncol(sz.out.1T.sel))
wt.in.1T.arr <- as.array(wt.in.1T.sel)
dim(wt.in.1T.arr) <- c(nrow(wt.in.1T.sel)/n.yr, n.yr, ncol(wt.in.1T.sel))
wt.out.1T.arr <- as.array(wt.out.1T.sel)
dim(wt.out.1T.arr) <- c(nrow(wt.out.1T.sel)/n.yr, n.yr, ncol(wt.out.1T.sel))

sz.in.1T.summ <- apply(sz.in.1T.arr, c(1,3), sum)
sz.out.1T.summ <- apply(sz.out.1T.arr, c(1,3), sum)
wt.in.1T.summ <- apply(wt.in.1T.arr, c(1,3), sum)
wt.out.1T.summ <- apply(wt.out.1T.arr, c(1,3), sum)


# Select data for lambdas
yrs.ind <- 1:length(yrs.lambda)
yrs.sel.lam <- yrs.ind[is.element(yrs.lambda, yrs.use)]
lambda.use <- list(NULL)
for (i in 1:5){
  lambda.use[[i]] <- lambda[[i]][,yrs.sel.lam,]
}

# Combine across years
lambda.summ <- list(NULL)
for (i in 1:5){
  lambda.summ[[i]] <- apply(lambda.use[[i]], c(1,3), sum, na.rm = T)
}

# Now combine all of these together

dat.all <- array(data = 0, dim = c(nrow(lambda.summ[[1]]), 13, ncol(lambda.summ[[1]])))
dat.all[,1,] <- sz.in.summ
dat.all[,2,] <- sz.out.summ
dat.all[,3,] <- wt.in.summ
dat.all[,4,] <- wt.out.summ
dat.all[,5,] <- sz.in.1T.summ
dat.all[,6,] <- sz.out.1T.summ
dat.all[,7,] <- wt.in.1T.summ
dat.all[,8,] <- wt.out.1T.summ
for (i in 1:5)
  dat.all[,(i + 8),] <- lambda.summ[[i]]

var.names <- c('sz.in', 'sz.out', 'wt.in', 'wt.out', 
               'sz.in.1T', 'sz.out.1T', 'wt.in.1T', 'wt.out.1T',
               'lambda.1', 'lambda.2', 'lambda.3', 'lambda.4', 'lambda.5')

dimnames(dat.all) <- list(ctry.use, var.names, NULL)


filename <- paste('ClusterVars_', mod.nm, '_dest_', ct_dest,'size_',size.use, 'Final.Rdata', sep = '')
saveRDS(dat.all, file = filename)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
