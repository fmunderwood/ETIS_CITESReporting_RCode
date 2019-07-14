# Simulates weights of worked ivory transactions
# For each year, country and ivory class
# Using worked weights distribution model
# and lambdas[4, 5] - the number of transactions in each worked ivory class
# from the final seizures model used to create the Transactions Index
#_______________________________________________________________
# INPUTS
# (a) Lambda's - lambda_modnm.Rdata (from Posterior distributions for lambda theta phi.R) 
# (b) Parameters from worked weights distribution model - a0, sigma2.y, dof
# (c) R packages:
#       tidyverse
#       gdata
#_______________________________________________________________
# OUTPUTS
# (a) Weights_wkd_modnm.Rdata
#===============================================================
 # set path name for working folder

path.out.wts <-  'C:/ETIS/2018 JAGS MCMCoutputs'
path.out.szs <-  'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

#_______________________________________________________________
# Set model name

mod.nm <-  'sz_Final_jags' # SET model name
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Extract relevant data used in the modelling

library(gdata)
library(tidyverse)

# Lambdas

setwd(path.out.szs)
load(file = paste('lambda_', mod.nm, '.Rdata', sep = ''))

# Remove lambda for the three raw ivory classes so only have worked ivory

lambda <-  list(lambda[[4]], lambda[[5]])

# results of weights per seizure models

setwd(path.out.wts)

load('a0_wkd_wt_dist_Final_long.Rdata')
load('sigma2.y_wkd_wt_dist_Final_long.Rdata')
load('nu_wkd_wt_dist_Final_long.Rdata')

# Set number of years
years <-  2008:2017


#_______________________________________________________________
# Getting the years sorted


yr.unq <-  years # sort(unique(df.szrecs$sz.yr))
n.yrs <-  length(yr.unq)
n.ctry <-  dim(lambda[[1]])[1]

#_______________________________________________________________
# Selecting the draws from the posterior distributions
# Use the same number as the lambda
# There must be at least as many iterations for
# the weights values as there are for the lambdas
# Otherwise there is an error message

n.sim <- dim(lambda[[1]])[3]
n.iter.wt <- length(a0)

ind.wt <- 1:n.iter.wt
num.each.ch <- n.sim/2
ch.len <- n.iter.wt/2
sel.it <- c((ch.len - num.each.ch+1):ch.len,
            (n.iter.wt - num.each.ch + 1):n.iter.wt)

ind.sel <- ind.wt[sel.it] 


a0.r.use <-  a0[ind.sel]
sigma2.r.use <-  sigma2.y[ind.sel]
nu.use <-  nu[ind.sel]

remove(a0, sigma2.y, nu)


#_______________________________________________________________
# Preparing where to store the results

y.sum.wkd <-  list(NULL)
for (k in 1:2){
  y.sum.wkd[[k]] <-  matrix(nrow = n.ctry*n.yrs, ncol = n.sim, data = 0)
  dim(y.sum.wkd[[k]]) <-  dim(lambda[[1]])
}


#_______________________________________________________________
# Loop to generate values

for (i in 1:n.sim){
  mu.use <-  a0.r.use[i]
  sigma.use <-  sigma2.r.use[i]
  dof.use <-  nu.use[i]
  for (j in 1:n.yrs){
    nums.lambda <-  list(round(lambda[[1]][, j, i]), round(lambda[[2]][, j, i]))
    tot.sim <-  unlist(lapply(nums.lambda, sum, na.rm = T))
    all.tot.sim <-  sum(tot.sim)
    
    cut.1 <-  (log(10) - mu.use)/sigma.use
    cut.2 <-  (log(10000) - mu.use)/sigma.use  
    
    y.sim.gp <-  list(NULL)

# Start by working in one weight category
    N.need <-   tot.sim[1]
    N.sim <- round(N.need/(1 - pt(cut.1, df = dof.use)))
    id.sim <-  1:N.sim
    y.sim <-  rt(n = N.sim, df = dof.use)
    id.use <-  id.sim[y.sim<cut.1]
    y.high <-  y.sim[ - id.use]
    y.use <-  y.sim[id.use]

    while(length(y.use)<N.need){
      y.new <-  rt(n = N.sim, df = dof.use)
      id.use <-  id.sim[y.new<cut.1]
      y.use <-  c(y.use, y.new[id.use])
      y.high <-  c(y.high, y.new[ - id.use])               
    }

    y.sim.gp[[1]] <-  exp(y.use[1:N.need]*sigma.use+mu.use)

    N.need <-  tot.sim[2]
    y.left <-  y.high
    y.use <-  y.left[y.left<cut.2]
    N.sim <-  round(N.need/(1 - pt(cut.1, df = dof.use)))

    while(length(y.use)<N.need){
      id.sim <-  1:N.sim
      y.new <-  rt(n = N.sim, df = dof.use)
      id.use <-  id.sim[y.new >= cut.1 & y.new<cut.2]
      y.use <-  c(y.use, y.new[id.use])
    }

    y.sim.gp[[2]] <-  exp(y.use[1:N.need]*sigma.use+mu.use)

# Now we need to allocate the weights to each country so that we can do the correct sums
    id.ctry <-  nums.lambda.no.na <-  list(NULL)
    for (k in 1:2){
      nums.lambda.no.na[[k]] <-  replace_na(nums.lambda[[k]], 0)
      id.ctry[[k]] <-  rep(1:n.ctry, nums.lambda.no.na[[k]])
    }
      
    for (k in 1:2){
      if (sum(nums.lambda.no.na[[k]])>0){
        y.sum.wkd[[k]][nums.lambda.no.na[[k]]>0, j, i] <-  tapply(y.sim.gp[[k]], id.ctry[[k]], sum)
      }  
    }
    remove(y.sim.gp)
  }
}

#_______________________________________________________________
# Save results
setwd(path.out.szs)

save(y.sum.wkd, file = paste('wts_wkd_', mod.nm, '.Rdata', sep = ''))

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
