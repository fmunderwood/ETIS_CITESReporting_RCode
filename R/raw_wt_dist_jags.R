# Fits a model to obtain the distribution of weights of raw ivory seizures
# Only uses seizures for which the raw weight is actually given (ie excludes estimated weights)
# Include a constraint that weight of a seizure can't be more than 10 tonnes
#______________________________________________________________________
#   INPUTS
#   (1) sz recs with estd wgts yyyy_yyyy.csv (from sz data setup.R)
#   (2) R packages:
#      XLConnect
#      jagsUI
#      coda
#      mcmcplots
#      tidyverse
#
#   Java (for XLConnect) need to be installed.
#______________________________________________________________________
#   OUTPUTS 
#     (1) Summary stats for model parameters, including DIC, saved in
#         Excel workbook - in subfolder 'Processed Data/JAGS Summaries':
#     (2) MCMC parameter chains, saved in R workspace (.Rdata) - in separate folder
#         C: ETIS/2018 JAGS MCMC outputs
#
#   Output files inherit the name assigned to the model in the variable
#   mod.nm.
#=======================================================================

# Set model name and maximum weight limit 

mod.nm <- 'raw_wt_dist_Final' # SET model name
max.wt <- 10000

#______________________________________________________________________

# Set pathnames

path.code <- 'C:/ETIS/analysis/R Code'
path.data <- 'C:/ETIS/analysis/Processed Data'
path.outputs <- 'C:/ETIS/2018 JAGS MCMCoutputs'
#__________________________________________________________________________

# Load packages

myView <- View
library(jagsUI)
View <- myView
library(XLConnect)
library(tidyverse)
library(gdata)
library(coda)
library(mcmcplots)
#__________________________________________________________________________

# Select relevant seizures data for modelling

setwd(path.data)
df.szrecs <- read.csv('sz recs with estd wgts 2007_2017.csv', header=T)
df.szrecs <- filter(df.szrecs, sz.yr > 2007)

setwd(path.code)

# RAW IVORY SEIZURES WITH MEASURED (rather than estimated) WEIGHTS
df.raw <- df.szrecs %>%
  filter(raw == TRUE) %>%
  mutate(grp.re = reorder(raw.grp, new.order = c(1, 2, 3, 5, 4, 6))) %>%
  select(sz.id = sz.id, sz.yr = sz.yr, disc.ct = disc.ct, pcs = raw.pcs,
         wgt = raw.wgt, lo.cl = raw_l, up.cl = raw_u, grp = grp.re) 

df.raw.kn <- df.raw %>%
  filter(is.na(lo.cl)) %>%
  mutate(disc.ct = drop.levels(disc.ct),
         grp.3 = as.factor(grp))

levels(df.raw.kn$grp.3) <- c(1,1,2,2,3,3)  

wgt.raw.all <- df.raw.kn %>%
  select(sz.id, ctry = disc.ct, wgt, grp.3)

#__________________________________________________________________________

# DATA
mod.raw.wgt.dat <- list(N      = dim(wgt.raw.all)[1],
                        l.wgt  = log(wgt.raw.all$wgt),
                        max.wt = log(max.wt)  
)

#__________________________________________________________________________

# MODEL
mod.raw.wgt <- "model {
  for(i in 1:N) {
    l.wgt[i] ~ dt(mu[i], tau.y, nu)T(,max.wt)
    mu[i] <- a0 
  }
  a0 ~ dnorm(0, 1.0E-4)
  nu ~ dunif(1,30)
  tau.y ~ dgamma(0.001, 0.001)
  sigma2.y <- 1/tau.y
} 
"

filename <- c('raw_wt_mod_08.txt')
writeLines(mod.raw.wgt, filename)

#__________________________________________________________________________

# INITS
mod.raw.wgt.inits <- function(){
  list(
    a0    = rnorm(1, 0, 1),
    tau.y = runif(1, 0, 1),
    nu    = runif(1, 1,5)
  )
}  

#__________________________________________________________________________

# PARAMETERS
pms <- c('a0', 'sigma2.y', 'nu') 

#__________________________________________________________________________

# FIT MODEL

mod.raw <- jags(data = mod.raw.wgt.dat,
              inits = mod.raw.wgt.inits,
              parameters.to.save = pms,
              model.file = "raw_wt_mod_08.txt",
              parallel = T,
              n.cores = 2,
              n.chains = 2,
              n.adapt = 1000,
              n.iter = 20000,
              n.burnin = 10000,
              n.thin = 2)

#__________________________________________________________________________

# CHECK FOR CONVERGENCE

mod.mc <- mod.raw$samples
gelman.plot(mod.mc, ask = F, ylim = c(0.8, 1.2))
traplot(mod.mc)
denplot(mod.mc)

#__________________________________________________________________________

# SAVE SUMMARY OF RESULTS

setwd(path.outputs)

outfile <- paste(path.outputs, '/Results_', mod.nm, '_long.xlsx', sep='')
if(file.exists(outfile)) file.remove(outfile)
wb <- loadWorkbook(outfile, create=T)
createSheet(wb, name='Coefficients')
cc <- colnames(mod.raw$summary)
cc[3:7] <- c('Q0.025','Q0.25','Q0.5','Q0.75','Q0.975')
writeWorksheet(wb, data.frame(Param=rownames(mod.raw$summary),mod.raw$summary), 'Coefficients')
createSheet(wb, name='DIC')
writeWorksheet(wb, data.frame(DIC=mod.raw$DIC,pD=mod.raw$pD), 'DIC')
createSheet(wb, name='Details')
writeWorksheet(wb, data.frame(adapt = mod.raw$mcmc.info$n.adapt,
                              iter = mod.raw$mcmc.info$n.iter,
                              burn = mod.raw$mcmc.info$n.burnin,
                              thin = mod.raw$mcmc.info$n.thin,
                              time = mod.raw$mcmc.info$elapsed.mins), 'Details')
saveWorkbook(wb)


#__________________________________________________________________________

# SAVE SIMULATIONS

for (i in 1:length(pms)){
  res.mc <- mod.raw$sims.list[[i]]
  assign(pms[i], res.mc)
  save(list = pms[i], file = paste(path.outputs, '/', pms[i], '_', mod.nm, '_long.Rdata', sep=''))
  rm(res.mc)
  rm(list = pms[i])
}

outfile <- paste(path.outputs, '/Results_', mod.nm, '_mod_long.Rdata', sep='')
save(mod.raw,file = outfile)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
