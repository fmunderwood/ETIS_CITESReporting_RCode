#   Fits a Bayesian hierarchical model to seizures data
#______________________________________________________________________
#   INPUTS
#     (1) Covariate data - covars_use_Final.csv (from Select final data.R)
#     (2) Number of seizures by country and year - szs_use_Final.csv (from Select final data.R)
#     (3) File Rhat functions.R to look at outputs
#     (4) R packages
#           XLConnect
#           jagsUI
#           coda
#           mcmcplots
#           tidyverse
#
#   Java (for XLConnect) need to be installed.
#______________________________________________________________________
#   OUTPUTS 
#     (1) Summary stats for model parameters, including DIC, saved in
#         Excel workbook - in subfolder 'Processed Data/JAGS Summaries':
#     (2) MCMC parameter chains, saved in R workspace (.Rdata) in separate folder      
#
#   Output files inherit the name assigned to the model in the variable
#   mod.nm.
#=======================================================================

# SET model name

mod.nm <- 'sz_Final' 

#______________________________________________________________________
#
#   Set path names for working folders and results.
#
path.code <- 'C:/ETIS/analysis/R Code'
path.data <- 'C:/ETIS/analysis/Processed Data'
path.out.sum <- 'JAGS summaries'
path.out.mcmc <- 'C:/ETIS/2018 JAGS MCMCoutputs'

#______________________________________________________________________

# Load Packages

myView <- View
library(jagsUI)
View <- myView
library(XLConnect)
library(coda)
library(mcmcplots)
library(tidyverse)
options(max.print=100000000)

#______________________________________________________________________

# get data and Rhat functions

file.cov <- 'covars_use_Final.csv'
file.szs <- 'szs_use_Final.csv'

setwd(path.code)
source('Rhat functions.R')

setwd(path.data)
df.covars <- read.csv(file.cov, header=T)
df.szs <- read.csv(file.szs, header=T)

#==========================================================================================
# Different models require edits from here onwards
#______________________________________________________________________

# MODEL

mod0 <- "model {
  for (i in 1:N) {
    for (k in 1:5) {
      n.sz[i, k] ~ dnegbin(p[i,k], r[k])
      p[i, k] <- r[k]/(mu[i, k] + r[k])
      mu[i, k] <- phi[i,k]*theta[i,k] * lambda[i,k]
      log(lambda[i, k])  <- a0[ctry[i], k] + a1[ctry[i], k] * Y.1[i]
                         + a2[k] * Y.2[i] + a3[k] * Y.3[i]  
                         + a4[k] * Y.4[i]
      logit(phi[i, k])   <- b1 * LE1[i]  + b2 * log.tr.ch2[i] 
                          

      logit(theta[i, k]) <- g1 * dc[i] + g2 * rep.l[i]
    } 
}


  for (k in 1:5) {
    lgr[k] ~ dunif(0, 5)
    log(r.cont[k]) <- lgr[k]
    r[k] <- round(r.cont[k])
  }
    for (j in 1:N.c) {
    a0[j, 1:5] ~ dmnorm(mu0[], Omega0.inv[,])
    a1[j, 1:5] ~ dmnorm(mu1[], Omega1.inv[,])
  }
  a2[1:5] ~ dmnorm(mn[], prec[,])
  a3[1:5] ~ dmnorm(mn[], prec[,])
  a4[1:5] ~ dmnorm(mn[], prec[,])

  b1 ~ dnorm(0, 1.0E-04)
  b2 ~ dnorm(0, 1.0E-04)
  g1 ~ dnorm(0, 1.0E-04)
  g2 ~ dnorm(0, 1.0E-04)

  mu0[1:5] ~ dmnorm(mn[], prec[,]) # mn & prec in data
  mu1[1:5] ~ dmnorm(mn[], prec[,])
  Omega0.inv[1:5, 1:5] ~ dwish(R0[,], 5) # R0, R1 in data
  Omega1.inv[1:5, 1:5] ~ dwish(R1[,], 5)
} #end model
"

setwd(path.data)

filename.use <- 'jags0.txt'
writeLines(mod0, filename.use)  # File for use in model
filename.save <- paste(mod.nm, 'txt', sep = '.') 
writeLines(mod0, filename.save) # Saved for reference

#______________________________________________________________________

# Creating the data to be used in the analysis

# Standardising variables
zsc.fn <- function(x){
  zz <- (x-mean(x))/sd(x)
  return(zz)
}

# Polynomial terms
Y.p <- poly(df.covars$year, 7)
dc <- df.covars$dc.pr + df.covars$dc.ta


# Values for R0 and R1
# Based on the covariance matrix of
# intercept and slope for each country and seizure type on log scale
df.szs$poly.yr <- poly(df.szs$year, 1)[,1]
df.szs.log <- df.szs
df.szs.log[,3:7] <- log(df.szs.log[,(3:7)] + 1)

n.ctry <- nlevels(df.szs$ctry)
ctry.lab <- levels(df.szs$ctry)
int.log <- slop.log <-  matrix(nrow = n.ctry, ncol = 5, data = 0)
for (i in 1:n.ctry){
  subs.log <- filter(df.szs.log, ctry == ctry.lab[i])
  for (k in 1:5){
    lm.subs.log <- lm(subs.log[,(k + 2)] ~ subs.log$poly.yr)
    int.log[i, k] <- lm.subs.log$coefficients[1]
    slop.log[i, k] <- lm.subs.log$coefficients[2]
  }  
}

cov.int.log <- cov(int.log)
cov.slop.log <- cov(slop.log)

#______________________________________________________________________

# DATA

basis.dat <- 
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
       rep.l  = zsc.fn(df.covars$rep.log),
       R0     = diag(round(diag(cov.int.log), 1)), 
       R1     = diag(round(diag(cov.slop.log))), 
       mn     = c(0,0,0,0,0),
       prec   = diag(0.0001,5,5)
)
#______________________________________________________________________

# INITIAL VALUES

mod.inits <- function(){  
  list(a0      = matrix(ncol = 5, data = runif(n.ctry * 5, -1, 1)), 
       a1      = matrix(ncol = 5, data = runif(n.ctry * 5, -1, 1)),
       a2      = runif(5, -1, 1),
       a3      = runif(5, -1, 1),
       a4      = runif(5, -1, 1),
       b1      = runif(1, -1, 1),
       b2      = runif(1, -1, 1),
       g1      = runif(1, -1, 1),
       g2      = runif(1, -1, 1),
       lgr     = runif(5, 0, 1),
       mu0     = runif(5, -1, 1),
       mu1     = runif(5, -1, 1),
       Omega0.inv = diag(runif(5, 0, 1), 5),
       Omega1.inv = diag(runif(5, 0, 1), 5)
  )  
}

#______________________________________________________________________

# PARAMETERS

pms <- c('a0', 'a1', 'a2', 'a3', 'a4', 'b1', 'b2', 'g1', 'g2',
         'r.cont', 'Omega0.inv', 'Omega1.inv', 'mu0', 'mu1') 

#______________________________________________________________________

# FIT MODEL

mod0 <- jags(data = basis.dat,
              inits = mod.inits,
              parameters.to.save = pms,
              model.file = "Jags0.txt",
              parallel = T,
              n.cores = 2,
              n.chains = 2,
              n.adapt = 50000,
              n.iter = 200000,
              n.burnin = 100000,
              n.thin = 10,
              seed = 15)

mod.use <- mod0

#______________________________________________________________________

# SAVE SUMMARY OF RESULTS

setwd(path.data)
path.out <- paste(path.data, path.out.sum, sep='/')
outfile <- paste(path.out, '/', mod.nm, '_jags_summaries.xlsx', sep='')
wb <- loadWorkbook(outfile, create=T)
createSheet(wb, name='Coefficients')
cc <- colnames(mod.use$summary)
cc[3:7] <- c('Q0.025', 'Q0.25', 'Q0.5', 'Q0.75', 'Q0.975')
colnames(mod.use$summary) <- cc
writeWorksheet(wb, data.frame(Param=rownames(mod.use$summary),mod.use$summary), 'Coefficients')
createSheet(wb, name='DIC')
writeWorksheet(wb, data.frame(DIC=mod.use$DIC, pD=mod.use$pD), 'DIC')
createSheet(wb, name='Details')
writeWorksheet(wb, data.frame(seed=mod.use$random.seed,adapt=mod.use$mcmc.info$n.adapt,
                              iter = mod.use$mcmc.info$n.iter,
                              burn = mod.use$mcmc.info$n.burnin,
                              thin = mod.use$mcmc.info$n.thin), 'Details')
saveWorkbook(wb)

#______________________________________________________________________

# SAVE SIMULATIONS

# Individual parameters
setwd(path.out.mcmc)
for (i in 1:length(pms)){
  res.mc <- mod.use$sims.list[[i]]
  assign(pms[i], res.mc)
  save(list=pms[i], file=paste(pms[i], '_', mod.nm, '_jags.Rdata', sep=''))
  rm(res.mc)
  rm(list=pms[i])
}

# Save everything together for further reference

setwd(path.out.mcmc)
save(mod.use,file=paste(mod.nm,'_jags_mod.Rdata',sep=""))

#______________________________________________________________________

# CHECKING FOR CONVERGENCE

# Overview

mod.use$mcmc.info$elapsed.mins
mod.use
mod.use$DIC  
mod.use$pD

# Preparation for closer inspection

mod.summ <- mod.use$summary

mod.mc <- mod.use$samples
mod.lst <- mod.use$sims.list

N.ctry <- dim(mod.lst[[1]])[2]

ind.names <- colnames(mod.mc[[1]])
num.nms <- ncol(mod.mc[[1]])
index <- 1:num.nms
pms.nms <- names(mod.use$sims.list)
num.pms <- length(pms.nms)

# a0 columns
a0.cols <- mod.use$sims.list$a0
# a1 columns
a1.cols <- mod.use$sims.list$a1
# a2 + other columns
pms.nms.noomeg <- pms.nms[!is.element(pms.nms, c("Omega0.inv", "Omega1.inv", "deviance"))]
a.ind <- grep("a", pms.nms.noomeg)
a.ind  <- a.ind[-c(1,2)]

if(length(a.ind) == 0){
  cov.ind <- (3): (num.pms  - 1 ) 
} else {
  cov.ind <- (max(a.ind) + 1): (num.pms  - 1 ) 
}

# CHECKING FOR CONVERGENCE

# Rhat values

# A0
rhat.check.as.fn(mod.use$Rhat$a0)
# A1
rhat.check.as.fn(mod.use$Rhat$a1)
# A2 - A... 
rhat.check.other.fn(mod.use$Rhat[a.ind])
# Covariates and deviance
rhat.check.other.fn(mod.use$Rhat[c(cov.ind, num.pms)])

# Gelman plots

# A0 Gelman plots 
for (j in 1:5)
  gelman.plot(mod.mc[,((j-1)*N.ctry + 1):(j*N.ctry)],ask=F, ylim = c(0.8, 1.2))

# A1 Gelman plots
for (j in 1:5)
  gelman.plot(mod.mc[,((5 + (j-1))*N.ctry+1):((5+j)*N.ctry)], ask=F, ylim = c(0.8, 1.2))

# Specific A1's for more detailed looking
j <- 3; gelman.plot(mod.mc[,((5 + (j-1))*N.ctry+1):((5+j)*N.ctry)], ask=F, ylim = c(0.8, 1.2))

# For other a terms
a.terms <- character()
j <- 0
for (k in a.ind){
  for (i in 1:5){
    j <- j + 1
    a.terms[j] <- paste(pms.nms[k], '[', i, ']', sep = '')
  }
}  
id.as <- index[is.element(ind.names,a.terms)]

gelman.plot(mod.mc[,id.as], ask = F, ylim = c(0.8, 1.2))

# For covariates
covars.use <- pms.nms[cov.ind]
covars.use <- covars.use[-grep("Omega", covars.use)]
covars.use <- covars.use[-grep("r.cont", covars.use)]
id.covars <- index[is.element(ind.names,covars.use)]

gelman.plot(mod.mc[,id.covars], ylim = c(0.8, 1.2))

# Omega0
id.O0 <- grep("Omega0.", ind.names)
gelman.plot(mod.mc[,id.O0], ylim = c(0.8, 1.2), ask = F)

# Omega1
id.O1 <- grep("Omega1.", ind.names)
gelman.plot(mod.mc[,id.O1], ylim = c(0.8, 1.2), ask = F)

# R
r.use <- c("r.cont[1]", "r.cont[2]", "r.cont[3]", "r.cont[4]", "r.cont[5]")
id.ruse <- index[is.element(ind.names,r.use)]

gelman.plot(mod.mc[,id.ruse], ylim = c(0.8, 1.2))

#______________________________________________________________________

# LOOK AT SUMMARY VALUES 

round(mod.summ,2)

# Model structure

denplot(mod.mc, "r.cont")
denplot(mod.mc, "a2")
denplot(mod.mc, "a3")
denplot(mod.mc, "a4")

apply(mod.use$sims.list$a2, 2, quantile, c(0.05, 0.5, 0.95))
apply(mod.use$sims.list$a3, 2, quantile, c(0.05, 0.5, 0.95))
apply(mod.use$sims.list$a4, 2, quantile, c(0.05, 0.5, 0.95))

# Bias adjusting covariates

quantile(mod.use$sims.list$b1, c(0.025, 0.5, 0.975))
quantile(mod.use$sims.list$b2, c(0.025, 0.5, 0.975))
quantile(mod.use$sims.list$g1, c(0.025, 0.5, 0.975))
quantile(mod.use$sims.list$g2, c(0.025, 0.5, 0.975))

caterplot(mod.mc, covars.use)
traplot(mod.mc, covars.use)
denplot(mod.mc, covars.use )
mcmcplot(mod.mc, covars.use)

# Other summaries

caterplot(mod.mc, "a2")
denplot(mod.mc, "Omega1.inv")
traplot(mod.mc, "g2")
caterplot(mod.mc, "Omega0.inv")

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
