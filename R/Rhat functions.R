# Functions to check rhat values from JAGS model fitted to seizures data
# rhat.check.as.fn  looks specifically at the a's 
#   - random intercept and slope for each country in each of the 5 ivory classes
# rhat.check.other.fn looks at other parameter values in the model

# Functions specify the parameter to be inspected 
#==================================================================================

rhat.check.as.fn <- function(dat.use, rhat.guide = c(1.05,1.1)){
  r.use <- stack(as.data.frame(dat.use))
  N.ctry <- nrow(dat.use)
  r.use$ctry <- rep(1:N.ctry, 5)
  g0 <- ggplot(data=r.use,aes(x=ctry,y=values))
  g1 <- g0 + geom_point() + facet_wrap(~ind,nrow=2)
  g1 + geom_hline(yintercept=rhat.guide,colour="purple",linetype=2) + theme_bw()
}

rhat.check.other.fn <- function(dat.use, rhat.guide = c(1.05,1.1)){
  len.gp <- numeric()
  for (k in 1:length(dat.use))
    len.gp[k] <- length(dat.use[[k]])
  r.use <- data.frame(y=unlist(dat.use),x=rep(names(dat.use),len.gp))
  g0 <- ggplot(data=r.use,aes(x=x,y=y))
  g1 <- g0 + geom_point() 
  g1 + geom_hline(yintercept=rhat.guide,colour="purple",linetype=2) + theme_bw()
}

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
