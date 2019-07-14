# Seizures for which weights need to be changed because of anomalies
#
# This information is used in wtg est.R
#
# These corrections are specific for the 2018 analysis
# The changes will then be hardwired into the database in the future
#===================================================================================

sz.wt.chg <- c(
  109137, # weight needs to be changed - correction came in after database signed off
  101114, # calculate worked from model and estimate rest as raw
  107677 # Weight is only given for one piece of worked ivory although quoted as if for 71
)

chg.df <- data.frame(sz.id = sz.wt.chg, new.raw.wgt = NA, new.wkd.wgt = NA, method = NA, notes = NA)
chg.df$new.raw.wgt[chg.df$sz.id == 109137] <- 1286
chg.df$method[chg.df$sz.id == 109137] <- 'correction'
chg.df$method[chg.df$sz.id == 101114] <- 'estimate W'
chg.df$method[chg.df$sz.id == 107677] <- 'estimate W'

chg.df$notes[chg.df$sz.id == 101114] <- c('calculate worked from model and estimate rest as raw')
chg.df$notes[chg.df$sz.id == 107677] <- c('Calculate weight for all but one piece which is the weight currently state')
#________________________________________________________________________________

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
