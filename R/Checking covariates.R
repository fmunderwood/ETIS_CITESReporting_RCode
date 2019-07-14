# Compares two sets of covariate values
# Those to be used in current analysis
# With previous analysis
#
# Specifically it compares past and current values of:
#    data collection score 
#    lagged LE ratio 
#    CITES reporting score
#
# The file plots a graph for each country over time with lines for both datasets
# Black line for most recent data
# Red line for previous data
# If all the data is in agreement then only the black line should show
# Discrepancies in the covariates are also written to a file 
# 
# These can then be checked by the ETIS team
#______________________________________________________________________________________
# INPUTS
# csv files of
#  (1) current covariates - covars_ddmmyyyy.csv
#  (2) past covariates - covars_ddmmyyyy.csv (in old directory)
#  (3) countries in most recent analysis - CTS_ALL_ddmmyyyy.csv
# R file with ggplot theme: ggplot theme.R
# R packages: tidyverse
#______________________________________________________________________________________
# OUTPUTS
# csv file with current and past values of covariate
# pdf with graph comparing the two sets of data 
#=======================================================================================

library(tidyverse)
path.code <- 'C:/ETIS/analysis/R Code'
setwd(path.code)
source('ggplot theme.R')

# Set working directory
path.new.data <- 'C:/ETIS/analysis/Processed Data'
file.new.data <- 'covars_19072018.csv'
ctry.data <- 'CTS_ALL_17072018.csv'
  
path.old.data <- 'C:/ETIS/analysis - 2017 Standing Committee tidy/Seizures Model'
file.old.data <- 'covars_17082017.csv'

# Get new data
setwd(path.new.data)
ff <-read.csv(file.new.data)
ctry.use <- levels(read.csv(ctry.data)$ctry)


# Get original data
setwd(path.old.data)
gg <-read.csv(file.old.data)


# Set years for comparision
yrs.from.comp <- max(min(ff$year), min(gg$year))
yrs.to.comp <- min(max(ff$year), max(gg$year))

        
# Select same set of data

ff.use <- filter(ff, is.element(ctry, ctry.use))
gg.use <- filter(gg, is.element(ctry,ctry.use))

ctry.both <- unique(ctry.use[is.element(ctry.use, unique(gg.use$ctry))])

years.from <- min(ff.use$year)
years.to <- max(ff.use$year)

ff.use <- filter(ff.use, year >= years.from, year <= years.to)
gg.use <- filter(gg.use, year >= years.from & year <= years.to)

ff.use$dc <- ff.use$dc.pr + ff.use$dc.ta
gg.use$dc <- gg.use$dc.pr + gg.use$dc.ta

# Data Collection Score

ff.ord <- ff.use
gg.ord <- gg.use

years.gg <-sort(unique(gg.use$year))

hh.ord <- ff.ord[,1:3]
hh.ord$dc <- ff.ord$dc
hh.ord$old.dc <- NA

for (i in 1:length(ctry.both))
  hh.ord$old.dc[hh.ord$ctry == ctry.both[i] & is.element(hh.ord$year, years.gg)] <- 
  gg.ord$dc[gg.ord$ctry == ctry.both[i]]

hh <-data.frame(rbind(hh.ord[,1:3], hh.ord[,1:3]))
hh$dc <- c(hh.ord$dc, hh.ord$old.dc)
hh$analysis <- as.factor(rep(c("current","previous"), each=nrow(hh.ord)))

kk <- hh[order(hh$ctry, hh$year, hh$analysis),]

g0 <- ggplot(data = kk, aes(x = year, y = dc, colour = analysis))
g1 <- g0 + geom_line() + facet_wrap(~ ctry, nrow = 8)
g2 <- g1 + theme.fiona + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + 
  ylab("Data collection score") + xlab("Year") +
  ggtitle("Data collection score") 
setwd(path.new.data)
ggsave("Data Collection Score.pdf",g2)

# Saving values for comparison
write.csv(hh.ord, file = paste("Data Collection Scores_",format(Sys.Date(),'%d%m%Y'),'.csv', sep=''))

# LE1 ratio
hh.ord <- ff.ord[,1:3]
hh.ord$LE1 <- ff.ord$LE1
hh.ord$old.LE1 <- NA
 for (i in 1:length(ctry.both))
  hh.ord$old.LE1[hh.ord$ctry == ctry.both[i] & is.element(hh.ord$year, years.gg)] <- 
  gg.ord$LE1[gg.ord$ctry == ctry.both[i]]

hh <-data.frame(rbind(hh.ord[, 1:3], hh.ord[, 1:3]))
hh$LE1 <- c(hh.ord$LE1, hh.ord$old.LE1)
hh$analysis <- as.factor(rep(c("current", "previous"), each = nrow(hh.ord)))

kk <- hh[order(hh$ctry, hh$year, hh$analysis), ]

g0 <- ggplot(data = kk, aes(x = year, y = LE1, colour = analysis))
g1 <- g0 + geom_line() + facet_wrap(~ ctry, nrow = 8)
g2 <- g1 + theme.fiona + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + 
  ylab("LE1") + xlab("Year") +
  ggtitle("LE1") 

setwd(path.new.data)
ggsave("LE1.pdf", g2)

write.csv(hh.ord, file = paste("LE1_", format(Sys.Date(), '%d%m%Y'), '.csv', sep = ''))


# Reporting Score

hh.ord <- ff.ord[, 1:3]
hh.ord$rep.sc <- ff.ord$rep.sc
hh.ord$old.rep.sc <- NA

for (i in 1:length(ctry.both))
  hh.ord$old.rep.sc[hh.ord$ctry == ctry.both[i] & is.element(hh.ord$year, years.gg)] <- 
  gg.ord$rep.sc[gg.ord$ctry == ctry.both[i]]

hh <-data.frame(rbind(hh.ord[, 1:3], hh.ord[, 1:3]))
hh$rep.sc <- c(hh.ord$rep.sc, hh.ord$old.rep.sc)
hh$analysis <- as.factor(rep(c("current", "previous"), each = nrow(hh.ord)))

kk <- hh[order(hh$ctry, hh$year, hh$analysis), ]

g0 <- ggplot(data = kk, aes(x = year, y = rep.sc, colour = analysis))
g1 <- g0 + geom_line() + facet_wrap(~ctry, nrow = 8)
g2 <- g1 + theme.fiona + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_x_continuous(breaks = c(2008, 2010, 2012, 2014, 2016)) + 
  ylab("RepSc") + xlab("Year") +
  ggtitle("RepSc") 

setwd(path.new.data)
ggsave("RepSc.pdf", g2)

write.csv(hh.ord, file = paste("RepSc_", format(Sys.Date(), '%d%m%Y'), '.csv',  sep = ''))

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #

