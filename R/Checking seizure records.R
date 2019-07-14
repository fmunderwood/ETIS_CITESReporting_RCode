# Compares two sets of seizure records - most recent with previous
#
# Here it compares those records used in the 2017 Standing Committee analysis 
# that used data from 2007 - 2016
# With the data to be used for the COP18 plus 2007: So from 2007 - 2018
#
# Graphs and tables compiled to show where there are differences
#
# These are to provide the ETIS team with opportunity for further data checking
# Differences do not mean that there are errors because new data can become available
# Or changes can be made to old records 
# Just provide an opportunity to check where there are differences
#___________________________________________________________________________
# INPUTS
# csv file of 
#    (1) Current sz in data: szs_ALL_ddmmyyyy.csv
#    (2) Past  sz in data: szs_ALL_ddmmyyyy.csv - different directory
#    (3) csv file of list of countries for recent analysis: CTS_ALL_ddmmyyyy.csv
# R file with ggplot theme: ggplot theme.R
# R packages: tidyverse
#___________________________________________________________________________
# OUTPUTS
# csv files:
#   (1) Differences between szs in  
#   (2) Current sz in - for common subset of data
#   (3) Past sz in - for common subset of data
# Graphs
#   Seizures in for both subsets of data
#=============================================================================

library(tidyverse)

path.code <- 'C:/ETIS/analysis/R Code'
setwd(path.code)
source('ggplot theme.R')

# path names
path.data.new <- 'C:/ETIS/analysis/Processed Data'
path.data.old <- 'C:/ETIS/analysis - 2017 Standing Committee tidy/Seizures model'

filename.new <- 'szs_ALL_17072018.csv'
filename.old <- 'szs_ALL_17082017.csv'
ctry.new <- 'CTS_ALL_17072018.csv'

# Loading new data
setwd(path.data.new)
ff <- read.csv(filename.new)
df.ctry <- read.csv(ctry.new)
ctry.use <- as.character(df.ctry$ctry)

# Loading old data
setwd(path.data.old)
gg <- read.csv(filename.old)

year.from.new <- min(ff$year)
year.to.new <- max(ff$year)

year.from.old <- min(gg$year)
year.to.old <- max(gg$year)

ff.sub <- ff %>%
  filter(is.element(ctry, levels(gg$ctry))) %>%
  filter(year >= year.from.new, year <= year.to.old) %>%
  arrange(year, ctry)
gg.sub <- gg %>%
  filter(is.element(ctry, levels(ff$ctry))) %>%
  filter(year >= year.from.new, year <= year.to.old) %>%
  arrange(year, ctry)


diff.ff <- ff.sub
diff.ff[,3:8] <- ff.sub[, 3:8] - gg.sub[, 3:8]

for (i in 1:6){
  ii <- diff.ff[,(i + 2)] != 0
  print(names(diff.ff)[i + 2])
  print(diff.ff[ii,c(1:2,(i + 2))])
  print(cbind(ff.sub[ii, c(1:2, (i + 2))],gg.sub[ii, c(1:2,(i + 2))]))
}  

diff.sum <- apply(diff.ff[, 3:8], 1, sum)
jj <- diff.sum != 0
diff.ff[jj, ]

diff.ff[, 9] <- diff.sum
kk <- diff.sum  < 0
diff.ff[kk, ]

# Save these records where there has been a decrease in number over time
setwd(path.data.new)
write.csv(diff.ff[kk,],file="Decrease in seizures.csv")

# Save these records where there has been a change in number over time
write.csv(diff.ff[jj,],file="Change in seizures.csv")


setwd(path.data.new)
write.csv(diff.ff, file = 'Differences.csv')
write.csv(ff.sub, file = "Currentsub.csv")
write.csv(gg.sub, file = "Previoussub.csv")


# Plot both years together
ff.use <- filter(ff, is.element(ctry, ctry.use))
gg.use <- filter(gg, is.element(ctry, ctry.use))

ctry.both <- unique(ctry.use[is.element(ctry.use, unique(gg.use$ctry))])

years.from <- min(ff.use$year)
years.to <- max(ff.use$year)

ff.use <- filter(ff.use, year >= years.from, year <= years.to)
gg.use <- filter(gg.use, year >= years.from, year <= years.to)

ff.ord <- ff.use
gg.ord <- gg.use

setwd(path.data.new)

years.gg <-sort(unique(gg.use$year))
lab.use <- c("Small Raw", "Medium Raw", "Large Raw", "Small Worked", "Medium Worked", "Large Worked")
for (j in 1:6){
  hh.ord <- ff.ord[,1:3]
  names(hh.ord)[3] <- "sz"
  hh.ord$sz <- ff.ord[,(j+2)]
  hh.ord$old.sz <- NA
  for (i in 1:length(ctry.both)){
    hh.ord$old.sz[hh.ord$ctry == ctry.both[i] & is.element(hh.ord$year, years.gg)] <- 
    gg.ord[gg.ord$ctry == ctry.both[i], (j + 2)]
  }
  hh <-data.frame(rbind(hh.ord[,1:3], hh.ord[,1:3]))
  hh$sz <- c(hh.ord$sz, hh.ord$old.sz)
  hh$analysis <- as.factor(rep(c("current", "previous"), each = nrow(hh.ord)))
  
  kk <- hh[order(hh$ctry, hh$year, hh$analysis), ]
  
  g0 <- ggplot(data = kk, aes(x = year,y = sz,colour = analysis))
  g1 <- g0 + geom_line() + facet_wrap(~ ctry, nrow = 8, scale = "free_y")
  g2 <- g1 + theme.fiona + 
    theme(axis.text.x = element_text(angle = 90)) + 
    scale_x_continuous(breaks = c(2006, 2008, 2010, 2012, 2014, 2016)) + 
    ylab("Number of seizures") + xlab("Year") +
    ggtitle(lab.use[j]) + 
    theme(legend.position = "bottom", 
          strip.text.x = element_text(size = 8),
          axis.text.x = element_text(size=6), 
          axis.text.y = element_text(size=6))
  filename <- paste('Seizures ',lab.use[j],'_freey.pdf',sep="")
  ggsave(filename,g2)
}

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #







