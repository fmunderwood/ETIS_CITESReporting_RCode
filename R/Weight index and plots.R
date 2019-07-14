# Provides plots for Weight Index for ETIS CoP and SC reports 
# Credible interval range is specified - usually 90% for COP reports
# WI values for each year are also saved with credible intervals
#_____________________________________________________________________________
# INPUTS
# (a) Simulated raw weights - Weights_raw_modnm.Rdata (from Simulating raw weights.R)
# (b) Simulated worked weights - Weights_wkd_modnm.Rdata (from Simulating wkd weights.R)
# (c) Seizures summary data - szs_use_Final.csv (from Select final data.R)
# (d) R packages:
#     XLConnect
#_____________________________________________________________________________
# OUTPUTS - model used is denoted by modnm
# (a) Figure 6: WIOverallmodnm.pdf
# (b) Figure 5: WIbarplotmodnm.pdf
# (c) Figure 5 with confidence interval: WIbarplot_CImodnm.pdf
# (d) xlsx files: WI_results_modnm_summaries.xlsx
#==============================================================================

# Determine credible intervals - 90% used here
int.lo <- 0.05
int.hi <- 0.95

#_____________________________________________________________________________

# Set labels for years
yr.use <- 2008:2017
num.yrs <- length(yr.use)
#_____________________________________________________________________________
# Set path name and get data

# Root directory
path.0 <- 'C:/ETIS/analysis'

# Sub directory where raw and worked seizures data are stored
path.data <- 'Processed Data'

# Directory where ouputs of weights index are stored
path.sims <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

# Sub Directory where final results are stored
path.out <- 'Model Outputs'
# 
#_____________________________________________________________________________
# 
# Files where raw and worked seizures data are stored
file.szs <- 'szs_use_Final.csv'

#_____________________________________________________________________________
# Set model name
mod.nm <- 'sz_Final_jags' # SET model name

# Note whether preliminary results or not
prelim <- F

# == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == == = 

setwd(path.0)

# Get data
setwd(paste(path.0, path.data, sep = '/'))
df.szs <- read.csv(file.szs, header = T)

# Get weights outputs
setwd(path.sims)
load(file = paste('wts_raw_', mod.nm, '.Rdata', sep = ''))
load(file = paste('wts_wkd_', mod.nm, '.Rdata', sep = ''))
y.sum <- list(y.sum.raw[[1]], y.sum.raw[[2]], y.sum.raw[[3]], y.sum.wkd[[1]], y.sum.wkd[[2]])

nyrs <- dim(y.sum.raw[[1]])[2]

# Directory where results are saved
dir.out <- paste(path.0, path.data, path.out, sep = '/')
setwd(dir.out)
#_____________________________________________________________________________
# Organise and calculate relevant data sumamries

x.use <- y.sum 

kk <- length(x.use)

x.yr.lo <- list(NULL)
x.yr.md <- list(NULL)
x.yr.hi <- list(NULL)

x.yr.sim <- list(NULL)
x.yr.mn <- list(NULL)


for (k in 1:kk){
  x.yr.sim[[k]] <- apply(x.use[[k]], c(2, 3), sum, na.rm = T)
  x.yr.lo[[k]] <- apply(x.yr.sim[[k]], 1, quantile, int.lo, na.rm = T)
  x.yr.md[[k]] <- apply(x.yr.sim[[k]], 1, quantile, 0.5, na.rm = T)
  x.yr.hi[[k]] <- apply(x.yr.sim[[k]], 1, quantile, int.hi, na.rm = T)
  x.yr.mn[[k]] <- apply(x.yr.sim[[k]], 1, mean, na.rm = T)
}

x.comb.yr <- x.yr.sim[[1]]
for (k in 2:kk){
  x.comb.yr <- x.comb.yr+x.yr.sim[[k]]
}

x.comb.yr.lo <- apply(x.comb.yr, 1, quantile, int.lo, na.rm = T)
x.comb.yr.md <- apply(x.comb.yr, 1, quantile, 0.5, na.rm = T)
x.comb.yr.hi <- apply(x.comb.yr, 1, quantile, int.hi, na.rm = T)

x.comb.yr.mn <- apply(x.comb.yr, 1, mean, na.rm = T)

yr.results <- list(x.yr.lo, x.yr.md, x.yr.hi)

comb.yr.results <- list(x.comb.yr.lo, x.comb.yr.mn, x.comb.yr.hi)

yr.mn.100 <- 100 * x.comb.yr.mn/x.comb.yr.mn[1]
yr.md.100 <- 100 * x.comb.yr.md/x.comb.yr.md[1]
yr.lo.100 <- 100 * x.comb.yr.lo/x.comb.yr.mn[1]
yr.hi.100 <- 100 * x.comb.yr.hi/x.comb.yr.mn[1]

#_____________________________________________________________________________
# Figure 6
# Global Weights index - standardised so that 1998 = 100
#_____________________________________________________________________________

#_______________________________________________________________
pdf(paste("WIOverall_", mod.nm, ".pdf"))
#jpeg(paste("WIOverall_", mod.nm, "_Final.jpg"), width = 600, height = 600)
#_______________________________________________________________

par(mfrow = c(1, 1), mar = c(2, 4, 2, 1))  
mx.use <- max(yr.hi.100)
plot(yr.mn.100 ~ yr.use, ylim = c(0, mx.use), type = 'n', main = '', 
     ylab = '', xaxt = 'n', yaxt = 'n')
if(prelim){
  text(median(yr.use), mx.use/2, "Preliminary", cex = 7, col = "lightgrey")
}
abline(h = 100, lty = 2, col = 'lightgrey')
abline(h = seq(from = 0, to = 1000, by = 100), col = 'lightgrey', lty = 3)
abline(v = yr.use, lty = 3, col = 'lightgrey')

points(yr.mn.100 ~ yr.use, pch = 19, )
segments(yr.use, yr.lo.100, yr.use, yr.hi.100)
axis(1, at = yr.use, yr.use)
axis(2, at = seq(from = 0, to = 400, by = 100), seq(from = 0, to = 400, by = 100), las = 1)
title(ylab = "Relative weight", cex.lab = 1.1)

#_______________________________________________________________
dev.off()
#_______________________________________________________________

#_____________________________________________________________________________
# Figure 5
# Barplot with proportion of total belonging to each ivory class
#_____________________________________________________________________________

mat.mn <- as.matrix(data.frame(x.yr.mn[[1]], x.yr.mn[[2]], x.yr.mn[[3]], x.yr.mn[[4]], x.yr.mn[[5]]))
mat.mn.scale <- sum(mat.mn[1, ])
mat.adj <- 100 * mat.mn/mat.mn.scale
wt.gp  <- c('Raw <10kg', 'Raw 10-100kg', 'Raw 100kg+', 'Worked <10kg', 'Worked 10kg+')

par(mfrow = c(1, 1), mar = c(2, 4, 1, 1))
dens <- c(-1, 25, -1, -1, 50, -1)
angl <- c(0, 45, 0, 0, 135, 0)
cols <- c('black', 'black', 'lightgrey', 'darkgrey', 'white')

#_______________________________________________________________
pdf(paste("WIbarplot_", mod.nm, ".pdf"))
#jpeg(paste("WIbarplot", mod.nm, "_Final.jpg", sep = ""), width = 600, height = 600)
#_______________________________________________________________

barplot(t(mat.adj), horiz = F, beside = F, col = cols, 
        main = '', ylab = 'Relative weight', 
        names.arg = yr.use, den = dens, angl = angl)#, yaxt = 'n', ylim = c(0, 300))
legend('topleft', legend = wt.gp[5:1], fill = cols[5:1], angle = angl[5:1], dens = dens[5:1], bty = 'n')
if(prelim){
  text(6,100, "Preliminary", col = "darkgrey", cex = 7 )
}
#_______________________________________________________________
dev.off()
#_______________________________________________________________

xx <- barplot(t(mat.adj[, 1:5]), horiz = F, beside = F, col = cols, 
              main = '', ylab = 'Relative weight', 
              names.arg = yr.use, den = dens, angl = angl)

legend('topleft', legend = wt.gp[5:1], fill = cols[5:1], angle = angl[5:1], dens = dens[5:1], bty = 'n')


#_______________________________________________________________
pdf(paste("WIBarplot_CI", mod.nm, ".pdf", sep = ""))
#jpeg(paste("WIBarplot_CI", mod.nm, "_Final.jpg", sep = ""), width = 600, height = 600)
#_______________________________________________________________

barplot(t(mat.adj[, 1:5]), horiz = F, beside = F, col = cols, 
        main = '', ylab = '', yaxt = "n", 
        names.arg = yr.use, den = dens, angl = angl, 
        ylim = c(0, 350))

legend('topleft', legend = wt.gp[5:1], fill = cols[5:1], angle = angl[5:1], dens = dens[5:1], bty = 'n')

segments(xx, yr.lo.100, xx, yr.hi.100)
points(xx, yr.mn.100, pch = 19)
axis(2, at = seq(from = 0, to = 400, by = 100), seq(from = 0, to = 400, by = 100), las = 1)
title(ylab = "Relative weight", cex.lab = 1.1)
#_______________________________________________________________
dev.off()
#_______________________________________________________________

# Write data to text files
results.lo <- matrix(nrow = num.yrs, ncol = 6, data = 0)
results.hi <- matrix(nrow = num.yrs, ncol = 6, data = 0)
results.mn <- matrix(nrow = num.yrs, ncol = 6, data = 0)

for (i in 1:5){
  results.lo[, i] <- x.yr.lo[[i]]
  results.hi[, i] <- x.yr.hi[[i]]
  results.mn[, i] <- x.yr.mn[[i]]
}

results.lo[, 6] <- x.comb.yr.lo
results.hi[, 6] <- x.comb.yr.hi
results.mn[, 6] <- x.comb.yr.mn

results.lo <- as.data.frame(results.lo)
results.hi <- as.data.frame(results.hi)
results.mn <- as.data.frame(results.mn)

names(results.lo) <- c('Raw:Small', 'Raw:Medium', 'Raw:Large', 'Worked:Small', 'Worked:MediumLarge', 'Total')
names(results.hi) <- c('Raw:Small', 'Raw:Medium', 'Raw:Large', 'Worked:Small', 'Worked:MediumLarge', 'Total')
names(results.mn) <- c('Raw:Small', 'Raw:Medium', 'Raw:Large', 'Worked:Small', 'Worked:MediumLarge', 'Total')
yr.names <- NULL
for (i in 1:num.yrs)
  yr.names <- c(yr.names, paste('Y', 2006+i, sep = ''))

rownames(results.lo) <- yr.names
rownames(results.hi) <- yr.names
rownames(results.mn) <- yr.names

results.lo.sc <- 100 * results.lo/results.mn[1, 6]
results.hi.sc <- 100 * results.hi/results.mn[1, 6]
results.mn.sc <- 100 * results.mn/results.mn[1, 6]

write.csv(round(results.lo.sc, 2), file = paste('WI_lo', mod.nm, '.csv', sep = ''))
write.csv(round(results.hi.sc, 2), file = paste('WI_hi', mod.nm, '.csv', sep = ''))
write.csv(round(results.mn.sc, 2), file = paste('WI_mn', mod.nm, '.csv', sep = ''))

# write to an excel workbook
library(XLConnect)

outfile <- paste('WI_results_', mod.nm, '_summaries.xlsx', sep = '')
if(file.exists(outfile)) file.remove(outfile)
wb <- loadWorkbook(outfile, create = T)

createSheet(wb, name = "Low")
low.df <- data.frame(Year = rownames(results.lo.sc), results.lo.sc)
writeWorksheet(wb, low.df, "Low")
createSheet(wb, name = "Mean")
mn.df <- data.frame(Year = rownames(results.mn.sc), results.mn.sc)
writeWorksheet(wb, mn.df, "Mean")
createSheet(wb, name = "High")
hi.df <- data.frame(Year = rownames(results.hi.sc), results.hi.sc)
writeWorksheet(wb, hi.df, "High")

saveWorkbook(wb)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
