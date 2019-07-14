# Cluster analysis for COP reports
# Uses only the last three years of data
#
# Produces dendrograms, PCAs and sensitivity outputs and writes to files
#
# The cluster analysis uses seizures and weights in and out 
# above and below a cutoff. 
# In the analysis for the CoP report a cut-off of 500kg was used. 
# Originally a value of 1 Tonne was used (as at the last CoP) 
# but there were only a  few seizures above this value so it was changed to 500kg
# This is why there is sometimes the suffix  '1T' in file or covariate names
#
# The sensitivity analysis uses 
# posterior predictive distributions of bias adjusted seizures & weights in and out
# and the lambdas - Transactions Index for each ivory class
# The sensitivity analysis does a cluster analysis using values from each draw from the the posterior predictive distribution
# And notes which countries appear in the same group
# The proportion of times each country appears with every other country is then calculated
#
# For the sensitivity analysis the number of groups must be specified
# For CoP18 report this was 15. This is decided in discussion with ETIS Team
# Different cut-offs can be explored.
#________________________________________________________________________________
# INPUTS
# (a) Files ontaining the prepared variables for the cluster analysis:
#  Root name is ClusterVars_modnm_dest_*_size_*.Rdata.pdf (from Creating variables for cluster analysis sims Final.R)
#  And these are required when dest = T or F and when there is no size part or size is the size cutoff of 500kg  
# (b) R packages:
#  cluster 
#  abind
#  XLConnect
#  gdata
#  tidyverse
#  RColorBrewer
#  dendsort
#________________________________________________________________________________
# OUTPUTS
# Based on the model name that specifies the size cut-off and numbers of groups
# (a) modnm.pdf - Dendrogram and sensitivity analysis results
# (b) modnm_Original data.csv - data used to produce the cluster analysis
# (b) modnm.xlsx - summary of variables for each group in the cluster analysis
# (c) modnm_biplot - Biplot from PCA analysis
# (d) modnm_Reorder - Final dendrogram for use in COP report
# ================================================================================

# Set working directories

path.data <- 'C:/ETIS/analysis/Processed Data'
path.jags.data <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'

#___________________________________________________________

# Model name
mod.nm <- 'sz_Final'

# Size for cutoff
size.cut <- 500

#=================================================

library(cluster)
library(abind)
library(XLConnect)
library(gdata)
library(tidyverse)
library(RColorBrewer)
library(dendsort)
setwd(path.jags.data)

#___________________________________________________________
# Country of destination included
ct_dest <- TRUE

# _______________________________________________
# Identify data file
file.use <- paste('ClusterVars_',mod.nm,'_dest_',ct_dest,'size_',size.cut,'Final.Rdata',sep='')

# _______________________________________________
# Read in data
df.clvars.t <- readRDS(file.use)
num.sims.t <- dim(df.clvars.t)[3]
ctry <- as.character(unlist(dimnames(df.clvars.t)[1]))
nctry <- length(ctry)

#___________________________________________________________
# Country of destination excluded

ct_dest <- FALSE

# _______________________________________________
# Identify data file

file.use <- paste('ClusterVars_',mod.nm,'_dest_',ct_dest,'size_',size.cut,'Final.Rdata',sep='')

# _______________________________________________
# Read in data
df.clvars.f <- readRDS(file.use)
num.sims.f <- dim(df.clvars.f)[3]
ctry <- as.character(unlist(dimnames(df.clvars.f)[1]))
nctry <- length(ctry)

# _______________________________________________
# Cluster analysis:
# Create the specific set of variables
# Use weight in and weight out from TRUE
# Divide into two categories: less than and at least the size category (500kg)
# Use seizures out from FALSE
# Divide into two categories: less than and at least the size category (500kg)
# The lambdas remain as they are

dimnames(df.clvars.t)[2]
dimnames(df.clvars.f)[2]

#[[1]]
#[1] "sz.in"     "sz.out"    "wt.in"     "wt.out"    "sz.in.1T"  "sz.out.1T" "wt.in.1T" 
#[8] "wt.out.1T" "lambda.1"  "lambda.2"  "lambda.3"  "lambda.4"  "lambda.5" 


# For dest = FALSE (no seizure opportunity and LE ratio definition of seizures out)
# Require the seizures out variable - for less than and more than size category (500kg)
# Less than 500kg sz.out is calculated as sz.out - sz.out.1T
df.clvars.0.f <- df.clvars.f[,c(2,6),] 
dimnames(df.clvars.0.f)[2][[1]] <- c('sz.out.less1T', 'sz.out.more1T')
df.clvars.0.f[,1,] <- df.clvars.f[,2,] - df.clvars.f[,6,]

# For dest = TRUE (seizure opportunity and Trade Flow definition of seizures out)
# Requires the weights in and out - for less than and more than size category (500kg)
# Less than 500kg wt.out is calculated as wt.out - wt.out.1T
df.clvars.0.t <- df.clvars.t[,c(3,4,7,8),] 
dimnames(df.clvars.0.t)[2][[1]] <- c('wt.in.less1T', 'wt.out.less1T', 'wt.in.more1T','wt.out.more1T')
df.clvars.0.t[,1,] <- df.clvars.t[,3,] - df.clvars.t[,7,]
df.clvars.0.t[,2,] <- df.clvars.t[,4,] - df.clvars.t[,8,]

df.clvars.0.lambda <- df.clvars.t[,9:13,]

df.clvars.0 <- abind(df.clvars.0.f, df.clvars.0.t, df.clvars.0.lambda, along = 2)


#Simulations
num.sims.use <- dim(df.clvars.0)[3]

gps.df <- data.frame(ctry = ctry)
ngp <- 15 # Specify after discussion with TRAFFIC
df.clus.1 <- log(df.clvars.0 + 1)
for (i in 1:num.sims.use){
  clus.1 <- agnes(df.clus.1[,,i], stand=T, method='ward')
  gps.df[,(i + 1)] <- cutree(clus.1, k = ngp)
} 

mat.ctry.test <- matrix(nrow = nctry, ncol = nctry, data = 0,
                        dimnames = list(ctry, ctry))
id <- 1:nctry
for (j in 1:num.sims.use){
  for (i in 1:ngp){
        id.1 <- id[gps.df[,(j + 1)] == i]
        len.1 <- length(id.1)
        mat.ctry.test[cbind(rep(id.1, len.1),rep(id.1, each = len.1))] <- 
          1 + mat.ctry.test[cbind(rep(id.1, len.1),rep(id.1, each = len.1))]
  }
}

mat.ctry.1 <- mat.ctry.test/num.sims.use

# Do cluster analysis on mean values
df.mn <- apply(df.clvars.0,c(1,2), mean)
df.mn.1 <- log(df.mn + 1)

clus.1.mn <- agnes(df.mn.1, stand=T, method='ward')
gps.mn.df <- cutree(clus.1.mn, k = ngp)
ord.1 <- clus.1.mn$order
mat.ctry.ord <- mat.ctry.1[ord.1, ord.1]

#====================================================================
file.name <- paste('Cluster1T_DendSens_',mod.nm,'_','size_',size.cut,'_',ngp,'Groups', sep = '')
#====================================================================
write.csv(df.mn, file = paste(file.name, 'Original Data.csv', sep = ''))
#====================================================================

pdf(paste(file.name, '.pdf', sep = ''))

#====================================================================
par(mar = c(3,4,0,4))
cut.off <- 5.2 # 
layout(matrix(nrow = 5, ncol = 1, data = c(1,1,1,2,2)))
par(mar = c(3,2,0,2))
plot(as.dendrogram(clus.1.mn))
abline(h = cut.off, col = 2, lty = 2)   
par(mar = c(3,4,0,4))
image(mat.ctry.ord, col = gray(0:9/9)[10:1], xaxt = 'n', yaxt = 'n')
seq.labs <- seq(from = 0, to = 1, length = 66)
seq.labs.1 <- seq.labs[seq(from = 1, by = 2, length = 33)]
seq.labs.2 <- seq.labs[seq(from = 2, by = 2, length = 33)]
seq.gp <- seq(from = 0, length = 12, by = 6)
axis(1, at = seq.labs,lab = clus.1.mn$order.lab, las = 2 )
axis(3, at = seq.labs,lab = clus.1.mn$order.lab, las = 2 )
abline(v = seq.labs[seq.gp], col = 'palevioletred', lty = 2)
abline(h = seq.labs[seq.gp], col = 'palevioletred', lty = 2)
axis(2, at = seq.labs.1,lab = clus.1.mn$order.lab[seq(from = 1, by = 2, length = 33)], las = 1 , cex = 0.5)
axis(4, at = seq.labs.2,lab = clus.1.mn$order.lab[seq(from = 2, by = 2, length = 33)], las = 1 , cex = 0.5)

#====================================================================
 dev.off()
#====================================================================

df.mn.1.clus <- cbind(df.mn.1, gps.mn.df)

# Create categories to understand data better
df.1.clus <- as.data.frame(df.mn.1.clus)
names(df.1.clus) <- colnames(df.mn.1.clus)
names(df.1.clus)[ncol(df.1.clus)] <- 'Group'

clus.1.gp.mn <- df.1.clus %>%
  group_by(Group) %>%
  summarise(
    sz.out.less1T = mean(sz.out.less1T),
    sz.out.more1T = mean(sz.out.more1T),
    wt.in.less1T = mean(wt.in.less1T),
    wt.out.less1T = mean(wt.out.less1T),
    wt.in.more1T = mean(wt.in.more1T),
    wt.out.more1T = mean(wt.out.more1T),
    lambda.1 = mean(lambda.1),
    lambda.2 = mean(lambda.2),
    lambda.3 = mean(lambda.3),
    lambda.4 = mean(lambda.4),
    lambda.5 = mean(lambda.5),
  )

clus.1.gp.sd <- df.1.clus %>%
  group_by(Group) %>%
  summarise(
    sz.out.less1T = sd(sz.out.less1T),
    sz.out.more1T = sd(sz.out.more1T),
    wt.in.less1T = sd(wt.in.less1T),
    wt.out.less1T = sd(wt.out.less1T),
    wt.in.more1T = sd(wt.in.more1T),
    wt.out.more1T = sd(wt.out.more1T),
    lambda.1 = sd(lambda.1),
    lambda.2 = sd(lambda.2),
    lambda.3 = sd(lambda.3),
    lambda.4 = sd(lambda.4),
    lambda.5 = sd(lambda.5),
  )


# Write out summaries to an Excel Book
outfile <- paste(file.name,'.xlsx', sep = '')

wb <- loadWorkbook(outfile, create=T)
createSheet(wb, name = 'MeanVars')
writeWorksheet(wb, data.frame(ctry = rownames(df.1.clus), df.1.clus), 'MeanVars')
createSheet(wb, name = 'Matrix')
writeWorksheet(wb, mat.ctry.ord, 'Matrix')
createSheet(wb, name = 'Group Mean')
writeWorksheet(wb, clus.1.gp.mn, 'Group Mean')
createSheet(wb, name = 'Group SD')
writeWorksheet(wb, clus.1.gp.sd, 'Group SD')
saveWorkbook(wb)

#___________________________________________________________________________
# Produce dendrogram with reordered groups

#====================================================================
# pdf(paste(file.name, '_Reorder.pdf', sep = ''))
#====================================================================
par(mfrow=c(1,1), cex = 0.7)
plot(dendsort(as.dendrogram(clus.1.mn)))
abline(h = cut.off, col = 2, lty = 2)   

#====================================================================
# dev.off()
#====================================================================


# _______________________________________________
# PCA and biplots
par(mfrow=c(1,1))
pca.1 <- princomp(df.mn.1, cor=T)
#============================================================
# pdf(paste(file.name, '_Biplot.pdf', sep = ''))
# jpeg(paste(file.name, '_Biplot.jpg', sep = ''))
#============================================================

biplot(pca.1, cex=0.75)

#============================================================
# dev.off()
#============================================================


# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #







