# Provides plots of the Transactions Index for the ETIS COP/SC reports 
# Includes options to make these preliminary for model inspections of alternative models
# Includes options to use mean or median for mid-point of plots
# Credible interval range is specified - usually 90% for COP reports
# Also TI values for each country in each year are also saved with credible intervals
#_____________________________________________________________________________
# INPUTS
#  (a) Lambda's: - lambda_modnm.Rdata (from Posterior distributions for lambda theta phi.R)
#  (b) Seizures data: - szs_use_Final.csv (from Select final data.R) 
#  (c) R file to cope with South Sudan: df_ctry_add_fn.R
#  (d) R Packages:
#       XLConnect
#       tidyverse
#_____________________________________________________________________________
# OUTPUTS - model used is denoted by modnm.
# Figures can be saved as pdfs or jpgs.
#  (a) Figure 4: TI_Overall_modnm.pdf
#  (b) Figure 2: TI_Group_modnm.pdf
#  (c) Figure 3: TI_Bar_modnm.pdf
#  (d) Figure 3 Alternate: TI_Bar_One_modnm.pdf
#
# When absolutly finalised csv and xlsx files are produced 
#  (e) TI_results_summaries_Final.xlsx
#  (f) TI_mn_ctry_modnm_cl_lo_Final.csv
#  (g) TI_mn_ctry_modnm_cl_hi_Final.csv
#============================================================================

# Determine credible intervals - 90% used here
int.lo <- 0.05
int.hi <- 0.95

#_____________________________________________________________________________

# Set labels for years
yr.use <- 2008:2017
yr.base <- min(yr.use) 
num.yrs <- length(yr.use)
#_____________________________________________________________________________

# Set path name
path.code <- 'C:/ETIS/analysis/R Code'
path.dat <- 'C:/ETIS/analysis/Processed Data'
path.dat.sims <- 'C:/ETIS/2018 JAGS MCMCoutputs/Processed Outputs'
path.out <- 'C:/ETIS/analysis/Processed Data/Model Outputs'

#_____________________________________________________________________________
# Set model name

mod.nm <- 'sz_Final_jags' # SET model name

#_____________________________________________________________________________
# Specify file names for seizures data

file.szs <- 'szs_use_Final.csv'

# Are these draft plots so need "preliminary" on them
prelim <- F
#==============================================================================

library(tidyverse)
library(XLConnect)

#_____________________________________________________________________________

# get data

setwd(path.dat)
df.szs<- read.csv(file.szs, header=T)

# Deal with partial data from South Sudan
setwd(path.code)
source('df_ctry_add_fn.R')

# Adjust for South Sudan with missing years
df.szs.orig <- df.szs
df.szs <- df.ctry.add.fn(df.szs.orig)
df.szs$sz.index <- 100*df.szs$year + as.numeric(df.szs$ctry)

# Ensure relevant years are selected
df.szs <- df.szs[df.szs$year>(yr.base-1),]

setwd(path.dat.sims)
load(file=paste('lambda_', mod.nm, '.Rdata', sep=''))

#___________________________________________________________
# Prepare data for suumarising

nyrs <- dim(lambda[[1]])[2]

x.use <- lambda ; indicator <- 'TI'

kk <- length(x.use)

x.yr.lo <- list(NULL)
x.yr.md <- list(NULL)
x.yr.hi <- list(NULL)

x.ctry.lo <- list(NULL)
x.ctry.md <- list(NULL)
x.ctry.hi <- list(NULL)
x.ctry.sim <- list(NULL)

x.yr.sim <- list(NULL)

x.ctry.yr.lo <- list(NULL)
x.ctry.yr.md <- list(NULL)
x.ctry.yr.hi <- list(NULL)

x.yr.mn <- list(NULL)
x.ctry.mn <- list(NULL)
x.ctry.yr.mn <- list(NULL)


#___________________________________________________________________________

for (k in 1:kk){
# Year summaries
  x.yr.sim[[k]] <- apply(x.use[[k]],c(2,3),sum,na.rm=T)
  x.yr.lo[[k]] <- apply(x.yr.sim[[k]],1,quantile,int.lo,na.rm=T)
  x.yr.md[[k]] <- apply(x.yr.sim[[k]],1,quantile,0.5,na.rm=T)
  x.yr.hi[[k]] <- apply(x.yr.sim[[k]],1,quantile,int.hi,na.rm=T)
  
# Country summaries
  x.ctry.sim[[k]] <- apply(x.use[[k]],c(1,3),sum,na.rm=T)
  x.ctry.lo[[k]] <- apply(x.ctry.sim[[k]],1,quantile,int.lo,na.rm=T)
  x.ctry.md[[k]] <- apply(x.ctry.sim[[k]],1,quantile,0.5,na.rm=T)
  x.ctry.hi[[k]] <- apply(x.ctry.sim[[k]],1,quantile,int.hi,na.rm=T)
  
# Country x Year summaries  
  x.ctry.yr.lo[[k]] <- apply(x.use[[k]],c(1,2),quantile,int.lo,na.rm=T)  
  x.ctry.yr.md[[k]] <- apply(x.use[[k]],c(1,2),quantile,0.5,na.rm=T)
  x.ctry.yr.hi[[k]] <- apply(x.use[[k]],c(1,2),quantile,int.hi,na.rm=T)

  x.yr.mn[[k]] <- apply(x.yr.sim[[k]],1,mean,na.rm=T)
  x.ctry.mn[[k]] <- apply(x.ctry.sim[[k]],1,mean,na.rm=T)
  x.ctry.yr.mn[[k]] <- apply(x.use[[k]],c(1,2),mean,na.rm=T)
  
}

x.comb.yr <- x.yr.sim[[1]]
x.comb.ctry <- x.ctry.sim[[1]]
x.comb <- x.use[[1]]
for (k in 2:kk){
  x.comb.yr <- x.comb.yr+x.yr.sim[[k]]
  x.comb.ctry <- x.comb.ctry+x.ctry.sim[[k]]
  x.comb <- x.comb+x.use[[k]]
}

x.comb.yr.lo <- apply(x.comb.yr,1,quantile,int.lo,na.rm=T)
x.comb.yr.md <- apply(x.comb.yr,1,quantile,0.5,na.rm=T)
x.comb.yr.hi <- apply(x.comb.yr,1,quantile,int.hi,na.rm=T)

x.comb.ctry.lo <- apply(x.comb.ctry,1,quantile,int.lo,na.rm=T)
x.comb.ctry.md <- apply(x.comb.ctry,1,quantile,0.5,na.rm=T)
x.comb.ctry.hi <- apply(x.comb.ctry,1,quantile,int.hi,na.rm=T)

x.comb.ctry.yr.lo <- apply(x.comb,c(1,2),quantile,int.lo,na.rm=T)
x.comb.ctry.yr.md <- apply(x.comb,c(1,2),quantile,0.5,na.rm=T)
x.comb.ctry.yr.hi <- apply(x.comb,c(1,2),quantile,int.hi,na.rm=T)

x.comb.yr.mn <- apply(x.comb.yr,1,mean,na.rm=T)
x.comb.ctry.mn <- apply(x.comb.ctry,1,mean,na.rm=T)
x.comb.ctry.yr.mn <- apply(x.comb,c(1,2),mean,na.rm=T)


yr.results <- list(x.yr.lo,x.yr.md,x.yr.hi,x.yr.mn)
ctry.results <- list(x.ctry.lo,x.ctry.md,x.ctry.hi,x.ctry.mn)
ctry.yr.results <- list(x.ctry.yr.lo,x.ctry.yr.md,x.ctry.yr.hi,x.ctry.yr.mn)

comb.yr.results <- list(x.comb.yr.lo,x.comb.yr.md,x.comb.yr.hi,x.comb.yr.mn)
comb.ctry.results <- list(x.comb.ctry.lo,x.comb.ctry.md,x.comb.ctry.hi,x.comb.ctry.mn)
comb.ctry.yr.results <- list(x.comb.ctry.yr.lo,x.comb.ctry.yr.md,x.comb.ctry.yr.hi,x.comb.ctry.yr.mn)

for (i in 1:5){
  rownames(x.ctry.yr.mn[[i]]) <- levels(df.szs$ctry)
  colnames(x.ctry.yr.mn[[i]]) <- yr.use
}

#_____________________________________________________________________
# Preparation for plotting

n.yr <- length(yr.use)
wt.gp  <- c('Raw <10kg','Raw 10-100kg', 'Raw 100kg+', 'Worked <10kg','Worked 10kg+')
kk <- length(wt.gp)

# Graphs
setwd(path.out)
#______________________________________________________________________________
# Figure 4
# Overall TI over time 
#______________________________________________________________________________

#______________________________________________________________________________
# Write to pdf
pdf(paste("TIOverall_",mod.nm,".pdf",sep=""))
#jpeg(paste("TIOverall_",mod.nm,".jpeg",sep=""),width=900,height=600)
#jpeg(paste("TIOverall_",mod.nm,"_Final.jpeg",sep=""),width=600,height=600)
#______________________________________________________________________________

# Scaling first for plots over time
# Use overall value for 1998 as scaling factor
md.mn <- 4 # mean=4 and md =2
y.use <- comb.yr.results
y.scale <- 100/y.use[[md.mn]][yr.use==yr.base]

par(mfrow=c(1,1),mar=c(2,4,2,1))  
mx.use <- max(y.use[[3]]*y.scale)
plot(I(y.use[[md.mn]]*y.scale)~yr.use,ylim=c(0,mx.use),type='n',main='',
     xaxt='n',yaxt='n',ylab='')
if(prelim){
  text(median(yr.use), 2*mx.use/3, "Preliminary", cex = 7, 
     col = "lightgrey", srt = 0)
}
abline(h=100,lty=2,col='lightgrey')
abline(h=seq(from=50,to=450,by=50),col='lightgrey',lty=3)
abline(v=yr.use,lty=3,col='lightgrey')

points(I(y.use[[md.mn]]*y.scale)~yr.use,pch=19,)
segments(yr.use,I(y.use[[1]]*y.scale),yr.use,I(y.use[[3]]*y.scale))
axis(1,at=yr.use,yr.use)
axis(2,at=seq(from=0,to=250,by=50),seq(from=0,to=250,by=50),las=1)
title(ylab="Relative number of transactions",cex.lab=1.2)


#______________________________________________________________________________
dev.off()
#______________________________________________________________________________

#______________________________________________________________________________

# Figure 2
# Plot for each group over time - use same scaling value for each group
# Segment plot 
#______________________________________________________________________________


#______________________________________________________________________________
# Write to pdf or other
pdf(paste("TI_Group_",mod.nm,".pdf",sep=""))
#jpeg(paste("TI_Group_",mod.nm,".jpeg",sep=""),width=900,height=600)
#jpeg(paste("TI_Group_",mod.nm,"_Final.jpeg",sep=""),width=600,height=600)

#______________________________________________________________________________

md.mn <- 4 # mean=4 and md =2

y.use <- yr.results
y.lo <- y.use[[1]]
y.md <- y.use[[md.mn]]
y.hi <- y.use[[3]]
cols <- rep(1,6)

par(mfcol=c(3,2),mar=c(2,4,2.5,2))  
for (k in 1:kk){
  if(k<4){
    par(mar=c(2,5,2.5,2))
  }
  else {
    par(mar=c(2,2,2.5,5))
  }
  y.scale <- 100/y.md[[k]][yr.use==yr.base]
  mx.use <- 650#max(y.scale*y.hi[[k]])#600
  plot(I(y.scale*y.md[[k]])~yr.use,pch=19,main=paste(wt.gp[k]),cex=1,#0.75,
       ylab='',xaxt='n',type='n',ylim=c(0,mx.use),col=cols[k],yaxt="n")
  if(prelim){
    text(median(yr.use), 2*mx.use/3, "Preliminary", cex = 5, 
       col = "lightgrey", srt = 0)
  }  
  abline(v=yr.use,lty=3,col='lightgrey')
  abline(h=100,lty=2,col='lightgrey')
  if(k < 6) {
    abline(h=seq(from=50,to=1000,by=50),col='lightgrey',lty=3)
  } else {
    abline(h=seq(from=500,to=10000,by=500),col='lightgrey',lty=3)
  }
  points(I(y.scale*y.md[[k]])~yr.use,pch=19,col=cols[k])
  segments(yr.use,y.scale*y.lo[[k]],yr.use,y.scale*y.hi[[k]],col=cols[k])
# axis(1,at=yr.use,yr.use,cex.axis=0.8)
  axis(1,at=yr.use,yr.use,cex.axis=1.1)
  axis(2,at=seq(from=0,to=700,by=100),seq(from=0,to=700,by=100),cex.axis=1.1,las=1)
  
  
}
mtext('Relative number of transactions',side=2,outer=TRUE,line=-2)

#______________________________________________________________________________
dev.off()
#______________________________________________________________________________

# Figure 3

# Stacked bar charts with legend - scaled so that total over all ivory classes in 1998 = 100
#______________________________________________________________________________

#______________________________________________________________________________
# Write to pdf
pdf(paste("TI_Bar_",mod.nm,".pdf",sep=""))
#jpeg(paste("TI_Bar_",mod.nm,".jpeg",sep=""))
#jpeg(paste("TI_Bar_",mod.nm,"_Final.jpeg",sep=""),width=600,height=600)

#______________________________________________________________________________

yr.results.mat <- matrix(unlist(x.yr.mn),nrow=nyrs,ncol=kk)
adj <- sum(yr.results.mat[1,])
yr.results.mat.adj <- 100*yr.results.mat/adj

par(mfrow=c(2,1),mar=c(2,5,2,2))
mx.use <- max(as.numeric(yr.results.mat[,1:3]))

cols <- c('black','darkgrey','white')
barplot(t(yr.results.mat.adj[,1:3]),horiz=F,beside=F,col=cols,
        main='Raw ivory weight groups',ylab='',
        names.arg=yr.use,legend.text=wt.gp[1:3],
        args.legend=list(x='topleft',bty='n',cex=0.8),yaxt='n',ylim=c(0,150))
if(prelim){
  text(5, 75, "Preliminary", cex = 5, col = "lightgrey", srt = 0)
}  
axis(2,at=seq(from=0,to=150,by=25),seq(from=0,to=150,by=25),las=1,cex.axis=0.8)

barplot(t(yr.results.mat.adj[,4:5]),horiz=F,beside=F,col=cols,
        main='Worked ivory weight groups',ylab='',
        names.arg=yr.use,cex.axis=0.8,legend.text=wt.gp[4:5],
        args.legend=list(x='topleft',bty='n',cex=0.8),yaxt='n',ylim=c(0,150))
if(prelim){
  text(5, 75, "Preliminary", cex = 5, col = "lightgrey", srt = 0)
}
axis(2,at=seq(from=0,to=250,by=25),seq(from=0,to=250,by=25),las=1,cex.axis=0.8)
mtext('Relative number of transactions',side=2,outer=TRUE,line=-2)

#______________________________________________________________________________
dev.off()
#______________________________________________________________________________

# Repeat of above but all in one chart

#______________________________________________________________________________
# Write to pdf
pdf(paste("TI_BarOne_",mod.nm,".pdf",sep=""))
#jpeg(paste("TI_BarOne_",mod.nm,"_Final.jpeg",sep=""),width=600,height=600)

#______________________________________________________________________________

par(mfrow=c(1,1),mar=c(2,4,1,1))
dens <- c(-1,25,-1,-1,-1)
angl <- c(0,45,0,0,0)
cols <- c('black','black','lightgrey','darkgrey','white')
barplot(t(yr.results.mat.adj[,1:5]),horiz=F,beside=F,
        col = cols,den=dens,angl=angl,
        main='',ylab='',
        names.arg=yr.use,legend.text=wt.gp[1:5],
        args.legend=list(x='topleft',bty='n',cex=0.8),yaxt='n')
if(prelim){
  text(6, 75, "Preliminary", cex = 7, col = "lightgrey", srt = 0)
}
axis(2,at=seq(from=0,to=400,by=50),seq(from=0,to=400,by=50),las=1,cex.axis=0.8)
mtext('Relative number of transactions',side=2,outer=TRUE,line=-1.5)

#______________________________________________________________________________
dev.off()
#______________________________________________________________________________
ncat <- length(x.use)

# When have final model

# Calculate proportions
yr.results.tot <- apply(yr.results.mat.adj,1,sum)
mat.tot <- matrix(nrow=nrow(yr.results.mat.adj),ncol=ncat,data=rep(yr.results.tot,each=ncat),byrow=T)
prop.yr <- (100*yr.results.mat.adj/mat.tot)

# Write data to text files
results.mn <- results.hi <-results.lo <- matrix(nrow=num.yrs,ncol=ncat+1,data=0)

for (i in 1:ncat){
  results.lo[,i] <- x.yr.lo[[i]]
  results.hi[,i] <- x.yr.hi[[i]]
  results.mn[,i] <- x.yr.mn[[i]]
}

results.lo[,(ncat+1)] <- x.comb.yr.lo
results.hi[,(ncat+1)] <- x.comb.yr.hi
results.mn[,(ncat+1)] <- x.comb.yr.mn

results.lo <- as.data.frame(results.lo)
results.hi <- as.data.frame(results.hi)
results.mn <- as.data.frame(results.mn)

nms <- c('Raw:Small','Raw:Medium','Raw:Large','Worked:Small','Worked:MediumLarge','Global')
names(results.lo) <- nms
names(results.hi) <- nms
names(results.mn) <- nms

yr.names <- NULL
for (i in 1:num.yrs)
  yr.names <- c(yr.names,paste('Y',yr.base-1+i,sep=''))

rownames(results.lo) <- yr.names
rownames(results.hi) <- yr.names
rownames(results.mn) <- yr.names

results.lo.sc <- 100*results.lo/results.mn[1,(ncat+1)]
results.hi.sc <- 100*results.hi/results.mn[1,(ncat+1)]
results.mn.sc <- 100*results.mn/results.mn[1,(ncat+1)]

#write.csv(round(results.lo.sc,2),file=paste('TI_lo_',mod.nm,'_Final.csv',sep=""))
#write.csv(round(results.hi.sc,2),file=paste('TI_hi_',mod.nm,'_Final.csv',sep=""))
#write.csv(round(results.mn.sc,2),file=paste('TI_mn_',mod.nm,'_Final.csv',sep=""))

res.ctry.yr.mn <- list(NULL)
nms.rw <- rep(c('Raw','Worked'),each=3)
nms.cl <- c('Small','Medium','Large','Small','MediumLarge')
for (i in 1:(ncat)){
  res.ctry.yr.mn[[i]] <- 100*x.ctry.yr.mn[[i]]/results.mn[1,(ncat+1)]
  rownames(res.ctry.yr.mn[[i]]) <- levels(df.szs$ctry)
  write.csv(round(res.ctry.yr.mn[[i]],3),file=paste('TI_mn_ctry_',mod.nm,'_',nms.rw[i],'_',nms.cl[i],'.csv',sep=''))
}


# Write to one file
res.ctry.yr.mn <- list(NULL)
nms.rw <- rep(c('Raw','Worked'),each=3)
nms.cl <- c('Small','Medium','Large','Small','MediumLarge')

outfile <- paste(path.out, '/', 'TI_results_',mod.nm,'_summaries_Final.xlsx', sep='')
if(file.exists(outfile)) file.remove(outfile)
wb <- loadWorkbook(outfile, create=T)
for (i in 1:(ncat)){
  name.sheet <- paste(nms.rw[i],'_', nms.cl[i], sep='')
  createSheet(wb, name=name.sheet)
  res.ctry.yr.mn[[i]] <- 100*x.ctry.yr.mn[[i]]/results.mn[1,(ncat+1)]
  res.df <- data.frame(ctry= levels(df.szs$ctry),res.ctry.yr.mn[[i]] )
  writeWorksheet(wb,res.df,name.sheet)
}
createSheet(wb, name="Low")
low.df <- data.frame(Year = rownames(results.lo.sc),results.lo.sc)
writeWorksheet(wb,low.df,"Low")
createSheet(wb, name="Mean")
mn.df <- data.frame(Year = rownames(results.mn.sc),results.mn.sc)
writeWorksheet(wb,mn.df,"Mean")
createSheet(wb, name="High")
hi.df <- data.frame(Year = rownames(results.hi.sc),results.hi.sc)
writeWorksheet(wb,hi.df,"High")

saveWorkbook(wb)

# Save lo and hi values as well
res.ctry.yr.lo <- res.ctry.yr.hi <- res.ctry.yr.mn <- list(NULL)
nms.rw <- rep(c('Raw','Worked'),each=3)
nms.cl <- c('Small','Medium','Large','Small','MediumLarge')
for (i in 1:(ncat)){
  res.ctry.yr.mn[[i]] <- 100*x.ctry.yr.mn[[i]]/results.mn[1,(ncat+1)]
  res.ctry.yr.lo[[i]] <- 100*x.ctry.yr.lo[[i]]/results.mn[1,(ncat+1)]
  res.ctry.yr.hi[[i]] <- 100*x.ctry.yr.hi[[i]]/results.mn[1,(ncat+1)]
  rownames(res.ctry.yr.mn[[i]]) <- levels(df.szs$ctry)
#  write.csv(round(res.ctry.yr.mn[[i]],3),file=paste('TI_mn_ctry_',mod.nm,'_',nms.rw[i],'_',nms.cl[i],'_Final.csv',sep=''))
  write.csv(round(res.ctry.yr.lo[[i]],3),file=paste('TI_mn_ctry_',mod.nm,'_',nms.rw[i],'_',nms.cl[i],'_lo_Final.csv',sep=''))
  write.csv(round(res.ctry.yr.hi[[i]],3),file=paste('TI_mn_ctry_',mod.nm,'_',nms.rw[i],'_',nms.cl[i],'_hi_Final.csv',sep=''))
}

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
