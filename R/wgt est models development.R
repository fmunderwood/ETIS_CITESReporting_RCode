#  Estimates weights of seizures from no. of pieces using all ivory
#    seizures in the database where both weights and no. of pieces
#    are known.
#
#  This is a development file - it explores different models and how well they fit
#  And has some rough working and notes
#  It has been included for completeness
#______________________________________________________________________
#  INPUTS
#    csv file 'ivory sz recs 1900_2100.csv' (created by 
#    'sz data setup.R' with yr.from = 1900 and yr.to = 2100)
#
#  The first part of the program looks at basic models. 
#    From these some odd values are identified and checked with ETIS administrators
#    This leads to a list of seizures recorded in sz id remove weight pieces model.R
#    which are to be omitted from all future analyses. 
#
#  Change the values of lambda.r and lambda.w after inspecting the
#    profile likelihoods from the Box-Cox method.  Then refit the
#    models, with new polynomial orders as necessary. 
#    In addition new outliers are identified and removed for refitting models
#______________________________________________________________________
#  OUTPUTS
#     sz id remove weight pieces model.R
#     Raw wt from pieces.Rdata 
#     Worked wt from pieces.Rdata
#   These contain  outliers, lambda.r, lambda.w and polynomial terms to use
#
#  This information is used in the file wgt est models.R
#=======================================================================
#
# Set path name, in quotes, for working folder
#
path.wkg <- 'C:/ETIS/analysis/Processed Data'
path.code <- 'C:/ETIS/analysis/R Code'

#______________________________________________________________________

# NOTE WE ARE NOW ONLY USING DATA FROM 1996 onwards

library(MASS)
library(tidyverse)
library(mgcv)

setwd(path.wkg)
df.szrecs <- read.csv('ivory sz recs 1900_2100.csv', header=T)


df.szrecs <- df.szrecs %>%
  filter(sz.yr > 1995)

df.raw <- df.szrecs %>%
  filter(raw) %>%
  select(sz.id:raw.wgt)

names(df.raw)[4:5] <- c('pcs', 'wgt')

df.wkd <- df.szrecs %>%
  filter(wkd) %>%
  select(sz.id, sz.yr, disc.ct, wkd.pcs, wkd.wgt)

names(df.wkd)[4:5] <- c('pcs', 'wgt')

df.r <- df.raw %>%
  filter(!is.na(pcs) & !is.na(wgt)) %>%
  mutate(wt.pc = wgt/pcs)

df.r.est <- df.raw %>%
  filter(!is.na(pcs) & is.na(wgt))

df.w <- df.wkd %>%
  filter(!is.na(pcs) & !is.na(wgt)) %>%
  mutate(wt.pc = wgt/pcs)

df.w.est <- df.wkd %>%
  filter(!is.na(pcs) & is.na(wgt))

raw.name <- paste('RawWeightPcs_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
wkd.name <- paste('WkdWeightPcs_', format(Sys.Date(), '%d%m%Y'), '.csv', sep = '')
#write.csv(df.r, file = raw.name)
#write.csv(df.w, file = wkd.name)

# Some summaries
# Number of seizures for which need to estimate
# By number of pieces

r.num.to.model <- df.r %>% 
  group_by(pcs) %>%
  summarise(num = n(),
            mn.wgt = mean(wgt),
            mn.wgt.pcs = mean(wt.pc))

r.num.to.est <- df.r.est %>% 
  group_by(pcs) %>%
  summarise(num = n())

w.num.to.model <- df.w %>% 
  group_by(pcs) %>%
  summarise(num = n(),
            mn.wgt = mean(wgt),
            mn.wgt.pcs = mean(wt.pc))

w.num.to.est <- df.w.est %>% 
  group_by(pcs) %>%
  summarise(num = n())

p0.mod <- ggplot(data = r.num.to.model, aes(x = pcs, y = num))
p0.mod + geom_point()

p0.est <- ggplot(data = r.num.to.est, aes(x = pcs, y = num))
p0.est + geom_point()

p0 <- ggplot(data = r.num.to.model, aes(x = pcs, y = num))
p1 <- p0 + geom_point() 
p2 <- p1 + geom_point(data = r.num.to.est, aes(x = pcs, y = num), colour = "red")
p2

hist.r.est.pcs <- hist(df.r.est$pcs, 
                     breaks = c(0, 10, 20, 50, 100, 200, 300, 400, 500, 1000,
                                2000, 5000, 10000),
                     plot = F, right = F)

hist.w.est.pcs <- hist(df.w.est$pcs, 
                       breaks = c(0, 10, 20, 50, 100, 200, 300, 400, 500, 1000,
                                  2000, 5000, 10000),
                       plot = F, right = F)

hist.r.mod.pcs <- hist(df.r$pcs, 
                       breaks = c(0, 10, 20, 50, 100, 200, 300, 400, 500, 1000,
                                  2000, 5000, 10000, 20000, 50000),
                       plot = F, right = F)

hist.w.mod.pcs <- hist(df.w$pcs, 
                       breaks = c(0, 10, 20, 50, 100, 200, 300, 400, 500, 1000,
                                  2000, 5000, 10000, 20000, 50000),
                       plot = F, right = F)


r0 <- ggplot(data = df.r)
r0 + geom_point(aes(x = sz.yr, y = wt.pc))
r0 + geom_boxplot(aes(x = as.factor(sz.yr), y = wt.pc))

df.r.yr <- df.r %>%
  group_by(sz.yr) %>%
  summarise(mean.wt = mean(wt.pc),
            sd.wt = sd(wt.pc),
            q.lo = quantile(wt.pc, 0.025),
            q.hi = quantile(wt.pc, 0.975),
            nobs = n())


r.mod.yr.pcs <- df.r %>% 
  group_by(pcs,sz.yr) %>%
  summarise(nobs = n())

r1 <- ggplot(data = df.r.yr, aes(x = sz.yr, y = mean.wt))
r1 + geom_line()

df.r <- df.r %>%
  mutate(x = log(pcs + 1))#log(sz.r$pcs+1) #l

r2 <- ggplot(data = df.r, aes(x = x, y = wgt))
r2 + geom_point()


lm.r0 <- lm(wgt ~ poly(x, degree = 10), data = df.r)#, subset = sz.yr < 2018) # Can change number of dof

boxcox(lm.r0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx <- boxcox(lm.r0, lambda = seq(0.05,0.15, length=50), plotit = T)
xx$x[xx$y == max(xx$y)]

#box-cox suggests ...
# In this case suggests a value of 0.093  
lambda.r <- xx$x[xx$y == max(xx$y)] # 0.093             # <<- CHANGE AS APPROPRIATE
df.r$y <- (df.r$wgt) ^ lambda.r

lm.r <- lm(y ~ poly(x,10), data = df.r)  # <<- CHANGE AS APPROPRIATE
gm.r <- gam(y ~ s(x), data = df.r)
gm.r1 <- gam(y ~ poly(x, 1), data = df.r)
gm.r7 <- gam(y ~ poly(x, 7), data = df.r)
lm.r.12 <- lm(y ~ poly(x, 12), data = df.r)
lm.r.11 <- lm(y ~ poly(x, 11), data = df.r)
lm.r.10 <- lm(y ~ poly(x, 10), data = df.r)
lm.r.9 <- lm(y ~ poly(x, 9), data = df.r)
lm.r.8 <- lm(y ~ poly(x, 8), data = df.r)
lm.r.7 <- lm(y ~ poly(x, 7), data = df.r)
lm.r.6 <- lm(y ~ poly(x, 6), data = df.r)
lm.r.5 <- lm(y ~ poly(x, 5), data = df.r)
lm.r.4 <- lm(y ~ poly(x, 4), data = df.r)
lm.r.3 <- lm(y ~ poly(x, 3), data = df.r)
lm.r.2 <- lm(y ~ poly(x, 2), data = df.r)
lm.r.1 <- lm(y ~ poly(x, 1), data = df.r)

AIC(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r, lm.r.11, lm.r.12)
anova(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12,
      test = "Chisq")
anova(lm.r.4, lm.r, test = "Chisq")

AIC.lst <- AIC(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r, lm.r.11, lm.r.12)
plot(I(-1 * AIC) ~ I(1:12), data = AIC.lst, type = "l")

plot(lm.r.10)

#lm.r.use <- lm.r.10

# For raw ivory a number of points are identified as having large weight per piece 
# Or high influence or leverage
# Review with some points removed
# rownumbers & seizure id are given
# Specifically 2745 = 104210 and 3023 = 106609
# And other outliers in the plot:
# 3211 = 107856, 1641 = 30259, 
# And 3679 = 110352 is noted as large but not with specific influence or leverage
sz.r.odd <- c(104210, 106609, 107856, 30259, 110352)
filter(df.r, is.element(sz.id, sz.r.odd))

# Look at seizures with very small residuals as rather a lot of them
df.r$res <- residuals(lm.r.10)
df.r <- mutate(df.r, stand.res = (res - mean(res))/sd(res))
r.ext <- filter(df.r, stand.res < (-3)) %>%
  arrange(stand.res)
write.csv(r.ext, "Raw extreme points.csv")

#=======================================================================
# Now consider worked ivory
# use same default as previous time to start with
df.w.large <- filter(df.w, pcs >= 10000)
# Omit all seizures of more than 10000 pieces - only two of these and very different
# No worked seizures require weights estimated for more than 10000 pieces

df.w1 <- filter(df.w, pcs < 10000)
df.w1$x <- log(df.w1$pcs + 1)

lm.w0 <- lm(wgt ~ poly(x, degree = 10), data = df.w1)
boxcox(lm.w0, lambda=seq(0.0, 0.1, length = 50), plotit = T)
bb <- boxcox(lm.w0, lambda = seq(0.0, 0.1, length=100), plotit = T)
bb$x[bb$y == max(bb$y)]
#box-cox suggests ...
lambda.w <- 0.061 # Same value is suggested if use 4 dof instead
# 0.070 # 0.035                   # <<- CHANGE AS APPROPRIATE
df.w1$y <- (df.w1$wgt)^lambda.w

lm.w.lst <- pred.w.lst <- list(NULL)
AIC.w <- numeric()
for (i in 1:10){
  lm.w.lst[[i]] <- lm(y ~ poly(x,i), data = df.w1)
  AIC.w[i] <- AIC(lm.w.lst[[i]])
  pred.w.lst[[i]] <- predict(lm.w.lst[[i]])
}

plot(AIC.w ~ I(1:10), type = 'l')

# Suggests 7 degrees of freedom
plot(lm.w.lst[[5]])

# Following rownumbers & seizure id have influence
# 1732 = 101123 - but only one of two seizures with more than 5000 pieces
# 2045 = 104488 - but only one of two seizures with more than 5000 pieces
# No seizures in database with more than 5000 pieces (max is 1800) without a weight 
# so reasonable to exclude
# 2017 = 104454 - also a piece with large number of pieces but not much influence
# These fit badly at upper end
# 1058 = 28189 - 12 pieces weighing 875kg
# 2821 = 111934 - this is the seizure with the largest weight for a single piece of worked ivory
# 2823 = 111938 - not too bad
sz.w.odd <- c(101123, 104488, 104454, 28189, 111934, 111938)
filter(df.w1, is.element(sz.id, sz.w.odd))

# Some odd values at the top
df.w1$res <- residuals(lm.w.lst[[5]])
df.w1 <- mutate(df.w1, stand.res = (res - mean(res))/sd(res) )

w1.ext <- filter(df.w1, stand.res > 2.5) %>%
  arrange(desc(stand.res))

write.csv(w1.ext, "Working extreme points.csv")
#____________________________________________________________________________
# Stop at this point and inspect extreme points and discuss with ETIS Team
# After inspection a number of raw and worked seizures are to be omitted
# These were identified after inspecting data from raw and worked seizures
# Both small and large seizures

# Load up points to be removed
setwd(path.code)
source('sz id remove weight pieces model.R')
setwd(path.wkg)

df.r1 <- df.r %>%
  filter(!is.element(df.r$sz.id, sz.rm))

df.w2 <- df.w1 %>%
  filter(!is.element(df.w1$sz.id, sz.rm))


# Now others to remove in either raw or worked because of issues or strong influence

lm.r1.0 <- lm(wgt ~ poly(x, degree = 10), data = df.r1)#, subset = sz.yr < 2018) # Can change number of dof

boxcox(lm.r1.0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx <- boxcox(lm.r1.0, lambda = seq(0.05,0.15, length=50), plotit = T)
xx$x[xx$y == max(xx$y)]

#box-cox suggests ...
# In this case suggests a value of 0.0884  
lambda.r1 <- xx$x[xx$y == max(xx$y)] # 0.0884            # <<- CHANGE AS APPROPRIATE
df.r1$y <- (df.r1$wgt) ^ lambda.r1

lm.r.12 <- lm(y ~ poly(x, 12), data = df.r1)
lm.r.11 <- lm(y ~ poly(x, 11), data = df.r1)
lm.r.10 <- lm(y ~ poly(x, 10), data = df.r1)
lm.r.9 <- lm(y ~ poly(x, 9), data = df.r1)
lm.r.8 <- lm(y ~ poly(x, 8), data = df.r1)
lm.r.7 <- lm(y ~ poly(x, 7), data = df.r1)
lm.r.6 <- lm(y ~ poly(x, 6), data = df.r1)
lm.r.5 <- lm(y ~ poly(x, 5), data = df.r1)
lm.r.4 <- lm(y ~ poly(x, 4), data = df.r1)
lm.r.3 <- lm(y ~ poly(x, 3), data = df.r1)
lm.r.2 <- lm(y ~ poly(x, 2), data = df.r1)
lm.r.1 <- lm(y ~ poly(x, 1), data = df.r1)


AIC.lm <- AIC(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12)
anova(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12,
      test = "Chisq")
plot(AIC.lm$AIC ~ I(1:12), type = 'l')

plot(lm.r.10)

# Points with high influence (row number & seizure id)
# 2741 =  104210  high leverage - large weight and large residual - most pieces in raw seizures
# 3019 =  106609 low residuaal and some leverage - small weight per piece
# 3675  = 110352 reasonable residual and highish leverage - largest weight of all raw seizures
# Point with small residuals all have small weight per piece
# 1639 = 30259 
# 3019 = 106609 large number of pieces
# 3207 = 107856

# Note maximum number of pieces in seizures for which weights need estimating is currently 1800

# Consider removing points with high leverage or lots of pieces
# And those with very small residuals
# In particular 104210, 106609, 110352, 30259, 107856
  
raw.omit <- c(104210, 106609, 110352, 30259, 107856)

df.r2 <- filter(df.r1, !is.element(sz.id, raw.omit))

# Redo analysis
lm.r2.0 <- lm(wgt ~ poly(x, degree = 10), data = df.r2)#, subset = sz.yr < 2018) # Can change number of dof

boxcox(lm.r2.0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx <- boxcox(lm.r2.0, lambda = seq(0.05,0.15, length=50), plotit = T)
xx$x[xx$y == max(xx$y)]

#box-cox suggests ...
# In this case suggests a value of 0.0975  
lambda.r2 <- xx$x[xx$y == max(xx$y)] # 0.0975           # <<- CHANGE AS APPROPRIATE
df.r2$y <- (df.r2$wgt) ^ lambda.r2

lm.r.12 <- lm(y ~ poly(x, 12), data = df.r2)
lm.r.11 <- lm(y ~ poly(x, 11), data = df.r2)
lm.r.10 <- lm(y ~ poly(x, 10), data = df.r2)
lm.r.9 <- lm(y ~ poly(x, 9), data = df.r2)
lm.r.8 <- lm(y ~ poly(x, 8), data = df.r2)
lm.r.7 <- lm(y ~ poly(x, 7), data = df.r2)
lm.r.6 <- lm(y ~ poly(x, 6), data = df.r2)
lm.r.5 <- lm(y ~ poly(x, 5), data = df.r2)
lm.r.4 <- lm(y ~ poly(x, 4), data = df.r2)
lm.r.3 <- lm(y ~ poly(x, 3), data = df.r2)
lm.r.2 <- lm(y ~ poly(x, 2), data = df.r2)
lm.r.1 <- lm(y ~ poly(x, 1), data = df.r2)


AIC.lm <- AIC(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12)
anova(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12,
      test = "Chisq")
plot(AIC.lm$AIC ~ I(1:12), type = 'l')

plot(lm.r.10)
lm.r.use.2 <- lm.r.10

df.r.chk <- df.r2 %>%
  mutate(pcs.gp = as.numeric(cut(pcs, breaks = c(0,1.1, 2.1, 3.1, 4.1, 5.1, 10000))))
cols.gp <- c(1,2,4,5,6,"lightgrey")
qqnorm(residuals(lm.r.use.2), col = cols.gp[df.r.chk$pcs.gp], pch = 19)

# Look at values where would switch from small to medium class
pred.r2 <- predict(lm.r.use.2)^(1/lambda.r2)

idd.2 <- 1:nrow(df.r2)

pcs.2.2 <- first(idd.2[df.r2$pcs==2])
pcs.3.2 <- first(idd.2[df.r2$pcs==3])
pcs.4.2 <- first(idd.2[df.r2$pcs==4])

pred.r2[pcs.3.2]
pred.r2[pcs.4.2]

pcs.35.2 <- first(idd.2[df.r2$pcs==35])
pcs.36.2 <- first(idd.2[df.r2$pcs==36])
pcs.37.2 <- first(idd.2[df.r2$pcs==37])

pred.r2[pcs.35.2]
pred.r2[pcs.36.2]
pred.r2[pcs.37.2]


plot(pred.r2 ~ df.r2$pcs)
points(df.r2$wgt ~ df.r2$pcs, col = 2, pch = 19, cex = 0.5)
points(pred.r2 ~ df.r2$pcs)

# Lets compare models with same points excluded
lm.lst.2 <- list(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6,
                 lm.r.7, lm.r.8, lm.r.9, lm.r.10, lm.r.11, lm.r.12)
pred.lst.2 <- list(NULL)
wt.3 <- wt.4 <- numeric()
for (i in 1:12){
  pred.lst.2[[i]] <- predict(lm.lst.2[[i]])^(1/lambda.r2)
  wt.3[i] <- pred.lst.2[[i]][pcs.3.2]
  wt.4[i] <- pred.lst.2[[i]][pcs.4.2]
}  

par(mfrow = c(3, 4), mar = c(2, 2, 2, 2))
for (i in 1:12){
  plot(df.r2$wgt ~ df.r2$pcs, col = 2, pch = 19, cex = 0.5, 
       main = paste(i,  round(wt.3[i],2), sep = ": "))
  points(pred.lst.2[[i]] ~ df.r2$pcs, pch = 19)
}  

par(mfrow = c(1,1), mar = c(2, 2, 2, 2))
plot(df.r2$wgt ~ df.r2$pcs, col = "lightgrey", pch = 19, cex = 0.5, xlim = c(0,10), ylim = c(0,100))
     for (i in 1:12){
       points(pred.lst.2[[i]] ~ df.r2$pcs, pch = 19, cex = 0.75, col = i)
}
  
plot(df.r2$wgt ~ df.r2$pcs, col = "lightgrey", pch = 19, cex = 0.5, xlim = c(0,10), ylim = c(0,100))
points(pred.lst.2[[10]] ~ df.r2$pcs, pch = 19, cex = 0.75, col = 1)


# Now only consider up to 10 pieces
df.r.10 <- filter(df.r2, pcs < 11)

lm.r10.0 <- lm(wgt ~ poly(x, degree = 8), data = df.r.10)#, subset = sz.yr < 2018) # Can change number of dof

boxcox(lm.r10.0, lambda = seq(0.05, 0.15, length = 50), plotit = T)
xx <- boxcox(lm.r10.0, lambda = seq(0.05,0.15, length=50), plotit = T)
xx$x[xx$y == max(xx$y)]

#box-cox suggests ...
# In this case suggests a value of 0.101
lambda.r.10 <- xx$x[xx$y == max(xx$y)] # 0.101            # <<- CHANGE AS APPROPRIATE
df.r.10$y <- (df.r.10$wgt) ^ lambda.r.10

lm.r.8 <- lm(y ~ poly(x, 8), data = df.r.10)
lm.r.7 <- lm(y ~ poly(x, 7), data = df.r.10)
lm.r.6 <- lm(y ~ poly(x, 6), data = df.r.10)
lm.r.5 <- lm(y ~ poly(x, 5), data = df.r.10)
lm.r.4 <- lm(y ~ poly(x, 4), data = df.r.10)
lm.r.3 <- lm(y ~ poly(x, 3), data = df.r.10)
lm.r.2 <- lm(y ~ poly(x, 2), data = df.r.10)
lm.r.1 <- lm(y ~ poly(x, 1), data = df.r.10)

AIC.lm <- AIC(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8)
anova(lm.r.1, lm.r.2, lm.r.3, lm.r.4, lm.r.5, lm.r.6, lm.r.7, lm.r.8,
      test = "Chisq")
plot(AIC.lm$AIC ~ I(1:8), type = 'l')

# Suggests needs 5 degrees of freedom
plot(lm.r.5)
lm.r.use.10 <- lm.r.5

# Look at values where would switch from small to medium class
pred.r.10 <- predict(lm.r.use.10)^(1/lambda.r.10)
#pred.r.10.2 <- predict(lm.r.2)^(1/lambda.r2)

idd.10 <- 1:nrow(df.r.10)

pcs.2.10 <- first(idd.10[df.r.10$pcs==2])
pcs.3.10 <- first(idd.10[df.r.10$pcs==3])
pcs.4.10 <- first(idd.10[df.r.10$pcs==4])

pred.r.10[pcs.3.10]
pred.r.10[pcs.4.10]

wt.sum.r10 <- df.r.10 %>%
  group_by(pcs) %>%
  summarise(mean.wgt = mean(wgt),
            med.wgt = median(wgt))

plot(pred.r.10 ~ df.r.10$pcs, pch = 19, col = "darkgrey", cex = 2)
points(pred.r2[df.r2$pcs<11] ~ df.r2$pcs[df.r2$pcs<11], col = 2, pch = 19)
#points(pred.r.10.1 ~ df.r.10$pcs, col = 4, pch = 19)
points(mean.wgt ~ pcs, col = 6, data = wt.sum.r10)
points(med.wgt ~ pcs, col = 6, pch = "X", data = wt.sum.r10)
abline(h = 10, lty = 2, col = "hotpink")

df.r2 <- df.r2 %>%
  mutate(y = wgt ^ lambda.r2) %>%
  mutate(x2 = x^2) %>%
  mutate(x3 = x^3) %>%
  mutate(x4 = x^4) %>%
  mutate(x5 = x^5) %>%
  mutate(x6 = x^6) %>%
  mutate(x7 = x^7) %>%
  mutate(x8 = x^8) %>%
  mutate(x9 = x^9) %>%
  mutate(x10 = x^10) 
 
 mod.r <- lm(y ~ x +  x2 + x3 + x4 + x5 + 
               x6 + x7 + x8 + x9 + x10, data = df.r2)
 
 pred.r.est <- df.r.est %>%
   mutate(x = log(pcs + 1)) %>%
   mutate(x2 = x^2) %>%
   mutate(x3 = x^3) %>%
   mutate(x4 = x^4) %>%
   mutate(x5 = x^5) %>%
   mutate(x6 = x^6) %>%
   mutate(x7 = x^7) %>%
   mutate(x8 = x^8) %>%
   mutate(x9 = x^9) %>%
   mutate(x10 = x^10) 
 
 pred.r.est <- pred.r.est %>%
   mutate(pred = (predict(mod.r, newdata = pred.r.est))^(1/lambda.r2))
 
 filter(pred.r.est, pcs > 1000)
 
 pred.r.est.orig <- df.r.est %>%
   mutate(x = log(pcs + 1)) 
   
 pred.r.orig <- (predict(lm.r.use.2, newdata = pred.r.est.orig))^(1/lambda.r2)
 
plot(pred.r.orig ~ pred.r.est$pred)
plot(I(pred.r.orig^lambda.r2) ~ I(pred.r.est$pred^lambda.r2))

# So these predictions work using this form of writing out the model
# So can use without worrying

# Some checks to see how well works when using half data as a training set only

n.r2 <-  nrow(df.r2)
r2.use <- df.r2 %>%
 mutate(x2 = x^2,
        x3 = x^3,
        x4 = x^4,
        x5 = x^5,
        x6 = x^6,
        x7 = x^7,
        x8 = x^8,
        x9 = x^9,
        x10 = x^10)
        
B <- 1000
check.res <- check.orig <-check.new <- matrix(nrow = n.r2, ncol = B, data = NA)

for (i in 1:B){
  xx <- sample(1:n.r2, size = n.r2/2, replace = F)
  r.mod <- r2.use[xx,]
  r.tst <- r2.use[-xx,]
  mod.use <- lm(y ~ x + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10, data = r.mod)
  pred.use <- predict(mod.use, newdata = r.tst)
  check.res[-xx,i] <- pred.use^(1/lambda.r2)
  check.orig[-xx,i] <- pred.use
  check.new[-xx,i] <- rnorm(mean = pred.use, sd = summary(mod.use)$sigma, n = n.r2/2)
  #  check.res[xx,i] <- w2.use$y[xx]^(1/lambda.w)
}


check.mn <- apply(check.res, 1, mean, na.rm = T)
check.md <- apply(check.res, 1, median, na.rm = T)
check.lo <- apply(check.res, 1, quantile, 0.05, na.rm = T)
check.hi <- apply(check.res, 1, quantile, 0.95, na.rm = T)

check.orig.mn <- apply(check.orig, 1, mean, na.rm = T)
check.orig.md <- apply(check.orig, 1, median, na.rm = T)
check.orig.lo <- apply(check.orig, 1, quantile, 0.05, na.rm = T)
check.orig.hi <- apply(check.orig, 1, quantile, 0.95, na.rm = T)

check.pred.mn <- apply(check.new, 1, mean, na.rm = T)
check.pred.md <- apply(check.new, 1, median, na.rm = T)
check.pred.lo <- apply(check.new, 1, quantile, 0.25, na.rm = T)
check.pred.hi <- apply(check.new, 1, quantile, 0.75, na.rm = T)


plot(check.md, check.md)#, ylim = c(0,200))
points(r2.use$wgt ~ check.md, pch = 19, cex = 0.5)
segments(x0 = check.md, y0 = check.lo, y1 = check.hi, col = 2)
#points(check.md ~ check.mn, col = 2, cex = 0.95)
abline(h = c(10, 100), col = 4, lty = 2)

plot(check.md, check.md, ylim = c(0,200), xlim = c(0,200))
points(r2.use$wgt ~ check.md, pch = 19, cex = 0.5)
segments(x0 = check.md, y0 = check.lo, y1 = check.hi, col = 2)
#points(check.md ~ check.mn, col = 2, cex = 0.95)
abline(h = c(10, 100), col = 4, lty = 2)

plot(check.pred.md, check.pred.md, ylim = c(0.5, 3), xlim = c(1, 2.5))
segments(x0 = check.pred.md, y0 = check.pred.lo, y1 = check.pred.hi, col = 2)
points(r2.use$y ~ check.pred.md, pch = 19, cex = 0.5)

# Calculate proportion of values that fall outside the 95% interval
r2.use$lo <- check.pred.lo
r2.use$hi <- check.pred.hi
pred.r.lo <- r2.use$y < r2.use$lo
pred.r.hi <- r2.use$y > r2.use$hi
mean(pred.r.lo)
mean(pred.r.hi)

# This is not bad - 24% are above or below 50% interval. In otherwords 48% are outside 50% interval
#
# I know it isn't a Bayesian analysis but we would expect similar results from a Bayesian appropach
# So it is pretty reasonable model to go with I think
# 

# Next check is what transformation value of lambda do we get if we use a different
# polynomial to start with?
lam <- numeric(12)
for (i in 1:12){
  lm.r2.test <- lm(wgt ~ poly(x, degree = i), data = df.r2)#, subset = sz.yr < 2018) # Can change number of dof
  
  xx <- boxcox(lm.r2.test, lambda = seq(0.05,0.15, length=50), plotit = T)
  lam[i] <- xx$x[xx$y == max(xx$y)]
}

plot(lam~I(1:12))
abline(h = lambda.r2)

# So not much difference so reasonable to keep as is

# Save final information about raw model
raw.save <- list(call = lm.r.use.2$call, lambda = lambda.r2, omit = raw.omit)
save(raw.save, file = "Raw wt from pieces.Rdata")


#=======================================================================
# Now consider worked ivory

lm.w0 <- lm(wgt ~ poly(x, degree = 10), data = df.w2)
boxcox(lm.w0, lambda=seq(0.0, 0.1, length = 50), plotit = T)
bb <- boxcox(lm.w0, lambda = seq(0.0, 0.1, length=100), plotit = T)
bb$x[bb$y == max(bb$y)]
#box-cox suggests ...
lambda.w <- bb$x[bb$y == max(bb$y)] #0.061 # 
df.w2$y <- (df.w2$wgt)^lambda.w

lm.w.lst <- pred.w.lst <- list(NULL)
AIC.w <- numeric()
for (i in 1:10){
  lm.w.lst[[i]] <- lm(y ~ poly(x,i), data = df.w2)
  AIC.w[i] <- AIC(lm.w.lst[[i]])
  pred.w.lst[[i]] <- predict(lm.w.lst[[i]])
}

plot(AIC.w~ I(1:10), type = 'l')

# Suggests 5 degrees of freedom
plot(lm.w.lst[[5]])

# Following rownumbers have influence
# 1731 = 101123 - but only one of two seizures with more than 5000 pieces
# 2044 = 104488 - but only one of two seizures with more than 5000 pieces
# 2016 = 104454 - also a piece with large number of pieces but not really a problem
# These fit badly at upper end
# 1058 = 28189 - 12 pieces weighing 875kg - largest and biggest residual
# Also listed as not fitting well but they aren't too bad given the totla number
# 2820 = 111938
# 2527 = 108439

df.w2[c(1731, 2044, 2016, 1058, 2826, 2820, 2527),]
df.w2$res <-  residuals(lm.w.lst[[5]])
df.w2 <- df.w2 %>%
  mutate(stand.res = (res - mean(res))/sd(res))
       
filter(df.w2, stand.res > 2.5) %>%
  arrange(desc(stand.res))

# Weights to omit
w2.omit <- c(28189, 101123, 104488)
df.w3 <- filter(df.w2,!is.element(sz.id, w2.omit))
df.w2.omit <- filter(df.w2,is.element(sz.id, w2.omit))

# Try again
lm.w0 <- lm(wgt ~ poly(x, degree = 10), data = df.w3)
boxcox(lm.w0, lambda=seq(0.0, 0.1, length = 50), plotit = T)
bb <- boxcox(lm.w0, lambda = seq(0.0, 0.1, length=100), plotit = T)
bb$x[bb$y == max(bb$y)]
#box-cox suggests ...
lambda.w <- bb$x[bb$y == max(bb$y)] #0.068 # Same value is suggested if use 4 dof instead
# 0.070 # 0.035                   # <<- CHANGE AS APPROPRIATE
df.w3$y <- (df.w3$wgt)^lambda.w

lm.w.lst <- pred.w.lst <- list(NULL)
AIC.w <- numeric()
for (i in 1:10){
  lm.w.lst[[i]] <- lm(y ~ poly(x,i), data = df.w3)
  AIC.w[i] <- AIC(lm.w.lst[[i]])
  pred.w.lst[[i]] <- predict(lm.w.lst[[i]])
}

par(mfrow = c(1, 1))
plot(AIC.w ~ I(1:10), type = 'l')

# Suggests 5 degrees of freedom
par(mfrow=c(1,1))
plot(lm.w.lst[[5]])

lm.w.use <- lm(y ~ poly(x,5), data = df.w3)


par(mfrow=c(3, 4), mar = c(2, 2, 2, 2))
for (i in 1:10){
  plot(wgt ~ pcs, data = df.w3, pch = 19, cex = 0.5, main = i)
  points((pred.w.lst[[i]]^(1/lambda.w)) ~ df.w3$pcs, col = 2, pch = 19, cex = 0.75)
}

par(mfrow=c(3, 4), mar = c(2, 2, 2, 2))
for (i in 1:10){
  plot(y ~ x, data = df.w3, pch = 19, cex = 0.5, main = i)
  points(pred.w.lst[[i]] ~ df.w3$x, col = 2, pch = 19, cex = 0.75)
}

# This all looks quite reasonable
pred.w <- pred.w.lst[[5]]^(1/lambda.w)
par(mfrow=c(1,1))
plot(pred.w ~ df.w3$pcs)
abline(h = c(10, 100))

# What happens if want to predict beyond scope of data
# Need to investigate this next


# Now can do checking by looking at what happens if only use subsets of the data etc
# Should also do this for raw ivory

n.w3 <-  nrow(df.w3)
w3.use <- df.w3 %>%
  mutate(x2 = x^2,
         x3 = x^3,
         x4 = x^4,
         x5 = x^5)
B <- 1000
check.res <- check.new <- matrix(nrow = n.w3, ncol = B, data = NA)

for (i in 1:B){
  xx <- sample(1:n.w3, size = n.w3/2, replace = F)
  w.mod <- w3.use[xx,]
  w.tst <- w3.use[-xx,]
  mod.use <- lm(y ~ x + x2 + x3 + x4 + x5, data = w.mod)
  pred.use <- predict(mod.use, newdata = w.tst)
  check.res[-xx,i] <- (predict(mod.use, newdata = w.tst))^(1/lambda.w)
  check.new[-xx,i] <- (rnorm(mean = pred.use, sd = summary(mod.use)$sigma, n = n.w3/2))
}

check.mn <- apply(check.res, 1, mean, na.rm = T)
check.md <- apply(check.res, 1, median, na.rm = T)
check.lo <- apply(check.res, 1, quantile, 0.05, na.rm = T)
check.hi <- apply(check.res, 1, quantile, 0.95, na.rm = T)

check.pred.mn <- apply(check.new, 1, mean, na.rm = T)
check.pred.md <- apply(check.new, 1, median, na.rm = T)
check.pred.lo <- apply(check.new, 1, quantile, 0.25, na.rm = T)
check.pred.hi <- apply(check.new, 1, quantile, 0.75, na.rm = T)

plot(check.md, check.md)#, ylim = c(0,200))
points(w3.use$wgt ~ check.md, pch = 19, cex = 0.5)
segments(x0 = check.md, y0 = check.lo, y1 = check.hi, col = 2)
abline(h = c(10, 100), col = 4, lty = 2)

plot(check.pred.md, check.pred.md, ylim = c(0.5, 2))
segments(x0 = check.pred.md, y0 = check.pred.lo, y1 = check.pred.hi, col = 2)
points(w3.use$y ~ check.pred.md, pch = 19, cex = 0.5)

# Calculate proportion of values that fall outside confidence interval
w3.use <- w3.use %>%
  mutate(lo = check.pred.lo,
         hi = check.pred.hi,
         below = y < check.pred.lo,
         above = y > check.pred.hi)
mean(w3.use$below)
mean(w3.use$above)

# Again just under 50% fall above or below the 50% interval
# Suggests that model fits reasonably well

# Check lambda if use different dof for starting model
lam <- numeric(12)
for (i in 1:12){
  lm.w3.test <- lm(wgt ~ poly(x, degree = i), data = df.w3)
  
  xx <- boxcox(lm.w3.test, lambda = seq(0,0.1, length = 100), plotit = T)
  lam[i] <- xx$x[xx$y == max(xx$y)]
}

plot(lam~I(1:12), ylim = c(0.06, 0.07))
abline(h = lambda.w)

# Now check how estimated weights for seizures with over 5,000 pieces works
# Given that have omitted the only two seizures with at least 5,000 pieces and a weight
# from the analysis

df.w.est$x <- log(df.w.est$pcs + 1)
df.w.est$pred <- predict(lm.w.use, newdata = df.w.est)
df.w.est$pred.wgt <- df.w.est$pred^(1/lambda.w)

filter(df.w.est, pcs > 5000)

filter(df.w, pcs > 5000) %>%
  arrange(pcs)

# Save worked information

wkd.save <- list(call = lm.w.use$call, lambda = lambda.w, omit = w2.omit)
save(wkd.save, file = "Worked wt from pieces.Rdata")

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
