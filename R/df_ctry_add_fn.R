# Function to account for countries which did not exist for the whole time period
# Used when requiring posterior distributions from 
# the Transaction Index models of seizures data

# Sets number of seizures and covariate values to be NA
# Sets polynomial terms for year to be same as for other countries

# Originally written for coping with SS 

#==================================================================================

df.ctry.add.fn <- function(df.use, ctry.add = "ss", ind = F){
  
  year.from <- min(df.use$year)
  year.to <- max(df.use$year)
  
  n.ctry <- nlevels(df.use$ctry)
  ctry.lab <- levels(df.use$ctry)
  ctry.index <- 1:length(ctry.lab)
  
  add.ind <- ctry.index[ctry.lab == ctry.add]
  add.min.yr <- min(filter(df.use, ctry == ctry.add)$year)
  add.diff.yr <- add.min.yr - year.from
  
  
  df.use.sort <- arrange(df.use, ctry, year)
  df.use.add.beg <- filter(df.use.sort, is.element(ctry, ctry.lab[1:(add.ind - 1)]))
  df.use.add.end <- filter(df.use.sort, is.element(ctry, ctry.lab[add.ind:length(ctry.lab)]))
  
  ctry.add.df <- data.frame(df.use.sort[1:add.diff.yr,])
  ctry.add.df$ctry <- ctry.add
  ctry.add.df$year <- year.from:(add.min.yr - 1)
  ctry.add.df[, -(1:2)] <- NA
  
  df.use.add.sort <- rbind(df.use.add.beg, ctry.add.df, df.use.add.end)
  
  df.use.add <- arrange(df.use.add.sort, year, ctry)
  if (ind == FALSE){
    return(df.use.add)
  } else {
    return(list(ind = add.ind, diff.yr = add.diff.yr))
  }  
}  

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
