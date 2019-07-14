#  Functions to assist with getting subsidiary data from the ETIS database.
#   The functions are called in:
#   'covars setup.R' - to set up data for TI modelling
#   'covars setup_ALL_cluster.R' - to set up additional data for use incluster analysis
#
#  The dsn variable must be assigned in the calling script - covars setup.R
#
#  Function get.subsid.names() 
#    displays the ids and names of subsid variables currently in DB.
#
#  Function get.subsid.data(var.id, countries, year.from, year.to)
#    returns a dataframe with variables specified by numeric ids in
#    vector var.id; 
#     default for countries is all countries; 
#     default years are all years up to current
#=======================================================================================

load.pkgs <- function() {
  library(RPostgreSQL)
}
suppressPackageStartupMessages(load.pkgs())
#
drv <- dbDriver('PostgreSQL')
con <- dbConnect(drv, host = host.name, port = pg.port, user = user.name,
                 password = passwd, dbname = db.name)

get.subsid.names <- function() {
  nm <- 'subsidiary_datum_origins'
  if (!nm %in% dbListTables(con)) {
    stop('Table "public.subsidiary_datum_origins" not found in DB.')
  }
  SQLstr <- paste('SELECT * FROM public.', nm, sep = '')
  res <- dbGetQuery(con, SQLstr)
  df0 <- res
  return(df0)
}
#================================================================================================

get.subsid.data <- function(var.id, countries, year.from = 1900, year.to = 2100) {
  year.start <- 1989
  # get all country codes (ccodes)
  SQLstr.c <- 'SELECT id, code FROM public.countries ORDER BY code;'
  res <- dbGetQuery(con, SQLstr.c)
  df.ctry <- res
  df.ctry$code <- tolower(df.ctry$code)
  df.ctry[is.na(df.ctry$code), 2] <- "na" # put Namibia back in
  ccodes <- as.character(df.ctry[, 2]) # all codes as ch. vector
  # get var names (var.nms)
  SQL.nms <- 'SELECT id, name FROM public.subsidiary_datum_origins ORDER BY id'
  res <- dbGetQuery(con, SQL.nms)
  if (is.null(res)) {
    stop('No data in table "public.subsidiary_datum_origins".')
  }
  var.nms.df <- res
  id.max <- max(var.nms.df$id)
  var.seq <- 1:id.max
  var.nms <- rep(NA,id.max)
  var.nms[var.seq %in% var.nms.df$id] <- var.nms.df$name
  # countries x years (maximal)
  year.now <- as.numeric(format(Sys.Date(),'%Y'))
  yrs <- seq(year.start, year.now)
  df1 <- expand.grid(ctry = ccodes, year = yrs)
  # get subsid vars
  for(i in 1:length(var.id)) {
    SQLstr <- 'SELECT country_id, year, value
                FROM public.subsidiary_data
                WHERE subsidiary_datum_origin_id ='
    SQLstr <- paste(SQLstr, var.id[i], 'ORDER BY year, country_id;', sep=' ')
    df0 <- dbGetQuery(con, SQLstr)
    df1$X <- NA
    for (j in 1:dim(df0)[1]) {
      cc <- df.ctry$code[df.ctry$id == df0$country_id[j]]
      yy <- df0$year[j]
      df1$X[df1$ctry == cc & df1$year == yy] <- df0$value[j]
    }
    names(df1)[i + 2] <- var.nms[var.id[i]]
  }
  # select countries
  if (missing(countries)) # if no ctries specified ...
    # ... default to all ctries (excluding pseudo-ctries and old Serbia)
    countries <- ccodes[(substr(ccodes,1,1) != 'x') & (ccodes != 'cs')]
  df1 <- df1[df1$ctry %in% countries, ]
  df1$ctry <- as.factor(as.character(df1$ctry))
  # select years
  y0 <- max(min(df1$year), year.from)
  y1 <- min(year.now, year.to)
  df1 <- df1[(df1$year >= y0) & (df1$year <= y1), ]
  return(df1)
}

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
