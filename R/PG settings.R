# Settings to link R with ETIS database
#____________________________________________________________________________________
#
# PostgreSQL installation details:
# To find the correct settings, log into pgAdmin and connect to the
# server where the ETIS database is. Click on the server name and locate
# Hostname, Port and Username in the Properties window ...
#===================================================================================== 
host.name <- 'localhost'
pg.port <- 5432
user.name <- 'postgres'
# 
# If a password is required, enter it between the quote marks here:
passwd <- 'XXXX'
#
# ... then set the database name:
db.name <- 'XXXX'

#____________________________________________________________________________________________

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
