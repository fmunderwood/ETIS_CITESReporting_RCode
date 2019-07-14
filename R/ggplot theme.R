# Fiona's theme used for plotting
#___________________________________________________________________________________
# INPUTS
#  R package: ggplot2
#====================================================================================

library(ggplot2)

# Set my theme
theme.fiona <- theme_grey() + 
  theme(axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        panel.background=element_rect(fill="white"),
        panel.grid.major=element_line(colour="lightgrey",linetype=3),
        panel.grid.minor=element_line(colour="white"),
        panel.border=element_rect(colour="darkgrey",fill=NA),
        strip.background=element_rect(colour="darkgrey") 
  )

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
