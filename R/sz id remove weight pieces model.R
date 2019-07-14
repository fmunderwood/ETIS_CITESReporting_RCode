# Seizures not to be included when estimating weights from pieces
# See document "Changes to data.docx - for internal use only 
#====================================================================================
sz.rm <- c(
  109137, # weight needs to be changed but also doesn't have pieces anyway
  101114, # calculate worked from model and estimate rest as raw
  111934, # Number of pieces seems too small for weight
  23770,  # Need to check whether it should be worked rather than raw - given weight is so small
  28668, # Need to check whether it should be worked rather than raw - given weight is so small
  107677, # Weight is only given for one piece although quoted as if for 71
  31881 # Noted as raw and worked but only weights and pieces given for raw
)

# --------------------------------------------------------------------------------- #
# © 2019 University of Reading/TRAFFIC International/RW Burn & FM Underwood         #
# Please see the License.md file at                                                 #
# https://github.com/fmunderwood/ETIS_CITESReporting_RCode/blob/v.CoP18/License.md  #
# for more information about the copyright and license details of this file.        #
# --------------------------------------------------------------------------------- #
