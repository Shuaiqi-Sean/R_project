# INSTALL REQUIRED PACKAGES
pkgs <- c(
  'shiny',
  'rhandsontable',
  'shinydashboard',
  'shinyFiles',
  'DT',
  'data.table',
  'lubridate',
  'h2o',
  'here',
  'plotly',
  'stringr',
  'openxlsx',
  'shinyjs'
)

for (pkg in pkgs) install.packages(pkg)

# CREATE REQUIRED (EMPTY) DIRECTORIES
library(here)
dir.create(here('model'))
dir.create(here('model', 'designs'))
dir.create(here('model', 'h2o'))
dir.create(here('model', 'rdata'))
dir.create(here('data', 'datasets'))
dir.create(here('analysis'))
dir.create(here('analysis', 'rebase'))
dir.create(here('analysis', 'tables'))

# # COPY BIN FOLDERS FOR CERTAIN VARIABLES
# vars <- c('D51_SCR', 'V41_USDOT_SCR_GRPS', 'BIPD_SYM_GRP_0518')
# file_root <- '//scsas23p/SCA_O/CENTPROD/wumbo-bin'
# for (var in vars) {
#   copy_bin_path <- paste0(file_root, '/data/vars/', var, '/bin')
#   unlink(here('data', 'vars', var, 'bin'), recursive=T) # DELETE CURRENT BIN
#   file.copy(from_bin_path, here('data', 'vars', var), recursive=T) # COPY FILES
# }
