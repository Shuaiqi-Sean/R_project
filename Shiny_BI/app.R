library(rhandsontable) # 0.3.7
library(shiny) # 1.3.2
library(shinydashboard) # 0.7.1
library(shinyFiles) # 0.7.3
library(DT) # 0.7
library(data.table) # 1.12.2
library(lubridate) # 1.7.4
library(h2o) # 3.24.0.5
library(here) # 0.1
library(plotly) # 4.9.0
library(stringr) # 1.4.0
library(openxlsx) # 4.1.0

options(scipen=999)

# LIBRARY PATHS
base_lib_path <<- 'data/base'
var_lib_path <<- 'data/vars'
data_lib_path <<- 'data/datasets'
mdl_lib_path <<- 'model'
analysis_lib_path <<- 'analysis'

# THEME
box_color <<- rgb(30, 115, 233, m=255)
nav_color <<- rgb(10, 51, 96, m=255)
color <<- 'primary'

# MODULES
source(here('modules', 'data_tab', 'data_tab_module.R'))
source(here('modules', 'model_tab', 'model_tab_module.R'))
source(here('modules', 'analysis_tab', 'analysis_tab_module.R'))
source(here('modules', 'pivot_tab', 'pivot_tab_module.R'))
source(here('modules', 'segmentation_tab', 'segmentation_tab_module.R'))
source(here('modules', 'docs_tab', 'docs_tab_module.R'))
source(here('modules', 'classes', 'model_class.R'))
source(here('modules', 'classes', 'rebase_rule_class.R'))
source(here('modules', 'classes', 'table_design_class.R'))
source(here('modules', 'classes', 'factor_tables_class.R'))
source(here('modules', 'classes', 'data_builder_class.R'))
source(here('modules', 'helpers', 'helpers.R'))

# HEADER UI
header <- dashboardHeader(
  title = 'Casper'
)

# SIDEBAR UI
sidebar <- dashboardSidebar(
  sidebarMenu(
    newMenuItem('DATA', selected=TRUE, tabName='data_tab', icon=icon('layer-group')),
    newMenuItem('MODEL', tabName='model_tab', icon=icon('flask')),
    newMenuItem('ANALYSIS', tabName='analysis_tab', icon=icon('chart-bar')),
    newMenuItem('PIVOT', tabName='pivot_tab', icon=icon('cube')),
    newMenuItem('SEGMENTATION', tabName='segmentation_tab', icon=icon('crosshairs')),
    newMenuItem('DOCS', tabName='docs_tab', icon=icon('book'))
  )
)

# BODY UI
body <- dashboardBody(
  css_style(box_color, nav_color),
  tabItems(
    data_tab_ui('data_tab'),
    model_tab_ui('model_tab'),
    analysis_tab_ui('analysis_tab'),
    pivot_tab_ui('pivot_tab'),
    segmentation_tab_ui('segmentation_tab'),
    docs_tab_ui('docs_tab')
  )
)

# ASSEMBLE UI
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# DEFINE SERVER CODE
server <- function(input, output, session) {
  callModule(data_tab_server, 'data_tab')
  callModule(model_tab_server, 'model_tab')
  callModule(analysis_tab_server, 'analysis_tab')
  callModule(pivot_tab_server, 'pivot_tab')
  callModule(segmentation_tab_server, 'segmentation_tab')
  session$onSessionEnded(stopApp)
}

# RUN APP
shinyApp(ui, server)
