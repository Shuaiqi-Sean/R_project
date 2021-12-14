
# DOCS TAB UI
docs_tab_ui <- function(id){
  
  # NAMESPACE TAG
  ns <- NS(id)
  
  # FUNCTION TO SHORTHAND COMMON BOX OPTIONS
  nbox <- function(title, ...) {
    box(
      title = title,
      status = color,
      solidHeader = TRUE,
      width = NULL,
      collapsible = TRUE,
      collapsed = TRUE,
      list(...)
    )
  }
  
  # UI
  tabItem(
    tabName = 'docs_tab',
    fluidRow(
      column(
        width = 6, 
        nbox(
          title = 'Usage Guide',
          includeMarkdown(here('docs', 'usage_guide.md'))
        ),
        nbox(
          title = 'Code Guide',
          includeMarkdown(here('docs', 'code_guide.md'))
        ),
        nbox(
          title = 'Adding a Variable to the Variable Library',
          includeMarkdown(here('docs', 'adding_a_variable.md'))
        ),
        nbox(
          title = 'app.R',
          includeMarkdown(here('docs', 'app.md'))
        ),
        nbox(
          title = 'data_tab_module.R',
          includeMarkdown(here('docs', 'data_tab_module.md'))
        ),
        nbox(
          title = 'model_tab_module.R',
          includeMarkdown(here('docs', 'model_tab_module.md'))
        ),
        nbox(
          title = 'analysis_tab_module.R',
          includeMarkdown(here('docs', 'analysis_tab_module.md'))
        ),
        nbox(
          title = 'data_builder_class.R',
          includeMarkdown(here('docs', 'data_builder_class.md'))
        ),
        nbox(
          title = 'factor_tables_class.R',
          includeMarkdown(here('docs', 'factor_tables_class.md'))
        ),
        nbox(
          title = 'model_class.R',
          includeMarkdown(here('docs', 'model_class.md'))
        ),
        nbox(
          title = 'rebase_rule_class.R',
          includeMarkdown(here('docs', 'rebase_rule_class.md'))
        ),
        nbox(
          title = 'table_design_class.R',
          includeMarkdown(here('docs', 'table_design_class.md'))
        ),
        nbox(
          title = 'helpers.R',
          includeMarkdown(here('docs', 'helpers.md'))
        )
      )
    )
  )
}

# DOCS TAB SERVER
docs_tab_server <- function(input, output, session){
  
}