# ui.R #

# find indices of transitions from obligate visible to conditionally visible to obligate invisible columns
# TODO: discuss with Mark that I think we should have a fourth section, thus:
# 1. obligate visible from the start
# 2. option to click to make visible
# 3. option to type in box to make visible (genes)
# 4. never visible (should we even have these in the dataframe we read in?)

# Define the overall UI
shinyUI(
  navbarPage( # title="Public Repository of Xenografts",
    title = span(
      img(
        src = "PRoXe-Blue-Small.png", width = "90px",
        style = "float:left; padding-right:5px; display:inline; position:relative; top:-10px"
      ),
      "Public Repository of Xenografts"
    ),
    id = "mainNavBar",
    windowTitle = "PRoXe: Public Repository of Xenografts",

    navbarMenu(
      "Liquid Tumors", # UIs are defined in ui/liquid_tumors/

      ui_liquid_tumors_database_explorer(),
      ui_liquid_tumors_pdx_gene_expression(),
      ui_liquid_tumors_pdx_viral_transcript_detection(),
      ui_liquid_tumors_pdx_mutations(),
      ui_liquid_tumors_contingency_table(),
      ui_liquid_tumors_line_report(),
      ui_liquid_tumors_glossary(),
      ui_liquid_tumors_methods()
    ),

    navbarMenu(
      "Solid Tumors", # UIs defined in ui/solid_tumors/
      # tabPanel("Solid Tumors (beta)",

      ui_solid_tumors_database_explorer(),
      ui_solid_tumors_pdx_gene_expression(),
      ui_solid_tumors_pdx_glossary()
    ),

    navbarMenu(
      "More", # UIs defined in ui/more/

      ui_more_line_requests_pricing(),
      ui_more_ilab_billing_platform(),
      ui_more_about(),
      ui_more_faq()
    )
    # navbarPage options
    , position = "fixed-top", collapsible = TRUE
  )
)
