ui_solid_tumors_database_explorer <- function() {
  tabPanel(
    "Database Explorer",
    h1("Solid Tumor Database Explorer"),
    br(),
    tags$div(
      style = "margin:15px;",
      # top row of app
      fluidRow(
        column(6,
          # dropdown buttons - left side
          id = "dt-col-select-solid",
          tags$div(
            style = "display: flex;",
            tags$h4("First, select columns to show:", style = "margin-right: 15px; font-weight: bold;"),
            mydropdownButton("administrative", solid_meta2, condVis_ind_solid, button_group = "solid", solid),
            mydropdownButton("patient", solid_meta2, condVis_ind_solid, button_group = "solid", solid),
            mydropdownButton("tumor", solid_meta2, condVis_ind_solid, button_group = "solid", solid),
            mydropdownButton("pdx", solid_meta2, condVis_ind_solid, button_group = "solid", solid)
          )
        ),
        column(6,
          # links, buttons on right side
          id = "email-request",
          p(
            a("Email us", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20feedback", target = "_top"),
            " with questions.",
            actionButton("Request_link_solid", "Request lines", icon("arrow-circle-o-right"),
              class = "btn btn-primary"
            ), # style="background-color: #1486ba; color: #fff; border-color: #2e6da4"
            align = "right"
          )
        )
      ),
      # 1. Display data table
      fluidRow(
        DT::dataTableOutput(outputId = "solid_table")
      ),
      # 2. Create download button below table
      fluidRow(h4(" ")), # just an empty space.
      fluidRow(
        p(class = "text-center", downloadButton("solid_download_filtered", "Download Filtered Data"))
      ),
      # 3. Allow user to choose from a number of visualizations and parameters.
      fluidRow(
        sidebarPanel(
          selectInput(
            "solid_plotType", "Plot Type",
            c(
              Histogram = "hist", Scatter = "scatter", Bar = "bar", "1D Scatter-Box" = "scatbox",
              "2D Contingency Table" = "ctable_plot"
            ),
            selected = "scatbox"
          ),

          # Option 1: show histogram.
          conditionalPanel(
            condition = "input.solid_plotType == 'hist'",
            selectInput("solid_hist_var", "Variable to plot",
              sort(names(solid)[solid_numeric_cols_vis]),
              selected = NULL
            ), # TODO: change to default value when numeric columns are added to solid.
            selectInput(
              "solid_hist_log", "Scaling",
              c(linear = FALSE, log10 = TRUE)
            ),
            selectInput(
              "solid_breaks", "Breaks",
              c(
                "[Custom]" = "custom", "Sturges",
                "Scott", "Freedman-Diaconis"
              )
            ),
            # Only show this sub-panel if Custom is selected
            conditionalPanel(
              condition = "input.solid_breaks == 'custom'",
              sliderInput("solid_breakCount", "Break Count", min = 1, max = 100, value = 25)
            )
          ),

          # Option 2: show scatterplot.
          conditionalPanel(
            condition = "input.solid_plotType == 'scatter'",
            selectInput("solid_scat_var_x", "X variable to plot", sort(names(solid)[solid_numeric_cols_vis]),
              selected = NULL
            ),
            selectInput("solid_scat_var_y", "Y variable to plot", sort(names(solid)[solid_numeric_cols_vis]),
              selected = NULL
            )
          ),

          # Option 3: show barplot.
          conditionalPanel(
            condition = "input.solid_plotType == 'bar'",
            selectInput("solid_bar_var", "Category to plot", sort(names(solid)[solid_factor_cols_vis]),
              selected = "COSMIC Primary Site"
            )
          ),

          # Option 4: show 1D-scatter+boxplot.
          conditionalPanel(
            condition = "input.solid_plotType == 'scatbox'",
            selectInput("solid_scatbox_cat", "Category to plot", sort(names(solid)[solid_factor_cols_vis]),
              selected = "COSMIC Primary Site"
            ),
            selectInput("solid_scatbox_num", "Numeric to plot", sort(names(solid)[solid_numeric_cols_vis]),
              selected = "Age"
            ),
            selectInput("solid_scatbox_log", "Numeric axis scaling", c("linear", "log"),
              selected = "linear"
            )
          ),
          #                # Option 5: contingency table of categories
          #                conditionalPanel(
          #                  condition = "input.plotType == 'ctable'",
          #                  selectInput("tablevarA","First category",sort(names(solid)[solid_numeric_cols_vis]),
          #                              selected = "WHO Category"),
          #                  selectInput("tablevarB","Second category",sort(names(solid)[solid_numeric_cols_vis]),
          #                              selected = "Latest Passage Banked")
          #                 ),
          # Option 6: mosaic plot of contingency table.
          conditionalPanel(
            condition = "input.solid_plotType == 'ctable_plot'",
            selectInput("solid_ctable_plot_var1", "First category", sort(names(solid)[solid_factor_cols_vis]),
              selected = "COSMIC Type"
            ),
            selectInput("solid_ctable_plot_var2", "Second category", sort(names(solid)[solid_factor_cols_vis]),
              selected = "COSMIC Primary Site"
            )
          )
        ),
        column(
          width = 8,
          ({
            #                   if (input$plotType == 'ctable') {
            #                      tableOutput("table_various")
            #                   } else
            plotOutput("solid_plot_various")
          })
        ) # end plot column
      ) # end fluidRow for custom plotting
    )
  )
}
