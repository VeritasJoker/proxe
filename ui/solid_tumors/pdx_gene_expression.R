ui_solid_tumors_pdx_gene_expression <- function() {
  tabPanel(
    "PDX Gene Expression",
    h1("Solid Tumor PDX Gene Expression"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        sliderInput(
          inputId = "solid_pdx_gene_expression_plot_height",
          label = "Plot height (px)",
          min = 600,
          max = 1600,
          step = 100,
          value = 800
        ),
        
        sliderInput(
          inputId = "solid_pdx_gene_expression_plot_width",
          label = "Plot width (px)",
          min = 600,
          max = 1600,
          step = 100,
          value = 1300
        ),
        
        # log(RPKM) or Z-score
        radioButtons(
          "z_log_solid", "Type of data to plot",
          c("Z-score" = "z", "Expression (log2)" = "log", "Expression (linear)" = "lin"),
          selected = "z"
        ),
        # graph type
        radioButtons(
          "expType_solid", "Graph type",
          c("Heatmap (2+ genes and samples)" = "heat", "Barplot (1 gene or sample)" = "bar"),
          selected = "heat"
        ),
        # Graph Type 1: heatmap
        conditionalPanel(
          condition = "input.expType_solid == 'heat'",
          radioButtons(
            "geneInput_solid", "Type of gene input",
            c("Individual genes" = "indiv", "Panels" = "panels"),
            selected = "indiv"
          ),
          # option 1: Individual genes
          conditionalPanel(
            condition = "input.geneInput_solid == 'indiv'",
            selectizeInput(
              inputId = "rna_genes_solid", label = "Select genes",
              choices = NULL, multiple = TRUE
            )
          ),

          # option 2: Gene lists
          conditionalPanel(
            condition = "input.geneInput_solid == 'panels'",
            selectInput("rna_panel_solid", "Select gene panel list", "OncoPanel",
              selected = "OncoPanel"
            )
          ),
          radioButtons(
            "sampleInput_solid", "Type of sample input",
            c("All samples" = "all", "Click rows in Database Explorer" = "click"),
            selected = "all"
          ),
          # filter for sample categories
          conditionalPanel(
            condition = "input.sampleInput_solid == 'all'",
            selectizeInput(
              inputId = "COSMIC_Type_solid",
              label = "Select COSMIC Type to show:",
              choices = levels(solid$`COSMIC Type`),
              multiple = TRUE,
              selected = c("carcinoma", "malignant_melanoma", "sarcoma")
            ),
            selectizeInput(
              inputId = "COSMIC_Subtype_solid",
              label = "Select COSMIC Subtype to show:",
              choices = levels(solid$`COSMIC Subtype`),
              multiple = TRUE,
              selected = c("ductal_carcinoma", "adenocarcinoma", "NS", "squamous_cell_carcinoma")
            )
          )
        ),
        # Graph Type 2: barplot
        conditionalPanel(
          condition = "input.expType_solid == 'bar'",
          radioButtons(
            "across_bar_solid", "Data type",
            c("One gene, many samples" = "samples", "One sample, many genes" = "gene_set"),
            selected = "samples"
          ),
          # option 1: Samples
          conditionalPanel(
            condition = "input.across_bar_solid == 'samples'",
            selectizeInput(
              inputId = "bar_gene_solid", label = "Enter/select gene name",
              choices = NULL, multiple = FALSE
            )
          ),

          # option 2: Gene set
          conditionalPanel(
            condition = "input.across_bar_solid == 'gene_set'",
            selectInput("bar_rna_panel_solid", "Select gene panel list", "OncoPanel",
              selected = "OncoPanel"
            ),
            selectizeInput(
              inputId = "bar_rna_sample_solid", label = "Enter/select sample name",
              choices = colnames(gao_rna), multiple = FALSE,
              selected = "NIBR-1004"
            )
          )
        )
      ),
      mainPanel(
        uiOutput("ui_plot_rna_solid")
      )
    )
  )
}
