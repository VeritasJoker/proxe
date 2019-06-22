ui_liquid_tumors_pdx_gene_expression <- function() {
  tabPanel(
    "PDX Gene Expression",
    h1("Liquid Tumor PDX Gene Expression"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        
        sliderInput(
          inputId = "liquid_pdx_gene_expression_plot_height",
          label = "Plot height (px)",
          min = 600,
          max = 1600,
          step = 100,
          value = 1300
        ),
        
        sliderInput(
          inputId = "liquid_pdx_gene_expression_plot_width",
          label = "Plot width (px)",
          min = 600,
          max = 1600,
          step = 100,
          value = 1300
        ),
        
        radioButtons(
          "expType", "Graph type",
          c("Heatmap (2+ genes and samples)" = "heat", "Barplot (1 gene or sample)" = "bar"),
          selected = "heat"
        ),
        # Graph Type 1: heatmap
        conditionalPanel(
          condition = "input.expType == 'heat'",
          radioButtons(
            "geneInput", "Type of gene input",
            c("Individual genes" = "indiv", "Panels" = "panels"),
            selected = "indiv"
          ),
          # option 1: Individual genes
          conditionalPanel(
            condition = "input.geneInput == 'indiv'",
            selectizeInput(
              inputId = "rna_genes", label = "Select genes",
              choices = NULL, multiple = TRUE
            )
          ),

          # option 2: Gene lists
          conditionalPanel(
            condition = "input.geneInput == 'panels'",
            selectInput("rna_panel", "Select gene panel list", names(genesets_list),
              selected = "HemoSeq_v2"
            )
          ),
          radioButtons(
            "sampleInput", "Type of sample input",
            c("All samples" = "all", "Click rows in Database Explorer" = "click"),
            selected = "all"
          ),
          helpText("Note: not all rows selected in Database Explorer will have associated RNA-seq data.")
        ),
        # Graph Type 2: barplot
        conditionalPanel(
          condition = "input.expType == 'bar'",
          radioButtons(
            "across_bar", "Data type",
            c("One gene, many samples" = "samples", "One sample, many genes" = "gene_set"),
            selected = "samples"
          ),
          # option 1: Samples
          conditionalPanel(
            condition = "input.across_bar == 'samples'",
            selectizeInput(
              inputId = "bar_gene", label = "Enter/select gene name",
              choices = NULL, multiple = FALSE
            )
          ),

          # option 2: Gene set
          conditionalPanel(
            condition = "input.across_bar == 'gene_set'",
            selectInput("bar_rna_panel", "Select gene panel list", names(genesets_list),
              selected = "HemoSeq_v2"
            ),
            selectizeInput(
              inputId = "bar_rna_sample", label = "Enter/select sample name",
              choices = colnames(rnamat_sub), multiple = FALSE,
              selected = "AML12.20140429.pe"
            )
          )
        ),
        HTML("<b>Download associated files:</b>"),
        br(),
        a("Gene Levels Filtered",
          target = "_blank",
          href = "https://www.dropbox.com/s/60699rvg3ftno2m/Cuff_Gene_Levels_filtered_proxe.csv?dl=0"
        ),
        br(),
        a("Gene Levels",
          target = "_blank",
          href = "https://www.dropbox.com/s/4ur0yvhsmfdommf/Cuff_Gene_Levels_proxe.csv?dl=0"
        ),
        HTML("* This data underlies this heatmap."),
        br(),
        a("Isoform Levels",
          target = "_blank",
          href = "https://www.dropbox.com/s/l1kdtynwhes804a/Cuff_Isoform_Levels_proxe.csv?dl=0"
        ),
        helpText("Files last updated 1/10/2019")
      ),
      mainPanel(
        uiOutput("ui_plot_rna")
      )
    )
  )
}
