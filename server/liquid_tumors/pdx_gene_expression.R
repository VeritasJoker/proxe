server_liquid_tumors_pdx_gene_expression <- function(input, output, server) {

  # PDX gene expression
  output$plot_rna <- renderPlot({

    # see this for making heatmaps
    # http://sebastianraschka.com/Articles/heatmaps_in_r.html

    if (input$expType == "heat") {
      # subset for only genes desired
      if (input$geneInput == "panels") {
        # use user-selected list from lists of genes to subset in RNA-seq clustering in app
        rnamat_sub2 <- rnamat_sub[rownames(rnamat_sub) %in% genesets_list[[input$rna_panel]], ]
      } else if (input$geneInput == "indiv") {
        rnamat_sub2 <- rnamat_sub[rownames(rnamat_sub) %in% input$rna_genes, ]
      }

      # subset for only samples desired
      df_idx <- input$table_rows_selected
      samp_names <- df$`PDX RNA-Seq Name`[df_idx]
      samp_names <- samp_names[complete.cases(samp_names)]
      rnamat_names <- as.character(dfr[dfr$`PDX RNA-Seq Name` %in% samp_names, ]$PDX_RNA_abrv)
      # Note great: nothing extra happens if both samples that point to a single RNA-seq are selected.

      if (input$sampleInput == "all") {
        rnamat_sub3 <- rnamat_sub2
      } else if (input$sampleInput == "click") {
        rnamat_sub3 <- rnamat_sub2[, colnames(rnamat_sub2) %in% rnamat_names]
      }

      # print(paste("debug: input$rna_genes =",input$rna_genes))
      # weird output: `[1] "debug: input$rna_genes = AKT3" "debug: input$rna_genes = TP53" "debug: input$rna_genes = EGFR"`
      
      validate(
        need(
          nrow(rnamat_sub3) > 1 &
            ncol(rnamat_sub3) > 1,
          "Please select at least two genes and two samples."
        )
      )

      # normalize RNA-seq data various ways #TODO more
      rnamat_sub3log2 <- log(rnamat_sub3 + 0.01, 2)

      print("didcomehere6")
      # plot
      # op <- par(no.readonly = T)
      # par(mar=c(50,5,5,6))
      heatmap.2(rnamat_sub3log2,
        # cellnote = toy2,  # same data set for cell labels
        main = "Log2( RPKM + 0.01 )", # heat map title
        notecol = "black", # change font color of cell labels to black
        density.info = "none", # turns off density plot inside color legend
        trace = "none", # turns off trace lines inside the heat map
        margins = c(19, 16), # widens margins around plot
        col = my_palette, # use on color palette defined earlier
        # breaks=col_breaks,    # enable color transition at specified limits
        cexRow = min(1.5, 0.2 + 1 / log10(
          nrow(rnamat_sub3log2)
        )),
        cexCol = min(1.5, 0.2 + 1 / log10(
          ncol(rnamat_sub3log2)
        )),
        keysize = 0.75,
        dendrogram = "both"
      ) # ,     # only draw a row dendrogram
      # Colv="NA")            # turn off column clustering
      # par(op)
    } else if (input$expType == "bar") {
      # subset rnamat_sub2 by gene, then transpose
      if (input$across_bar == "samples") {
        rnamat_onegene <- rnamat_sub[rownames(rnamat_sub) == input$bar_gene, ]
        rnadf_onegene <- as.data.frame(rnamat_onegene)
        names(rnadf_onegene) <- "gene"
        rnadf_onegene$name <- rownames(rnadf_onegene)
        rnadf_onegene <- transform(rnadf_onegene,
          name = reorder(name, gene)
        )
        ggplot(rnadf_onegene, aes(x = name, y = gene)) +
          geom_bar(stat = "identity", fill = "blue") +
          ggtitle(paste(input$bar_gene, "expression levels (RPKM)")) +
          xlab("sample") + coord_flip() + ylab(NULL)
      } else if (input$across_bar == "gene_set") {
        # for plotting one sample and a gene set on a barplot
        rnamat_geneset <- rnamat_sub[rownames(rnamat_sub) %in% genesets_list[[input$bar_rna_panel]], ]
        rnamat_geneset <- rnamat_geneset[, colnames(rnamat_geneset) == input$bar_rna_sample]
        rnadf_geneset <- as.data.frame(rnamat_geneset)
        names(rnadf_geneset) <- "expr"
        rnadf_geneset$genename <- rownames(rnadf_geneset)
        rnadf_geneset$expr <- log2(rnadf_geneset$expr + 0.01)
        rnadf_geneset <- transform(rnadf_geneset,
          name = reorder(genename, expr)
        )
        ggplot(rnadf_geneset, aes(x = name, y = expr)) +
          geom_bar(stat = "identity", fill = "blue") +
          ggtitle(paste("log2( expression levels (RPKM) + 0.01) in", input$bar_rna_sample)) +
          xlab(paste("Gene panel:", input$bar_rna_panel)) + coord_flip() +
          ylab(NULL)
      }
    }
  })
  
  output$ui_plot_rna <- renderUI(
    plotOutput(
      "plot_rna",
      height = input$liquid_pdx_gene_expression_plot_height,
      width = input$liquid_pdx_gene_expression_plot_width
    )
  )
}
