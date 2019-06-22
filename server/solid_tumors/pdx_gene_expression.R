server_solid_tumors_pdx_gene_expression <- function(input, output, session) {
  output$plot_rna_solid <- renderPlot({

    # see this for making heatmaps
    # http://sebastianraschka.com/Articles/heatmaps_in_r.html

    gao_rna <- as.matrix(gao_rna)

    if (input$z_log_solid == "z") {
      gao_rna <- t(scale(t(log2(gao_rna + 0.01))))
    } else if (input$z_log_solid == "log") {
      gao_rna <- log2(gao_rna + 0.01)
    } else if (input$z_log_solid == "lin") {
      # continue
    }

    if (input$expType_solid == "heat") {

      # subset for only genes desired
      if (input$geneInput_solid == "panels") {
        # use user-selected list from lists of genes to subset in RNA-seq clustering in app
        gao_rna2 <- gao_rna[rownames(gao_rna) %in% genesets_list[[input$rna_panel_solid]], ]
      } else if (input$geneInput_solid == "indiv") {
        gao_rna2 <- gao_rna[rownames(gao_rna) %in% input$rna_genes_solid, ]
      }

      # subset for only samples desired
      rnamat_names <- solid[input$solid_table_rows_selected, ]$`PDX Name`
      if (input$sampleInput_solid == "all") {
        # filter 'solid' by COSMIC_Type_solid and COSMIC_Subtype_solid
        pdx_to_keep <- solid[solid$`COSMIC Type` %in% input$COSMIC_Type_solid &
          solid$`COSMIC Subtype` %in% input$COSMIC_Subtype_solid, ]$`PDX Name`
        gao_rna3 <- gao_rna2[, pdx_to_keep]
      } else if (input$sampleInput_solid == "click") {
        gao_rna3 <- gao_rna2[, colnames(gao_rna2) %in% rnamat_names]
      }
      
      validate(
        need(
          nrow(gao_rna3) > 1 &
            ncol(gao_rna3) > 1,
          "Please select at least two genes and two samples."
        )
      )

      print("didcomehere6_solid")

      # plot
      # op <- par(no.readonly = T)
      # par(mar=c(50,5,5,6))
      if (F) {
        heatmap.2(gao_rna3,
          # cellnote = toy2,  # same data set for cell labels
          main = switch(input$z_log_solid, z = "Z-score", log = "Log2( RPKM + 0.01 )", lin = "RPKM"), # heat map title
          notecol = "black", # change font color of cell labels to black
          density.info = "none", # turns off density plot inside color legend
          trace = "none", # turns off trace lines inside the heat map
          margins = c(19, 16), # widens margins around plot
          col = my_palette, # use on color palette defined earlier
          # breaks=col_breaks,    # enable color transition at specified limits
          keysize = 0.75,
          dendrogram = "both"
        ) # ,     # only draw a row dendrogram
        # Colv="NA")            # turn off column clustering
        # par(op)
      }

      # sample code for pulling pheatmap params
      if (F) {
        # -- plot heatmaps of lfc -- ##

        # 1. Generate annotations for rows and columns

        annotation_row <- data.frame(
          treatment = demog$treatment_abv,
          time = demog$time,
          donor = as.factor(demog$donor_abv)
        )
        rownames(annotation_row) <- demog$foreignkey_short

        # utility function for shifting arrays
        shifter <- function(x, n = 1) {
          if (n == 0) x else c(tail(x, -n), head(x, n))
        }

        # set annotation colors
        library(RColorBrewer)
        ann_colors <- list(
          treatment = structure(
            c(brewer.pal(3, "Dark2"), "white"),
            names = levels(annotation_row$treatment)
          ),
          time = c("day4" = "gray", "day7" = "black"),
          donor = structure(
            rev(shifter(rainbow(length(levels(annotation_row$donor))), 14)),
            # colorRampPalette(c("red","yellow","blue"))(length(levels(annotation_row$donor))),
            names = levels(annotation_row$donor)
          )
        )

        # set color-mapping parameters
        color <- colorRampPalette(rev(brewer.pal(
          n = 7, name =
            "RdBu"
        )))(100)
        breaks <- seq(-1, 1, length.out = 101)

        # 2a. plot sample-number-ordered heatmap of Pearson of LFC
        library(pheatmap)
        pheatmap(
          mat = cor(cpm_filt_lfc),
          color = color, breaks = breaks,
          cluster_rows = FALSE,
          cluster_cols = FALSE,
          annotation_col = annotation_row,
          annotation_colors = ann_colors,
          annotation_row = annotation_row
        )
      }
      # set up annotations
      annotation_row <- data.frame(
        # primary_site = solid$`COSMIC Primary Site`,
        COSMIC_Type = solid$`COSMIC Type`,
        COSMIC_Subtype = solid$`COSMIC Subtype`
      )
      rownames(annotation_row) <- solid$`PDX Name`
      # set annotation colors
      library(RColorBrewer)
      # utility function for shifting arrays
      # shifter <- function(x, n = 1) {
      #   if (n == 0) x else c(tail(x, -n), head(x, n))
      # }
      # ann_colors <- list(
      #   primary_site = structure(
      #     rev(shifter(rainbow(length(levels(annotation_row$primary_site))),length(levels(annotation_row$primary_site)))),
      #     names=levels(annotation_row$primary_site)
      #   ),
      #   COSMIC_Type = structure(
      #     rev(shifter(rainbow(length(levels(annotation_row$COSMIC_Type))),length(levels(annotation_row$COSMIC_Type)))),
      #     names=levels(annotation_row$COSMIC_Type)
      #   )
      # )
      # plot heatmap
      pheatmap(
        gao_rna3,
        color = colorRampPalette(rev(brewer.pal(
          n = 7, name =
            "RdBu"
        )))(100),
        # breaks = seq(-15,15,length.out=101),
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        annotation_col = annotation_row
        # ,annotation_colors=ann_colors
      )
      
    } else if (input$expType_solid == "bar") {
      # subset gao_rna2 by gene, then transpose
      if (input$across_bar_solid == "samples") {
        rnamat_onegene <- gao_rna[rownames(gao_rna) == input$bar_gene_solid, ]
        rnadf_onegene <- as.data.frame(rnamat_onegene)
        names(rnadf_onegene) <- "gene"
        rnadf_onegene$name <- rownames(rnadf_onegene)
        rnadf_onegene <- transform(rnadf_onegene,
          name = reorder(name, gene)
        )
        ggplot(rnadf_onegene, aes(x = name, y = gene)) +
          geom_bar(stat = "identity", fill = "blue") +
          ggtitle(paste(input$bar_gene, switch(input$z_log_solid, z = "Z-score", log = "Log2( RPKM + 0.01 )", lin = "RPKM"))) +
          xlab("sample") + coord_flip() + ylab(NULL)
      } else if (input$across_bar_solid == "gene_set") {
        # for plotting one sample and a gene set on a barplot
        rnamat_geneset <- gao_rna[rownames(gao_rna) %in% genesets_list[[input$bar_rna_panel_solid]], ]
        rnamat_geneset <- rnamat_geneset[, colnames(rnamat_geneset) == input$bar_rna_sample_solid]
        rnadf_geneset <- as.data.frame(rnamat_geneset)
        names(rnadf_geneset) <- "expr"
        rnadf_geneset$genename <- rownames(rnadf_geneset)
        rnadf_geneset <- transform(rnadf_geneset,
          name = reorder(genename, expr)
        )
        ggplot(rnadf_geneset, aes(x = name, y = expr)) +
          geom_bar(stat = "identity", fill = "blue") +
          ggtitle(paste(switch(input$z_log_solid, z = "Z-score", log = "Log2( RPKM + 0.01 )", lin = "RPKM"), "in", input$bar_rna_sample_solid)) +
          xlab(paste("Gene panel:", input$bar_rna_panel_solid)) + coord_flip() +
          ylab(NULL)
      }
    }
  })
  
  output$ui_plot_rna_solid <- renderUI(
    plotOutput(
      "plot_rna_solid",
      height = input$solid_pdx_gene_expression_plot_height,
      width = input$solid_pdx_gene_expression_plot_width
    )
  )
}
