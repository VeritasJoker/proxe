server_liquid_tumors_pdx_mutations <- function(input, output, server) {
  onco2_input <- eventReactive(input$onco2_go, {
    # filter v1mat by selected features:
    # Filter on WHO Category
    v1_colnms <- stringr::str_sub(colnames(v1mat), 1, 10)
    df_whocatnms <- df[df$`WHO Category` %in% input$onco2_whocat, ]$namenum
    v1mat <- v1mat[, v1_colnms %in% df_whocatnms]
    # Filter on WHO classifcation
    v1_colnms <- stringr::str_sub(colnames(v1mat), 1, 10)
    df_whoclassnms <- df[df$`WHO Classification` %in% input$onco2_whoclass, ]$namenum
    v1mat <- v1mat[, v1_colnms %in% df_whoclassnms]
    v1mat
  }, ignoreNULL = FALSE)

  output$plot_oncoprint2 <- renderPlot({

    # accept filtered input
    v1mat <- onco2_input()

    # remove NA rows
    v1mat <- v1mat[rowSums(!is.na(v1mat)) != 0, ]

    # match to df for column metadata plotting as bottom annotation (ba)
    vcols <- data.frame(
      pdx_name = colnames(v1mat),
      pdx_id = substring(colnames(v1mat), 1, 10)
    )
    df_abrv <- df[!duplicated(df$namenum), ]
    vmeta <- merge(vcols, df_abrv, by.x = "pdx_id", by.y = "namenum", all.x = T)
    vmeta$Treated <- vmeta$`Treatment Phase at Time of Sample` != "Untreated"
    # rownames(vmeta) <- vmeta$pdx_name

    # printing plot
    cols_to_show <- c(
      "Sex", "WHO Category", "Treatment Phase at Time of Sample",
      "Treated", "Cytogenetic Risk Category", "Age"
    )


    # check if vmeta has any values for each in cols_to_show, removes if not.
    cols_na <- apply(
      X = vmeta[cols_to_show],
      MARGIN = 2,
      FUN = function(colmn) {
        all(is.na(colmn))
      }
    )
    cols_to_show <- cols_to_show[!cols_na]

    ba <- HeatmapAnnotation(
      df = vmeta[
        cols_to_show
      ],
      col = list(
        Sex = c("M" = "lightblue", "F" = "khaki1", "NA" = "gray")
      ),
      show_annotation_name = TRUE
      # ,colname = anno_text(vmeta$pdx_name, rot = 90, just = "right", offset = unit(1, "npc") - unit(2, "mm")),
      ,height = unit.c(unit(5, "mm"), max_text_width(vmeta$pdx_name) + unit(2, "mm"))
    )

    col <- c(snv = "red", indel = "blue", splice = "yellow")

    ComplexHeatmap::oncoPrint(v1mat,
      get_type = function(x) strsplit(x, ";")[[1]],
      alter_fun = list(
        background = function(x, y, w, h) {
          grid.rect(x, y, w - unit(0.5, "mm"), h - unit(0.5, "mm"), gp = gpar(fill = "#CCCCCC", col = NA))
        },
        snv = function(x, y, w, h) grid.rect(x, y, w * 0.9, h * 0.9, gp = gpar(fill = col["snv"], col = NA)),
        indel = function(x, y, w, h) grid.rect(x, y, w * 0.9, h * 0.4, gp = gpar(fill = col["indel"], col = NA)),
        splice = function(x, y, w, h) grid.rect(x, y, w * 0.5, h * 0.5, gp = gpar(fill = col["splice"], col = NA))
      ), col = col,
      show_column_names = TRUE,
      row_names_gp = gpar(fontsize = 10),
      row_title_gp = gpar(fontsize = 10),
      column_title_gp = gpar(fontsize = 10),
      column_names_gp = gpar(fontsize = 10),
      bottom_annotation = ba
      #,bottom_annotation_height = unit(length(cols_to_show) * 4, "mm")
    )
  })

  output$onco2.ui <- renderUI({
    plotOutput("plot_oncoprint2", height = input$onco2_plotheight, width = "100%")
  })
}
