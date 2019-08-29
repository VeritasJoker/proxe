server_liquid_tumors_pdx_viral_transcript_detection <-
  function(input, output, server) {
    output$plot_pdx_viral_transcript_detection <- renderPlot({
      pdx_viral_transcript_detection_data <- switch(
        input$pdx_viral_transcript_measure,
        log2_fpkm = log(virusseq_fpkm_matrix + 0.01, base = 2),
        fpkm = virusseq_fpkm_matrix,
        counts = virusseq_counts_matrix,
        log_counts = log(virusseq_counts_matrix + 0.01)
      )

      pdx_viral_transcript_detection_plot_title <- switch(
        input$pdx_viral_transcript_measure,
        log2_fpkm = "Log2(FPKM + 0.01)",
        fpkm = "FPKM",
        counts = "Counts",
        log_counts = "Log(Counts)"
      )

      virusseq_virus_transcripts <- function(input_virus) {
        virus_transcripts <- virusseq_transcript_virus_lookup[virusseq_transcript_virus_lookup[["virus"]] %in% input_virus, ][["TranscriptID"]]
        pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% virus_transcripts, , drop = FALSE]
      }

      virusseq_matrix_selected_transcripts <- switch(
        input$pdx_viral_transcript_all_transcripts,
        All = pdx_viral_transcript_detection_data[!(rownames(pdx_viral_transcript_detection_data) == "XMRV"), ], # do not show XMRV by default, but leave as an option
        transcript = pdx_viral_transcript_detection_data[rownames(pdx_viral_transcript_detection_data) %in% input$pdx_viral_transcript_transcripts, , drop = FALSE],
        virus = virusseq_virus_transcripts(input$pdx_viral_transcript_virus)
      )

      lines_from_database_explorer <- function(rows_selected) {
        sample_names <- df[rows_selected, "PDX Name"]
        virusseq_matrix_selected_transcripts[, sample_names, drop = FALSE]
      }

      lines_from_who_category <- function(who_category) {
        sample_names <- df[
          df[["WHO Category"]] %in% who_category,
          "PDX Name"
        ]
        valid_sample_names <-
          sample_names[sample_names %in% colnames(virusseq_matrix_selected_transcripts)]
        virusseq_matrix_selected_transcripts[, valid_sample_names, drop = FALSE]
      }

      lines_from_who_classification <- function(who_classification) {
        sample_names <- df[
          df[["WHO Classification"]] %in% who_classification,
          "PDX Name"
        ]
        valid_sample_names <-
          sample_names[sample_names %in% colnames(virusseq_matrix_selected_transcripts)]
        virusseq_matrix_selected_transcripts[, valid_sample_names, drop = FALSE]
      }

      virusseq_matrix_selected_lines <- switch(
        input$pdx_viral_transcript_selection_method,
        all = virusseq_matrix_selected_transcripts,
        line_name = virusseq_matrix_selected_transcripts[, input$pdx_viral_transcript_line_name, drop = FALSE], # retain matrix structure
        database_explorer = lines_from_database_explorer(input$table_rows_selected),
        who_category = lines_from_who_category(input$pdx_viral_transcript_who_category),
        who_classification = lines_from_who_classification(input$pdx_viral_transcript_who_classification),
        virusseq_matrix_selected_transcripts
      )

      validate(
        need(
          nrow(virusseq_matrix_selected_lines) > 1 &
            ncol(virusseq_matrix_selected_lines) > 1,
          "Please select at least two transcripts and two samples."
        )
      )

      # heatmap.2 breaks if all values are identical AND they are all zeros (does not happen if all identical and non-zero). this may happen in cases of raw Counts and FPKM. The fix is to remove the colour key, see: https://stackoverflow.com/questions/9721785/r-trying-to-make-a-heatmap-from-a-matrix-all-the-values-in-the-matrix-are-the
      data_all_zeros <- all(unique(c(virusseq_matrix_selected_lines)) == 0)

      if (!data_all_zeros) {
        heatmap.2(
          virusseq_matrix_selected_lines,
          main = pdx_viral_transcript_detection_plot_title,
          notecol = "black",
          density.info = "none",
          trace = "none",
          margins = c(19, 16),
          col = my_palette,
          cexRow = min(1.5, 0.2 + 1 / log10(
            nrow(virusseq_matrix_selected_lines)
          )),
          cexCol = min(1.5, 0.2 + 1 / log10(
            ncol(virusseq_matrix_selected_lines)
          )),
          keysize = 0.75,
          dendrogram = "column",
          Rowv = FALSE
        )
      }
      else {
        heatmap.2(
          virusseq_matrix_selected_lines,
          main = pdx_viral_transcript_detection_plot_title,
          notecol = "black",
          density.info = "none",
          trace = "none",
          margins = c(19, 16),
          col = my_palette,
          cexRow = min(1.5, 0.2 + 1 / log10(
            nrow(virusseq_matrix_selected_lines)
          )),
          cexCol = min(1.5, 0.2 + 1 / log10(
            ncol(virusseq_matrix_selected_lines)
          )),
          key = FALSE,
          dendrogram = "column",
          Rowv = FALSE
        )
      }
    })

    output$pdx_viral_transcript_download <- downloadHandler(
      filename = function() {
        "liquid_tumor_pdx_viral_transcript_detection.csv"
      },
      content = function(file) {
        write.csv(liquid_tumor_pdx_viral_transcript_detection, file, row.names = FALSE)
      }
    )

    output$ui_plot_pdx_viral_transcript_detection <- renderUI({
      plotOutput("plot_pdx_viral_transcript_detection",
        height = input$pdx_viral_transcript_plot_height,
        width = input$pdx_viral_transcript_plot_width
      )
    })
  }
