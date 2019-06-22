# server.R

print("didcomehere4")
# Define a server for the Shiny app
shinyServer(function(input, output, session) { # TODO: read on what 'session' means here.

  # for observer functions
  observe({

    # liquid RNA-seq plots
    updateSelectizeInput(session,
      inputId = "rna_genes",
      choices = sort(rownames(rnamat_sub)), server = TRUE,
      selected = c("BCL2", "TP53", "FLT3", "MYC", "JAK2")
    )
    updateSelectizeInput(session,
      inputId = "bar_gene",
      choices = sort(rownames(rnamat_sub)), server = TRUE,
      selected = c("BCL2")
    )

    # solid RNA-seq plots
    updateSelectizeInput(session,
      inputId = "rna_genes_solid",
      choices = sort(rownames(rnamat_sub)), server = TRUE,
      selected = c("BCL2", "TP53", "KRAS", "MYC", "PTEN", "PIK3CA")
    )
    updateSelectizeInput(session,
      inputId = "bar_gene_solid",
      choices = sort(rownames(rnamat_sub)), server = TRUE,
      selected = c("TP53")
    )

    # Shiny 'links' between tab panels
    if (input$Request_link != 0) updateTabsetPanel(session, inputId = "mainNavBar", selected = "Line Request/Pricing")
    if (input$Methods_link != 0) updateTabsetPanel(session, inputId = "mainNavBar", selected = "Methods")
    # Line Request link button for solid tab
    if (input$Request_link_solid != 0) updateTabsetPanel(session, inputId = "mainNavBar", selected = "Line Request/Pricing")
  })

  # Liquid tumors, defined in server/liquid_tumors/

  server_liquid_tumors_database_explorer(input, output, session)
  server_liquid_tumors_pdx_gene_expression(input, output, session)
  server_liquid_tumors_pdx_mutations(input, output, session)
  server_liquid_tumors_pdx_viral_transcript_detection(input, output, session)
  server_liquid_tumors_pdx_gene_fusion_predictions(input, output, session)
  server_liquid_tumors_contingency_table(input, output, session)
  server_liquid_tumors_line_report(input, output, session)
  server_liquid_tumors_glossary(input, output, session)
  server_liquid_tumors_methods(input, output, session)

  # Solid Tumors, defined in server/solid_tumors

  server_solid_tumors_database_explorer(input, output, session)
  server_solid_tumors_pdx_gene_expression(input, output, session)
  server_solid_tumors_glossary(input, output, session)

  # 'More' menu, defined in server/more/

  server_more_line_request_pricing(input, output, session)
  server_more_ilab_billing_platform(input, output, session)

  ######### -- observing dropdownMenu objects -- ########

  # TODO, if possible: convert all of this into a Shiny module that is 1) purely dependent on column metadata, and 2) not hardcoded separately per button.
  # Note I tried doing `2. selecting all` by loop, but it failed. Keep looking for solution.

  ## 1. sorting a2z

  # liquid

  observeEvent(input[["a2z_liquid_administrative"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_liquid_administrative",
      choices = sort(meta3[(meta3$`Column Groupings` == "administrative"), ]$`PRoXe Column Header`),
      selected = input$check2_liquid_administrative
    )
  })

  observeEvent(input[["a2z_liquid_tumor"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_liquid_tumor",
      choices = sort(meta3[(meta3$`Column Groupings` == "tumor"), ]$`PRoXe Column Header`),
      selected = input$check2_liquid_tumor
    )
  })

  observeEvent(input[["a2z_liquid_patient"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_liquid_patient",
      choices = sort(meta3[(meta3$`Column Groupings` == "patient"), ]$`PRoXe Column Header`),
      selected = input$check2_liquid_patient
    )
  })

  observeEvent(input[["a2z_liquid_pdx"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_liquid_pdx",
      choices = sort(meta3[(meta3$`Column Groupings` == "pdx"), ]$`PRoXe Column Header`),
      selected = input$check2_liquid_pdx
    )
  })

  # solid

  observeEvent(input[["a2z_solid_administrative"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_solid_administrative",
      choices = sort(solid_meta2[(solid_meta2$`Column Groupings` == "administrative"), ]$`PRoXe Column Header`),
      selected = input$check2_solid_administrative
    )
  })

  observeEvent(input[["a2z_solid_tumor"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_solid_tumor",
      choices = sort(solid_meta2[(solid_meta2$`Column Groupings` == "tumor"), ]$`PRoXe Column Header`),
      selected = input$check2_solid_tumor
    )
  })

  observeEvent(input[["a2z_solid_pdx"]], {
    updateCheckboxGroupInput(
      session = session, inputId = "check2_solid_pdx",
      choices = sort(solid_meta2[(solid_meta2$`Column Groupings` == "pdx"), ]$`PRoXe Column Header`),
      selected = input$check2_solid_pdx
    )
  })

  # NOTE: there is currently no _patient version for solid because we do not yet have patient data.
  # Remove if(F) below manually when we get 'patient' data. (see solid_meta2$`Column Groupings`)
  if (F) {
    observeEvent(input[["a2z_solid_patient"]], {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_patient",
        choices = sort(solid_meta2[(solid_meta2$`Column Groupings` == "patient"), ]$`PRoXe Column Header`),
        selected = input$check2_solid_patient
      )
    })
  }

  ## 2. selecting all

  # TODO: functionalize call of observeEvent()s below via for loop or other mechanism
  if (F) { # Note this does not work. Determine why. # Test: does it work ok to access input as list like this?
    for (colgrp in levels(as.factor(meta3$`Column Groupings`))) {
      observeEvent(input[[paste0("all_", colgrp)]], {
        if (all(meta3[meta3$`Column Groupings` == colgrp, ]$`PRoXe Column Header` %in% input[[paste0("check2_", colgrp)]])) {
          updateCheckboxGroupInput(
            session = session, inputId = input[[paste0("check2_", colgrp)]], selected = ""
          )
        } else {
          updateCheckboxGroupInput(
            session = session, inputId = input[[paste0("check2_", colgrp)]], selected = names(df[1:(obInvisRet_ind - 1)])
          )
        }
      })
    }
  } # end of if(F)

  # if(F){ # purpose -- commenting out this section below for testing.

  # liquid

  observeEvent(input$all_liquid_administrative, {
    if (all(meta3[meta3$`Column Groupings` == "administrative", ]$`PRoXe Column Header` %in% input$check2_liquid_administrative)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_administrative", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_administrative", selected = names(df[1:(obInvisRet_ind - 1)])
      )
    }
  })

  observeEvent(input$all_liquid_tumor, {
    if (all(meta3[meta3$`Column Groupings` == "tumor", ]$`PRoXe Column Header` %in% input$check2_liquid_tumor)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_tumor", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_tumor", selected = names(df[1:(obInvisRet_ind - 1)])
      )
    }
  })

  observeEvent(input$all_liquid_patient, {
    if (all(meta3[meta3$`Column Groupings` == "patient", ]$`PRoXe Column Header` %in% input$check2_liquid_patient)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_patient", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_patient", selected = names(df[1:(obInvisRet_ind - 1)])
      )
    }
  })

  observeEvent(input$all_liquid_pdx, {
    if (all(meta3[meta3$`Column Groupings` == "pdx", ]$`PRoXe Column Header` %in% input$check2_liquid_pdx)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_pdx", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_liquid_pdx", selected = names(df[1:(obInvisRet_ind - 1)])
      )
    }
  })

  # solid

  observeEvent(input$all_solid_pdx, {
    if (all(solid_meta2[solid_meta2$`Column Groupings` == "pdx", ]$`PRoXe Column Header` %in% input$check2_solid_pdx)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_pdx", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_pdx", selected = names(solid[1:(obInvisRet_ind_solid - 1)])
      )
    }
  })

  observeEvent(input$all_solid_administrative, {
    if (all(solid_meta2[solid_meta2$`Column Groupings` == "administrative", ]$`PRoXe Column Header` %in% input$check2_solid_administrative)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_administrative", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_administrative", selected = names(solid[1:(obInvisRet_ind_solid - 1)])
      )
    }
  })

  observeEvent(input$all_solid_tumor, {
    if (all(solid_meta2[solid_meta2$`Column Groupings` == "tumor", ]$`PRoXe Column Header` %in% input$check2_solid_tumor)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_tumor", selected = ""
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2_solid_tumor", selected = names(solid[1:(obInvisRet_ind_solid - 1)])
      )
    }
  })

  # } # end of if(F)

  # Frequently asked questions
  output$FAQ <- renderUI({
    # TODO: later functionalize this
    filename <- dir(path = "www", pattern = "_PROXE_FAQ.pdf", full.names = T)
    if (length(filename) < 1) warning("Where is the FAQ pdf?")
    if (length(filename) > 1) {
      warning("> 1 FAQ PDFs in www folder. Taking last saved.")
      tmp <- sapply(filename, function(i) file.info(i)$mtime)
      newest_file <- sort(tmp, decreasing = T)[1]
      filename <- names(newest_file)
    }
    filename <- gsub("www/", "", filename)
    tags$iframe(
      src = filename,
      width = "100%",
      height = "800px"
    )
  })
})
