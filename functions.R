### --- this script is for new functions in the PRoXe app --- ###

# source of oncoprint functions, which I revised minorly: https://gist.github.com/armish/564a65ab874a770e2c26

# This oncoprint function sorts the matrix for better visualization of mutual exclusivity across genes
memoSort <- function(M) {
  geneOrder <- sort(rowSums(M), decreasing = TRUE, index.return = TRUE)$ix
  scoreCol <- function(x) {
    score <- 0
    for (i in 1:length(x)) {
      if (x[i]) {
        score <- score + 2^(length(x) - i)
      }
    }
    return(score)
  }
  scores <- apply(M[geneOrder, ], 2, scoreCol)
  sampleOrder <- sort(scores, decreasing = TRUE, index.return = TRUE)$ix
  return(M[geneOrder, sampleOrder])
}

# Oncoprint plotting function
oncoPrint <- function(M, sort = TRUE) {
  if (sort) {
    alts <- memoSort(M)
  } else {
    alts <- M
  }

  ngenes <- nrow(alts)
  nsamples <- ncol(alts)
  coverage <- sum(colSums(alts) > 0) # SPK: edited from mistake: rowSums

  ### OncoPrint
  numOfOncos <- ngenes * nsamples
  oncoCords <- matrix(rep(0, numOfOncos * 5), nrow = numOfOncos)
  colnames(oncoCords) <- c("xleft", "ybottom", "xright", "ytop", "altered")

  xpadding <- .01
  ypadding <- .01
  cnt <- 1
  for (i in 1:ngenes) {
    for (j in 1:nsamples) {
      xleft <- j - 1 + xpadding
      ybottom <- ((ngenes - i + 1) - 1) + ypadding
      xright <- j - xpadding
      ytop <- (ngenes - i + 1) - ypadding
      altered <- alts[i, j]

      oncoCords[cnt, ] <- c(xleft, ybottom, xright, ytop, altered)
      cnt <- cnt + 1
    }
  }

  colors <- rep("lightgray", cnt)
  colors[ which(oncoCords[, "altered"] == 1) ] <- "black"
  plot(c(0, nsamples), c(0, ngenes),
    type = "n",
    main = sprintf("Gene set altered in %.2f%%: %d of %d cases", coverage / nsamples * 100, coverage, nsamples),
    xlab = "", ylab = "", yaxt = "n", xaxt = "n"
  ) # note SPK added xaxt
  rect(oncoCords[, "xleft"], oncoCords[, "ybottom"], oncoCords[, "xright"], oncoCords[, "ytop"], col = colors, border = "white")
  axis(2, at = (ngenes:1) - .5, labels = rownames(alts), las = 2)
  axis(1, at = (1:nsamples) - .5, labels = colnames(alts), las = 2) # SPK new line in function
}

# function from http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe
moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp) - 1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    }
  )
  x
}

# source: http://stackoverflow.com/questions/7680959/convert-type-of-multiple-columns-of-a-dataframe-at-once
convert.magic <- function(obj, types) {
  for (i in 1:length(obj)) {
    FUN <- switch(types[i], character = as.character,
      numeric = as.numeric,
      factor = as.factor,
      logical = as.logical,
      integer = as.integer,
      date = as.POSIXct
    )
    if (class(obj[, i]) != types[i]) {
      obj[, i] <- FUN(obj[, i])
    }
  }
  obj
}


# dropdownMenu button
# original version: http://stackoverflow.com/questions/34530142/drop-down-checkbox-input-in-shiny
dropdownButton <- function(
                           label = "",
                           # note label should be simple. Only A-Za-z0-9_ are allowed in CSS IDs. TODO later, perhaps: check this input.
                           status = c("default", "primary", "success", "info", "warning", "danger"),
                           button_group = "liquid", # e.g. 'liquid' vs 'solid', or for use on some other tab.
                           ...,
                           width = NULL) {

  status <- match.arg(status)
  css_id <- paste0("dropdownButton-", button_group, "-", label)
  # dropdown button content
  html_ul <- list(
    id = css_id,
    class = "dropdown-menu",
    style = if (!is.null(width)) {
      paste0("width: ", validateCssUnit(width), ";")
    },
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status, " dropdown-toggle"),
    type = "button",
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul), tags$script(
      paste0("
        $('#", css_id, "').click(function(e) {
        // $('.dropdown').click(function(e) { //
          e.stopPropagation();
        });
      ")
    )
  )
}

mydropdownButton <- function(lab, column_metadata = meta3, condVis_ind, button_group = "liquid", table_df = df) {

  # abbreviate variables
  cm <- column_metadata
  df <- table_df
  # remove underscores from cm if they exist
  names(cm) <- gsub("_", " ", names(cm))

  # prep variables for checkboxGroupInput
  # 0. for testing: # lab = "administrative"
  # 1. choices
  cm <- cm[(cm$`Column Groupings` == lab), ]
  my_choices <- cm[order(cm$`Row Order`), ]$`PRoXe Column Header`
  # 2. selected
  my_selected <- intersect(names(df)[1:(condVis_ind - 1)], my_choices)

  dropdownButton(
    label = lab, status = "primary", button_group = button_group,
    # dynamic width based on content length:
    width = paste0(max(sapply(my_choices, nchar)) / 1.5, "em"),
    tags$div(
      actionButton(inputId = paste0("all_", button_group, "_", lab), label = "(Un)select all"),
      actionButton(inputId = paste0("a2z_", button_group, "_", lab), label = "Sort A to Z", icon = icon(paste0("sort-alpha-asc")))
    ),
    checkboxGroupInput(
      inputId = paste0("check2_", button_group, "_", lab), label = NULL,
      choices = my_choices,
      selected = my_selected,
      width = "100%"
    )
  )
}

## LATER TODO: generalize myDropdownButton and myDropdownButton_solid

# Module UI function
# mydropdownButton3 <- function(id, label = "default label") {
#   # Create a namespace function using the provided id
#   ns <- NS(id)
#
#   dropdownButton(
#     label = label, status = "primary", width = 10,
#     actionButton(inputId = ns("a2z"), label = "Sort A to Z", icon = icon(paste0("sort-alpha-asc"))),
#     actionButton(inputId = ns("all"), label = "(Un)select all"),
#     checkboxGroupInput(inputId = ns("check2"), label = "Choose",
#       choices = {meta4 <- meta3[(meta3$`Column Groupings` == id),]; meta4[order(meta4$`Row Order`),]$`PRoXe Column Header`},
#       selected = names(df)[1:(condVis_ind-1)])
#   )
# }
# # for server.R
# observeA2Z <- function(label){
#   observeEvent(input[[paste0("a2z_",label)]], {
#     updateCheckboxGroupInput(
#       session = session, inputId = paste0("check2_",label),
#       choices = sort(meta3[(meta3$`Column Groupings` == label),]),
#       selected = input[[paste0("check2_",label)]]
#       )
#   })
# }

# for(lab in unique(meta3$`Column Groupings`)){
#   lab = "administrative"
#   observeEvent(input[[paste0("a2z_",lab)]], {
#     updateCheckboxGroupInput(
#       session = session, inputId = paste0("check2_",lab),
#       choices = sort(meta3[(meta3$`Column Groupings` == lab),]$`PRoXe Column Header`),
#       selected = input[["check2_",lab]]
#     )
#   })
# }
# observeEvent(input[[paste0("a2z_",lab)]], {
#   updateCheckboxGroupInput(
#     session = session, inputId = paste0("check2_",lab),
#     choices = sort(meta3[(meta3$`Column Groupings` == lab),]$`PRoXe Column Header`),
#     selected = input[["check2_",lab]]
#   )
# })

# # Module server function -- not yet done (if ever)
# observeA3 <- function(input, output, session, id) {  # other params necy? add in if so.
#   # The selected file, if any
#   userFile <- reactive({
#     # If no file is selected, don't do anything
#     validate(need(input$a2z, message = FALSE))
#     input$a2z
#   })
#
#   # The user's data, parsed into a data frame
#   dataframe <- reactive({
#     read.csv(userFile()$datapath,
#       header = input$heading,
#       quote = input$quote,
#       stringsAsFactors = stringsAsFactors)
#   })
#
#   # We can run observers in here if we want to
#   observe({
#     msg <- sprintf("File %s was uploaded", userFile()$name)
#     cat(msg, "\n")
#   })
#
#   # Return the reactive that yields the data frame
#   return(dataframe)
# }
