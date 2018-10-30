#' Add checkbox
#' 
#' @param choices A character vector for all the choices
#' @param selected A numeric vector for the index of all selected/checked items.
#' @param label Text string for label row. Default NULL.
#' @param inline T/F for whether choices should be aligned inline. Default T.
#' @param label_inline T/F for whether the label row should be placed inline 
#' with the choices. Default T.
#' @param format "latex" or "html". Controlled by global option 
#' `rmdWidgets.format`
#' 
#' @importFrom knitr asis_output
#' @import glue
#' @export
rmd_checkbox <- function(choices, selected = NULL, label = NULL, 
                         inline = T, label_inline = T, 
                         format = "latex") {
  if (format == "latex") {
    return(
      rmd_checkbox_latex(choices, selected, label, inline, label_inline)
    )
  }
  if (format == "html") {
    return(
      rmd_checkbox_html(choices, selected, label, inline, label_inline)
    )
  }
}

rmd_checkbox_latex <- function(choices, selected, label, inline, label_inline) {
  
  check_symbols <- rep("$\\square$", length(choices))
  check_symbols[selected] <- "$\\boxtimes$"
  check_items <- paste(check_symbols, choices)
  
  if (inline) {
    
    if (is.null(label)) {
      label <- "\\hfill"
    }
    if (!is.null(label)) {
      after_label <- switch(label_inline + 1, "\n\n\\hfill", "\\hfill")
      label <- paste(label, after_label)
    }
    check_items <- paste(check_items, collapse = " ")
    
  } else {
    
    if (is.null(label)) {
      label <- ""
    }
    if (!is.null(label)) {
      after_label <- "\n\n"
      label <- paste(label, after_label)
    }
    check_items <- paste(check_items, collapse = "\n\n")
    
  }
  knitr::asis_output(paste(label, check_items, "  "))
}

rmd_checkbox_html <- function(choices, selected, label, inline, label_inline) {
  
  check_symbols <- rep('<input type="checkbox">', length(choices))
  check_symbols[selected] <- '<input type="checkbox" checked="checked">'
  check_items <- paste(check_symbols, choices)
  
  if (inline) {
    
    if (is.null(label)) {
      label <- '<p style="text-align:left;"><span style="float:right;">'
    }
    if (!is.null(label)) {
      label <- switch(label_inline + 1,
                      paste('<p style="text-align:left;">', label, '<br><span style="float:right;">'),
                      paste('<p style="text-align:left;">', label, '<span style="float:right;">'))
    }
    check_items <- paste(check_items, collapse = " ")
    
    knitr::asis_output(paste(label, check_items, "</span></p><br>"))
  } else {
    
    if (is.null(label)) {
      label <- '<p>'
    }
    if (!is.null(label)) {
      after_label <- '<p><br>'
      label <- paste(label, after_label)
    }
    check_items <- paste(check_items, collapse = "<br>")
    
    knitr::asis_output(paste(label, check_items, "</p><br>"))
  }
}

