#' Add textbox
#' 
#' @param text Content of the text box. 
#' @param textcolor Color of the text. Default black. 
#' @param title Title of the text box. Default empty. 
#' @param titlecolor Color of the title. Default white.  
#' @param titleback Color of the background of the title. Default black. 
#' @param backcolor Color of the background of the text. Default white.  
#' @param rounded Describes how much the corner of the text box would be rounded. 
#' @param format "latex" or "html". Controlled by global option 
#' `rmdWidgets.format`
#' 
#' @importFrom knitr asis_output
#' @import glue
#' @export
rmd_textbox <- function(text, textcolor = "black", 
                        title = "", titlecolor = "black", titleback = "yellow", 
                        backcolor = "white", rounded = "1", 
                        format = "latex") {
  if (format == "latex") {
    return(
      rmd_textbox_latex(text, textcolor, title, titlecolor, titleback, 
                        backcolor, rounded)
    )
  }
}

rmd_textbox_latex <- function(text, textcolor, title, titlecolor, titleback, 
                              backcolor, rounded) {
  
  knitr::asis_output(paste0(
    glue("\\begin{tcolorbox}[width=\\textwidth,
                            colupper=(textcolor),
                            title=(title),
                            coltitle=(titlecolor),
                            colbacktitle=(titleback),
                            colback=(backcolor),
                            outer arc=(rounded)mm]
           (text) 
           \\end{tcolorbox}",
         .open = "(", .close = ")")
  ))
}
