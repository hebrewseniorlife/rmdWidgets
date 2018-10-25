#' Add textbox
#' 
#' @param text Content of the text box. 
#' @param textcolor Color of the text. Default black. 
#' @param title Title of the text box. Default empty. 
#' @param titlecolor Color of the title. Default white.  
#' @param titleback Color of the background of the title. Default black. 
#' @param backcolor Color of the background of the text. Default white.  
#' @param rounded Describes how much the corner of the text box would be rounded. 
#' @param type Type of the textbox, typically info or warning. 
#' @param format "latex" or "html". Controlled by global option 
#' `rmdWidgets.format`
#' 
#' @importFrom knitr asis_output
#' @import glue
#' @export
rmd_textbox <- function(text, textcolor = "black", 
                        title = "", titlecolor = "black", titleback = "yellow", 
                        backcolor = "white", rounded = "1", type = "info", 
                        format = "latex") {
  if (format == "latex") {
    return(
      rmd_textbox_latex(text, textcolor, title, titlecolor, titleback, 
                        backcolor, rounded, type)
    )
  }
  if (format == "html") {
    return(
      rmd_textbox_html(text, textcolor, title, titlecolor, titleback, 
                        backcolor, rounded, type)
    )
  }
}

rmd_textbox_latex <- function(text, textcolor, title, titlecolor, titleback, 
                              backcolor, rounded, type) {
  if (type == "info"|is.na(type)) {
    knitr::asis_output(paste0(
      glue("\\begin{tcolorbox}[width=\\textwidth,
                            colupper=(textcolor),
                            title=\bcinfo(title),
                            coltitle=(titlecolor),
                            colbacktitle=(titleback),
                            colback=(backcolor),
                            outer arc=(rounded)mm]
           (text) 
           \\end{tcolorbox}",
           .open = "(", .close = ")")
    ))
  }
  if (type == "warning") {
    knitr::asis_output(paste0(
      glue("\\begin{tcolorbox}[width=\\textwidth,
                            colupper=(textcolor),
                            title=\bcinterdit(title),
                            coltitle=(titlecolor),
                            colbacktitle=(titleback),
                            colback=(backcolor),
                            outer arc=(rounded)mm]
           (text) 
           \\end{tcolorbox}",
           .open = "(", .close = ")")
    ))
  }
}

rmd_textbox_html <- function(text, textcolor, title, titlecolor, titleback, 
                              backcolor, rounded, type) {
  icon <- if (type == "info"|is.na(type)) {
    '<i class="material-icons">&#xe001;</i>'
  }
  
  icon <- if (type == "warning") {
    '&#9940;'
  }
  
  title <- if (type == "info"|is.na(type)) {
    'Info:'
  }
  
  title <- if (type == "warning") {
    'Warning!'
  }
  
  knitr::asis_output(paste0(
    glue('<head>
         <meta name="viewport" content="width=device-width, initial-scale=1">
         <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
         
         <style>
         .alert {
         padding: 20px;
         background-color: yellow;
         color: black;
         }
         .closebtn {
         margin-left: 15px;
         color: black;
         font-weight: bold;
         float: right;
         font-size: 22px;
         line-height: 20px;
         cursor: pointer;
         transition: 0.3s;
         }
        .closebtn:hover {
         color: black;
         }
         </style>
         </head>
         
         <body>
         <br>
         <div class="alert">
         <strong>',
         .open = "[", .close = "]"), 
    icon, 
    title, 
    glue('</strong> [text]
         </div>
         <br>
         </body>',
         .open = "[", .close = "]")
    ))
}
