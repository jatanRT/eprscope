#
## =============== GLOBAL SETTINGS fFOR THE `SHINYAPP01` ==================
#
## in order to load files with > 5MB size
options(shiny.maxRequestSize = 30*1024^2)
#
## style section character string
h3.style.string <- "color: lightgray;
                   background-color: #0148A4;
                   border-radius: 6px;
                   padding: 6px;"
#
## condition for the origin:
origin.cond <- function(orig){
  if (orig == "Xenon"){
    return(2)
  }
  if (orig == "WinEpr") {
    return(1)
  }
  if (orig == "Magnettech") {
    return(0)
  }
}
#
## function to convert string into list elements:
convert_str2list <- function(str){
  if (grepl("^[0-9.]+$", str)) {
    return(as.numeric(str))
  } else {
    return(str)
  }
}
#
## color selection:
color.select <- c("red","gray","darkcyan","black",
                  "darkgray","magenta","steelblue",
                  "blue","darkblue","darkred",
                  "orange","darkorange","cyan",
                  "yellow","green","darkgreen",
                  "violet","darkviolet","lightblue",
                  "lightyellow","pink","lightgreen",
                  "lightgray","royalblue","skyblue",
                  "slateblue","violetred","tomato",
                  "brown","seagreen","lightcyan")
#
