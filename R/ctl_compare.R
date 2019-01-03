#' @title Visually compare CTLs
#' @description Visually compare two CTL files
#' @param ctl1 character, lines of CTL (left hand side)
#' @param ctl2 character, lines of CTL (right hand side)
#' @return htmlwidget
#' @seealso 
#'  \code{\link[diffr]{diffr}}
#' @rdname ctl_compare
#' @export 
#' @importFrom diffr diffr
ctl_compare <- function(ctl1,ctl2){
  
  tf1 <- tempfile(fileext = '.ctl')
  tf2 <- tempfile(fileext = '.ctl')
  
  ctl1%>%
    paste0(collapse = '\n')%>%
    ctl_style()%>%
    ctl_block_file()%>%
    cat(file=tf1,sep='\n')
  
  ctl2%>%
    paste0(collapse = '\n')%>%
    ctl_style()%>%
    ctl_block_file()%>%
    cat(file=tf2,sep='\n')
  
  diffr::diffr(tf1,tf2)
}
