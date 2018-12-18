#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param BLOCK PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[glue]{glue}}
#' @rdname ctl_block
#' @export 
#' @importFrom glue glue
ctl_block <- function(BLOCK){
  
  pad_left   <- floor((40 - (nchar(BLOCK) + 6))/2)
  
  pad_right  <- ceiling((40 - (nchar(BLOCK) + 6))/2)
  
  BLOCK_TEXT <- sprintf(
    ';;;%s%s%s;;;',
    strrep(' ',pad_left),
    BLOCK,
    strrep(' ',pad_right)
  )
  
  glue::glue(
    ';;;----------------------------------;;;\n',
    BLOCK_TEXT,
    '\n',
    ';;;----------------------------------;;;\n\n'
  ) 
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param BLOCKS PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[purrr]{map}},\code{\link[purrr]{set_names}}
#' @rdname ctl_blocks
#' @export 
#' @importFrom purrr map set_names
ctl_blocks <- function(BLOCKS){
  BLOCKS%>%
    purrr::map(ctl_block)%>%
    purrr::set_names(sprintf('%s_BLOCK',names(BLOCKS)))
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname ctl_wrap
#' @export 

ctl_wrap <- function(x){
  paste0(strwrap(x,width = 80),collapse = '\n')
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param template PARAM_DESCRIPTION
#' @param ... PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[whisker]{whisker.render}}
#'  \code{\link[purrr]{flatten}}
#' @rdname ctl_create
#' @export 
#' @importFrom whisker whisker.render
#' @importFrom purrr flatten
ctl_create <- function(template, ...){
  whisker::whisker.render(
    template = readLines(template),
    data     = purrr::flatten(list(...))
  )
}