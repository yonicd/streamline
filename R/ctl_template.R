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
ctl_block <- function(BLOCK,align = 'c'){

  BLOCKS <- strsplit(BLOCK,'\n')[[1]]
  
  BLOCKS <- gsub(';*|^;*-*|^;*-*;*$','',BLOCKS)
  BLOCKS <- gsub('\\s{2,}','',BLOCKS)
  BLOCKS <- BLOCKS[nzchar(BLOCKS)]
  BLOCKS <- strwrap(BLOCKS,width = 40)
  mb <- pmax(50,max(nchar(BLOCKS)))
  
  
  BLOCK_TEXT <- purrr::map(BLOCKS,block_text,align = align,mb = mb)%>%paste0(collapse = '\n')
  
  glue::glue(
    sprintf(';;;%s;;;\n',strrep('-',mb-6)),
    BLOCK_TEXT,
    '\n',
    sprintf(';;;%s;;;\n\n',strrep('-',mb-6))
  ) 
}

block_text <- function(BLOCK,align,mb){
  
  if(align=='c'){
    pad_left   <- pmax(floor((mb - (nchar(BLOCK) + 6))/2),1)
    pad_right  <- pmax(ceiling((mb - (nchar(BLOCK) + 6))/2),1)
  }

  if(align=='l'){
    pad_left   <- 0
    pad_right  <- floor((mb - (nchar(BLOCK) + 6))/2) + ceiling((mb - (nchar(BLOCK) + 6))/2)
  }
  
  if(align=='r'){
    pad_left   <- floor((mb - (nchar(BLOCK) + 6))/2) + ceiling((mb - (nchar(BLOCK) + 6))/2)
    pad_right  <- 0  
  }
  
  sprintf(
    ';;;%s%s%s;;;',
    strrep(' ',pad_left),
    BLOCK,
    strrep(' ',pad_right)
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

find_comment_blocks <- function(obj){
  x <- obj[grep('^;;;(.*?);;;$',obj)]
  x1 <- gsub('[-; ]','',x)
  grep('^\\$',x1,value = TRUE)
}

ctl_block_file <- function(full_text){
  
  if(length(full_text)==1){
    full_text <- strsplit(full_text,'\n')[[1]]  
  }
  
  idx <- grep('^\\$',full_text)
  
  old_text <- full_text[idx]
  
  old_blocks <- gsub('\\s(.*?)$','',old_text)
  
  new_text <- purrr::map(old_blocks,ctl_block,align = 'c')
  
  new_text <- purrr::map2_chr(new_text,old_text,function(x,y) sprintf('%s\n%s',x,y))
  
  full_text[idx] <- new_text

  paste0(full_text,collapse = '\n')
}