#' @title Parse CTL blocks into matrices
#' @description Convert CTL blocks into matrices or matrices fo comments
#' @param x clean_ctl object
#' @param type character, extract numeric of comment from x, Default: c("numeric", "comment")
#' @return matrix or list containing matrices
#' @details expected format for comment in CTL is "VALUE [FIXED,SAME] ;  [COVTYPE] LABEL"
#' @examples 
#' \dontrun{
#' project <- system.file('streams',package = 'streamline')
#' 
#' run <- '101'
#' 
#' xml_raw   <- read_nmlist(run, project)
#' 
#' x   <- xml_raw%>%
#'   xml2::xml_find_first('.//nm:control_stream')%>%
#'   xml2::xml_text()%>%
#'   clean_ctl()
#' 
#' ctl_to_mat(x$OMEGA)
#' ctl_to_mat(x$OMEGA,type='comment')
#' }
#' @export
#' @rdname ctl_to_mat
#' @importFrom purrr map
ctl_to_mat <- function(x,type=c('numeric','comment')){
  
  if(inherits(x,'character'))
    x <- list(x)
  
  y <- iov_locf(x)
  
  out <- purrr::map(y,function(.y){
    
    type <- match.arg(type,c('numeric','comment'))
    
    if(grepl('BLOCK',.y[1])){
      
      ret <- parse_mat_block(.y, type)      
      
    }else{
      
      ret <- parse_mat_noblock(.y, type)
      
    }
    return(ret)
  })
  
  # flatten if length of 1
  
  if(length(out)==1) out <- out[[1]]
  
  out
}

parse_mat_block <- function(x,type){
  
  dimn <- gsub('^(.*?)\\(|\\)(.*?)$','',x[1])%>%
    as.numeric()
  
  x[1] <- gsub('^(.*?)\\s+','',x[1])  
  
  mat  <- diag(dimn)
  
  x_num <- parse_mat_numeric(x)
  x_str <- parse_mat_comment(x)
  
  if(type=='numeric'){
    
    mat[upper.tri(mat,diag = TRUE)] <- x_num
    
  }
  
  if(type=='comment'){
    
    if(length(x_str)==dimn[1]){
      
      diag(mat) <- x_str
      
    }else{
      
      mat[upper.tri(mat,diag = TRUE)] <- x_str  
      
    }
    
    mat[mat=='0'] <- ''
    
  }
  
  #reset to lower triangle indexation
  mat <- t(mat)
  
  mat
  
}

parse_mat_noblock <- function(x,type){
  
  x_num <- parse_mat_numeric(x)
  x_str <- parse_mat_comment(x)
  
  if(type=='numeric'){
    
    if(length(x_num)>1){
      
      mat <- diag(x_num)
      
    }else{
      
      mat <- matrix(x_num,nrow = 1,ncol = 1)
      
    }
    
  }
  
  if(type=='comment'){
    
    mat <- diag(length(x_num))
    diag(mat) <- x_str
    mat[mat=='0'] <- ''
    
  }
  
  mat
}

#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @importFrom dplyr mutate group_by nth n if_else ungroup pull
#' @importFrom purrr map_dbl map map_chr
iov_locf <- function(x){
  tibble::tibble(
    raw=x) %>%
    dplyr::mutate(
      SAME = purrr::map_dbl(.data[['raw']],.f=function(x) {
        as.numeric(!any(grepl('\\bSAME\\b',x)))
      })%>%
        cumsum()
    )%>%
    dplyr::group_by(.data[['SAME']]) %>%
    dplyr::mutate(new = list(dplyr::nth(.data[['raw']], 1)),
                  id=1:dplyr::n()) %>%
    dplyr::mutate(new = dplyr::if_else(id>1,purrr::map(.data[['new']],.f=function(x){
      purrr::map_chr(x,.f=function(x){ 
        
        if(!grepl('FIX',x)){
          if(!grepl('[;]',x)){
            x <- sprintf('%s ;',x)
          }
          x <- gsub(';','FIXED ;',x) 
        }
        
        x
      })
      
    }),.data[['new']]))%>%
    dplyr::ungroup()%>%
    dplyr::pull(.data[['new']])
}

#' @title Parse $THETA into tabular form
#' @description Parse $THETA from the control stream.
#' @param x list of character vectors making up the $THETA section of a control stream
#' @return tibble containing: variable number (i.e., THETA_i, i=1,...,N); FIXED, indicating presence of FIX or FIXED in the initial condition; LB, INITITAL, and UB describing the initial condition itself; and UNIT and LABEL which are parsed from the comment.  
#' @details  Parse $THETA into a form that shows initial values and fixed or not fixed. They are created from the comment and follows the form \code{<init> ; [UNIT] LABEL}.
#' @seealso 
#' [stri_count][stringi::stri_count] [stri_match_all][stringi::stri_match_all]
#' @rdname parse_theta
#' @export
#' @importFrom stringi stri_count stri_match
#' @importFrom tibble tibble
parse_theta <- function(x){
  if(inherits(x,'list'))
    x <- unlist(x)
  
  x1 <- strsplit(x,'\\;')
  
  bounds <- sapply(x1,'[',1)
  labels <- sapply(x1,'[',2)
  
  bounds <- gsub('\\s+|\\(|\\)','',bounds)
  
  fixed <- grepl('FIX',bounds)
  
  bounds <- gsub('FIXED|FIX','',bounds)
  
  count_bounds <- stringi::stri_count(bounds,regex=',')
  
  for(i in seq_along(count_bounds)){
    if(count_bounds[i]==0) bounds[i] <- sprintf(' ,%s, ',bounds[i])
    if(count_bounds[i]==1) bounds[i] <- sprintf('%s, ',bounds[i])
  }
  
  bounds <- strsplit(bounds,',')
  lb <- as.numeric(sapply(bounds,'[',1))
  initial <- as.numeric(sapply(bounds,'[',2))
  
  ub <- as.numeric(sapply(bounds,'[',3))
  
  lb[is.na(lb)] <- -Inf
  ub[is.na(ub)] <- Inf
  
  label <- gsub('^(.*?)\\]','',labels)
  
  unit <- stringi::stri_match(labels,regex = '\\[(.*?)\\]')
  
  unit <- unit[,ncol(unit)]
  
  out <- tibble::tibble(
    Var1 = as.integer(1:length(lb)),
    Var2 = 1,
    FIXED = c('','FIXED')[as.numeric(fixed)+1],
    UNIT = unit,
    LABEL = label,
    LB = lb,
    INITIAL = initial,
    UB = ub
  )
  
  out
}

#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @rdname gather
#' @export 
gather.matrix <- function(data, key = "key", value = "value", ..., na.rm = FALSE, 
                          convert = FALSE, factor_key = FALSE){
  
  s <- dim(data)
  nm <- dimnames(data)
  
  if(is.null(nm)){
    new_list <- lapply(s,seq,from=1)
  }else{
    new_list <- mapply(function(x,s){
      if(is.null(x)){
        seq(1,s)
      }else{
        x
      }
    },x=nm,s=s,SIMPLIFY = FALSE)
  }
  
  data%>%
    tibble::as_tibble()%>%
    tidyr::gather()%>%
    dplyr::bind_cols(expand.grid(new_list,stringsAsFactors = FALSE))%>%
    dplyr::select(.data[['Var1']],
                  .data[['Var2']],
                  .data[['value']])
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
#' @seealso 
#'  [set_names][purrr::set_names]
#' @rdname clean_ctl
#' @export 
#' @importFrom purrr set_names
clean_ctl <- function(x){
  x0 <- ctl_rm_comments(x)
  x1 <- gsub('(\n\n|\n|\n;|\n\n;)$','',strsplit(x0,'\\$')[[1]])
  x2 <- sub('\n',' ',x1)
  x3 <- as.list(gsub('^(.*?)\\s+','',x2))
  names(x3) <- gsub('\\s+(.*?)$','',x2)
  
  x3 <- x3[sapply(x3,nzchar)]
  
  x3 <- split(x3,names(x3))
  
  x3 <- lapply(x3,purrr::set_names,nm=NULL)
  
  x4 <- lapply(x3,function(x){
    if(is.list(x)){
      out <- lapply(x,function(xx){
        out <- gsub('^\\s+|\\s+$','',strsplit(xx,'\n')[[1]])
        out[nzchar(out)]
      })
      list(out)
    }else{
      out <- gsub('^\\s+|\\s+$','',strsplit(x,'\n')[[1]])
      out[nzchar(out)]
    }
    
  })
  
  nc <- names(x4)[(!nzchar(gsub('[A-Z]','',names(x4))))]
  
  x4 <- x4[nc[nchar(nc)>1]]
  
  unlist(x4,recursive = FALSE)
}

#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
combine_blocks <- function(COMMENT_BLOCK){
  
  COMMENT <- lapply(COMMENT_BLOCK,tibble::as_tibble)
  
  for(i in 2:length(COMMENT)){
    nc1 <- max(as.numeric(gsub('\\D','',names(COMMENT[[i-1]]))))
    nc2 <- ncol(COMMENT[[i]])
    names(COMMENT[[i]]) <- sprintf('V%s',(nc1+1):(nc1+nc2))  
  }
  
  COMMENT <- COMMENT%>%
    dplyr::bind_rows()%>%
    as.matrix()
  
  colnames(COMMENT) <- NULL
  
  COMMENT[is.na(COMMENT)] <- ''
  
  COMMENT
}

ctl_rm_comments <- function(x){
  x0 <- strsplit(x,'\n')[[1]]
  paste0(x0[!grepl('^\\s*;',x0)],collapse = '\n')
}


#' @title Import Utility for NONMEM xml files
#' @description Converts xml files to xml_document class
#' @param run character, run number(s) to parse
#' @param project character, root path of the NONMEM run(s)
#' @param rundir character, path containing the project, Default: file.path(project, run)
#' @return xml_document class
#' @examples 
#' read_nmlist(run = c('101'), project = system.file('streams',package = 'streamline'))
#' @seealso 
#' [read_xml][xml2::read_xml]
#' @rdname read_nmlist
#' @export 
#' @importFrom xml2 read_xml
read_nmlist <- function(run, project, rundir = file.path(project,run)) {
  xmlfile <- file.path(rundir, paste0(run, ".xml"))
  xml2::read_xml(xmlfile)
}

#' @importFrom stringi stri_count_regex
parse_mat_comment <- function(x){
  
  x_fixed <- grepl('FIX|SAME',gsub(';(.*?)$|\\(|\\)','',x))
  
  x[!grepl('\\;',x)] <- sprintf('%s;',x[!grepl('\\;',x)])
  
  x_str <- gsub('^\\s+|\\s+$','',gsub('^(.*?);','',x))
  
  x_str <- sapply(x_str,function(x){
    
    x_fixed_i <- grepl('FIX|SAME',gsub(';(.*?)$|\\(|\\)','',x))
    
    if(!nzchar(x)){
      if(x_fixed_i){
        x <- ' '
      }else{
        x <- '  '  
      }
      
    }else{
      if(!grepl('\\s',x)){
        if(grepl('\\[|\\]',x)){
          x <- sprintf('%s ',x)
        }else{
          x <- sprintf(' %s',x)
        }
      }  
    }
    
    x
    
  },
  USE.NAMES = FALSE,
  simplify = TRUE) 
  
  x_str <- sprintf('%s %s',c('NOFIXED','FIXED')[as.numeric(x_fixed)+1],x_str)
  x_str <- gsub('\\s','||',x_str)
  x_str <- gsub('NOFIXED','',x_str)
  
  one_col <- stringi::stri_count_regex(x_str,'||')==1
  x_str[one_col] <- sprintf('||%s',x_str[one_col])
  
  sort_comment(x_str)
  
}

parse_mat_numeric <- function(x){
  
  x_num   <- gsub(';(.*?)$|\\(|\\)','',x)
  
  x_num   <- gsub("FIXED|FIX|SAME",'',x_num)
  x_num   <- gsub('^\\s+|\\s+$','',x_num)
  
  x_num   <- strsplit(x_num,'\\s+')%>%
    unlist()%>%
    as.numeric()
  
  x_num
}


sort_comment <- function(x){
  
  sapply(strsplit(x,'\\|\\|'),function(y){
    
    fixed <- 1
    
    brackets <- which(grepl('\\[|\\]',y))
    
    comment <- paste0(y[-c(fixed,brackets)],collapse = ' ')
    
    if(length(brackets)==0){
      y <- c(y,'')
      brackets <- length(y)
    }
    
    new_str <- gsub('^\\s|\\s$','',c(y[fixed],y[brackets],comment))
    
    if(sum(!nzchar(new_str))==3){
      new_str <- c(new_str,'')
    }
    
    paste0(new_str,collapse ='||')  
  })
  
}
