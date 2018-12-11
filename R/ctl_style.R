ctl_style <- function(x){
  x0 <- strsplit(x,'\n')[[1]]
  x1 <- x0[!grepl('^\\s*;',x0)]
  x2 <- gsub('^\\s*$','',x1)
  x3 <- x2[nzchar(x2)]
  x4 <- gsub('^\\s+','',gsub('\\s+',' ',x3))
  x4[grepl('^[$]',x4)] <- sub('\\s','\n  ',x4[grepl('^[$]',x4)])
  x4[!grepl('^[$]',x4)] <- sprintf('  %s',x4[!grepl('^[$]',x4)])
  x_space <- ctl_space(x4)
  x_align <- ctl_align(x_space)
  x_collapse <- paste0(x_align,collapse = '\n')
  x_return <- gsub('\\n[$]','\n\n$',x_collapse)
  x_return
}

ctl_space <- function(x){
  x <- gsub('\\s+[=]|[=]\\s+','=',x)
  x <- gsub('=',' = ',x)
  x <- gsub('\\s+[,]|[,]\\s+',',',x)
  x <- gsub(',',', ',x)
  x <- gsub('\\s+[;]|[;]\\s+',';',x)
  gsub(';',' ; ',x)
}

ctl_align <- function(x){
  idxs <- grep('^\\$',x)
  y <- vector('numeric',length(x))
  y[idxs] <- 1 
  y <- cumsum(y)
  
  y1 <- lapply(split(x,y),function(x){
    x <- align(x,char = '=')
    x <- align(x,char = ';')
    x
  })
  
  unlist(y1,use.names = FALSE)
  
}

align <- function(x,char = '='){
  ydx <- regexpr(char,x)
  if(all(ydx==-1))
    return(x)
  idx_eq <- which(ydx!=-1)
  new_eq <- rep(char,length(ydx))
  new_eq[idx_eq] <- sprintf('%s%s',strrep(' ',max(ydx[idx_eq]) - ydx[idx_eq]),char)
  for(i in 1:length(ydx)){
    x[i] <- sub(char,new_eq[i],x[i])
  }
  x
}
