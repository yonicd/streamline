ctl_style <- function(x){
  x0 <- strsplit(x,'\n')[[1]]
  x1 <- x0[!grepl('^\\s*;',x0)]
  x2 <- gsub('^\\s*$','',x1)
  x3 <- x2[nzchar(x2)]
  x4 <- gsub('^\\s+','',gsub('\\s+',' ',x3))
  x4[grepl('^[$]',x4)] <- sub('\\s','\n  ',x4[grepl('^[$]',x4)])
  x4[!grepl('^[$]',x4)] <- sprintf('  %s',x4[!grepl('^[$]',x4)])
  x5 <- paste0(x4,collapse = '\n')
  ret <- gsub('\\n[$]','\n\n$',x5)
  
  ret
}
