comment_block_file_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  full_text <- adc$contents
  
  block_text <- ctl_block_file(full_text)
  
  rng <- rstudioapi::document_range(
    rstudioapi::document_position(1,1),
    rstudioapi::document_position(length(full_text),Inf)
    )
  
  rstudioapi::modifyRange(
    location = rng,
    text = block_text,
    id=adc$id
  )
  
  rstudioapi::setCursorPosition(
    position = rstudioapi::document_position(1,1),
    id = adc$id
  )
  
}

comment_block_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  
  old_text <- adc$selection[[1]]$text
  
  new_text <- ctl_block(old_text,align = 'c')
  
  if(grepl('^\\$',old_text))
    new_text <- sprintf('%s\n%s',new_text,old_text)
  
  rstudioapi::modifyRange(adc$selection[[1]]$range,new_text,id=adc$id)
  
}

comment_line_addin <- function(){
  
  adc <- rstudioapi::getSourceEditorContext()
  old_text <- strsplit(adc$selection[[1]]$text,'\n')[[1]]
  old_text <- gsub(';*|^;*-*|^;*-*;*$','',old_text)
  old_text <- gsub('\\s{2,}','',old_text)
  old_text <- old_text[nzchar(old_text)]
  new_text <- paste0(sprintf(';;; %s',old_text),collapse = '\n')
  rstudioapi::modifyRange(adc$selection[[1]]$range,new_text,id=adc$id)
  
}