#' @title Reactive Validation
#' @description NONMEM Control Stream Comment Validation
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  validate_comments()
#'  }
#' }
#' @rdname validate_comments
#' @export 
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @import shiny
#' @importFrom shinyAce aceEditor updateAceEditor
#' @import tidynm
#' @importFrom dplyr select rename mutate left_join
#' @importFrom tidyr separate
validate_comments <- function(
  project = system.file('extdata',package = 'tidynm')
  ) {
  
  PARAMS <- c('THETA','OMEGA','SIGMA')
  
  ui <- miniUI::miniPage(
            miniUI::gadgetTitleBar(
              title = '',
              left = miniUI::miniTitleBarButton("qt", "Quit")
            ),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::fileInput('file','Control Stream File')
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel('Control Stream',
                            shinyAce::aceEditor(
                              outputId = 'ctl_text',
                              value = paste0(sprintf('$%s',PARAMS),collapse = '\n\n'),
                              mode='fortran',
                              wordWrap = TRUE,
                              debounce = 10,
                              height = '600px')
                            ),
            shiny::tabPanel('Validation',
                            shiny::radioButtons(
                              inputId = 'type',
                              label = 'Statistic',
                              choices = PARAMS,
                              selected = 'THETA',
                              inline = TRUE),
                            shiny::dataTableOutput('param_table'))
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {

    shiny::observeEvent(input$file,{
        shinyAce::updateAceEditor(
          session = session,
          editorId = 'ctl_text',
          value = readLines(input$file$datapath)%>%
            paste0(collapse = '\n')%>%
            ctl_style()%>%
            ctl_block_file()
        )
      })
    
    clean_reactive <- shiny::eventReactive(c(input$file,input$ctl_text),{
      input$ctl_text%>%clean_ctl()
    })
    
    shiny::observeEvent(c(input$file,input$ctl_text),{
      clean <- clean_reactive()
      new_values <- intersect(PARAMS,names(clean))
      shiny::updateRadioButtons(session,inputId = 'type',label = 'Statistic',choices = new_values,selected = new_values[1],inline = TRUE)  
    })
    
    
    shiny::observeEvent(c(input$type,input$ctl_text),{

      output$param_table <- shiny::renderDataTable({
        
        clean <- clean_reactive()
        
        ret <- NULL

        THIS_PARAMS <- intersect(names(clean),PARAMS)

        if(!input$type%in%THIS_PARAMS)
          return(ret)
        
        if(input$type=='THETA'){

          if(clean[[input$type]]%in%THIS_PARAMS)
            return(ret)
          
          ret <- clean[[input$type]]%>%
            tidynm::parse_theta()%>%
            dplyr::select(-Var2)%>%
            dplyr::rename(ROW = Var1,LOWER = LB, UPPER = UB)
        }
        
        if(input$type%in%c('OMEGA','SIGMA')){
          
          if(any(clean[[input$type]]%in%THIS_PARAMS))
            return(ret)

            ret_comment <- clean[[input$type]]%>%
              tidynm::ctl_to_mat(type='comment')
            
            if(inherits(ret_comment,'list'))
              ret_comment <- ret_comment%>%combine_blocks()

            ret_comment <- ret_comment%>%
              tidynm::gather()%>%
              dplyr::mutate(
                value = ifelse(nzchar(value),value,strrep('|',6)),
                value = gsub(strrep('\\|',6),strrep('|',4),value)
                )%>%
              tidyr::separate(value,c('FIXED','TYPE','LABEL'),sep=strrep('\\|',2),fill='left')%>%
              dplyr::rename(ROW = Var1,COL = Var2)
            
            ret_value <- clean[[input$type]]%>%
              tidynm::ctl_to_mat()
            
            if(inherits(ret_value,'list'))
              ret_value <- ret_value%>%combine_blocks()
            
            ret_value <- ret_value%>%
              tidynm::gather()%>%
              dplyr::rename(ROW = Var1,COL = Var2,VALUE=value)
            
            ret <- ret_value%>%dplyr::left_join(ret_comment,by=c('ROW','COL'))

        }
        
        ret
        
      },
      options = list( 
        scrollY = '600px', 
        paging = FALSE 
      ))  
    })
      
    shiny::observeEvent(input$qt, {
      shiny::stopApp()
    })
    
  }
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
}
  

clean_ctl <- get('clean_ctl',envir = asNamespace('tidynm'))
combine_blocks <- get('combine_blocks',envir = asNamespace('tidynm'))