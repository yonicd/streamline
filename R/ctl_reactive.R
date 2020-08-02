#' @title Reactive Validation
#' @description NONMEM Control Stream Comment Validation
#' @param project Path to NONMEM project 
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
#' @importFrom dplyr select rename mutate left_join
#' @importFrom tidyr separate
#' @importFrom rlang sym
#' @importFrom shinyFiles shinyFilesButton shinyFileChoose parseFilePaths
validate_comments <- function(
  project = system.file('streams',package = 'streamline')
  ) {
  
  PARAMS  <- c('THETA','OMEGA','SIGMA')
  volumes <- c('Current Directory' = getwd(),
               Examples = project,
               User = '~',
               shinyFiles::getVolumes()()
              )
  
  ui <- miniUI::miniPage(
            miniUI::gadgetTitleBar(
              title = '',
              left = miniUI::miniTitleBarButton("qt", "Quit")
            ),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shinyFiles::shinyFilesButton(id = "files",label =  "File select",title =  "Please select a control stream file", multiple = FALSE),
          shinyFiles::shinySaveButton(id = "save",label =  "Save to File",title =  "Save Content to File")
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel('Control Stream',
                            shinyAce::aceEditor(
                              outputId = 'ctl_text',
                              value = paste0(sprintf('$%s',PARAMS),collapse = '\n\n'),
                              mode="assembly_x86",
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

    shinyFiles::shinyFileChoose(input = input,id =  "files", roots = volumes, session = session)
    shinyFiles::shinyFileSave(input = input,id = 'save',session = session,roots = volumes)
    
    shiny::observeEvent(input$files,{
      if(is.list(input$files)){
        
        thisfile <- shinyFiles::parseFilePaths(roots = volumes,selection = input$files)

        shinyAce::updateAceEditor(
          session = session,
          editorId = 'ctl_text',
          value = readLines(thisfile$datapath)%>%
            paste0(collapse = '\n')%>%
            ctl_style()%>%
            ctl_block_file()
        )    
      }
    })
    
    clean_reactive <- shiny::eventReactive(c(input$files,input$ctl_text),{
      input$ctl_text%>%clean_ctl()
    })
    
    shiny::observeEvent(c(input$files,input$ctl_text),{
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
            parse_theta()%>%
            dplyr::select(-!!rlang::sym('Var2'))%>%
            dplyr::rename(ROW = !!rlang::sym('Var1'),
                          LOWER = !!rlang::sym('LB'), 
                          UPPER = !!rlang::sym('UB'))
        }
        
        if(input$type%in%c('OMEGA','SIGMA')){
          
          if(any(clean[[input$type]]%in%THIS_PARAMS))
            return(ret)

            ret_comment <- clean[[input$type]]%>%
              ctl_to_mat(type='comment')
            
            if(inherits(ret_comment,'list'))
              ret_comment <- ret_comment%>%combine_blocks()

            ret_comment <- ret_comment%>%
              gather()%>%
              dplyr::mutate(
                value = ifelse(nzchar(!!rlang::sym('value')),
                               !!rlang::sym('value'),
                               strrep('|',6)),
                value = gsub(strrep('\\|',6),strrep('|',4),!!rlang::sym('value'))
                )%>%
              tidyr::separate(!!rlang::sym('value'),c('FIXED','TYPE','LABEL'),
                              sep = strrep('\\|',2),fill='left')%>%
              dplyr::rename(ROW = !!rlang::sym('Var1'),COL = !!rlang::sym('Var2'))
            
            ret_value <- clean[[input$type]]%>%
              ctl_to_mat()
            
            if(inherits(ret_value,'list'))
              ret_value <- ret_value%>%combine_blocks()
            
            ret_value <- ret_value%>%
              gather()%>%
              dplyr::rename(ROW = !!rlang::sym('Var1'),
                            COL = !!rlang::sym('Var2'),
                            VALUE = !!rlang::sym('value'))
            
            ret <- ret_value%>%
              dplyr::left_join(ret_comment,by=c('ROW','COL'))%>%
              dplyr::mutate(
                TYPE = ifelse(is.na(!!(rlang::sym('TYPE')))|!nzchar(!!(rlang::sym('TYPE'))), "[A]", !!(rlang::sym('TYPE')))
              )%>%
              nm_create()
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
