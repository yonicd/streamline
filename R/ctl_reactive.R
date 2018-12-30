#' @title Reactive Validation
#' @description NONMEM Control Stream Comment Validation
#' @param project character, path to project containing runs, 
#'   Default: system.file('extdata',package = 'tidynm')
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  validate_comments()
#'  }
#' }
#' @rdname validate_comments
#' @export 
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @importFrom shiny sidebarLayout sidebarPanel textInput uiOutput actionButton mainPanel tabsetPanel tabPanel radioButtons dataTableOutput renderUI selectInput eventReactive observeEvent renderDataTable stopApp runGadget paneViewer
#' @importFrom shinyAce aceEditor updateAceEditor
#' @importFrom tidynm read_nmlist parse_theta gather
#' @importFrom xml2 xml_find_first xml_text
#' @importFrom dplyr select rename mutate
#' @importFrom tidyr separate
validate_comments <- function(
  project = system.file('extdata',package = 'tidynm')
  ) {
  ui <- miniUI::miniPage(
            miniUI::gadgetTitleBar(
              title = '',
              left = miniUI::miniTitleBarButton("qt", "Quit")
            ),
    miniUI::miniContentPanel(
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shiny::textInput(
            inputId = 'project',
            label = 'Project Path',
            value = project,
            placeholder = 'Path to NONMEM project'),
          shiny::uiOutput('run'),
          shiny::actionButton('read','Read')
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel('Control Stream',
                            shinyAce::aceEditor(
                              outputId = 'ctl_text',
                              value = '',
                              mode='r',
                              wordWrap = TRUE,
                              debounce = 10)
                            ),
            shiny::tabPanel('Validation',
                            shiny::radioButtons(
                              inputId = 'type',
                              label = 'Statistic',
                              choices = c('THETA',
                                          'OMEGA',
                                          'SIGMA'),
                              selected = 'THETA',
                              inline = TRUE),
                            shiny::dataTableOutput('param_table'))
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
  
      output$run <- shiny::renderUI({
        
        runs <- basename(list.dirs(input$project)[-1])
        
        shiny::selectInput(
          inputId = 'run',
          label = 'Run',
          choices = runs,
          selected = runs[1])
      })  
    
    
    ctl <- shiny::eventReactive(input$read,{
      tidynm::read_nmlist(input$run, input$project)%>%
        xml2::xml_find_first('.//nm:control_stream')%>%
        xml2::xml_text()
    })
   
    shiny::observeEvent(c(input$type,input$read),{
        shinyAce::updateAceEditor(
          session = session,
          editorId = 'ctl_text',
          value = ctl()%>%
            ctl_style()
        )
      })
    
        shiny::observeEvent(c(input$type,input$read,input$ctl_text),{
          output$param_table <- shiny::renderDataTable({

            ret <- NULL
            
            clean <- input$ctl_text%>%
              tidynm:::clean_ctl()
            
            if(input$type=='THETA'){
              ret <- clean[[input$type]]%>%
                tidynm::parse_theta()%>%
                dplyr::select(-Var2)%>%
                dplyr::rename(ROW = Var1,LOWER = LB, UPPER = UB)
            }
            
            if(input$type%in%c('OMEGA','SIGMA')){
              suppressWarnings({
                ret <- clean[[input$type]]%>%
                  ctl_to_mat(type='comment')%>%
                  tidynm::gather()%>%
                  dplyr::mutate(value=ifelse(nzchar(value),value,'||||||'))%>%
                  tidyr::separate(value,c('FIXED','TYPE','LABEL'),sep='\\|\\|')%>%
                  dplyr::rename(ROW = Var1,COL = Var2)
              })
              
            }
            
            ret
            
          })  
        })
      
    shiny::observeEvent(input$qt, {
      shiny::stopApp()
    })
  }
  
  shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
}
  