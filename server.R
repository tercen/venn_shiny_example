#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# .libPaths("/srv/shiny-server/VennDiagram/libs")
gitversion <- function(){ 
  return("no_version")
}

library(tercen)
library(dplyr)

library(shiny)
library(VennDiagram)
# library(xlsx)
 
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  getCtx = reactive({
    # retreive url query parameters provided by tercen
    query = parseQueryString(session$clientData$url_search)
    
    token = query[["token"]]
    taskId = query[["taskId"]]
    
    # create a Tercen context object using the taskId and token
    ctx = tercenCtx(taskId=taskId, authToken=token)
    
    # for testing
    # options("tercen.workflowId"= "3aa8703da6b7534488c2f9632a0c0b0d")
    # options("tercen.stepId"= "11-6")
    # ctx = tercenCtx()
    
  })
   
  # reformat input data
  plot.data <- reactive({
    
    ctx = getCtx()
    
    args = as.list(ctx %>% cselect())
    args$sep='.'
    
    category.names = do.call(paste, args)
    
    if (length(category.names) < 1) stop('A column is required')
    if (length(category.names) > 5) stop('Too many columns, maximum 5 columns')
    
    data = ctx %>% select(.ci , .ri ) %>% 
      group_by(.ci) %>%
      summarise(.ris = list(.ri)) 
     
    names(data$.ris) = category.names
    data$.ris
  })
  
  # reformat input color
  plot.color <- reactive({
    venncolors <- input$venncolors
    color.tmp <- gsub(' ', '', venncolors)
    color.tmp <- unlist(strsplit(venncolors, ','))
  })
  output$testtext <- renderText({
    paste( length(rep(input$linetype, 2)))
  })
  
  # online plot
  output$venn <- renderPlot({
    VD <- venn.diagram(plot.data(), filename = NULL, fill = plot.color(), cex = input$num.fontsize,
                       margin = input$marginsize, cat.cex = input$cat.fontsize, 
                       cat.fontface = input$cat.face, fontface = input$cat.face,
                       cat.fontfamily = input$cat.font, fontfamily = input$cat.font, lwd = input$linewidth,
                       lty = rep(as.numeric(input$linetype), length(plot.data)))
    grid.draw(VD)
  })
  
  # table of objects in overlap
  output$overlap <- renderTable({
    if (input$table){
      ctx = getCtx()
      
      args = as.list(ctx %>% rselect())
      args$sep='.'
      
      feature.names = do.call(paste, args)
      
      OV = calculate.overlap(plot.data())
       
      OV = lapply(OV[sapply(OV, length) > 0], function(c) {
        sapply(c, function(i) feature.names[i+1])
      })
       
      n.obs <- sapply(OV, length)
      seq.max <- seq_len(max(n.obs))
      mat <- t(sapply(OV, "[", i = seq.max))
      data.frame(t(mat))
    } else {
      return(NULL)
    }
  })
  
  # to download plot
  output$downloadPlot <- downloadHandler(

    # specify file name
    filename = function(){
      paste0(input$outfile,".",gitversion(),'.pdf')
    },
    content = function(filename){
      # open device
      pdf(filename)

      # create plot
      VD <- venn.diagram(plot.data(), filename = NULL, fill = plot.color(), cex = input$num.fontsize,
                         margin = input$marginsize, cat.cex = input$cat.fontsize, 
                         cat.fontface = input$cat.face, fontface = input$cat.face,
                         cat.fontfamily = input$cat.font, fontfamily = input$cat.font, lwd = input$linewidth,
                         lty = rep(as.numeric(input$linetype), length(plot.data)))
      grid.draw(VD)

      # close device
      dev.off()
    }
  )
  output$downloadTable <- downloadHandler(
    # contentType = "text/csv",
    filename = function() {
      paste(input$outfile,".overlap.",gitversion(),".csv", sep = "")
    },
    content = function(file) {
      ctx = getCtx()
      
      args = as.list(ctx %>% rselect())
      args$sep='.'
      
      feature.names = do.call(paste, args)
      
      OV = calculate.overlap(plot.data())
      
      OV = lapply(OV[sapply(OV, length) > 0], function(c) {
        sapply(c, function(i) feature.names[i+1])
      })
      
      n.obs <- sapply(OV, length)
      seq.max <- seq_len(max(n.obs))
      mat <- t(sapply(OV, "[", i = seq.max))
      OV = data.frame(t(mat))
       
      write.csv(OV, file, row.names = FALSE, quote = FALSE, sep = '\t')
    }
  )
  
  output$appversion <- renderText ({ 
    paste0('App version: <b>',gitversion(),'</b>')
  })
})
