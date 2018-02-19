#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(shinythemes)
library(utils)
library(dplyr)
library(ggplot2)
library(ggrepel) # label points


PCAtestdata <- read.delim("PCAtestdata.txt", stringsAsFactors = F)
PCAlist <-c("PCA1", "PCA2", "PCA3", "PCA4", "PCA5", "PCA6", "PCA7", "PCA8", "PCA9", "PCA10")
ethnicspecific <- c("African", "East Asian", "Southeast Asian", "East or Southeast Asian", "South Asian", "European", "Hispanic", "Melanesian", "East Polynesian",  "West Polynesian", "Niuean", "Pukapukan", "Polynesian", "Unspecified")


##################################
# Define UI 
ui = fluidPage(
  
  theme = shinytheme("spacelab"),
  headerPanel(h1('PCA clustering', align = "center")),
  fluidRow(
    column(width = 4,
           selectInput('xcol', 'X Variable Plot1', PCAlist),
           selectInput('ycol', 'Y Variable Plot1', PCAlist, selected=PCAlist[2]),
           selectInput('ethnicclass', 'Choose ethnic groups to include', choices = ethnicspecific, multiple = TRUE ),
           
           hr(),
           textInput("highlight_subjects", "Type in subject IDs to highlight on plot (comma separated)"),
           h4("Highlighted subject data"),
           tableOutput("subjectdata"),
           
           hr(),
           h4("Points near click in plot1"),
           tableOutput("click_info")
    ), 
    
    
    column(width = 8,
           plotOutput('plot1', width = "80%", height = "900px",
                      click = clickOpts(id = "plot1_click"),
                      brush = brushOpts(id = "plot1_brush")
           ),
           
           textInput("filenameplot1", "File name for download of Plot1", placeholder = "Plot1"),
           downloadButton('downloadDataPlot1', 'Download Data from Plot1'),
           downloadButton('downloadImagePlot1', 'Download Image from Plot1')
    )
  ),
  
  fluidRow(
    column(width = 12,
           h4("Brushed points from plot1 which are fed into plot2"),
           dataTableOutput("brush_info")
    )
  ),
  
  hr(),
  
  
  fluidRow(
    column(width = 4,
           selectInput('xcol2', 'X Variable Plot2', PCAlist, selected=PCAlist[3]),
           selectInput('ycol2', 'Y Variable Plot2', PCAlist, selected=PCAlist[4]),
           hr(),
           h4("Points near click for plot2"),
           tableOutput("click_info2")
    ),
    column(width = 8,
           plotOutput('plot2', width = "80%", height = "900px",
                      click = clickOpts(id = "plot2_click"),
                      brush = brushOpts(id = "plot2_brush")
           ),
           textInput("filenameplot2", "File name for download of Plot2", placeholder = "Plot2"),
           downloadButton('downloadDataPlot2', 'Download Data from Plot2'),
           downloadButton('downloadImagePlot2', 'Download Image from Plot2')
    )
  ),
  
  fluidRow(
    column(width = 12, 
           h4("Brushed points from plot2"),
           dataTableOutput("brush_info2")
    )
  )
)



####################################################################################################
server <- function(input, output, session) {
  
  
  # define the colour scale to be used for plotting
  eth_col_scale <- scale_colour_manual(values=c("African" = "#F8766D",
                                                "East Asian"="#619CFF",
                                                "Southeast Asian"="#00B0F6",
                                                "East or Southeast Asian" = "#1565C0",
                                                "South Asian"="#E58700",
                                                "European" = "#00BA38",
                                                "Hispanic"= "#009966",
                                                "Melanesian"="#6C2DC7",
                                                "East Polynesian"="#E9B000",
                                                "West Polynesian"="#B983FF",
                                                "Niuean"="#FF67A4",
                                                "Pukapukan"="#E76BF3",
                                                "Polynesian"="#00C0AF",
                                                "Unspecified"="#999999"),
                                       name="Ethnicities"
  )
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    eth <- input$ethnicclass
    if(is.null(eth)) eth <- ethnicspecific  
    return(PCAtestdata %>% 
             select(SUBJECT, ETH_DESCRIP, which(input$xcol == names(.)), which(input$ycol == names(.)), PCAETHBROAD, PCAETHSPECIFIC) %>% 
             filter(PCAETHSPECIFIC %in% eth))
  })
  

  highlight_sub <- reactive({ 
    subj <-gsub(pattern = " ", replacement = "", input$highlight_subjects)
    subjectlist <-as.data.frame(strsplit(subj, ","))
    colnames(subjectlist)[1]<-"SUBJECT"
    return(PCAtestdata %>% select(SUBJECT, ETH_DESCRIP, which(input$xcol == names(.)), which(input$ycol == names(.)), PCAETHSPECIFIC) %>% filter(SUBJECT %in% subjectlist$SUBJECT))
  })
  
  output$subjectdata <- renderTable({ highlight_sub()}, bordered = TRUE)
  
  plotInput <- reactive({
    dat <- selectedData()%>% dplyr::rename_(xcol = input$xcol, ycol = input$ycol)
    subj <-gsub(pattern = " ", replacement = "", input$highlight_subjects)
    subjectlist <-as.data.frame(strsplit(subj, ","))
    colnames(subjectlist)[1]<-"SUBJECT"
    highlight <- dat %>% filter(SUBJECT %in% subjectlist$SUBJECT)
    dat   %>% 
      ggplot(., aes(x = xcol, y = ycol, colour = PCAETHSPECIFIC)) + 
      geom_point() + 
      eth_col_scale+ # the colour scale defined further up
      xlab(input$xcol) +
      ylab(input$ycol) + 
      ylim(c(min(PCAtestdata[, input$ycol]), max(PCAtestdata[, input$ycol]) )) + 
      xlim(c(min(PCAtestdata[, input$xcol]), max(PCAtestdata[, input$xcol]) )) +
      theme(legend.position='bottom',
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.7)))+
      geom_point(data = highlight, shape = 18, colour = 'black', size = 5) + 
      geom_text_repel(data = highlight,aes(x = xcol, y = ycol,  label = SUBJECT), colour="black",fontface = 'bold', nudge_x = 0.002, nudge_y = 0.002 )
  })
  
  output$plot1 <- renderPlot({ 
    plotInput()
  })
  
  output$downloadDataPlot1 <- downloadHandler(
    filename = function() {paste(input$filenameplot1,input$xcol,input$ycol, '.csv', sep='')},
    content = function(file) {write.csv(selectedData(), file)})
  output$downloadImagePlot1 <- downloadHandler(
    filename = function() { paste(input$filenameplot1,input$xcol,input$ycol, '.png', sep='') },
    content = function(file) {ggsave(file,plotInput(), device='png', width = 12, height = 12, units = "in", dpi = 300)}) 
  
  output$click_info <- renderTable({
    dat <- selectedData()
    nearPoints(dat[,c("SUBJECT",input$xcol,input$ycol,"ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_click, xvar=input$xcol, yvar=input$ycol, addDist = FALSE)},
    bordered = TRUE
  )
  
  plot1brushselected<- reactive({
    dat <- selectedData()
    brushedPoints(dat[,c("SUBJECT",input$xcol,input$ycol, "ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_brush, xvar=input$xcol, yvar=input$ycol)})
  
  output$brush_info <- renderDataTable({ plot1brushselected()},
                                       #options = list(lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All')), pageLength = 10, autoWidth=FALSE)
                                       options = list(scrollY = "300px", scrollCollapse = TRUE, paging = FALSE, searching=TRUE) #for vertical scrolling
  )
  
  
  plot1brushselected2<- reactive({
    dat <- selectedData()
    dat2 <- PCAtestdata %>% filter(SUBJECT %in% dat$SUBJECT)
    return(brushedPoints(dat2[,c("SUBJECT",input$xcol,input$ycol,input$xcol2,input$ycol2, "ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot1_brush, xvar=input$xcol, yvar=input$ycol))
  })
  
  plotInput2 <- reactive({
    dat3 <- plot1brushselected2()%>% dplyr::rename_(xcol = input$xcol2, ycol = input$ycol2)
    subj <-gsub(pattern = " ", replacement = "", input$highlight_subjects)
    subjectlist <-as.data.frame(strsplit(subj, ","))
    colnames(subjectlist)[1]<-"SUBJECT"
    highlight <- dat3 %>% filter(SUBJECT %in% subjectlist$SUBJECT)
    dat3 %>%
      ggplot(., aes(x = xcol, y = ycol, colour = PCAETHSPECIFIC) ) +
      geom_point() +
      xlab(input$xcol2) +
      ylab(input$ycol2) +
      eth_col_scale +
      ylim(c(min(PCAtestdata[, input$ycol2]), max(PCAtestdata[, input$ycol2]) )) +
      xlim(c(min(PCAtestdata[, input$xcol2]), max(PCAtestdata[, input$xcol2]) )) +
      theme(legend.position='right',
            legend.text = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(1.7)))+
      geom_point(data = highlight, shape = 18, colour = 'black', size = 5) +
      geom_text_repel(data = highlight,aes(x = xcol, y = ycol,  label = SUBJECT), colour="black",fontface = 'bold', nudge_x = 0.002, nudge_y = 0.002 )
  })
  
  output$plot2 <- renderPlot({
    plotInput2()
  })  
  
  output$downloadDataPlot2 <- downloadHandler(
    filename = function() {paste(input$filenameplot2,input$xcol2,input$ycol2, '.csv', sep='')},
    content = function(file) {write.csv(plot1brushselected2(), file)})
  
  output$downloadImagePlot2 <- downloadHandler(
    filename = function() { paste(input$filenameplot2,input$xcol2,input$ycol2, '.png', sep='') },
    content = function(file) {ggsave(file,plotInput2(), device='png', width = 12, height = 12, units = "in", dpi = 300)})
  
  
  output$click_info2 <- renderTable({
    dat3 <- plot1brushselected2()
    nearPoints(dat3[,c("SUBJECT",input$xcol2,input$ycol2,"ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot2_click, xvar=input$xcol2, yvar=input$ycol2, addDist = FALSE)
  }, bordered = TRUE)
  
  plot2brushselected<- reactive({
    dat3 <- plot1brushselected2()
    brushedPoints(dat3[,c("SUBJECT",input$xcol2,input$ycol2,"ETH_DESCRIP","PCAETHBROAD","PCAETHSPECIFIC")], input$plot2_brush, xvar=input$xcol2, yvar=input$ycol2)
  })
  
  
  output$brush_info2 <- renderDataTable({ plot2brushselected() },
                                        options = list(lengthMenu = list(c(10, 25, 50, -1), c('10','25','50','All')), pageLength = 10, autoWidth=FALSE)
  )
  
} #end of server

# Run the application 
shinyApp(ui = ui, server = server)

