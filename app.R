suppressMessages(library(tidyverse))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(tm))
suppressMessages(library(wordcloud))
suppressMessages(library(ggthemes))
suppressMessages(library(RColorBrewer))
suppressMessages(library(lubridate))




ui <- navbarPage("Whastapp Vizualizer",
                 tabPanel("Import",
                          tags$a(href="https://quantumofcosmos.netlify.app/", tags$h1("Quantum of cosmos")) ,
                          sidebarPanel(fileInput("file", "WhatsApp chat",
                                                 multiple = TRUE,
                                                 accept = c(".txt",
                                                            ".zip")),
                                       "Whatsapp chat history can be exported from options of the chat. File is expected in txt format.
                                       Uploaded files are not saved on the server.",br(),br(),
                                       "Continue to Analyze tab after uploading the chat history file.")),
                 
                 tabPanel("Analyze",conditionalPanel(condition = "output.Choose_Users", 
                          titlePanel("Whastapp Vizualizer"),
                          sidebarPanel(uiOutput("Choose_Users"),"select participants to analyze"),
                          titlePanel(""),
                          mainPanel(tabsetPanel(type = "tabs",
                                                tabPanel("Wordcloud",h1("Word Cloud Of words used in the conversation"), plotOutput("plot")),
                                                tabPanel("When - Distribution over day", plotOutput("overDayPlot"),br(),plotOutput("overHourPlot"),div("Wrapping the day over at 04:00 AM", align = "center")),
                                                tabPanel("What - length and number of messages",htmlOutput("lengthtext")),
                                                tabPanel("Table", dataTableOutput("table"))
                                                )
                                    )
                          )
                 ))


server <- function(input, output, session) {
  whatsapp_chat <- eventReactive(input$file,{
    rwhatsapp::rwa_read(input$file$datapath)})
  output$Choose_Users <- renderUI(
    pickerInput("users","select users",choices = whatsapp_chat() %>% distinct(author) %>% drop_na() %>% pull() %>% as.character(),
                multiple = TRUE, options = list(`actions-box` = TRUE)))
  source("cloud_prep.R")
  avglen <- reactive({lengthprep(whatsapp_chat(),input$users)})
  output$data <- renderText(input$variable)
  output$plot <- renderPlot({
    df <- cloud_prep(whatsapp_chat())
    wordcloud(words = df$word,
              freq = df$freq,
              min.freq = 2,
              scale=c(4.5,.25),
              max.words=200,
              random.order=FALSE,
              rot.per=0.35,
              colors=brewer.pal(8, "Dark2")
              )
    })
  
  output$lengthtext <- renderUI({
    emg <- avglen()
    HTML(emg)
  })
  
  output$lengthPlot <- renderPlot({
    whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% 
    ggplot(., aes(x = hour(time), fill = author)) + 
      stat_count(position = "stack", show.legend = TRUE) + 
      ggtitle("Conversations per Hour") + ylab("# of messages") + 
      xlab("time") + 
      theme(plot.title = element_text(face = "italic"))+ 
      scale_x_continuous(breaks=seq(0,23,1))
  })
  
  output$overDayPlot <- renderPlot({
    whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% 
    ggplot(., aes(x = mday(time), fill = author)) + 
      stat_count(position = "stack", show.legend = TRUE) + 
      ggtitle("Average Messages per day") + ylab("# of messages") + 
      xlab("Date") + 
      theme(plot.title = element_text(face = "italic"))+ 
      scale_x_continuous(breaks=seq(0,31,1))
  })
  
  output$overHourPlot <- renderPlot({
    box_data <- whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% mutate(hour=hour(time)) %>% mutate(hour = ifelse(hour<4,hour+24,hour)) %>% group_by(author,hour) %>% tally()
    ggplot(box_data)+ 
      geom_boxplot(aes(x=author,y=hour,fill=author)) + 
      geom_point(aes(x=author,y=hour,size=n),color="grey40") +
      ylab("Hour of the Day") +scale_y_continuous(breaks=seq(0,28,1))+ 
      xlab("Participant")+theme(axis.text.x = element_text(angle = 45))
  })
  
  output$table <- renderDataTable(whatsapp_chat() %>% drop_na() %>% filter(author %in% input$users) %>% select(-source) %>% mutate(time = as.character(time)))
  outputOptions(output, "Choose_Users", suspendWhenHidden = FALSE)
}

shinyApp(ui, server)