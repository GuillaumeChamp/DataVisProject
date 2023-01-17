#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyr)
library(ggplot2)
library(data.table)
library(purrr)
library(scales)

df <- read.csv(text = readLines("data/header.txt", warn = TRUE),header=TRUE)
str(df)
df <- separate(df, col = coordinate, into = c("x_coordinate", "y_coordinate"), sep = ",")
str(df)
df$x_coordinate = as.numeric(as.character(df$x_coordinate))
df$y_coordinate = as.numeric(as.character(df$y_coordinate))
df$timestamp <- as.POSIXct(df$timestamp) #, tz = "UTC" 
df <- df[order(df$timestamp),]
min_time <- df$timestamp[1]
max_time <- df$timestamp[length(df$timestamp)]
min_canvas = 0
max_canvas = 999
canvas <- expand.grid(x = seq(min_canvas, max_canvas), y = seq(min_canvas, max_canvas))


computeActiveUsers <- function(x){
  tstart <- as.POSIXct(x) - 30*10
  tempodata <- subset(df,as.POSIXct(tstart) <= timestamp & timestamp <=as.POSIXct(x))
  return(
    length(unique(tempodata$user_id))
  )
}


ui <- fluidPage(
  sidebarPanel(
    sliderInput("range", "Time:",min = min_time, max = max_time, value = c(min_time,max_time)),
    sliderInput("xrange", "X axis:",min = min_canvas, max = max_canvas, value = c(min_canvas,max_canvas)),
    sliderInput("yrange", "Y axis:",min = min_canvas, max = max_canvas, value = c(min_canvas,max_canvas)),
    sliderInput("IdleTime", "Idle time out",min = 1, max = 10, value = 3),
    selectInput("color", 
                label = "Choose a color to display",
                choices = c("#000000", 
                            "#00756F",
                            "#009EAA",
                            "#00A368",
                            "#00CC78",
                            "#2450A4",
                            "#3690EA",
                            "#493AX1",
                            "#51E9F4",
                            "#6A5CFF",
                            "#6D482F",
                            "#7EED56",
                            "#811E9F",
                            "#898D90",
                            "#9C6926",
                            "#B44AC0",
                            "#BE0039",
                            "#D4D7D9",
                            "#FF3881",
                            "#FF4500",
                            "#FF99AA",
                            "#FFA800",
                            "#FFD635",
                            "#FFFFFF",
                            "All"),
                selected = "All"),
  ),
  
  fluidRow(
    column(6, align = "center", 
    plotOutput("heatMapPlot", width = "100%", height = 400),
    plotOutput("line")),
    column(6, align = "center", 
    plotOutput("canvasPlot", width = "100%", height = 400)),
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  data <- reactive({
    df_color <- if(input$color != 'All') subset(df, pixel_color == input$color) else data.frame(df)
    df_time <- subset(df_color, input$range[1] <= timestamp & timestamp <= input$range[2]) 
    df_scale <- subset(df_time, input$xrange[1] <= x_coordinate & x_coordinate <= input$xrange[2] & input$yrange[1] <= y_coordinate & y_coordinate <= input$yrange[2])
    df_scale <- rename(count(df_scale, x_coordinate, y_coordinate), Freq = n)
    canvas_freq <- data.frame(canvas)
    result <- canvas_freq %>% left_join(df_scale, 
                                        by=c('x'='x_coordinate', 
                                             'y'='y_coordinate'))
    result[is.na(result)] <- 0
    subset(result, input$xrange[1] <= x & x <= input$xrange[2] & input$yrange[1] <= y & y <= input$yrange[2])
  })
  
  data_canvas <- reactive({
    df_color <- if(input$color != 'All') subset(df, pixel_color == input$color) else data.frame(df)
    df_time <- subset(df_color, input$range[1] <= timestamp & timestamp <= input$range[2]) 
    df_scale <- subset(df_time, input$xrange[1] <= x_coordinate & x_coordinate <= input$xrange[2] & input$yrange[1] <= y_coordinate & y_coordinate <= input$yrange[2])
    df_reverse <- df_scale[order(df_scale$timestamp, decreasing = TRUE),]
    df_select <- df_reverse[,c("x_coordinate", "y_coordinate", "pixel_color")]
    df_distinct <- distinct(df_select, x_coordinate, y_coordinate, .keep_all = TRUE)
    
    canvas_img <- data.frame(canvas)
    result <- canvas_img %>% left_join(df_distinct, 
                                       by=c('x'='x_coordinate', 
                                            'y'='y_coordinate'))
    result[is.na(result)] <- as.character("#FFFFFF")
    subset(result, input$xrange[1] <= x & x <= input$xrange[2] & input$yrange[1] <= y & y <= input$yrange[2])
  })
  
  
  computePop <- reactive({
    tempdf <- data.frame(t = seq(input$range[1],input$range[2],by=60*input$IdleTime[1]))
    for (i in 1:length(tempdf$t)) {
      tempdf$activeUsers[i] <- computeActiveUsers(tempdf$t[i])
    }
    tempdf

  })
  
  output$line <- renderPlot({
    ggplot(computePop(), aes(t,activeUsers)) +
    geom_line() +
    scale_x_datetime(labels = date_format("%H:%M:%S"))
  })
  
  output$heatMapPlot <- renderPlot({
    ggplot(data(), aes(x, y, fill = data()$Freq)) + 
      geom_tile() +
      scale_fill_gradientn(colors = hcl.colors(10, "Reds")) +
      coord_fixed()
  })
  
  output$canvasPlot <- renderPlot({
    ggplot(data_canvas(), aes(x, y, colour = pixel_color)) + 
      geom_tile() +
      scale_colour_identity() +
      coord_fixed()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
