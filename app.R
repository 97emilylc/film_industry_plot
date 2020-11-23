library(shinythemes)
library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(tidyverse)
library(dplyr)
library(stringr)
library(png)
library(shinyjs)
library(DT)
library(visNetwork)
library(rintrojs)
library(shinydashboard)
library(future)
library(sparklyr)

# SPARK
#sc <- spark_connect(master = "local", spark_version = "2.3.0")
#spark_df <- spark_read_csv(sc, "movies_with_focus_actors.csv")
#all_films_with_focus <- as.data.frame(spark_df)

top300actors <- read_csv("top300actors.csv")
top300actors['gender'] = "M"
top300actresses <- read_csv("top300actresses.csv")
top300actresses['gender'] = 'F'
both_actors <- rbind(top300actors, top300actresses)

all_names <- read_csv("all_names.csv", 
                      col_types = cols(X1 = col_skip()))
all_films_with_focus <- read_csv("movies_with_focus_actors.csv")

movies_actors <- read_csv("top30actorsmovies.csv")
movies_actresses <- read_csv("top30actressmovies.csv")

master_movies <- read_csv("master_movies.csv", 
                          col_types = cols(`Unnamed: 0` = col_skip(), 
                                           X1 = col_skip(), budget = col_skip(), 
                                           genres = col_skip(), homepage = col_skip(), 
                                           id = col_skip(), keywords = col_skip(), 
                                           original_language = col_skip(), original_title = col_skip(), 
                                           overview = col_skip(), production_companies = col_skip(), 
                                           production_countries = col_skip(), 
                                           spoken_languages = col_skip(), status = col_skip(), tagline = col_skip()))


shinyApp(
  ui = fluidPage(
    setBackgroundColor(color = "GhostWhite"),
    theme = shinytheme("united"),
    navbarPage(title = "DSCI551 Project",
      tabPanel("Home", icon = icon("home"),
               sidebarPanel(
                 includeHTML("intro_text.html"),
               ),
               mainPanel(
                 includeHTML("main.html")
               )),
      tabPanel("Actors",
               sidebarPanel(
                 selectInput("variable", "Plot Gender:",
                             c("Male" = "m",
                               "Female" = "f",
                               "Both" = "b")),
                 h4("Shown here is a plot of actor and actresses, their current ages against the number of films they have been attributed to in this particular data set."),
                 h4(""),
                 h4("Note that the scale has been kept the same for both groups in order to compare.")
                 ),
               mainPanel(
                 plotlyOutput("dot")),
               h2("These are the top 300 actors and actresses."),
               splitLayout(DT::dataTableOutput("actorTable"))
               
               
               ),
      tabPanel("Movies",
               sidebarPanel(
                 selectInput("gen",
                             "Genre:",
                             c("All",
                               unique(as.character(master_movies$genre_name)))),
                 selectInput("prod",
                             "Production Company:",
                             c("All",
                               unique(as.character(master_movies$production_company)))),
                 splitLayout(h4(strong("# of Films: ")), h4(textOutput("num_selected")))
                  ),
               #mainPanel(h4("stuff")),
               DT::dataTableOutput("master_movies_table")
                 
               ),
      tabPanel("Career Tables",
               sidebarPanel(
                 selectInput("career_male_table",
                             "Select Actor:",
                             c(unique(as.character(movies_actors$focus_actor)))),
                 selectInput("career_female_table",
                             "Select Actress:",
                             c(unique(as.character(movies_actresses$focus_actress)))),
               ),
               verticalLayout(
               h2(textOutput("result")),
               DT::dataTableOutput("selected_actor_table"), 
               h2(textOutput("result2")),
               DT::dataTableOutput("selected_actress_table"))),
      #splitLayout(DT::dataTableOutput("selected_actor_table"))
               
      tabPanel("Custom Charts",
               sidebarPanel(
                 selectizeInput(inputId = "career_search",
                           label = "Search for an actor or actress:",
                           choices = all_names[['0']],
                           multiple = FALSE,
                           options = list(maxItems = 1,
                                          openOnFocus = TRUE,
                                          placeholder = "Search for an actor")),
                 selectizeInput(inputId = "career_search2",
                                label = "Search for an actor or actress:",
                                choices = all_names[['0']],
                                multiple = FALSE,
                                options = list(maxItems = 1,
                                               openOnFocus = TRUE,
                                               placeholder = "Search for an actor"))),
               selectInput("y_value",
                           "Pick a Y-Axis Value:",
                           c("Box Office" = "revenue", "Vote Count" = "vote_count", "Vote Average" = "vote_average", "Popularity" = "popularity")),
               plotlyOutput("custom_bar"),
               plotlyOutput("custom_bar2")
               )
    )
  ),
  server = function(input, output) {
    ### CAREERS SELECTION
    
    ### CUSTOM TABLE ************************************************************************************************
    
      custom_search <- reactive(input$career_search)
      custom_search2 <- reactive(input$career_search2)
      y_value <- reactive(input$y_value)
    
      output$custom_bar <- renderPlotly({
        df <- all_films_with_focus[all_films_with_focus$focus_actor == custom_search(), ]
      
        
        if(y_value() =="revenue")
        {
          y_df <- df$revenue
        }
        else if(y_value() == "vote_count")
        {
          y_df <- df$vote_count
        }
        else if(y_value() == "vote_average")
        {
          y_df <- df$vote_average
        }
        else if(y_value() == "popularity")
        {
          y_df <- df$popularity
        }
        
          custom_fig <- plot_ly(
                                 x = df$title,
                                 y = y_df,
                                 type = "bar",
                                 text = df$age_at_release,
                                 color = df$genre_name,
                                 colors = "Set1"
          ) %>%
            layout(
              title = paste("Films Featuring ", custom_search(), ": Total: ", nrow(df)),
              xaxis = list(showticklabels = FALSE,
                           categoryorder = "array",
                           categoryarray = df$title,
                           title = "Career Timeline"),
              yaxis = list(title = y_value())
            )
          
        
        custom_fig
        
      })
      output$custom_bar2 <- renderPlotly({
        df <- all_films_with_focus[all_films_with_focus$focus_actor == custom_search2(), ]
        
        
        if(y_value() =="revenue")
        {
          y_df <- df$revenue
        }
        else if(y_value() == "vote_count")
        {
          y_df <- df$vote_count
        }
        else if(y_value() == "vote_average")
        {
          y_df <- df$vote_average
        }
        else if(y_value() == "popularity")
        {
          y_df <- df$popularity
        }
        
        custom_fig <- plot_ly(
          x = df$title,
          y = y_df,
          type = "bar",
          text = df$age_at_release,
          color = df$genre_name,
          colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", custom_search2(), ": Total: ", nrow(df)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = df$title,
                         title = "Career Timeline"),
            yaxis = list(title = y_value())
          )
        
        
        custom_fig
        
      })
      
      
   
    
    ### TABLE - ACTORS***********************************************************************************************
      actor_name <- reactive(input$career_male_table)
     output$selected_actor_table <- DT::renderDataTable(DT::datatable({
      actor_table <- movies_actors[movies_actors$focus_actor == actor_name(), ][,-2][, -1]
      actor_table
    },
    options = list(
      pageLength = 10, autoWidth = TRUE,
      columnDefs = list(list( targets = 8, width = '300px')),
      scrollX = TRUE
    )
    ))
     output$result <- renderText({
       paste(input$career_male_table, " Chart")
     })
    
    ### TABLE - ACTRESSES***********************************************************************************************
    actress_name <- reactive(input$career_female_table)
    
    output$selected_actress_table <- DT::renderDataTable(DT::datatable({
      actress_table <- movies_actresses[movies_actresses$focus_actress == actress_name(), ][,-2][, -1]
      actress_table
    },
    options = list(
      pageLength = 10, autoWidth = TRUE,
      columnDefs = list(list( targets = 8, width = '300px')),
      scrollX = TRUE
    )
    ))
    
    output$result2 <- renderText({
      paste(input$career_female_table, " Chart")
    })
    
    ### BAR CHART ************************************************************************************************
    search_name <- reactive({input$career_female})
    compare2 <- reactive({input$compare_value})
    
    output$career_bar2 <- renderPlotly({
      df <- movies_actresses[movies_actresses$focus_actress == search_name(), ]
      
      if(compare2() == "Box Office")
      {
        custom_fig <- plot_ly(data = df,
                              x = ~title,
                              y = ~revenue,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", search_name(), ": Total: ", nrow(df)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Box Office Gross")
          )
        
      }
      else if(compare2() == "Vote Count")
      {
        custom_fig <- plot_ly(data = df,
                              x = ~title,
                              y = ~vote_count,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", search_name(), ": Total: ", nrow(df)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Votes Cast")
          )
      }
      else if(compare2() == "Vote Average")
      {
        custom_fig <- plot_ly(data = df,
                              x = ~title,
                              y = ~vote_average,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", search_name(), ": Total: ", nrow(df)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Box Office Gross")
          )
      }
      else if (compare2() == "Popularity")
      {
        custom_fig <- plot_ly(data = df,
                              x = ~title,
                              y = ~popularity,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", search_name(), ": Total: ", nrow(df)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Film Popularity")
          )
      }

        custom_fig
      
    })
    ### BAR CHART ************************************************************************************************
    actor_in<-reactive({input$career_male})
    compare <- reactive({input$compare_value})
    
    output$career_bar1 <- renderPlotly({
      
      focus_data <- movies_actors[movies_actors$focus_actor == actor_in(), ]
      if(compare() == "Box Office")
      {
        career_fig <- plot_ly(data = focus_data,
                              x = ~title,
                              y = ~revenue,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", actor_in(), ": Total: ", nrow(focus_data)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Box Office Gross")
          )
        
      }
      else if(compare() == "Vote Count")
      {
        career_fig <- plot_ly(data = focus_data,
                              x = ~title,
                              y = ~vote_count,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", actor_in(), ": Total: ", nrow(focus_data)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Votes Cast")
          )
      }
      else if(compare() == "Vote Average")
      {
        career_fig <- plot_ly(data = focus_data,
                              x = ~title,
                              y = ~vote_average,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", actor_in(), ": Total: ", nrow(focus_data)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Box Office Gross")
          )
      }
      else if (compare() == "Popularity")
      {
        career_fig <- plot_ly(data = focus_data,
                              x = ~title,
                              y = ~popularity,
                              type = "bar",
                              text = ~age_at_release,
                              color = ~genre_name,
                              colors = "Set1"
        ) %>%
          layout(
            title = paste("Films Featuring ", actor_in(), ": Total: ", nrow(focus_data)),
            xaxis = list(showticklabels = FALSE,
                         categoryorder = "array",
                         categoryarray = ~title,
                         title = "Career Timeline"),
            yaxis = list(title = "Film Popularity")
          )
      }
      
      career_fig
    })
    
    x<-reactive({input$variable})
    output$master_movies_table <- DT::renderDataTable(DT::datatable({
      
      data1 <- master_movies
      output$num_selected <- renderText(nrow(data1))
      if (input$gen != "All") {
        data1 <- data1[data1$genre_name == input$gen,]
        output$num_selected <- renderText(nrow(data1))
      }
      if (input$prod != "All") {
        data1 <- data1[data1$production_company == input$prod,]
        output$num_selected <- renderText(nrow(data1))
      }
      #num_of_rows <- length(data1)
      data1
      #num_of_rows <- length(data1)

    },
    options = list(
      pageLength = 200, autoWidth = TRUE,
      columnDefs = list(list( targets = 8, width = '425px')),
      scrollX = TRUE
    )
    ))
    
    output$actorTable <- DT::renderDataTable({
      if (x() == "m")
      {
        DT::datatable(top300actors[c("Name", "Count", "isDead", "age")])
      }
      else if (x() == "f")
      {
        DT::datatable(top300actresses[c("Name", "Count", "isDead", "age")])
      }
      else
      {
        DT::datatable(both_actors[c("Name", "Count", "isDead", "age", "gender")])
      }
      
      })
    
    ### ACTORS PLOT **********************************
    output$dot <- renderPlotly({
    if (x() == "m")
    {
      fig1 <- plot_ly(top300actors, x = ~age, y = ~Count, text = ~Name, type = 'scatter', mode = 'markers',
                      opacity = .5, symbol = ~isDead, size = 75) %>% 
        layout(
          xaxis = list(range =c(20, 100)),
          yaxis = list(range = c(8, 60))
        )
    }
    else if (x() == "f")
    {
      fig2 <- plot_ly(top300actresses, x = ~age, y = ~Count, text = ~Name, type = 'scatter', mode = 'markers',
                      opacity = .5, symbol = ~isDead, size = 75) %>% 
        layout(
          xaxis = list(range = c(20, 100)),
          yaxis = list(range = c(8, 60)))
    }
    else
    {
      fig3 <- plot_ly(both_actors, x = ~age, y = ~Count, text = ~Name, type = 'scatter', mode = 'markers',
                      opacity = .5, symbol = ~isDead, size = 75, color = ~gender) %>% 
        layout(
          xaxis = list(range = c(20, 100),
          yaxis = list(range = c(8, 60))))
      
    }
    })
  }
)

