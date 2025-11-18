library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(DT)
library(stringr)
library(tidyr)

# Load data

prizes <- read_csv("prizes_clean.csv", show_col_types = FALSE)

prizes <- prizes |>
  mutate(
    prize_year  = as.integer(prize_year),
    prize_genre = str_to_title(prize_genre),
    highest_degree = replace_na(highest_degree, "Unknown"),
    degree_field_category = replace_na(degree_field_category, "Unknown"),
    ethnicity_macro = replace_na(ethnicity_macro, "Unknown"),
    gender = replace_na(gender, "Unknown")
  )

year_min <- min(prizes$prize_year, na.rm = TRUE)
year_max <- max(prizes$prize_year, na.rm = TRUE)
genre_choices <- sort(unique(prizes$prize_genre))
role_choices  <- sort(unique(prizes$person_role))
degree_choices <- sort(unique(prizes$highest_degree))

# UI

ui <- navbarPage(
  "British Literary Prizes Explorer",

  tabPanel(
    "Overview",
    fluidPage(
      br(),
      h3("Quick overview of the dataset"),
      br(),
      fluidRow(
        column(3, wellPanel(h4("Prize series"), strong(textOutput("n_prize_series")))),
        column(3, wellPanel(h4("Time span"), strong(textOutput("years_range")))),
        column(3, wellPanel(h4("Unique authors"), strong(textOutput("n_authors")))),
        column(3, wellPanel(h4("Unique books"), strong(textOutput("n_books"))))
      ),
      br(),
      fluidRow(
        column(6, h4("Counts by prize genre"), plotOutput("genre_bar", height = "350px")),
        column(6, h4("Counts by role"), plotOutput("role_bar", height = "350px"))
      )
    )
  ),

  tabPanel(
    "Prizes over time",
    sidebarLayout(
      sidebarPanel(
        sliderInput("year_time", "Prize year", min = year_min, max = year_max,
                    value = c(year_min, year_max), sep = ""),
        selectInput("genre_time", "Prize genre", choices = genre_choices,
                    selected = genre_choices, multiple = TRUE),
        checkboxGroupInput("role_time", "Person role",
                           choices = role_choices, selected = role_choices)
      ),
      mainPanel(
        h4("Number of prize records over time"),
        plotOutput("time_plot", height = "450px"),
        h4("Underlying data"),
        DTOutput("time_table")
      )
    )
  ),

  tabPanel(
    "Demographics",
    sidebarLayout(
      sidebarPanel(
        sliderInput("year_demo", "Prize year", min = year_min, max = year_max,
                    value = c(year_min, year_max), sep = ""),
        selectInput("genre_demo", "Prize genre",
                    choices = genre_choices, selected = genre_choices,
                    multiple = TRUE),
        checkboxGroupInput("role_demo", "Person role",
                           choices = role_choices, selected = role_choices),
        selectInput("demo_var", "Demographic variable", choices = c(
          "Gender" = "gender",
          "Ethnicity (macro)" = "ethnicity_macro",
          "UK residence" = "uk_residence"
        ))
      ),
      mainPanel(
        h4("Distribution of demographic groups"),
        plotOutput("demo_bar", height = "450px"),
        h4("Counts by group"),
        DTOutput("demo_table")
      )
    )
  ),

  tabPanel(
    "Education",
    sidebarLayout(
      sidebarPanel(
        sliderInput("year_edu", "Prize year", min = year_min, max = year_max,
                    value = c(year_min, year_max), sep = ""),
        selectInput("genre_edu", "Prize genre",
                    choices = genre_choices, selected = genre_choices,
                    multiple = TRUE),
        checkboxGroupInput("role_edu", "Person role",
                           choices = role_choices, selected = role_choices),
        checkboxGroupInput("degree_filter", "Highest degree",
                           choices = degree_choices, selected = degree_choices)
      ),
      mainPanel(
        h4("Highest degree vs. degree field"),
        plotOutput("degree_field_heatmap", height = "450px"),
        h4("Top 10 degree institutions"),
        plotOutput("top_institutions", height = "400px")
      )
    )
  ),

  tabPanel(
    "Data",
    fluidPage(
      h3("Full dataset"),
      DTOutput("data_table_full")
    )
  )
)

# SERVER

server <- function(input, output, session) {

  output$n_prize_series <- renderText(n_distinct(prizes$prize_alias))
  output$years_range <- renderText(paste(min(prizes$prize_year, na.rm=TRUE),
                                         max(prizes$prize_year, na.rm=TRUE),
                                         sep=" – "))
  output$n_authors <- renderText(
    if ("person_id" %in% names(prizes)) n_distinct(prizes$person_id)
    else n_distinct(paste(prizes$first_name, prizes$last_name))
  )
  output$n_books <- renderText(
    if ("book_id" %in% names(prizes)) n_distinct(prizes$book_id)
    else n_distinct(prizes$book_title)
  )

  output$genre_bar <- renderPlot({
    prizes |>
      count(prize_genre, sort=TRUE) |>
      ggplot(aes(x=reorder(prize_genre, n), y=n)) +
      geom_col() + coord_flip() +
      labs(title="Prize records by genre", x=NULL, y="Count") +
      theme_minimal()
  })

  output$role_bar <- renderPlot({
    prizes |>
      count(person_role, sort=TRUE) |>
      ggplot(aes(x=reorder(person_role, n), y=n)) +
      geom_col() + coord_flip() +
      labs(title="Prize records by role", x=NULL, y="Count") +
      theme_minimal()
  })

  filtered_time <- reactive({
    prizes |>
      filter(
        prize_year >= input$year_time[1],
        prize_year <= input$year_time[2],
        prize_genre %in% input$genre_time,
        person_role %in% input$role_time
      )
  })

  output$time_plot <- renderPlot({
    filtered_time() |>
      count(prize_year, prize_genre) |>
      ggplot(aes(prize_year, n, color=prize_genre)) +
      geom_line() + geom_point() +
      labs(title="Prizes over time", x="Year", y="Count", color="Genre") +
      theme_minimal()
  })

  output$time_table <- renderDT({
    filtered_time() |> 
      select(prize_year, prize_genre, person_role,
             prize_name, first_name, last_name, book_title) |>
      datatable(options=list(pageLength=15))
  })

  filtered_demo <- reactive({
    prizes |>
      filter(
        prize_year >= input$year_demo[1],
        prize_year <= input$year_demo[2],
        prize_genre %in% input$genre_demo,
        person_role %in% input$role_demo
      )
  })

  output$demo_bar <- renderPlot({
    demo <- input$demo_var
    filtered_demo() |>
      count(.data[[demo]]) |>
      mutate(p = n / sum(n)) |>
      ggplot(aes(x=reorder(.data[[demo]], p), y=p, fill=.data[[demo]])) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_y_continuous(labels=scales::percent) +
      labs(title=paste("Distribution of", demo), x=NULL, y="Percent") +
      theme_minimal()
  })

  output$demo_table <- renderDT({
    demo <- input$demo_var
    filtered_demo() |>
      count(.data[[demo]]) |>
      mutate(percent = round(100*n/sum(n), 1)) |>
      datatable(options=list(pageLength=10))
  })

  filtered_edu <- reactive({
    prizes |>
      filter(
        prize_year >= input$year_edu[1],
        prize_year <= input$year_edu[2],
        prize_genre %in% input$genre_edu,
        person_role %in% input$role_edu,
        highest_degree %in% input$degree_filter
      )
  })

  output$degree_field_heatmap <- renderPlot({
    df <- filtered_edu() |>
      filter(highest_degree!="Unknown", degree_field_category!="Unknown") |>
      count(degree_field_category, highest_degree)

    ggplot(df, aes(highest_degree, degree_field_category, fill=n)) +
      geom_tile() +
      scale_fill_continuous(type="viridis") +
      labs(title="Degree vs Field", x="Highest Degree", y="Field", fill="Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$top_institutions <- renderPlot({
    filtered_edu() |>
      filter(!is.na(degree_institution), degree_institution!="none") |>
      count(degree_institution, sort=TRUE) |>
      slice_head(n=10) |>
      ggplot(aes(x=reorder(degree_institution, n), y=n)) +
      geom_col() + coord_flip() +
      labs(title="Top 10 degree institutions", x=NULL, y="Count") +
      theme_minimal()
  })

  output$data_table_full <- renderDT(
    datatable(prizes, options=list(pageLength=20, scrollX=TRUE))
  )
}

# Run app
shinyApp(ui, server)
