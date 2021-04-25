co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

co2_2000s_df <- co2_df %>%
  filter(iso_code != "", year >= 2000) %>%
  select(country, year, co2, co2_growth_prct, co2_per_capita)

co2_global_trend_df <- co2_2000s_df %>%
  group_by(year) %>%
  summarize(co2 = sum(co2, na.rm = TRUE))

my_ui <- fluidPage(
  titlePanel("21st Century Emissions"),

  sidebarLayout(
    sidebarPanel(
      h2(strong("Location and Time Period")),

      br(),

      selectInput(
        "country",
        "Country:",
        c(
          "All",
          unique(as.character(co2_2000s_df$country))
        )
      ),

      br(),

      sliderInput("year",
        "Year Range:",
        sep = "",
        min = 2000,
        max = 2019,
        value = c(2000, 2019)
      ),

      br(),

      textOutput(outputId = "message")
    ),

    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Table", dataTableOutput(outputId = "table")),
        tabPanel("Graph", plotOutput(outputId = "plot"))
      )
    )
  )
)
