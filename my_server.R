co2_df <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

co2_2000s_df <- co2_df %>%
  filter(iso_code != "", year >= 2000) %>%
  select(country, year, co2, co2_growth_prct, co2_per_capita)

co2_global_trend_df <- co2_2000s_df %>%
  group_by(year) %>%
  summarize(co2 = sum(co2, na.rm = TRUE))

my_server <- function(input, output) {

  # generate output table
  output$table <- renderDataTable(
    datatable(
      class = "cell-border stripe",
      rownames = FALSE,
      colnames = c("Country", "Year", "CO2 Emissions (Millions of Tonnes)", "CO2 Yearly Growth %", "CO2 Emissions per Capita"),
      {
        table <- co2_2000s_df

        if (input$country != "All") {
          table <- table[table$country == input$country, ]
        }

        # slider for table not working
        table <- table %>% filter(year >= input$year[1] & year <= input$year[2])

        table
      }
    )
  )

  output$plot <- renderPlot({
    if (input$country != "All") {
      co2_2000s_df <- co2_2000s_df[co2_2000s_df$country == input$country, ]
    }

    if (input$country == "All") {
      co2_2000s_df <- co2_global_trend_df
    }

    ggplot(data = co2_2000s_df) +
      geom_point(mapping = aes(
        x = year, y = co2
      ), color = "purple") +
      geom_line(mapping = aes(
        x = year, y = co2
      ), color = "purple") +
      scale_x_continuous(limits = input$year) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme_minimal() +
      labs(
        x = "Year",
        y = "CO2 Emissions (Millions of Tonnes)",
        title = "CO2 Emissions Over Time"
      )
  })

  output$message <- renderText({
    if (input$country == "All") {
      area <- "world wide"
    }

    if (input$country != "All") {
      area <- paste(input$country, "'s", sep = "")
    }

    year_text <- paste(min(input$year), "-", max(input$year), sep = "")

    message <- paste("The Emissions plot and table are displaying ", area, "CO2 emissions data in the year(s) ", year_text)

    message
  })
}
