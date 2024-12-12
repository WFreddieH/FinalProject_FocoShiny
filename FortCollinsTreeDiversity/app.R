
library(shiny)
library(dplyr)
library(ggplot2)
library(viridis)
library(DT)
library(tmap)
library(bslib)
library(ggpubr)

load("focopoints.RData")
load("focotrees.RData")

tmap_mode("view")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "spacelab"),
  titlePanel("Fort Collins Tree Diversity"),
  sidebarLayout(
    sidebarPanel(
      p("This app displays the total tree diversity of publicly owned trees in Fort Collins, Colorado."),
      h4("Tree Genus Frequency"),
      DTOutput("genusTable"),
      sliderInput("numGenera",
                  "Number of Genera to Display:", 
                  min = 1, 
                  max = 78,
                  value = 5),
      radioButtons("chartType", 
                   "Select Displayed Data", 
                   choices = c("Trees Per Genus" = "bar", "Canopy Diversity" = "pie"),
                   selected = "bar")
    ),
    mainPanel(
      tmapOutput("focoMap"),
      br(), br(),
      plotOutput("genusPlot"),
      br(), br()
    )
  )
)
server <- function(input, output, session) {
  genus_count <- reactive({
    focotrees %>%
      group_by(Genus) %>%
      summarize(Count = n(), .groups = 'drop') %>%
      arrange(desc(Count))
  })
  selected_genera <- reactive({
    genus_count() %>%
      head(input$numGenera) %>%
      pull(Genus)
  })
  output$focoMap <- renderTmap({
    tm_shape(focopoints) +
      tm_dots(size = 0.1, col = "#D8BFD8", legend.show = FALSE) +
      tm_basemap("Esri.WorldImagery")
  })
  output$genusPlot <- renderPlot({
    data_to_plot <- genus_count() %>% head(input$numGenera)
    
    if (input$chartType == "bar") {
      # Bar chart
      ggplot(data_to_plot, aes(x = reorder(Genus, -Count), y = Count, fill = Genus)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(
          title = "Trees Per Genus in Fort Collins",
          x = "Genus",
          y = "Count"
        ) +
        scale_fill_viridis_d() +
        theme_pubr() +
        theme(legend.position = "none") 
    } else {
      # Pie chart
      total_count <- sum(data_to_plot$Count)
      total_other <- sum(genus_count()$Count) - total_count
      pie_data <- bind_rows(
        data_to_plot,
        data.frame(Genus = "Other Species", Count = total_other)
      )
      pie_data <- pie_data %>%
        mutate(
          ypos = cumsum(Count) - 0.5 * Count
        )
      color_palette <- c(setNames(viridis(nrow(data_to_plot)), data_to_plot$Genus), "Other Species" = "grey")
      ggplot(pie_data, aes(x = "", y = Count, fill = Genus)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar(theta = "y") +
        labs(
          title = "Percentage of Total Trees by Genus in Fort Collins",
          fill = "Genus"
        ) +
        scale_fill_manual(values = color_palette) +
        theme_pubr() +
        theme(legend.position = "left")
    }
  })
  output$genusTable <- renderDT({
    datatable(
      genus_count(),
      options = list(
        pageLength = 20,
        lengthChange = FALSE,
        scrollY = "300px"
      )
    )
  })
}

shinyApp(ui, server)