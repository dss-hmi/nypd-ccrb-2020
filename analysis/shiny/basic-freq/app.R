#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# ---- load-libraies -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(TabularManifest)
library(shinythemes)

# ---- declare-globals ---------------------------------------------------------

# ---- load-data ---------------------------------------------------------------

ds0 <- readr::read_rds("../../../data-unshared/derived/dto.rds")

# ds0 %>% summarize_all(n_distinct) %>% t() %>% as.data.frame() %>%

# ds0 %>%
#     summarize_all(n_distinct) %>%
#     tidyr::pivot_longer(cols = names(.),names_to = "variable",  values_to = "n_unique") %>%
#     mutate(
#         case_when(n_uniuqe)
#     )
#     print(n = nrow(.))

# Define UI for application that draws a histogram
ui <- shinyUI(
    navbarPage(
        "NYPD-CCRB"
        ,theme = shinytheme("slate")
        ,tabPanel(
            "Data Explorer"
            ,sidebarLayout(
                sidebarPanel(
                    selectInput(
                        inputId  = "univariate_variable"
                        ,label = "Choose Variable"
                        ,choices = names(ds0)
                    )
                )
                ,mainPanel(
                    plotOutput("plot")

                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {

    graph <- reactive(
        {
            var <- input$univariate_variable

            d <- ds0 %>% select(all_of(var))


            if(typeof(pull(d)) %in% c("double","integer")){
                g <- d %>% TabularManifest::histogram_continuous(var)
            } else {
                g <- d %>% TabularManifest::histogram_discrete(var)
            }

            return(g)
        }
    )

    output$plot <- renderPlot({ graph()}, height = 900, width = 1200)
})

# Run the application
shinyApp(ui = ui, server = server)
