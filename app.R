library(shiny)

library(dplyr)
library(glue)
library(stringr)
library(xml2)
library(jsonlite)
library(rvest)
library(tibble)
library(purrr)
library(tidyr)

library(DT)

source("get_vivinfo.R")

base_url = "https://api.etilbudsavis.dk/v2/offers/search?r_lat={r_lat}&r_lng={r_lng}&r_radius={r_radius}&query={query}&offset={offset}&limit={limit}"

# Define UI for application that draws a histogram
ui <- fluidPage(

    titlePanel("Tilbudsvin"),

    mainPanel(
        width = 12,
        DT::dataTableOutput("wine_table")
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    wine_df <- reactive({
        
        withProgress(message = "Henter vindata...", min = 0, max = 1, {


            setProgress(detail = "Henter tilbudsdata...", value = 0.1)

            url <- glue::glue(base_url, query = "vin",
                             r_lat =  55.6656223,
                             r_lng =  12.6009489,
                             r_radius = 4000,
                             offset = 0,
                             limit = 100)

            data <- jsonlite::fromJSON(url)

            vindata <- tibble(
                heading = data$heading,
                description = data$description,
                price = data$pricing$price,
                size = data$quantity$size$from,
                symbol = data$quantity$unit$symbol,
                pieces = data$quantity$pieces$from,
                store = data$branding$name,
                image = data$images$zoom
            )

            vindata <-
                vindata %>%
                mutate(size_cl = ifelse(symbol == "cl", size, size * 100)) %>%
                mutate(prize_piece = round(price / pieces,2)) %>%
                mutate(prize_cl = round(price / (pieces * size_cl),2)) %>%
                mutate(tilbud_image = glue::glue('<a href="{image}" target="_blank"><img border="0" src="{image}" height="100" width="auto"></a>')) %>%
                mutate(tilbuds_navn = heading) %>%
                mutate(heading = str_remove_all(heading, "[-, \\.]([hH]vid|[rR]ød|[rR]os[eé])-?(vin)?")) %>%
                mutate(heading = str_split(heading, ",|eller")) %>%
                unnest_legacy() %>%
                mutate(heading = str_squish(heading)) %>%
                filter(!(heading %in% c("", "-", ",")))

            setProgress(detail = "Henter Vivino data...")

            pb_scale <- scales::rescale(1:length(vindata$heading), to = c(0.2, 0.9))

            vivino <- purrr::map_dfr(vindata$heading, function(q){
                setProgress(value = pb_scale[which(q == vindata$heading)])
                get_vivinfo(q)
            })


            setProgress(detail = NULL, value = 1)

            wine_df <- bind_cols(vindata, vivino)

            wine_df <-
                wine_df %>%
                mutate(wine_rating = as.numeric(wine_rating),
                       wine_no_ratings = as.numeric(wine_no_ratings),
                       prize_cl = as.numeric(prize_cl),
                       prize_piece = as.numeric(prize_piece),
                       store = as.factor(store)) %>%
                mutate(rating_cl = round(wine_rating / prize_cl, 2)) %>%
                mutate(vivino_image = glue::glue('<a href="{wine_url}" target="_blank"><img border="0" src="http://{wine_image}" height="100" width="auto"></a>')) %>%
                mutate(description = paste0(tilbuds_navn, ".</br>", description)) %>%
                select(heading, description, store, prize_piece, wine_rating, wine_no_ratings, prize_cl, rating_cl, tilbud_image, vivino_image) %>%
                arrange(desc(wine_rating))

            names(wine_df) <- c("Navn", "Tilbudsbeskrivelse", "Butik", "Stk. pris", "Bedømmelse", "Antal bedømmelser", "Pris pr. cl.", "Bedømmelse pr. cl.", "", "")
            
        })
        
        return(wine_df)
        
    })
    
    
    output$wine_table <-
        DT::renderDataTable({
            datatable(
                wine_df(),
                rownames = FALSE,
                selection = "none",
                autoHideNavigation = TRUE,
                escape = FALSE,
                filter = "top",
                extensions = "Responsive",
                options = list(
                    language = list(
                        decimal = ",",
                        thousands = ".",
                        emptyTable = "Søgningen returnerede ingen vin",
                        info = "Viser _START_ til _END_ af ialt _TOTAL_ vin",
                        infoEmpty = "Viser 0 til 0 af 0 vin",
                        infoFiltered = "(filtreret fra _MAX_ samlede vin)",
                        lengthMenu = "Vis _MENU_ vin",
                        search = "S\u00F8g",
                        zeroRecords = "Der blev ikke fundet nogen matchende vin",
                        paginate = list(
                            "first" = "Første",
                            "last" = "Sidste",
                            "next" = "Næste",
                            "previous" = "Forrige"
                        )
                    ),
                    columnDefs = list(
                        list(responsivePriority = 0, targets = c(0, 3, 4)),
                        list(responsivePriority = 1, targets = c(2, 5)),
                        list(responsivePriority = 2, targets = c(6, 7)),
                        list(className = "dt-center", targets = 2:9),
                        list(searchable = FALSE, targets = c(8, 9)),
                        list(orderable = FALSE, targets = c(8, 9))
                    ))
            )
        })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
