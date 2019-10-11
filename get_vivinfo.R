get_vivinfo <- function(q){
  
  suppressWarnings({
    
    tryCatch({
      q = str_replace_all(q, "\\s+", "+")
      
      url = glue::glue("https://www.vivino.com/search/wines?q={q}")
      
      url = URLencode(url)
      
      html_data = xml2::read_html(url)
      
      # We only try the first result
      
      first_card = html_data %>% rvest::html_node(".card")
      
      wine_name = first_card %>%
        html_node(".bold") %>%
        html_text() %>%
        str_squish()
      
      wine_rating_price = first_card %>%
        html_nodes(".average__number") %>%
        html_text() %>%
        str_squish() %>%
        str_replace_all(",", ".")
      
      wine_rating = wine_rating_price[1]
      wine_price = wine_rating_price[2]
      
      wine_no_ratings = first_card %>%
        html_nodes(".text-micro") %>%
        html_text() %>%
        str_squish() %>%
        str_replace_all("[^[:digit:]]", "")
      
      wine_url = first_card %>%
        html_node("div.wine-card__image-wrapper a") %>%
        html_attr("href") %>%
        str_c("https://www.vivino.com", .)
      
      wine_image = first_card %>%
        html_node("div.wine-card__image-wrapper a") %>%
        str_extract("images.vivino.com.*\\)") %>%
        str_remove("\\)$")
      
      tibble(wine_name, wine_rating, wine_price, 
             wine_no_ratings, wine_url, wine_image)
      
    }, error=function(e){
      tibble(wine_name = NA_character_, wine_rating = NA_character_, wine_price = NA_character_, 
             wine_no_ratings = NA_character_, wine_url = NA_character_, wine_image = NA_character_)
    })
    
    
  })
  
}