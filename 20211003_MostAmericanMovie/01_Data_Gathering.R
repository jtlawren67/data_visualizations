library(rvest) # Scrape Table From BoxOfficeMofo
library(tidyverse) # Data Manipulations
library(glue) # String Interpolation
library(httr) # Accessing the OMDB API
library(here) # Referencing Directory Structure

### Read in TOp 1000 Movies Globally by Lifetime Gross and Get IMDB IDs

tbl <- map_dfr(seq(0, 800, 200),
               function(x){
                 base <- glue("https://www.boxofficemojo.com/chart/ww_top_lifetime_gross/?offset={x}") %>%
                   read_html() %>% 
                   html_element('table')
                 
                   bind_cols(
                     #Get Actual Table Data
                     base %>% html_table(convert = F),
                     
                     #Get IMDB IDs From Links
                     imdb_id = base %>% 
                       html_elements('a') %>% 
                       html_attr('href') %>%
                       keep(~str_detect(.x, 'tt')) %>%
                       str_extract('tt\\d+')
                     
                   )
               })


###Clean Box Office Mojo Data
tbl_clean <- tbl %>% 
  janitor::clean_names() %>% 
  mutate(
    rank = parse_number(rank),
    worldwide_lifetime_gross = parse_number(worldwide_lifetime_gross),
    domestic_lifetime_gross = parse_number(domestic_lifetime_gross),
    domestic_percent = parse_number(domestic_percent)/100,
    foreign_lifetime_gross = parse_number(foreign_lifetime_gross),
    foreign_percent = parse_number(foreign_percent)/100,
    year = parse_number(year),
    # Developing a way to get the highest foreign percentages that also did well domestically
    foreign_score = (foreign_percent / domestic_percent)*log2(domestic_lifetime_gross)
  ) %>%
  arrange(foreign_score) %>%
  # Keep The Top 30 As Candidates for the API
  head(30)


###Use OMDB Data for the Country Filters and Poster Data
omdb_data <- map_dfr(tbl_clean$imdb_id,
                      function(id){
                        omdb_resp <- GET(URLencode(glue("https://www.omdbapi.com/?apikey={Sys.getenv('OMDB_API_KEY')}&i={id}&type=movie&r=json")))
                        if(content(omdb_resp)$Response == "True"){
                          return(
                            content(omdb_resp, as = 'parsed') %>% 
                              tibble(
                                imdb_id = id,
                                api_title = .$Title,
                                release_date = .$Released,
                                runtime = .$Runtime,
                                language = .$Language,
                                country = .$Country,
                                awards = .$Awards,
                                poster_url = .$Poster,
                                ratings_source = .$Ratings[[2]]$Source,
                                rating = .$Ratings[[2]]$Value,
                              ) %>% select(-.) %>% distinct() 
                          )
                        }
                      })

#Combine All Data
combine_dt <- tbl_clean %>% 
  inner_join(omdb_data, by = "imdb_id") %>%
  #Keep US Movies
  filter(str_detect(country, "United States")) %>%
  extract(awards, "num_oscars", "Won (\\d+) Oscar", remove = F, convert = T) %>%
  replace_na(list(num_oscars = 0))


#Output Processed Data
saveRDS(combine_dt, glue('{here::here()}/20211003_MostAmericanMovie/box_office_data.RDS'))
