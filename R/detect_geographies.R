#' @title Detect geographies from messy text data or email addresses
#' @description Convert messy text data into standardized geographic identifiers such as countries, country codes, and continents.
#' @details 
#' This function standardizes messy text data that contains city, region, and/or country names 
#' as well as email domains into standardized geographic entities. The detect_geographies() function 
#' relies on a "funnel matching" method that unnests text and then reiterates over n-grams, matching 
#' all words sequences from n to n = 1 without much use of regular expressions or text cleaning. 
#' Currently, the function offers 14 output types, including countries, continents, flag emojis, and seven languages. 
#'
#' @param data A data frame or data frame extension (e.g. a tibble).
#' @param id A numeric or character vector unique to each entry.
#' @param input Character string giving the name of a column
#' in \code{data} that will be checked for geographic information.
#' This column can include the name of cities, states, 
#' and/or countries that will be standardized into country names or country codes.
#' @param output A character string specifying what kind of geographic output to extract.
#' Options include 'country', 'iso2', 'iso3', 'flag', 'continent', 'region', 'sub_region', 'int_region', 'country_arabic', 'country_chinese', 'country_french', 'country_russian', and 'country_spanish'.
#' @param email Optional character string giving the name of a column in \code{data}
#' containing email or email domain information.
#' @param cities Optional argument to detect major cities in each country. Defaults to TRUE.
#' @param demonyms Optional argument to detect demonyms of inhabitants of each country. Defaults to TRUE.
#'
#' @return An updated version of the input dataset (\code{data}),
#' with new columns added. A new column will be created with the naming structure
#' \code{"{output}_{input}"}. For example, if the user specified \code{input = "home"}
#' and \code{output = "country"}, then a new column named \code{"country_home"} will be created.
#' If the \code{email} parameter is used, then a new column will be created with the naming structure
#' \code{"{output}_email"}.
#' 
#' If an individual identified by \code{id} has
#' multiple countries detected based on the \code{input} variable,
#' then the output data will contain multiple rows for that value of \code{id}.
#' 
#' 
#' @examples
#'
#' library(tidyverse)
#' 
#' example_data <- data.frame(
#'   login      = c("colin_robinson", "jackie_daytona", "nadja"),
#'   email_addr = c("travelbug54@aol.com", "laszlo@cravensworth.uk", "nadja@gmail.com"),
#'   bio        = c("new york", "new york by way of england", "small greek island")
#' )
#'
#' example_data %>%
#'   detect_geographies(
#'     id     = login, 
#'     input  = "bio", 
#'     output = "country",
#'     email  = "email_addr"
#'   )
#'
#' @export
detect_geographies <- function(data, id, input, 
                               output = c("country", "iso_2", "iso_3", "flag", 
                                          "continent", "region", "sub_region", 
                                          "int_region", "country_chinese", "country_russian", 
                                          "country_french", "country_spanish", "country_arabic"), 
                              email = NULL, cities = TRUE, demonyms = TRUE) {
  pb <- progress::progress_bar$new(total = 100)
  pb$tick(0)
  # 2. convert all vars with enquos
  id <- dplyr::enquo(id)
  # --------------------------------------------UPDATED-----------------------------------------------

  # input <- dplyr::enquo(input)
  output <- rlang::arg_match(output)
  `%notin%` <- base::Negate(`%in%`)

  # Ensure input is a character vector of column names
  if (!is.character(input)) {
    stop("Error: 'input' must be a character vector of column names.")
  }

  dictionary <- diverstidy::countries_data

  # 1b. create all the conditions to detect countries, regions and cities 
  dictionary <- dictionary %>% 
    dplyr::rename(catch_terms = countries, recode_column = recode_countries)
  
  if (missing(id)) { 
    stop("Must specify an 'id' column in the data.")
  } else if (missing(input)) { 
    stop("'input' column requires character vector.")
  } else if (cities) {
    dictionary <- dictionary %>% 
      tidyr::unite(catch_terms, c("catch_terms", "cities"), sep="|") %>% 
      tidyr::unite(recode_column, c("recode_column", "recode_cities"), sep="|") %>% 
      dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
      dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  } else if (demonyms) {
    dictionary <- dictionary %>% 
      tidyr::unite(catch_terms, c("catch_terms", "demonyms"), sep="|") %>% 
      tidyr::unite(recode_column, c("recode_column", "recode_demonyms"), sep="|") %>% 
      dplyr::mutate(catch_terms = stringr::str_replace_all(catch_terms, "\\|NULL", "")) %>% 
      dplyr::mutate(recode_column = stringr::str_replace_all(recode_column, "\\|NULL", ""))
  }

  # 1d. prior to running the for loop, we need the max string length 
  max_n <- dictionary %>%
    tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
    dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) 
  max_n <- max(max_n$word_count) 
  
  # 3. drop missing, convert to lower case, convert foreign characters to english
  suppressMessages(string_corrections <- diverstidy::string_corrections)
  string_corrections <- string_corrections %>% 
    dplyr::mutate(recode_column = paste0(" ",recode_column," ")) %>%
    dplyr::select(original_string, recode_column) %>% tibble::deframe()

  # need to rename column if the original df has "country" as a column name 
  if ((output == "country") && ("country" %in% colnames(data))) {
    data <- plyr::rename(data, replace = c(country="country_original"), warn_missing = FALSE)
      warning("The original data frame contained the same name as your 'output' variable. The column will be renamed.")
  } else if ((output == "continent") && ("continent" %in% colnames(data))) {
    data <- plyr::rename(data, replace = c(continent="continent_original"), warn_missing = FALSE)
    warning("The original data frame contained the same name as your 'output' variable. The column will be renamed.")
  } # need to expand the rest of this as well  
  

  for (col in input) {
    pb$tick(1)
    # Create a temporary version of the column; if the value is "" or "<chr>" or NA, set to NA
    data <- data %>% 
      dplyr::mutate(!!paste0("temp_", col) := ifelse(
        .data[[col]] == "" | .data[[col]] == "<chr>" | is.na(.data[[col]]),
        NA_character_,
        as.character(.data[[col]])
      ))
    
    # Prepare a temporary dataframe with id and the input text for funnel matching
    temp_data <- data %>% dplyr::select(!!id, !!paste0("temp_", col))
    temp_data <- temp_data %>% dplyr::rename(input_text = !!paste0("temp_", col))
    
    # Clean the text
    temp_data <- temp_data %>% 
      dplyr::mutate(input_text = tolower(input_text),
                    input_text = stringr::str_replace_all(input_text, "/", " "),
                    input_text = stringr::str_replace_all(input_text, "\\.", " "),
                    input_text = stringr::str_replace_all(input_text, "·", " "),
                    input_text = stringr::str_replace_all(input_text, "_", " "),
                    input_text = stringr::str_replace_all(input_text, "\\b(:)\\b", " "),
                    input_text = stringr::str_replace_all(input_text, "u\\.s\\.", "united states"),
                    input_text = stringr::str_replace_all(input_text, "u\\.s\\.a\\.", "united states"),
                    input_text = stringr::str_replace_all(input_text, ",", " "))
    pb$tick(2)
    
    # Initialize an empty dataframe for funnelized matches
    funnelized <- data.frame()
    
    # Loop through n-grams (from max_n down to 2)
    for (n_word in max_n:2) {
      pb$tick(1)
      subdictionary <- dictionary %>%
        tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
        dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
        dplyr::filter(word_count == n_word)
      subdictionary <- stats::na.omit(subdictionary$catch_terms)
      
      temp_matches <- temp_data %>%
        tidytext::unnest_tokens(words, input_text, token = "ngrams", n = n_word, to_lower = TRUE) %>%
        dplyr::filter(words %in% subdictionary) %>%
        dplyr::select(!!id, words)
      
      funnelized <- dplyr::bind_rows(funnelized, temp_matches)
    }
    
    # Process single token matching
    pb$tick(1)
    subdictionary <- dictionary %>%
      tidyr::unnest_legacy(catch_terms = base::strsplit(catch_terms, "\\|")) %>%
      dplyr::mutate(word_count = lengths(base::strsplit(catch_terms, "\\W+"))) %>%
      dplyr::filter(word_count == 1)
    subdictionary <- stats::na.omit(subdictionary$catch_terms)
    
    temp_matches <- temp_data %>%
      tidytext::unnest_tokens(words, input_text, to_lower = TRUE) %>%
      dplyr::filter(words %in% subdictionary) %>%
      dplyr::select(!!id, words)
    
    funnelized <- dplyr::bind_rows(funnelized, temp_matches) %>% 
      dplyr::select(!!id, words)
    pb$tick(1)
    
    # Prepare a mapping dictionary: use the recode_column to create regex patterns
    dict_map <- dictionary %>%
      dplyr::mutate(original_string = paste0("\\b(?i)(", recode_column, ")\\b")) %>%
      dplyr::select(original_string, !!output) %>%
      tibble::deframe()
    pb$tick(1)
    
    # Function to obtain the standardized geo code
    get_geo_code <- function(word, dict_map) {
      regex_patterns <- tolower(names(dict_map))
      regex_patterns <- regex_patterns[!is.na(regex_patterns)]
      matched_indices <- stringr::str_detect(word, regex_patterns)
      matches <- dict_map[matched_indices]
      if (length(matches) > 0) {
        # Collapse multiple matches into a single string
        return(paste(unique(matches), collapse = "|"))
      } else {
        return(NA_character_)
      }
    }

    # Then, in your mapping step:
    all_matched_data <- funnelized %>%
      dplyr::mutate(geo_code = purrr::map_chr(words, ~ get_geo_code(.x, dict_map))) %>%
      dplyr::select(!!id, geo_code)
    pb$tick(2)
    
    # Define the list of valid geographies from countries_data
    geography_list <- c(stats::na.omit(countries_data$country), 
                        stats::na.omit(countries_data$iso_2),
                        stats::na.omit(countries_data$iso_3), 
                        stats::na.omit(countries_data$continent),
                        stats::na.omit(countries_data$flag), 
                        stats::na.omit(countries_data$region),
                        stats::na.omit(countries_data$sub_region), 
                        stats::na.omit(countries_data$int_region),
                        stats::na.omit(countries_data$country_arabic), 
                        stats::na.omit(countries_data$country_chinese), 
                        stats::na.omit(countries_data$country_french), 
                        stats::na.omit(countries_data$country_russian),
                        stats::na.omit(countries_data$country_spanish))
    
    # Aggregate matches by id and collapse multiple matches using "|"
    all_matched_data <- all_matched_data %>%
      dplyr::distinct() %>%
      dplyr::filter((is.na(geo_code) | geo_code %in% geography_list) & geo_code != "NA") %>%
      dplyr::group_by(!!id, geo_code) %>%
      dplyr::summarise(geo_code = paste(geo_code, collapse = "|"), .groups = "drop") %>%
      dplyr::mutate(geo_code = dplyr::na_if(geo_code, "NA"))
    
    # Rename the geo code column to include the input column name, e.g. "country_location"
    output_col <- paste0(output, "_", col)
    all_matched_data <- all_matched_data %>%
      dplyr::rename(!!output_col := geo_code)
    pb$tick(1)
    
    # Merge the result for this input column back into the main data frame
    data <- data %>% dplyr::left_join(all_matched_data, by = rlang::as_name(id))
    pb$tick(1)
    
    # Remove the temporary working column
    data <- data %>% dplyr::select(-dplyr::all_of(paste0("temp_", col)))
  }
  # 6. If email matching is desired, perform the email branch as before
  if (!is.null(email)) {
    if ((is.character(email)) && (length(email) == 1)) {
      if (!email %in% colnames(data)) {
        stop("The specified `email` variable is not in the data.")
      }
    } else {
      stop("The `email` argument must be a string giving the name of a variable in `data`.")
    }
    # Build the country dictionary as before
    country_dictionary <- diverstidy::countries_data %>%
      tidyr::drop_na(iso_domain) %>%
      tidyr::unnest_legacy(iso_domain = base::strsplit(iso_domain, "\\|")) %>% 
      dplyr::select(iso_domain, !!output)
    
    country_vector <- stats::na.omit(country_dictionary$iso_domain)
    
    country_dictionary <- country_dictionary %>%
      dplyr::mutate(beginning = "\\b(?i)(", ending = ")\\b", 
                    iso_domain = stringr::str_replace(iso_domain, "\\.", ""),
                    iso_domain = paste0(beginning, iso_domain, ending)) %>%
      dplyr::select(iso_domain, !!output) %>%
      tibble::deframe()
    
    matched_by_email <- data %>%
      tidyr::drop_na(.data[[email]]) %>%             # use the email column name directly
      dplyr::mutate(temp_email = tolower(.data[[email]])) %>%
      dplyr::mutate(domain = sub('.*@', '', .data[[email]])) %>%
      dplyr::mutate(domain = sub('.*\\.', '.', domain)) %>%
      dplyr::filter(domain %in% country_vector &
                      domain != ".ag" & domain != ".ai" & domain != ".am" &
                      domain != ".as" & domain != ".cc" & domain != ".fm" &
                      domain != ".io" & domain != ".im" & domain != ".me") %>%
      dplyr::mutate(domain = stringr::str_replace(domain, '\\.', ''))
    
    matched_by_email <- matched_by_email %>%
      dplyr::mutate(geo_code = stringr::str_replace_all(domain, country_dictionary)) %>%
      dplyr::select(!!id, geo_code) %>%
      dplyr::distinct() %>%
      dplyr::group_by(!!id, geo_code) %>%
      dplyr::summarise(geo_code = paste(geo_code, collapse = "|"), .groups = "drop") %>%
      dplyr::mutate(geo_code = dplyr::na_if(geo_code, "NA")) %>%
      dplyr::rename(!!paste0(output, "_email") := geo_code)
    
    data <- data %>% dplyr::left_join(matched_by_email, by = rlang::as_name(id))
  }
  pb$tick(1)
  return(data)
}
