library(dplyr)

test_that("Small example works correctly", {

  example_data <- data.frame(
    login      = c("nandor", "colin_robinson", "jackie_daytona", "nadja"),
    email_addr = c("relentless@hotmail.com", "travelbug54@aol.com", "laszlo@cravensworth.uk", "nadja@gmail.com"),
    home       = c("staten island", "new york", "new york by way of england", "small greek island"),
    bio        = c("from iran", "from america", "from england- i mean america", "from greece")
  )

  result <- example_data %>%
    detect_geographies(
      id     = login, 
      input  = c("home", "bio"), 
      output = "country",
      email  = "email_addr",
      demonyms = FALSE
    )
  
  expect_equal(result[['country_home']], c(
    NA_character_, "United States", "United Kingdom", "United Kingdom", "United States", "United States", NA_character_
  ))
  expect_equal(result[['country_email']], c(
    NA_character_, NA_character_, "United Kingdom", "United Kingdom", "United Kingdom", "United Kingdom", NA_character_
  ))
  expect_equal(result[['country_bio']], c(
    "Iran", "United States", "United Kingdom", "United States", "United Kingdom", "United States", "Greece"
  ))
  
})

test_that("Expected results from small sample of GitHub data", {

  expected_outputs <- dplyr::tribble(
    ~login,          ~location,                                           ~country_location,        
    "1",             "Liechtenstein, Switzerland",                        "Liechtenstein",         
    "1",             "Liechtenstein, Switzerland",                        "Switzerland",           
    "2",             "UAE, Qatar, Oman, Saudi, Kuwait, Bahrain",          "Bahrain",               
    "2",             "UAE, Qatar, Oman, Saudi, Kuwait, Bahrain",          "Kuwait",                
    "2",             "UAE, Qatar, Oman, Saudi, Kuwait, Bahrain",          "Oman",                  
    "2",             "UAE, Qatar, Oman, Saudi, Kuwait, Bahrain",          "Qatar",                 
    "2",             "UAE, Qatar, Oman, Saudi, Kuwait, Bahrain",          "United Arab Emirates",  
    "3",             "Tahiti, French Polynesia, Polynésie française",     "French Polynesia",      
    "4",             "Lima, Perú",                                        "Peru",                  
    "5",             "Banja Luka, Serb Republic, Bosnia and Herzegovina", "Bosnia and Herzegovina",
    "6",             "Munich",                                            "Germany",
    "7",             "Berlin",                                            "Germany"
  )

  inputs <- expected_outputs |> distinct(login, location, .keep_all = FALSE)

  result <- inputs %>%
    detect_geographies(
      id     = login, 
      input  = c("location"), 
      output = "country",
      demonyms = FALSE
    )
  
  expect_equal(
    object = anti_join(
      x = expected_outputs,
      y = result,
      by = c("login", "location", "country_location")
    ) |> nrow(),
    expected = 0
  )
  
  expect_equal(
    object = anti_join(
      x = result,
      y = expected_outputs,
      by = c("login", "location", "country_location")
    ) |> nrow(),
    expected = 0
  )

})
