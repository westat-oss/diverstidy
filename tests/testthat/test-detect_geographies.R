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
