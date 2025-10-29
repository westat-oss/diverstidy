library(dplyr)

test_that("Small example works correctly", {

  example_data <- data.frame(
    login      = c("nandor", "colin_robinson", "jackie_daytona", "nadja"),
    email_addr = c("relentless@hotmail.com", "travelbug54@aol.com", "laszlo@cravensworth.uk", "nadja@gmail.com"),
    bio        = c("staten island", "new york", "new york by way of england", "small greek island")
  )

  result <- example_data %>%
    detect_geographies(
      id     = login, 
      input  = "bio", 
      output = "country",
      email  = "email_addr",
      demonyms = FALSE
    )
  
  expect_equal(result[['country_bio']], c(
    NA_character_, "United States", "United Kingdom", "United States", NA_character_
  ))
  expect_equal(result[['country_email']], c(
    NA_character_, NA_character_, "United Kingdom", "United Kingdom", NA_character_
  ))
  
})
