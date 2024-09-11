example_aggregate_data <- tibble::tibble(
  score = sample(1:10, 10),
  lower_geography_code = LETTERS[1:10],
  higher_geography_code = c(rep("X", 4), rep("Y", 3), rep("Z", 3)),
  lower_geography_population = sample(100:1000, 10)
)

usethis::use_data(example_aggregate_data, internal = TRUE, overwrite = TRUE)
