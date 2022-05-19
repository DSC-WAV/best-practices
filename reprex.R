reprex::reprex({
  suppressWarnings(library(tidyverse))
  x <- c(3, 2, 1, 1, 4, 5, 6)
  ds <- tibble(x) %>%
    mutate(x_grp = 
      case_when(
        x %in% c(1, 2, 3) ~ "sustainable",
        x == 4            ~ "experimental",
        x %in% c(5, 6) ~ "fossil fuel"
      )
    )
  ds
})
