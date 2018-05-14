timeline_regular <- function(from = 1, to = 10, by = 1, unit = "Unit") {
  the_timeline <- list(sequence=seq(from, to, by), unit=unit)
  return(the_timeline)
}

