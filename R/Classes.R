community_model <- R6Class("community_model",
  public = list(
    pattern = NULL,
    evolve = NULL,
    timeline = NULL,

    initialize = function(pattern = pattern_grid(), evolve = evolve_none, timeline = timeline_regular()) {
      self$pattern <- pattern
      self$evolve <- function(deltaT) NA
      body(self$evolve) <- body(evolve)
      self$timeline <- timeline
    },

    plot = function(...) plot(self$pattern, ...),

    run = function(animate = FALSE, sleep = 0) {
      if(animate) self$plot(main="Initial")

      previous_time <- 0
      for(time in self$timeline$sequence) {
        delta_t <- time-previous_time
        self$evolve(delta_t)
        if(animate) {
          self$plot(main=paste("Time:", time))
          if (sleep > 0) Sys.sleep(sleep)
        }
      }
    }
  )
)



community_gridmodel <- R6Class("community_gridmodel",
  inherit = community_model,
  public = list(
    tess = NULL,

    initialize = function(pattern = pattern_grid(), evolve = evolve_none, timeline = timeline_regular()) {
      super$initialize(pattern=pattern, evolve=evolve, timeline=timeline)
      self$tess <- spatstat::dirichlet(self$pattern)
    },


    plot = function(...) {
      marks(self$tess) <- marks(self$pattern)
      plot(self$tess, do.col=TRUE, ...)
    }
  )
)



community_matrixmodel <- R6Class("community_matrixmodel",
  inherit = community_model,
  public = list(
    plot = function(...) {
      image(x=1:ncol(self$pattern), y=1:nrow(self$pattern), t(self$pattern),
            col = rainbow(max(self$pattern)), xlab="", ylab="", axes = FALSE, asp=1, ...)
    }
  )
)
