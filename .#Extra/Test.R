# Test du package
library("CommSim")

# Classe de base
myModel <- community_model$new()
myModel$plot()
myModel$timeline
myModel$run(animate = TRUE)


# Modèle sur grille
gridSize <- 34
# Creation du modèle
myModel <- cg_drift$new(pattern_grid(gridSize, S=50), timeline=0:10)
myModel$plot()
system.time(myModel$run(animate = FALSE, save=TRUE))
plot(myModel)


# Version matricielle
myModel <- cm_drift$new(pattern_matrix_individuals(nx=34, S=50), neighborhood="Moore 1", timeline=0:10)
myModel$autoplot()
myModel$plot(main="")
myModel$plot(time=0)
system.time(myModel$run(animate = FALSE, sleep=0.04, save=TRUE))
plot(myModel)
myModel$run(more_time = 11:12, animate = TRUE)
# Statistiques
plot(myModel$along_time(entropart::Richness, Correction="None", CheckArguments=FALSE))

# Conway. Oscillateur
myModel <- cm_Conway$new(pm_Conway_blinker(), timeline=0:10)
myModel$run(animate = TRUE, sleep=0.1, save=TRUE)
# Conway. Glider
myModel <- cm_Conway$new(pm_Conway_glider(50), timeline=0:100)
myModel$autoplot()
myModel$plot()
myModel$run(animate = TRUE, sleep=0.1, save=FALSE)


# Modèle wmppp
# Creation du modèle
myModel <- cp_drift$new(timeline = 0:10)
myModel$plot()
system.time(myModel$run(animate = FALSE, save=TRUE))
system.time(myModel$run(more_time = 11:1000, save=TRUE))
plot(myModel)
# Statistiques
plot(myModel$along_time(entropart::Richness, Correction="None", CheckArguments=FALSE), type="l")

