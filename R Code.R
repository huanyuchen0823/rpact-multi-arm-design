# Package
library(ggplot2)
library(rpact)


# Compare with EAST

design <- getDesignGroupSequential(kMax = 2, typeOfDesign = "noEarlyEfficacy",
                                   alpha = 0.1, typeBetaSpending = "bsUser",
                                   userBetaSpending = c(0.035, 0.198))

summary(getPowerSurvival(design, directionUpper = FALSE, lambda2 = 1.2, hazardRatio = 0.6,
                         maxNumberOfEvents = 80, allocationRatioPlanned = 2,
                         accrualTime = c(0, 1.83), accrualIntensity = 78.7))

# Design

design <- getDesignGroupSequential(kMax = 2, typeOfDesign = "noEarlyEfficacy",
                                   alpha = 0.1, typeBetaSpending = "bsOF") # rho family
summary(design)

# HR = 0.6 Sample Size

accrualTime = c(0, 1.83)
studyDuration = 2.33

summary(getSampleSizeSurvival(design, lambda2 = 1.2, hazardRatio = 0.6,
                              allocationRatioPlanned = 2, accrualTime = c(0, 1.83),
                              followUpTime = min(studyDuration - max(accrualTime), 1)
                              )
)

# HR = 0.6 Power

summary(getPowerSurvival(design, thetaH0 = 1, typeOfComputation = "Schoenfeld",
                         directionUpper = FALSE, lambda2 = 1.2, hazardRatio = 0.6,
                         maxNumberOfSubjects = 124.6, maxNumberOfEvents = 83.1,
                         allocationRatioPlanned = 2, accrualTime = c(0, 1.83), kappa = 1))

# Different HR Sample Size

summary(getSampleSizeSurvival(design, lambda2 = 1.2, hazardRatio = c(0.6, 0.7, 0.8, 0.9),
                              allocationRatioPlanned = 2, accrualTime = c(0, 1.83),
                              followUpTime = min(studyDuration - max(accrualTime), 1)))

# Different HR Power
summary(getPowerSurvival(design, thetaH0 = 1, typeOfComputation = "Schoenfeld",
                         directionUpper = FALSE, lambda2 = 1.2,
                         hazardRatio = c(0.6, 0.7, 0.8, 0.9),
                         maxNumberOfSubjects = 124.6, maxNumberOfEvents = 83.1,
                         allocationRatioPlanned = 2, accrualTime = c(0, 1.83), kappa = 1))

# Multi-Arm best
design <- getDesignInverseNormal(
kMax = 2, 
alpha = 0.1,
beta = 0.2,
typeOfDesign = "noEarlyEfficacy",
typeBetaSpending = "bsOF"
)

hazardRatios <- c(0.6, 0.7)
effectMatrix <- matrix(hazardRatios, ncol = 2, byrow = TRUE)
plannedEvents <- c(350, 700)

simulationResults <- getSimulationMultiArmSurvival(
  design = design,
  activeArms = 2,
  effectMatrix = effectMatrix,
  typeOfShape = "userDefined", # effect matrix
  intersectionTest = "Hierarchical",
  directionUpper = FALSE,
  typeOfSelection = "best",
  plannedEvents = plannedEvents,
  allocationRatioPlanned = 2,
  minNumberOfEventsPerStage = c(NA_real_, 300),
  maxNumberOfEventsPerStage = c(NA_real_, 600),
  conditionalPower = 0.8,
  maxNumberOfIterations = 1000,
  seed = 12345
)

summary(simulationResults)