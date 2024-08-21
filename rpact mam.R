

library(rpact)
packageVersion("rpact")

#https://www.rpact.org/vignettes/planning/rpact_mams_design_and_analysis/index.html
# first and second stage futility on z-scale
# fut1 <- 0.09256995
# fut2 <- 0.264

#scenario 1 0.2 v 0.15

fut1 <- 0.3316
fut2 <- 0.946


d_fut <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)



c_assum <- 0.2 # assumed rate in control
effect_assum <- 0.25 # relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
ssc_fut <- getSampleSizeRates(
  design = d_fut,
  riskRatio = TRUE,
  pi1 = c_assum * (1 - effect_assum),
  pi2 = c_assum
)

ssc_fut$futilityBoundsEffectScale


d <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)
(summary(d))


c_rate <- 0.2 # assumed rate in control
effect <- 0.25 # relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
d_sample <- getSampleSizeRates(
  design = d,
  riskRatio = TRUE,
  pi1 = c_rate * (1 - effect),
  pi2 = c_rate
)
(summary(d_sample))

#---------------------
#scenario 2 0.3 v 0.2




fut1 <- 0.243
fut2 <- 0.69


d_fut <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)



c_assum <- 0.3 # assumed rate in control
effect_assum <- 1/3 # relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
ssc_fut <- getSampleSizeRates(
  design = d_fut,
  riskRatio = TRUE,
  pi1 = c_assum * (1 - effect_assum),
  pi2 = c_assum
)

ssc_fut$futilityBoundsEffectScale


d <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)
(summary(d))


c_rate <- 0.3 # assumed rate in control
effect <- 1/3 # relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
d_sample <- getSampleSizeRates(
  design = d,
  riskRatio = TRUE,
  pi1 = c_rate * (1 - effect),
  pi2 = c_rate
)
(summary(d_sample))


#---------------------
#scenario 3 0.4 v 0.3




fut1 <- 0.338
fut2 <- 0.96


d_fut <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)



c_assum <- 0.4 # assumed rate in control
effect_assum <- .25 # relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
ssc_fut <- getSampleSizeRates(
  design = d_fut,
  riskRatio = TRUE,
  pi1 = c_assum * (1 - effect_assum),
  pi2 = c_assum
)

ssc_fut$futilityBoundsEffectScale


d <- getDesignGroupSequential(
  kMax = 3,
  alpha = 0.025,
  beta = 0.2,
  sided = 1,
  typeOfDesign = "asOF",
  informationRates = c(1 / 3, 2 / 3, 1),
  futilityBounds = c(fut1, fut2),
  bindingFutility = FALSE
)
(summary(d))


c_rate <- 0.4 # assumed rate in control
effect <- .25# relative reduction that is to be detected with probability of 0.8

# rates indicate binary endpoint
d_sample <- getSampleSizeRates(
  design = d,
  riskRatio = TRUE,
  pi1 = c_rate * (1 - effect),
  pi2 = c_rate
)
(summary(d_sample))











