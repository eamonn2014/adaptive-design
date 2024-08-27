# 3 arm study, dropping an arm at IA
# here is how provide estimates of expected number of patients required, binary endpoint

# lets not wory about presenting futility at this stage
# this was used for 
# I looked at information at 0.5, 0.7, 0.8
# I looked at gamma 1 and -4
# I looked at 3 scenarios for expected rates
# below is one scenario only

# for ref here is 3 arm power, using 2 arm and multipling up , so no multiple adjustments 
sum(ceiling(Hmisc::bsamsize(p1=.15, p2=.2, fraction=.5, alpha=.05, power=.8)))*3/2

library(rpact)
packageVersion("rpact")


d <- getDesignGroupSequential(typeOfDesign = "asHSD", 
                              informationRates = c(0.8, 1),
                              gammaA = -4
                              ,
                              kMax = 2,
                              alpha = 0.025,
                              beta = 0.2,
                              sided = 1#,
                              #futilityBounds = c(fut1),
                              #bindingFutility = FALSE
)

summary(getSampleSizeRates(d, pi1 = c(0.15), pi2 = 0.2))





# I took 1472.4/2*3, dividing by 2 for 1 arm , multiplying by 3 for arms = 2209 Total N at interim
# I took 1840.5/2*3, dividing by 2 for 1 arm , multiplying by 3 for arms = 2761 total N
# using the Exit probability for efficacy (under H1) 0.5930 
# I calcualted the expected number of patients needed when always dropping 1 arm at interim
# interim <-0.5930*2209
# final <- (1-.5930)*2761/6*5, we will always drop an arm at interim (did I miscalc this)




# Sample size calculation for a binary endpoint
# 
# Sequential analysis with a maximum of 2 looks (group sequential design), overall 
# significance level 2.5% (one-sided).
# The results were calculated for a two-sample test for rates (normal approximation),
# H0: pi(1) - pi(2) = 0, H1: treatment rate pi(1) = 0.15, control rate pi(2) = 0.2, 
# power 80%.
# 
# Stage                                         1      2 
# Planned information rate                    80%   100% 
#   Efficacy boundary (z-value scale)         2.291  2.011 
# Cumulative power                         0.5930 0.8000 
# Number of subjects                       1472.4 1840.5 
# Expected number of subjects under H1     1622.2 
# Cumulative alpha spent                   0.0110 0.0250 
# One-sided local significance level       0.0110 0.0221 
# Efficacy boundary (t)                    -0.046 -0.036 
# Exit probability for efficacy (under H0) 0.0110 
# Exit probability for efficacy (under H1) 0.5930 

Legend: