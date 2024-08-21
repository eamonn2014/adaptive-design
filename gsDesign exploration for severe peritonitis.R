



design <-gsDesign(k = 2,
                  
                  beta=0.1,
                  
                  alpha = 0.025,
                  
                  sfu = sfHSD,
                  
                  sfupar =  1,
                  
                  timing = c(.8)
                  
                  #sfl = sfHSD,
                  
                  #sflpar = 1
                  
)
design

# Extract and print the adjusted alpha for the interim analysis
alpha_adj_interim <- 1-pnorm(design$upper$bound[1])
alpha_adj_final <- 1-pnorm(design$upper$bound[2])
cat("Adjusted alpha for interim analysis:", alpha_adj_interim, "\n")
cat("Adjusted alpha for final analysis:", alpha_adj_final, "\n")

# for a trial no interim..
p1=.5
p2=.425
tn <- n <- ceiling( sum(Hmisc::bsamsize(p1=p1, p2=p2, alpha=.05, power=.9)))

# so I need this p2 at interim to stop for success
# bpower is two sided so I multiply by 2
p2 = .419 # trial and error to find this proportion that gives 90% power

ratio <- design$n.I[1]
Hmisc::bpower(p1=.5, p2=p2 , alpha=alpha_adj_interim*2, n=tn*ratio)  ## .855 ss ratio from gDesign

# https://github.com/eamonn2014/wilcox_sim/blob/main/simulate%20diff%20in%20proportions.R
# spot on!
n1 <- n2 <-(tn*ratio)/2

Z1 <- p1-p2
# 
Z2 <- ( p1*n1 + p2*n2 )  / (n1+n2)  # weight by sample size
# 
Z3<- sqrt( Z2*(1-Z2)*( 1/n1 + 1/n2  ))
# 
Z <- Z1/Z3
# 
ZB <- qnorm(alpha_adj_interim*1) + Z
# 
ZR <- -qnorm(alpha_adj_interim*1)+ Z
# 
pnorm(ZB) + 1-pnorm(ZR)

# can i rearrange to find p2?

#https://stats.stackexchange.com/questions/354556/what-is-the-standard-error-for-hypothesis-testing-of-two-proportions-using-a-z-t
#https://stats.stackexchange.com/questions/258522/how-to-calculate-the-power-of-a-test-that-compares-two-proportions

#---------------------------------------------


# another check

# beta0 placebo  0.2
# trt1 proportion 0.1
 
p1odd <- p1/(1-p1)
p2odd <- p2/(1-p2)
or <- p2odd/p1odd


#not on log scale
beta0= log(p1odd)  # 0.25 intrecept odd of pacebo  #
beta1= log(or)     # odds ratio v placebo   #

#Your significance threshold
alpha=alpha_adj_interim*2

#Number of simulations 
nsim = 1000

N= (tn*ratio) # total sample size

#If we are interested in rejecting the null
power.vec = c()

#If we are interested in assessing parameter bias
bias.vec = c()

#If we are interested in computing Mac Fadden pseudo R2 
R2.vec = c()

for(i in 1:nsim){
  
  #Assuming one trt predictor and N individuals
  X = rbinom(N, 1, 0.5)
  
  
  #Our logistic model
  pr = exp(beta0+X*beta1)/(1+exp(beta0+X*beta1))
  
  y = rbinom(N, 1,pr)
  
  model = glm(y~X, family = binomial(link="logit"))
  sm = summary(model)
  
  power.vec = c(power.vec, sm$coefficients[2,4]<alpha)
  bias.vec = c(bias.vec, beta1-sm$coefficients[2,1])
  R2.vec = c(R2.vec, 1-(sm$deviance/sm$null.deviance))
}

#The power (here rejecting the null for beta 1 depends on the number of individuals)
sum(power.vec)/nsim



