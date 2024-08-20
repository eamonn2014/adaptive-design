



gsDesign(k = 3,
         
         beta=0.1,
         
         alpha = 0.025,
         
         sfu = sfHSD,
         
         sfupar =  -4,
         
         timing = c(.4,.75)
         
         #sfl = sfHSD,
         
         #sflpar = 1
         
)


#alex's way of finding diff at interim? not sure about this ...power is not accounted for?

N_SOC <- 966/2
N_TRT <- 966/2
placebo_response <- 0.5
trt_response <- 0.404


x1 <- matrix(c(trt_response*N_TRT , N_TRT -trt_response*N_TRT,
              placebo_response*N_SOC, N_SOC-placebo_response  *N_SOC),2,byrow=TRUE, dimnames=
              list("Treatment"=c("TRT","SOC"),
                   "Response"=c("yes","No")))
            
            x1
            
chisq.test(x1, correct = FALSE)

# eamonn way to find mortality at interim to be significant


library(pwr)
 
(p.out <- pwr.2p.test(h = ES.h(p1 = 0.425, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.80, 
                      n=1385/2,
                      alternative="less"))

(p.out <- pwr.2p.test(h = ES.h(p1 = 0.399, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=966/2,
                      alternative="less"))


(p.out <- pwr.2p.test(h = ES.h(p1 = 0.395, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1060/2,
                      alternative="less"))


(p.out <- pwr.2p.test(h = ES.h(p1 = 0.406, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1283/2,
                      alternative="less"))


(p.out <- pwr.2p.test(h = ES.h(p1 = 0.40, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1378/2,
                      alternative="less"))


(p.out <- pwr.2p.test(h = ES.h(p1 = 0.414, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1494/2,
                      alternative="less"))

(p.out <- pwr.2p.test(h = ES.h(p1 = 0.410, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1585/2,
                      alternative="less"))
 

## 2 interim calcs



(p.out <- pwr.2p.test(h = ES.h(p1 = 0.3742, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1009/2,
                      alternative="less"))



(p.out <- pwr.2p.test(h = ES.h(p1 = 0.4103, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1615/2,
                      alternative="less"))



(p.out <- pwr.2p.test(h = ES.h(p1 = 0.3986, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1127/2,
                      alternative="less"))



(p.out <- pwr.2p.test(h = ES.h(p1 = 0.4164, p2 = 0.5),  # given powern what is n
                      sig.level = NULL,
                      power = 0.90, 
                      n=1804/2,
                      alternative="less"))

 # checking
 Hmisc::bpower(p1=.5, p2=.3986, alpha=0.016*2, n=1127)  ## yes!!!!
 Hmisc::bpower(p1=.5, p2=.4164, alpha=0.0112*2, n=1804)  ## yes!!!!!!!! 

 # continue with harrell function
 
 Hmisc::bpower(p1=.5, p2=.39, alpha=0.005*2, n=1211)  ## 
 Hmisc::bpower(p1=.5, p2=.41, alpha=0.0091*2, n=1615)  ## 

 Hmisc::bpower(p1=.5, p2=.4083, alpha=0.018*2, n=1340)  ## 
 Hmisc::bpower(p1=.5, p2=.4155, alpha=0.0105*2, n=1787)  ##
 
 
 Hmisc::bpower(p1=.5, p2=.354, alpha=0.0018*2, n=801)  ## 
 Hmisc::bpower(p1=.5, p2=.4054, alpha=0.0079*2, n=1504)  ##
 
 
 Hmisc::bpower(p1=.5, p2=.3845, alpha=0.013*2, n=903)  ## 
 Hmisc::bpower(p1=.5, p2=.4142, alpha=0.0116*2, n=1693)  ##
 
 
 
 
 
 