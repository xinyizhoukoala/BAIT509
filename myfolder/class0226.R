install.packages("tidyverse")
install.packages("ISLR")
install.packages("knitr")
library(tidyverse)

genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

genreg(10)

dat <- genreg(1000)

dat <- mutate(dat,
       yhat=0,
       yhat1=5-x1,
       yhat2=5+2*x2,
       yhat12=5-x1+2*x2)
dat

(mse <- mean(dat$yhat - dat$y)^2)
(mse1 <- mean(dat$yhat1 - dat$y)^2)
(mse2 <- mean(dat$yhat2 - dat$y)^2)
(mse12 <- mean(dat$yhat12 - dat$y)^2)
# x2 is more dependent on the y

## Oracle classification
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}

# when x=1
pA=0.2
pB=0.8/(1+exp(-1))
pB #0.5848469
pC=1-pA-pB 
pC #0.2151531
# when x=-2
pA=0.2
pB=0.8/(1+exp(2))
pB #0.09536234
pC=1-pA-pB
pC #0.7046377
# when i do the classification, i tend to choose the mode 
# if x<0, C
# if x>0, B

library(ggplot2)
gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(x) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, x, 1-x-0.2)))
  tibble(x=x, y=y)
}
dat2 <- gencla(1000)
dat2 <- mutate(dat2,
               yhat=sapply(x,function(x_)
                 if(x_<0) "C" else "B"))
1-mean(dat2$yhat == dat2$y)
