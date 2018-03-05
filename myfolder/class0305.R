library (knitr)
library(tidyverse)

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

plot(dat,pch=16,col="blue")
dat$d <- abs(dat$x-0)
dat$d
arrange(dat,d)

dat.sub <- arrange(dat,d)
dat.sub1 <- dat.sub[1:5, ]
dat.sub1