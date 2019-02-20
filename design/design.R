
# Martin R. Vasilev, 2017

setwd("C:/Users/marti/Desktop/design")


library(readr)

design<- NULL

for(i in 1:51){
  t <- read_delim(paste("P", toString(i), ".txt", sep=''), 
                       " ", escape_double = FALSE, trim_ws = TRUE)
  
  t$sub<- i
  design<- rbind(design, t)
}

design<- subset(design, item<25) # remove practice items

table(design$item, design$cond, design$frame)

table(design$sub, design$item, design$cond)
