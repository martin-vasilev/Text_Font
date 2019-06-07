sentexcel <- read.csv("C:/Users/Public/Documents/Text_Font/Experiment/corpus/sentexcel.txt", header=TRUE)
tapply(sentexcel$Var7, sentexcel$Var2, FUN= mean)
str(sentexcel)


sentexcel$Var5 = as.character(sentexcel$Var5)
sentexcel$Var6 = as.character(sentexcel$Var6)

### Number of words percondition
m= sapply(strsplit(sentexcel$Var5,  " "), length)
tapply (m, sentexcel$Var2, FUN= mean)
tapply (m, sentexcel$Var2, FUN= range)

### average number of words on second line

m= sapply(strsplit(sentexcel$Var6,  " "), length)
tapply (m, sentexcel$Var2, FUN= mean)
tapply (m, sentexcel$Var2, FUN= range)

### average number of characters on second line

m= sapply(strsplit(sentexcel$Var6,  ""), length)
tapply (m, sentexcel$Var2, FUN= mean)
tapply (m, sentexcel$Var2, FUN= range)

### Number of charcters percondition
m= sapply(strsplit(sentexcel$Var5,  ""), length)
tapply (m, sentexcel$Var2, FUN= mean)
tapply (m, sentexcel$Var2, FUN= range)

mean accuracy
y= tapply(Quest$accuracy, Quest$sub, FUN= mean)
mean(y)
