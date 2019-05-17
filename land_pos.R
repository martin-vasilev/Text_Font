
# Martin Vasilev, 2019

rm(list= ls())
  

# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2") # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}

# Load data:
load("C:/Users/duniv/Documents/GitHub/Text_Font/data/Alldata.Rda")
load("C:/Users/duniv/Documents/GitHub/Text_Font/data/Return_sweep.Rda")


############################################################################################################
#examine data 
head(RS)
str(RS)

#class variables
RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)
RS$Rtn_sweep_type = as.factor(RS$Rtn_sweep_type)

#examine data again 
str(RS)
#exclude outliers less than 80ms and more than 800ms 
RS = RS[RS$fix_dur >80 & RS$fix_dur <1000, ]
RS = RS [RS$LandStartLet<40, ] ########

#visualize distribution 
qqnorm(RS$LandStartLet)
qqnorm(RS$LandStartVA)


#descriptive statistics (letters)
tapply (RS$LandStartLet,RS$cond, FUN= mean, na.rm= T )
tapply (RS$LandStartLet,RS$cond, FUN= sd, na.rm= T)

#descriptive statistics (letters)
tapply (RS$LandStartVA,RS$cond, FUN= mean, na.rm= T )
tapply (RS$LandStartVA,RS$cond, FUN= sd, na.rm= T)

plot(RS$LandStartLet~RS$cond)
plot(RS$LandStartVA~RS$cond)

ggplot(data= RS, aes(x= RS$cond, y= RS$LandStartLet), na.rm= T)


#merge conditions for main effect analysis 

RS$line_len=  as.factor(ifelse(RS$cond== 1 & 2, 1, 2))
RS$font_size= as.factor(ifelse(RS$cond== 1 & 3, 1, 2))

RS$line_len= factor(RS$line_len, levels = c(1,2), labels = c("short line", "long line"))
RS$font_size= factor(RS$font_size, levels = c(1,2), labels = c("small font", "big font"))

#descriptives based on line length
Let= tapply (RS$LandStartLet, RS$line_len, FUN= mean)
VA= tapply (RS$LandStartVA, RS$line_len, FUN= mean)
line_len= cbind( Let, VA) 
line_len

#descriptives based on font_size
Let1= tapply (RS$LandStartLet, RS$font_size, FUN= mean)
VA1= tapply (RS$LandStartVA, RS$font_size, FUN= mean)
font_size= cbind( Let1, VA1)
font_size
