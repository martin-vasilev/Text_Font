rm(list= ls())


#Packages
install.packages("arm")
install.packages("MASS")
install.packages("lattice")
install.packages("lme4")
install.packages("effects")



#Data Get#
load("data/Alldata.Rda")
load("data/Return_sweep.Rda")


#Factors Get#

RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)
RS$Rtn_sweep_type = as.factor(RS$Rtn_sweep_type)

#Outlier Exclusion#

RS = RS[RS$fix_dur >80 & RS$fix_dur <1000, ]
RS = RS [RS$LandStartLet<40, ]

#merge conditions for main effect analysis 

RS$line_len= ifelse(RS$cond==1| RS$cond==2, 1,2 )
RS$font_size= ifelse(RS$cond==1| RS$cond==3, 1,2 )
RS$line_len= factor(RS$line_len, levels = c(1,2), labels = c("short line", "long line"))
RS$font_size= factor(RS$font_size, levels = c(1,2), labels = c("small font", "big font"))
#descriptives based on line length
Let= tapply (RS$LandStartLet, RS$line_len, FUN= mean, na.rm=T)
VA= tapply (RS$LandStartVA, RS$line_len, FUN= mean, na.rm=T)
line_len= cbind( Let, VA) 
line_len


#descriptives based on font_size
Let1= tapply (RS$LandStartLet, RS$font_size, FUN= mean,na.rm=T)
VA1= tapply (RS$LandStartVA, RS$font_size, FUN= mean, na.rm=T)
font_size= cbind( Let1, VA1)
font_size


#Remember to Check out Library Books#
library(lme4)
library(MASS)
library(effects)
library(arm)

#contrast

contrasts(RS$line_len) <- contr.sdif(2)  
contrasts(RS$font_size) <- contr.sdif(2)

#check

str(RS)


GLM1=glmer(undersweep_prob ~ font_size * line_len + launchDistVA + 
             
             (line_len|item), data= RS, family= binomial)
summary(GLM1)



plot(GLM1)








