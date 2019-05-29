
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
load("data/Alldata.Rda")
load("data/Return_sweep.Rda")


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
#RS = RS[RS$fix_dur >80 & RS$fix_dur <1000, ]
#RS = RS [RS$LandStartLet<40, ] ########

#visualize distribution 
qqnorm(RS$LandStartLet)
qqnorm(RS$LandStartVA)

#descriptive statistics (letters)
tapply (RS$LandStartLet,RS$cond, FUN= mean, na.rm= T )
tapply (RS$LandStartLet,RS$cond, FUN= sd, na.rm= T)

#descriptive statistics (letters)
tapply (RS$LandStartVA,RS$cond, FUN= mean, na.rm= T )
tapply (RS$LandStartVA,RS$cond, FUN= sd, na.rm= T)

#plots across all 4 conditions 
plot(RS$LandStartLet~RS$cond)
plot(RS$LandStartVA~RS$cond)

ggplot(RS, aes(x=as.factor(RS$cond), y=RS$LandStartLet)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("Conditions")


ggplot(RS, aes(x=as.factor(RS$cond), y=RS$LandStartVA)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("Conditions")
  
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

DesTime1<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
               measure=c("LandStartLet", "LandStartVA"), na.rm=TRUE)
mTime1<- cast(DesTime1, font_size ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

# df1<- data.frame(font_size= mTime1$font_size, line_len= mTime1$line_len, Mean= mTime1$LandStartLet_M, 
#                  SD= mTime1$LandStartLet_SD, Measure= "Letters")
# df2<- data.frame(font_size= mTime1$font_size, line_len= mTime1$line_len, Mean= mTime1$LandStartVA_M, 
#                  SD=mTime1$LandStartVA_SD, Measure= "Visual Angle")

 df1<- data.frame(font_size= mTime1$font_size, Mean= mTime1$LandStartLet_M, 
                  SD= mTime1$LandStartLet_SD, Measure= "Letters")
 df2<- data.frame(font_size= mTime1$font_size, Mean= mTime1$LandStartVA_M, 
                  SD=mTime1$LandStartVA_SD, Measure= "Visual Angle")

df<- rbind(df1, df2)

df$SE<- df$SD/sqrt(64)

Dplot<- ggplot(data= df, aes(x=font_size, y= Mean, group=1, ymax = Mean + SE, ymin= Mean - SE))+ 
  #scale_y_continuous(breaks=c(200, 250, 300, 350, 400, 450))+
  theme_bw(24) + theme(panel.grid.major = element_line(colour = "#E3E5E6", size=0.7), 
                       axis.line = element_line(colour = "black", size=1),
                       panel.border = element_rect(colour = "black", size=1.5, fill = NA))+
  geom_line(size=2)+ ylab('Landing position')+ xlab('Font size')+
  geom_point(size=7) + facet_grid(.~ Measure, scales = "free") + theme(strip.text.x = element_text(size = 22,  face="bold",family="serif"),
  strip.background = element_rect(fill="#F5F7F7", colour="black", size=1.5),
  legend.key = element_rect(colour = "#000000", size=1)) + geom_ribbon(alpha=0.10, 
  colour=NA); Dplot


##plots 

#line length
ggplot(RS, aes(x=as.factor(RS$line_len), y=RS$LandStartVA)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
xlab("Line Length")

ggplot(RS, aes(x=as.factor(RS$line_len), y=RS$LandStartLet)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
xlab("Line Length")

#font size
ggplot(RS, aes(x=as.factor(RS$font_size), y=RS$LandStartVA)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
  xlab("Font Size")

ggplot(RS, aes(x=as.factor(RS$font_size), y=RS$LandStartLet)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) +
xlab("Font Size")


###### LMM for Character(LandStartLet)

library(lmer4)
land_pos.lm= lmer(LandStartLet ~ line_len *font_size*launchDistLet + 
                    (1|item) + (1+ line_len+ font_size|sub), RS, REML=T)

summary(land_pos.lm)

##### setting up contrasts for main effects 

contrasts(RS$line_len) <- contr.sdif(2)  # contr.sdif sets up the contrast
contrasts(RS$font_size) <- contr.sdif(2)


contr.land_pos.lm= lmer(LandStartLet ~ line_len *font_size*launchDistLet + 
                    (1|item) + (1+ line_len+ font_size|sub), RS, REML=T)


summary(contr.land_pos.lm)

####### plot effects
library(effects)
plot(allEffects(contr.land_pos.lm))


###### LMM for visual angle (LandStartVA)


land_pos.lm2a= lmer(LandStartVA ~ line_len *font_size*launchDistVA + 
                          (1|item) + (1+ line_len|sub), RS, REML=T)
#####or 
land_pos.lm2b= lmer(LandStartVA ~ line_len *font_size*launchDistVA + 
                     (1|item) + (1+ font_size|sub), RS, REML=T)
summary(land_pos.lm2a)
summary(land_pos.lm2b)

####### plot effects
plot(allEffects(land_pos.lm2a))
plot(allEffects(land_pos.lm2b))


rm(list= ls())
