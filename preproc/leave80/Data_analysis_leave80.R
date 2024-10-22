
#  2019.  Martin R. Vasilev

rm(list= ls())



#######################################################
#             Data column explanation:                #
#######################################################

# sub:              Subject number
# item:             Item (sentence) number from the corpus file
# cond:             Condition number (1= small font short line; 2= big font short line; 3= small font long line; 
#                                     4= big font long line)
# seq:              Trial sequence, or the order in which trials were presented in the experiment
# trialStart:       Start of trial time stamp in raw asc file
# SFIX:             Start of fixation flag in the asc data (search for it in raw data to quickly locate it)
# EFIX:             End of fixation flag in the asc data (search for it in raw data to quickly locate it)
# xPos:             X position of the fixation on the screen
# yPos:             Y position of the fixation on the screen
# fix_num:          Fixation number in the current trial
# fix_dur:          Fixation duration (in ms)
# sacc_dur:         Incoming saccade duration (in ms)
# line:             Line of text on which the fixation occured
# word:             Word number in the trial on which the fixation occured
# char_trial:       Character number in the trial on which the fixation occured (includes all text in the trial)
# char_line:        Character number on the current line on which the fixation occured (each line starts at 0)
# prevChar:         Character number (line) on which the previous fixation occured
# nextChar:         Character number (line) on which the next fixation occured
# prevX:            X position on which the previous fixation occured
# nextX:            X position on which the next fixation occured
# regress:          A logical indicating whether the current fixation is a regressive one (1= yes; 0= no)
# wordID:           A string of the word on which the fixation occured
# land_pos:         Landing position in characters on the current word (0= empty space before word)
# sacc_len:         Incoming saccade length (in characters)
# outsideText:      A logical indicating whether the current fixation occured outside the text area (1= yes; 0=no)
# max_char_line:    Maximum number of characters on the current line
# Rtn_sweep:        A logical indicating whether the current fixation is a return sweep one (1= yes; 0= no)
# Rtn_sweep_type:   Type of return sweep saccade ("undersweep" or "accurate")
# LandStartLet:     Landing position relative to the start of the line in letters (equivalent to char_line)
# LandStartVA:      Landing position relative to the start of the line in degrees per visual angle
# undersweep_prob:  A logical indicating whether the current fixation is an undersweep one (1= yes; 0=no)
# launchDistLet:    Launch site distance in letters (equivalent to sacc_len)
# launchDistVA:     Launch site distance in degrees per visual angle
# launchSite:       Launch site distance relative to end of the line (letters)
# launchSiteVA:     Launch site distance relative to end of the line (visual angle)
# prev_fix_dur      Duration of previous fixation

# Please note: counting in the present data (e.g., fixation/ character/ line numbers) always starts at 1.


# load/ install required packages:
packages= c("reshape", "lme4", "ggplot2", "MASS", "arm", "effects", "lattice",
            "mgcv", "itsadug", 'ggpubr') # list of used packages:

for(i in 1:length(packages)){
  
  if(packages[i] %in% rownames(installed.packages())==FALSE){
    install.packages(packages[i])
    library(packages[i], character.only=TRUE)
  }else{
    library(packages[i], character.only=TRUE)
  }
}


# colorblind palletes: # https://venngage.com/blog/color-blind-friendly-palette/
pallete1= c("#CA3542", "#27647B", "#849FA0", "#AECBC9", "#57575F") # "Classic & trustworthy"

source("Functions/CohensD_raw.R")

########################
# Prepare data frames: #
########################

# Load data:
load("data/Alldata.Rda")
load("preproc/leave80/Return_sweep.Rda")
load("data/Quest.Rda")


#classify data
RS$sub = as.factor(RS$sub)
RS$item = as.factor(RS$item)
RS$cond= as.factor(RS$cond)
RS$land_pos = as.numeric(RS$land_pos)

Alldata$sub = as.factor(Alldata$sub)
Alldata$item = as.factor(Alldata$item)
Alldata$cond= as.factor(Alldata$cond)
Alldata$land_pos = as.numeric(Alldata$land_pos)

Quest$sub<- as.factor(Quest$sub)
Quest$item<- as.factor(Quest$item)

#examine data 
str(RS)
head(RS)

str(Alldata)
head(Alldata)
#merge conditions for main effect analysis 

RS$line_len= factor(ifelse(RS$cond==1| RS$cond==2, 1,2),labels = c("short line", "long line"))
RS$font_size= factor(ifelse(RS$cond==1| RS$cond==3, 1,2), labels = c("small font", "big font"))

Alldata$line_len= factor(ifelse(Alldata$cond==1| Alldata$cond==2, 1,2),labels = c("short line", "long line"))
Alldata$font_size= factor(ifelse(Alldata$cond==1| Alldata$cond==3, 1,2), labels = c("small font", "big font"))

Quest$line_len= factor(ifelse(Quest$cond==1| Quest$cond==2, 1,2),labels = c("short line", "long line"))
Quest$font_size= factor(ifelse(Quest$cond==1| Quest$cond==3, 1,2), labels = c("small font", "big font"))


############################
#  Comprehension accuracy  #
############################

100*round(mean(Quest$accuracy),3) # mean accuracy (%)
100*round(sd(Quest$accuracy),3) # SD accuracy (%)

DesQuest<- melt(Quest, id=c('sub', 'item', 'cond', 'line_len', 'font_size'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, line_len+font_size ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

# GLMM model:
Quest$line_len<- as.factor(Quest$line_len)
Quest$font_size<- as.factor(Quest$font_size)

contrasts(Quest$line_len)<- c(-1, 1)
contrasts(Quest$font_size)<- c(-1, 1)

if(!file.exists("Models/CGM.Rda")){
  # model does not converge with any other random slopes
  CGM<- glmer(accuracy ~ font_size*line_len + (1|sub)+ (line_len|item), data = Quest, family= binomial)
  save(CGM, file= "Models/CGM.Rda")
  summary(CGM)
}else{
  load("Models/CGM.Rda")
  summary(CGM)
}

round(coef(summary(CGM)), 2)



############################
#       Descriptives       #
############################

#mean under_sweep per condition 
USP<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
                measure=c("undersweep_prob"), na.rm=TRUE)
mUSP<- cast(USP, font_size + line_len ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#mean land_pos in letters per condition 
LP<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("LandStartLet"), na.rm=TRUE)
mLP<- cast(LP, font_size + line_len ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

#mean land_pos in visual angle per condition 
LP1<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
          measure=c("LandStartVA"), na.rm=TRUE)
mLP1<- cast(LP1, font_size + line_len ~ variable
           ,function(x) c(M=signif(mean(x),3)
                          , SD= sd(x) ))

#mean launch site in visual angle per condition 
LS<- melt(RS, id=c('sub', 'item', 'font_size', 'line_len'), 
           measure=c("launchSiteVA"), na.rm=TRUE)
mLS<- cast(LS, font_size + line_len ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))



#mean incoming saccade length in visual angle per fixation type 
Alldata2 <- Alldata[Alldata$regress < 1,]
SLVA1<- melt(Alldata2, id=c('sub', 'item', 'line_len', 'font_size', 'Rtn_sweep'), 
           measure=c("launchDistVA"), na.rm=TRUE)
mSLVA1<- cast(SLVA1, line_len + font_size + Rtn_sweep ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

#mean incoming saccade length in visual angle per saccade type 
Alldata3 <- Alldata[Alldata$Rtn_sweep < 1,]
SLVA2<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
             measure=c("launchDistVA"), na.rm=TRUE)
mSLVA2<- cast(SLVA2, font_size + regress ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

#mean incoming saccade length in letters per saccade type 
SLlet<- melt(Alldata3, id=c('sub', 'item', 'font_size', 'regress'), 
             measure=c("launchDistLet"), na.rm=TRUE)
mSLlet<- cast(SLlet, font_size + regress ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))


#########################################
#              (G)LMMS                  #
#########################################

#setting contrast
contrasts(RS$line_len) <- c(-1, 1)  
contrasts(RS$font_size) <- c(-1, 1)  
Alldata2$Rtn_sweep = as.factor(Alldata2$Rtn_sweep)
contrasts(Alldata2$Rtn_sweep) <- c(-1, 1)  
contrasts(Alldata2$font_size) <- c(-1, 1)  

#centering launch distance
RS$launchDistVA_C<- scale(RS$launchDistVA)
RS$launchDistLet_C<- scale(RS$launchDistLet)
Alldata2$launchDistVA_C<- scale(Alldata2$launchDistVA)
Alldata2$launchDistLet_C<- scale(Alldata2$launchDistLet)

# centre launch site:
RS$launchSiteVA_C<- scale(RS$launchSiteVA)



#------------------------------#
#   Undersweep probability     #
#------------------------------#

#if(!file.exists("Models/GLM1.Rda")){
  GLM1<- glmer(undersweep_prob ~ font_size* line_len*launchSiteVA_C +(line_len|sub)+(line_len|item),
               data= RS, family= binomial)
#  save(GLM1, file= "Models/GLM1.Rda")
#}else{
#  load("Models/GLM1.Rda")
#}

summary(GLM1)

round(coef(summary(GLM1)),3)
#write.csv(round(coef(summary(GLM1)),3), 'Models/Undersweep_prob_GLM1.csv')

# Effect size:
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'undersweep_prob', group_var = 'font_size', baseline = 'small font')


#------------------------------#
#      Landing Position        #
#------------------------------#

#if(!file.exists('Models/LM1.Rda')){
  LM1<- lmer(LandStartVA ~  font_size*line_len*launchSiteVA_C + (line_len|sub) + (1|item),
             data=RS, REML=T)
#  save(LM1, file= "Models/LM1.Rda")
#}else{
#  load('Models/LM1.Rda')
#}

summary(LM1)

LPM= round(coef(summary(LM1)), 2)
#write.csv(LPM, file= "Models/LPM.csv") 

CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'line_len', baseline = 'short line')
CohensD_raw(data = RS, measure = 'LandStartVA', group_var = 'font_size', baseline = 'small font')


plot(effect('font_size:launchSiteVA_C', LM1))
plot(effect('line_len:font_size:launchSiteVA_C', LM1))

## Add previous fixation duration as a covariate:

# RS$prev_fix_dur_c<- scale(RS$prev_fix_dur)
# LM1_PH<- lmer(LandStartVA ~  font_size*line_len*launchSiteVA_C*prev_fix_dur_c + (line_len|sub) + (1|item),
#            data=RS, REML=T)
# 
# summary(LM1_PH)
#------------------------------#
#        Saccade length        #
#------------------------------#

#if(!file.exists("Models/LM2.Rda")){
  LM2<- lmer(launchDistVA ~ font_size*line_len*Rtn_sweep + (1|item) + (font_size|sub), Alldata2, REML=T)
#  save(LM2, file= "Models/LM2.Rda")
#}else{
#  load("Models/LM2.Rda")
#}

summary(LM2)

round(coef(summary(LM2)),3)

#write.csv(round(coef(summary(LM2)),3), 'Models/SaccLen_LM2.csv')

contrasts(Alldata2$font_size)
contrasts(Alldata2$Rtn_sweep)

plot(effect('font_size', LM2))
plot(effect('font_size:Rtn_sweep', LM2))


### font size in letters
dat3<- subset(Alldata2, Rtn_sweep==0)

summary(LM3<- lmer(sacc_len ~ font_size+ (1|sub) + (1|item), dat3, REML=T))


#########################################
#             RESULTS PLOTS             #
#########################################


#------------------------------#
#      Descriptives plot       #
#------------------------------#

# UNDERSWEEP PROBABILITY:
dfU<- subset(RS, !is.na(RS$launchDistVA_C))
dfU$fitted<- fitted(GLM1)

# average over subjects:
DesUSP_M<- melt(dfU, id=c('sub', 'item', 'font_size', 'line_len'), 
                measure=c("fitted", 'undersweep_prob'), na.rm=TRUE)
dfUSP<- cast(DesUSP_M, font_size + line_len+ sub ~ variable
             ,function(x) c(M=signif(mean(x),3)
                            , SD= sd(x) ))

dfUSP$font_size= paste(' ', dfUSP$font_size, '    ', sep = '')
dfUSP$font_size<- as.factor(dfUSP$font_size)
dfUSP$font_size<- factor(dfUSP$font_size, levels = c(" small font    ", " big font    "))



USP_plot <- ggplot(dfUSP, aes(x=line_len, y=fitted_M, fill= font_size, shape= font_size, linetype= font_size)) + 
  theme_classic (22)+ geom_violin(alpha= 0.3, size=1)+ ylim(0, 1)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position=position_dodge(0.9), alpha= 0.8)+
  geom_boxplot(width=0.1, position=position_dodge(0.9), size=1 , notch = T, notchwidth = 0.4, varwidth =F,
               fill= 'white', color= 'black', show.legend=FALSE, linetype='solid')+
  labs(x= "Line length", y= "Under-sweep probability", color= "", shape= '', linetype= '', fill= '')+
  theme(legend.position = 'none', legend.title=element_blank(),
        legend.key.width = unit(1.25, 'cm'), legend.key.height = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[4]))+
  scale_color_manual(values=c(pallete1[1], pallete1[4]))+ ggtitle('b)')

USP_plot

ggsave(filename = 'Plots/USP.pdf', plot = USP_plot, width = 7, height = 7)



# LANDING POSITION:
dfL<- subset(RS, !is.na(RS$launchDistVA_C))
dfL$fitted<- fitted(LM1)

# average over subjects:
DesLP_M<- melt(dfL, id=c('sub', 'item', 'font_size', 'line_len'), 
               measure=c("fitted", 'LandStartVA'), na.rm=TRUE)
dfLP<- cast(DesLP_M, font_size + line_len+ sub ~ variable
            ,function(x) c(M=signif(mean(x),3)
                           , SD= sd(x) ))

dfLP$font_size= paste(' ', dfLP$font_size, '    ', sep = '')
dfLP$font_size<- as.factor(dfLP$font_size)
dfLP$font_size<- factor(dfLP$font_size, levels = c(" small font    ", " big font    "))



LP_plot <- ggplot(dfLP, aes(x=line_len, y=fitted_M, fill= font_size, shape= font_size, linetype= font_size)) + 
  theme_classic (22)+ geom_violin(alpha= 0.3, size=1)+ #ylim(0, 8)+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.6, position=position_dodge(0.9), alpha= 0.8)+
  geom_boxplot(width=0.1, position=position_dodge(0.9), size=1 , notch = T, notchwidth = 0.4, varwidth =F,
               fill= 'white', color= 'black', show.legend=FALSE, linetype= 'solid')+
  labs(x= "Line length", y= "Landing position (deg)", color= "", shape= '', linetype= '', fill= '')+
  theme(legend.position =  c(0.2, 0.88), legend.title=element_blank(),
        legend.key.width = unit(1.25, 'cm'), legend.key.height = unit(1, 'cm'),
        plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values=c(pallete1[1], pallete1[4]))+
  scale_color_manual(values=c(pallete1[1], pallete1[4]))+ ggtitle('a)')

LP_plot

ggsave(filename = 'Plots/LP.pdf', plot = LP_plot, width = 7, height = 7)

#### merge two descriptives plots:
figure <- ggarrange(LP_plot, USP_plot, ncol = 2, nrow = 1, common.legend = TRUE, legend = "top")
ggsave(filename = 'Plots/Des_merged.pdf', plot = figure, width = 15, height = 7)




#---------------------------------#
#   3-way interaction plot (LP)   #
#---------------------------------#

###ALL EFFECTS PLOT -THREE WAY INTERACTION
lp= allEffects(LM1)
summary(lp)
x= as.data.frame(lp)
x=as.data.frame(x)
colnames(x)= c("font_size", "line_len", "launchSiteVA_C", "fit", "se", "lower", "upper")

#geom_errorbar(aes(ymin=fit-se, ymax=fit+se),  geom_point()
# width=0.2)

# ggplot(x, aes(x= launchSiteVA_C, y=fit, color=font_size)) + 
#    theme_gray(base_size=15) +geom_line(aes(linetype = font_size), size=0.8, color= 1.5)+ geom_point(color=2)+
#   labs(title= "", x= "Launch site from end of first line (deg)", y= "Landing position (deg)") +facet_wrap(~line_len)
#   


df<-  x
df$line_len <- droplevels(df$line_len)
#levels(df$line_len)<- c("long", 'short')
df$line_len<- factor(df$line_len, levels= c("short line", "long line"))
levels(df$line_len)<- c(" short line", " long line")

df$font_size<- as.factor(df$font_size)
df$font_size<- droplevels(df$font_size)
levels(df$font_size)<- c("large font", "small font")


G1<- ggplot(df, aes(x= launchSiteVA_C, y=fit, ymax= upper, ymin= lower,
                    color=line_len, linetype= line_len, fill= line_len, shape= line_len)) + theme_bw (22)+
  geom_line(size= 1)+ geom_point(size=4)+
  labs(x= "Launch site from end of first line (centred in deg)", y= "Landing position (deg)", 
       color= "", shape= '', linetype= '', fill= '') +
  facet_wrap(~font_size)+
  geom_ribbon(alpha= 0.2, color= NA) + theme(legend.position = c(0.87, 0.88), legend.title=element_blank(),
  legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
  panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
  strip.background = element_rect(colour="white", fill="white"),
  strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"))+
  scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
  scale_color_manual(values=c(pallete1[1], pallete1[2])); G1

ggsave(filename = 'Plots/3-way.pdf', plot = G1, width = 10, height = 7)



##############################################################
#                 Modulation by trial order:                 #
##############################################################

#-------------------------------
# Prepare dataset for analsysis:
#-------------------------------

# check contrast coding:
contrasts(RS$font_size)
contrasts(RS$line_len)
RS$sub<- as.numeric(as.character(RS$sub))

# Add block order:

RS$block_order<- NA
RS$big_font_block<- NA
RS$small_font_block<- NA

for(i in 1:nrow(RS)){
  
  if(RS$sub[i]%%2==1){ # odd subject, small font first
   
    RS$big_font_block[i]<- 2
    RS$small_font_block[i]<- 1
    
    if(RS$font_size[i]== "small font"){ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i]
    }else{ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }
     
  }else{ # even subject, big font first
    
    RS$big_font_block[i]<- 1
    RS$small_font_block[i]<- 2
    
    if(RS$font_size[i]== "small font"){ # big font, block seq = seq-50 (50 is halfway point; 100 items)
      RS$block_order[i]<- RS$seq[i]- 50
    }else{ # small font, block seq= seq
      RS$block_order[i]<- RS$seq[i] 
    }
    
  }
  
}


# take a smaller, more managable dataset:
tDat<- RS[,c("sub", "item", "seq", "cond", "font_size", "line_len", "LandStartVA", "undersweep_prob" ,  "block_order",
             "big_font_block", "small_font_block")]

#tDat$big_font_block<- as.factor(tDat$big_font_block)
#tDat$big_font_block<- factor(tDat$big_font_block, levels= c("small font", "big font"))
#contrasts(tDat$big_font_block)
is.numeric(tDat$block_order)


# gamm model:
tDat$big_font_block<- as.factor(tDat$big_font_block)
contrasts(tDat$big_font_block)<- c(-1, 1)

tDat$small_font_block<- as.factor(tDat$small_font_block)
contrasts(tDat$small_font_block)<- c(-1, 1)

### big font

bigF<- subset(tDat, font_size== "big font")

gam1 <- bam(LandStartVA ~ big_font_block+
              s(sub, bs="re", k=10) +
              s(sub, big_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, big_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= big_font_block, k=10, bs= "cr")+
#              s(big_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= bigF)

summary(gam1)

#plot(gam1)


plot_smooth(gam1, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Landing position (deg)", main= "Big font sentences as a function of block order",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4)
legend(x = 2, y= 2.5, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam1, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[2], main= "Big font (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)


###########

### small font

smallF<- subset(tDat, font_size== "small font")

gam2 <- bam(LandStartVA ~ small_font_block+
              s(sub, bs="re", k=10) +
              s(sub, small_font_block, bs="re", k=10) +
              s(item, bs= "re", k=10)+
              s(item, small_font_block, bs="re") +
              s(block_order, bs= "cr", k=10)+
              s(block_order, by= small_font_block, k=10, bs= "cr")+
              #              s(small_font_block, by= font_size, k=10, bs= "cr")+
              s(block_order, sub, bs= "fs", m=1, k=4),
            data= smallF)

summary(gam2)

#plot(gam2)


plot_smooth(gam2, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial order within block",
            ylab= "Landing position (deg)", main= "Small font sentences as a function of block order",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.2, cex.lab= 1.4, legend_plot_all = list(x=0, y=0))
legend(x = 35, y= 2.4, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white")

plot_diff(gam2, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[2], main= "Small font (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)



####################
# GAMM panel plot: #
####################

pdf('Plots/GAMMs.pdf', width = 11, height = 11)
par(mfrow=c(2,2), mar= c(4,4,4,2))


## big font condition

plot_smooth(gam1, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "a) Large font sentences",
            col = c(pallete1[1], pallete1[2]), legend_plot_all = list(x=0, y=0), family= "serif",
            cex.axis= 1.4, cex.lab= 1.5, hide.label = T, lwd= 2, lty= c(2,1), ylim= c(1.15, 2.35),
            cex.main=1.5)


# small font condition
plot_smooth(gam2, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial number within block",
            ylab= "Landing position (deg)", main= "b) Small font sentences",
            col = c(pallete1[2], pallete1[1]), family= "serif",
            cex.axis= 1.4, cex.lab= 1.5, legend_plot_all = list(x=0, y=0),
            hide.label = T, lwd= 2, lty= c(1,2), ylim= c(1.15, 2.35), cex.main=1.5)

legend(x = 25, y= 2.37, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
       box.col = "white", lty= c(1,2), seg.len=2, cex = 1.3)


### Add block order effect:
plot_diff(gam1, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
          col = pallete1[2], main= "c) Large font sentences (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.4, cex.lab= 1.5, cex.main= 1.5, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))


plot_diff(gam2, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
          col = pallete1[2], main= "c) Small font sentences (2nd- 1st block difference)",
          ylab= "Mean diff.in landing position (deg)", xlab= "Trial number within block", print.summary = T, 
          family= "serif", cex.axis= 1.4, cex.lab= 1.5, cex.main= 1.5, lwd= 2, hide.label = T, ylim= c(-0.6, 0.6))


dev.off()




### Post-hoc- centre of gravity effects on second line:
sentexcel <- read.delim("Experiment/corpus/sentexcel.txt")
sentexcel$Var6<- as.character(sentexcel$Var6)

RS$line2Char<- NA
RS$line2Pix<- NA

for(i in 1:nrow(RS)){
 a<- which(sentexcel$Var1== as.numeric(as.character(RS$item[i])))[1]
 RS$line2Char[i]<- nchar(sentexcel$Var6[a])
 
 if(is.element(RS$cond[i], c(1,3))){
   RS$line2Pix[i]<- RS$line2Char[i]*12
 }else{
   RS$line2Pix[i]<- RS$line2Char[i]*16
 }
 
}

RS$line2Pix_C<- scale(RS$line2Pix)

summary(LM3P<- lmer(LandStartVA ~  font_size*line2Pix_C + (font_size|sub) + (1|item),
           data=RS, REML=T))



plot(effect('font_size', LM3P), ylab= "Landing position (in deg)")
plot(effect('line2Pix_C', LM3P), xlab= 'Length of second line in pixels (centred)', ylab= "Landing position (in deg)",
     main= "Length of second line (main effect)")
plot(effect('font_size:line2Pix_C', LM3P), main= 'Length 2nd line x Font size interaction')


# Undersweep probability:

summary(GLM2P<- glmer(undersweep_prob ~ font_size* line2Pix_C +(font_size|sub)+(1|item),
             data= RS, family= binomial))



######
# Length of line-initial word:
sent <- read.delim("Experiment/corpus/sentexcel.txt")
sent$Var6<- as.character(sentexcel$Var6)
sent<- subset(sent, Var2==1)


len<- NULL

for(i in 1:nrow(sent)){
  string<- sent$Var6[i]
  len[i]<- nchar(unlist(strsplit(string, ' '))[1])
  
}

hist(len)

mean(len)
sd(len)
range(len)
table(len)

# ##############
# # same analysis, but for undersweep probability
# 
# gam3 <- bam(undersweep_prob ~ big_font_block+
#               s(sub, bs="re", k=10) +
#               s(sub, big_font_block, bs="re", k=10) +
#               s(item, bs= "re", k=10)+
#               s(item, big_font_block, bs="re") +
#               s(block_order, bs= "cr", k=10)+
#               s(block_order, by= big_font_block, k=10, bs= "cr")+
#               s(block_order, sub, bs= "fs", m=1, k=4),
#             data= bigF, family = binomial)
# 
# summary(gam3)
# 
# plot_smooth(gam3, view="block_order", plot_all="big_font_block", rug=F, xlab= "Trial order within block",
#             ylab= "Undersweep probability", main= "Big font sentences as a function of block order",
#             col = c(pallete1[1], pallete1[2]), family= "serif",
#             cex.axis= 1.2, cex.lab= 1.4, lwd= 2, legend_plot_all = list(x=2, y=2))
# legend(x = 2, y= 1.25, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
#        box.col = "white")
# 
# plot_diff(gam3, view = "block_order", rm.ranef = F, comp = list(big_font_block = c(1,  2)), 
#           col = pallete1[2], main= "Big font (2nd- 1st block difference)",
#           ylab= "Mean diff. in undersweep probability", xlab= "Trial order within block", print.summary = T, 
#           family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)
# 
# 
# 
# ### small font
# 
# gam4 <- bam(undersweep_prob ~ small_font_block+
#               s(sub, bs="re", k=10) +
#               s(sub, small_font_block, bs="re", k=10) +
#               s(item, bs= "re", k=10)+
#               s(item, small_font_block, bs="re") +
#               s(block_order, bs= "cr", k=10)+
#               s(block_order, by= small_font_block, k=10, bs= "cr")+
#               #              s(small_font_block, by= font_size, k=10, bs= "cr")+
#               s(block_order, sub, bs= "fs", m=1, k=4),
#             data= smallF, family = binomial)
# 
# summary(gam4)
# 
# #plot(gam2)
# 
# 
# plot_smooth(gam4, view="block_order", plot_all="small_font_block", rug=F, xlab= "Trial order within block",
#             ylab= "Undersweep probability", main= "Small font sentences as a function of block order",
#             col = c(pallete1[2], pallete1[1]), family= "serif",
#             cex.axis= 1.2, cex.lab= 1.4, lwd= 2, legend_plot_all = list(x=-2, y=-2))
# legend(x = 35, y= 1.4, legend = c("first block", "second block"), col = c(pallete1[2], pallete1[1]), lwd = c(2, 2), 
#        box.col = "white")
# 
# plot_diff(gam4, view = "block_order", rm.ranef = F, comp = list(small_font_block = c(1,  2)), 
#           col = pallete1[2], main= "Small font (2nd- 1st block difference)",
#           ylab= "Mean diff. in undersweep probability", xlab= "Trial order within block", print.summary = T, 
#           family= "serif", cex.axis= 1.2, cex.lab= 1.4, cex.main= 1.3, lwd= 2, hide.label = T)



#### Calvin, undersweep probability:


# # Plots:
# G= Effect(c("font_size", "line_len","launchSiteVA_C"), GLM1)
# 
# summary(G)
# 
# GD= as.data.frame(G)
# NP4USP<- ggplot(GD, aes(x= launchSiteVA_C, y=fit, ymax= upper, ymin= lower,
#                         color=line_len, linetype= line_len, fill= line_len, shape= line_len)) + theme_bw (22)+
#   geom_line(size= 1)+ geom_point(size=4)+
#   labs(x= "Launch distance from end of 1st line", y= "Undersweep Probability", 
#        color= "", shape= '', linetype= '', fill= '') +
#   geom_ribbon(alpha= 0.2, color= NA) + theme(legend.position = c(0.87, 0.88), legend.title=element_blank(),
#                                              legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
#                                              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
#                                              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
#                                              strip.background = element_rect(colour="white", fill="white"),
#                                              strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"))+
#   scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
#   scale_color_manual(values=c(pallete1[1], pallete1[2])); NP4USP+facet_wrap(~font_size)
# 
# 
#   #save(GD, file= "Models/GD.Rda")
#   save(NP4USP,file="Plots/NP4USP.png")


# Victoria, landing positions:


# #### EFFECT PLOT - TWO WAY INTERACTION 
# twi= Effect(c("font_size", "launchSiteVA_C"), LM1)
# summary(twi)
# a= as.data.frame(twi)
# a= as.data.frame(a)
# 
# ggplot(a,aes(x= launchSiteVA_C, y=fit, color= font_size)) + 
#   theme_gray(base_size=15) +geom_line(aes(linetype = font_size), size=0.8, color= 1.5)+ geom_point(color=2)+
#   labs(title= "", x= "Launch site from end of first line (deg)", y= "Landing position (deg)")
# 
# #### EFFECT PLOT - TWO WAY INTERACTION 
# twi2= Effect(c("font_size", "line_len"), LM1)
# summary(twi2)
# b= as.data.frame(twi2)
# b= as.data.frame(b)
# 
# ggplot(b,aes(x= line_len, y=fit, color= font_size)) + 
#   theme_gray(base_size=15)+ geom_point()+ geom_line(aes(group = font_size), size=0.8, color= 1.5)+
#   labs(title= "", x= "Line length", y= "Landing position (deg)")
# 
# ggplot(x, aes(fixtype, fit, color=Modality)) + geom_point() + 
#   geom_errorbar(aes(ymin=fit-se, ymax=fit+se), 
#                 width=0.1) + theme_gray(base_size=15) +geom_line(aes(group = Modality))


## Tim, saccade length:


#SL= allEffects(LM2)
#summary(SL)
#z= as.data.frame(SL)
#z=as.data.frame(z)
#colnames(z)= c("font_size", "line_len", "Rtn_sweep", "fit", "se", "lower", "upper")
# twiSL= Effect(c("font_size", "Rtn_sweep"), LM2)
# summary(twiSL)
# SLa= as.data.frame(twiSL)
# SLa= as.data.frame(SLa)
# dfSL<-  SLa
# dfSL$font_size <- droplevels(dfSL$font_size)
# dfSL$Rtn_sweep <- droplevels(dfSL$Rtn_sweep)
# #levels(df$line_len)<- c("long", 'short')
# #dfSL$Rtn_sweep<- factor(dfSL$Rtn_sweep, levels= c("Intra-line", "Return-sweep"))
# levels(dfSL$Rtn_sweep)<- c("Intra-line", 'Return-sweep')
# 
# G7<- ggplot(dfSL, aes(x= font_size, y=fit, ymax= upper, ymin= lower,
#                     color=Rtn_sweep, linetype= Rtn_sweep, fill= Rtn_sweep, shape= Rtn_sweep)) + theme_bw (22)+
#   geom_line(size= 1)+ geom_point(size=4)+
#   labs(x= "Saccade Type (intra-line vs. return-sweep)", y= "Saccade Length (deg)", 
#        color= "", shape= '', linetype= '', fill= '') +
#   geom_ribbon(alpha= 0.2, color= NA) + theme(legend.position = c(0.87, 0.88), legend.title=element_blank(),
#                                              legend.key.width = unit(1.5, 'cm'), legend.key.height = unit(0.75, 'cm'), 
#                                              panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "white"), 
#                                              panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "white"),
#                                              strip.background = element_rect(colour="white", fill="white"),
#                                              strip.text = element_text(size=22, face="bold"), text=element_text(family="serif"))+
#   scale_fill_manual(values=c(pallete1[1], pallete1[2]))+
#   scale_color_manual(values=c(pallete1[1], pallete1[2])); G7
# 
# ggsave(filename = 'Plots/SL.pdf', plot = G7, width = 10, height = 7)
# 
# plot(effect('Rtn_sweep', LM2))
# plot(effect('line_len:Rtn_sweep', LM2))
