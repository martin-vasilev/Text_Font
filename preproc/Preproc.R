
# Martin R. Vasilev, 2019

# Data pre-processing:


# Manual pre-processing of asc files:

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+

#Paddedfiles <-EyeDoctor_PadLines(data_dir = data_dir, paddingSize = 5)

# Install/ load R package used in preprocessing:

if('EMreading' %in% rownames(installed.packages())==FALSE){
  if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
  install_github('martin-vasilev/EMreading')
}else{
  library(EMreading)
}


data_dir= "D:/Data/Font_size" # Martin
#data_dir= "E:/FontSizeData" # Victoria

###########################
# Comprehension accuracy: #
###########################

if(!file.exists("data/Quest.Rda")){
  Quest<- Question(data_list = data_dir, maxtrial = 100)
  save(Quest, file= "data/Quest.Rda")
  write.csv2(Quest, "data/Quest.csv")
} else{
  load("data/Quest.Rda")
}


library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mQuest

################
# Trial times: #
################

if(!file.exists("data/Trial_time.Rda")){
  
  Trialt<- trialTime(data_list = data_dir, maxtrial = 100)
  save(Trialt, file= "data/Trial_time.Rda")
  write.csv2(Trialt, "data/Trial_time.csv")
}else{
  load("data/Trial_time.Rda")
}

DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))
mTime


##################
# Raw Fixations: #
##################

if(!file.exists("preproc/raw_fix.Rda")){
  # extract raw data & merge it with da1 files:
  raw_fix<- preprocFromDA1(data_dir = data_dir, maxtrial = 100, padding = 5)
  save(raw_fix, file= "preproc/raw_fix.Rda")
}else{
  load("preproc/raw_fix.Rda")
}





