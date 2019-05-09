
# MArtin R. Vasilev, 2019

# Data pre-processing:


#hgfdgdfcf

# Calvin 1-21
# Victoria- 22- 43
# Martin 44+

library(EMreading)

#data_dir= "D:/Data/Font_size" # Martin
data_dir= "E:/FontSizeData" # Victoria

###########################
# Comprehension accuracy: #
###########################

Quest<- Question(data_list = data_dir, maxtrial = 100)


save(Quest, file= "data/Quest.Rda")


# Trial times:
Trialt<- trialTime(data_list = data_dir, maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")

M<- MultiLine(data_list = data_dir, maxtrial = 100, reAlign = F)

#data_dir<-setwd()

Paddedfiles <-EyeDoctor_PadLines(data_dir = data_dir, paddingSize = 5)

library(reshape)
DesQuest<- melt(Quest, id=c('sub', 'item', 'cond'), 
                measure=c("accuracy"), na.rm=TRUE)
mQuest<- cast(DesQuest, sub ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))

################
# Trial times: #
################

Trialt<- trialTime(data_list = data_dir, maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")


DesTime<- melt(Trialt, id=c('sub', 'item', 'cond'), 
                measure=c("duration_ms"), na.rm=TRUE)
mTime<- cast(DesTime, cond ~ variable
              ,function(x) c(M=signif(mean(x),3)
                             , SD= sd(x) ))



#M<- MultiLine(data_list = "D:/Data/Font_size/new", maxtrial = 100, reAlign = F)


EyeDoctor_PadLines(data_dir)

