
# MArtin R. Vasilev, 2019

# Data pre-processing:

library(EMreading)

#hgfdgdfcf

# COmprehension accuracy:

Quest<- Question(data_list = "E:/FontSizeData", maxtrial = 100)

save(Quest, file= "data/Quest.Rda")


# Trial times:
Trialt<- trialTime(data_list = "E:/FontSizeData", maxtrial = 100)

save(Trialt, file= "data/Trial_time.Rda")

M<- MultiLine(data_list = "D:/Data/Font_size/new", maxtrial = 100, reAlign = F)

data_dir<-setwd()

Paddedfiles <-EyeDoctor_PadLines(data_dir = "E:/FontSizeData", paddingSize = 5)
