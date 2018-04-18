#Dependency Parsing
#Lalitia Apsari Thesis 2018

setwd("~/Documents/01DH/S2/Thesis/")

#install package UD Pipe
install.packages("udpipe")
library(udpipe)

#tokenize, tag, dependency parsing annotation. output in CoNLL-U format (TEST)
txt = c("Itu kalimatnya, ini sudah lama aman-aman saja, kenapa sekarang")
x = udpipe_annotate(ud_id, x = txt)
cat(x$conllu)

#download Indonesian treebank model
id = udpipe_download_model(language = "indonesian")
ud_id = udpipe_load_model(id$file_model)

#WRITTEN DATA
#import TXT files
written_sen = scan(file = "Written_txt/written_sen.txt", what = "character", sep = "\n")
#parse using UDPipe
written_senp = udpipe_annotate(ud_id, x = written_sen)
#screen result
cat(written_senp$conllu)
#write to CSV
write.csv(written_senp, file = "Written_parse/written_senp_raw.csv")


#AUDIO DATA
#import TXT files
audio_sen = scan(file = "Audio_txt/audio_sen.txt", what = "character", sep = "\n")
#parse using UDPipe
audio_senp = udpipe_annotate(ud_id, x = audio_sen)
#screen result
cat(audio_senp$conllu)
#write to CSV
write.csv(audio_senp, file = "Audio_parse/audio_senp_raw.csv")

#MANUAL CHECK
#choose sentence only
t = read.csv("written_parsed_raw.csv")
ts = t$sentence
write.csv(ts, file = "written_sen.csv")
l = read.csv("Audio_parse/audio_senp_raw.csv")
ls = l$sentence
write.csv(ls, file = "Audio_txt/audio_sen.csv")

#DATA VISUALIZATION
datatulis = read.csv("Written_analysis/written_cleaned1804_out.csv")
tulis = datatulis[!rev(duplicated(rev(datatulis$doc_id))),]
t = tulis[which(tulis$sum_dependency_length < 400),]
ts = t[which(t$mdd_negative < 10),]

tsmax10 = ts[which(ts$token_length <= 10),]
tsmax20 = ts[which(ts$token_length > 10 & ts$token_length <= 20),]
tsmin20 = ts[which(ts$token_length > 20),]



datalisan = read.csv("Audio_analysis/audio_cleaned1804_out.csv")
lisan = datalisan[!rev(duplicated(rev(datalisan$doc_id))),]
l = lisan[which(lisan$sum_dependency_length < 300),]
ls = l[which(l$token_length > 1),]

ls4 = ls[which(ls$token_length == 4),]
write.csv(ls5, file = "Audio_parse/ls5.csv")

ls5 = ls[which(ls$token_length == 5),]
ls12 = ls[which(ls$token_length == 12),]
ls22 = ls[which(ls$token_length == 22),]
lstop = rbind(ls5, ls12, ls22)

lsmax10 = ls[which(ls$token_length <= 10),]
lsmax20 = ls[which(ls$token_length > 10 & ls$token_length <= 20),]
lsmin20 = ls[which(ls$token_length > 20),]

#load plotly
library(plotly)
packageVersion('plotly')

#plot sentence length in a bar chart
tsl = table(ts$token_length)
tsl = as.data.frame(tsl)
plot_ly(tsl, x = tsl$Var1, y = (tsl$Freq/sum(tsl$Freq)), type = "bar") 

lsl = table(ls$token_length)
lsl = as.data.frame(lsl)
plot_ly(x = lsl$Var1, y = (lsl$Freq/sum(lsl$Freq)), type = "bar")

tlsl = cbind(tsl, lsl$Freq)

#plot dependency length in a bar chart
tsd = table(ts$sum_dependency_length)
tsd = as.data.frame(tsd)
plot_ly(x = tsd$Var1, y = tsd$Freq, type = "bar")

lsd = table(ls$sum_dependency_length)
lsd = as.data.frame(lsd)
plot_ly(x = lsd$Var1, y = lsd$Freq, type = "bar")
lsd5 = table(ls5$sum_dependency_length)
lsd5 = as.data.frame(lsd5)
plot_ly(x = lsd5$Var1, y = lsd5$Freq, type = "bar")
lsd12 = table(ls12$sum_dependency_length)
lsd12 = as.data.frame(lsd12)
plot_ly(x = lsd12$Var1, y = lsd12$Freq, type = "bar")
lsd22 = table(ls22$sum_dependency_length)
lsd22 = as.data.frame(lsd22)
plot_ly(x = lsd22$Var1, y = lsd22$Freq, type = "bar")

#plot Mean Dependency Distance in a bar chart
tsm = table(ts$mdd)
tsm = as.data.frame(tsm)
plot_ly(x = tsm$Var1, y = tsm$Freq, type = "bar")

lsm = table(ls$mdd)
lsm = as.data.frame(lsm)
plot_ly(x = lsm$Var1, y = lsm$Freq, type = "bar")
lsm5 = table(ls5$mdd)
lsm5 = as.data.frame(lsm5)
plot_ly(x = lsm5$Var1, y = lsm5$Freq, type = "bar")
lsm12 = table(ls12$mdd)
lsm12 = as.data.frame(lsm12)
plot_ly(x = lsm12$Var1, y = lsm12$Freq, type = "bar")
lsm22 = table(ls22$mdd)
lsm22 = as.data.frame(lsm22)
plot_ly(x = lsm22$Var1, y = lsm22$Freq, type = "bar")

#plot sum dependency length in a scatter plot
#total sum dependency length
ggplot(ts, aes(x = ts$token_length, y = ts$sum_dependency_length)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$sum_dependency_length) 
#positive sum dependency length
ggplot(ts, aes(x = ts$token_length, y = ts$sum_positive)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$sum_positive) 
#negative sum dependency length
ggplot(ts, aes(x = ts$token_length, y = ts$sum_negative)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$sum_negative) 

#total sum dependency length
ggplot(ls, aes(x = ls$token_length, y = ls$sum_dependency_length)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$sum_dependency_length) 
#positive sum dependency length
ggplot(ls, aes(x = ls$token_length, y = ls$sum_positive)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$sum_positive) 
#negative sum dependency length
ggplot(ls, aes(x = ls$token_length, y = ls$sum_negative)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$sum_negative) 
#cluster sum dependency length
ggplot(lsmax10, aes(x = lsmax10$token_length, y = lsmax10$sum_dependency_length)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmax10$token_length, y = lsmax10$sum_dependency_length) 
ggplot(lsmax20, aes(x = lsmax20$token_length, y = lsmax20$sum_dependency_length)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmax20$token_length, y = lsmax20$sum_dependency_length)
ggplot(lsmin20, aes(x = lsmin20$token_length, y = lsmin20$sum_dependency_length)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmin20$token_length, y = lsmin20$sum_dependency_length)

#plot Mean Dependency Distance in a scatter plot
#total MDD
ggplot(ts, aes(x = ts$token_length, y = ts$mdd)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$mdd)
#positive MDD
ggplot(ts, aes(x = ts$token_length, y = ts$mdd_positive)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$mdd_positive)
#negative MDD
ggplot(ts, aes(x = ts$token_length, y = ts$mdd_negative)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ts$token_length, y = ts$mdd_negative)

#total MDD
ggplot(ls, aes(x = ls$token_length, y = ls$mdd)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$mdd)
#positive MDD
ggplot(ls, aes(x = ls$token_length, y = ls$mdd_positive)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$mdd_positive)
#negative MDD
ggplot(ls, aes(x = ls$token_length, y = ls$mdd_negative)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = ls$token_length, y = ls$mdd_negative)
ggplot(lsmax10, aes(x = lsmax10$token_length, y = lsmax10$mdd)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmax10$token_length, y = lsmax10$mdd)
ggplot(lsmax20, aes(x = lsmax20$token_length, y = lsmax20$mdd)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmax20$token_length, y = lsmax20$mdd)
ggplot(lsmin20, aes(x = lsmin20$token_length, y = lsmin20$mdd)) + 
  guides(color = 'legend') + scale_size_area(max_size = 5) + #using geom point with transparency
  geom_point(alpha = 1/10) +
  stat_smooth(method = "loess", size = 0.5, level = 0.95, color = "red") +
  labs(x = lsmin20$token_length, y = lsmin20$mdd)
  
ggplot(ls, aes(x = ls$token_length, y = ls$mdd)) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') + scale_size_area(max_size = 5) +
  stat_smooth(method = "lm", formula = y ~ x + poly(x, 2)) +
  labs(x = ls$token_length, y = ls$mdd)
ggplot(lsmax10, aes(x = lsmax10$token_length, y = lsmax10$mdd)) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') + scale_size_area(max_size = 5) +
  stat_smooth(method = "lm", formula = y ~ x + poly(x, 2)) +
  labs(x = lsmax10$token_length, y = lsmax10$mdd)
ggplot(lsmax20, aes(x = lsmax20$token_length, y = lsmax20$mdd)) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') + scale_size_area(max_size = 5) +
  stat_smooth(method = "lm", formula = y ~ x + poly(x, 2)) +
  labs(x = lsmax20$token_length, y = lsmax20$mdd)
ggplot(lsmin20, aes(x = lsmin20$token_length, y = lsmin20$mdd)) + 
  geom_count(aes(color = ..n.., size = ..n..)) +
  guides(color = 'legend') + scale_size_area(max_size = 5) +
  stat_smooth(method = "lm", formula = y ~ x + poly(x, 2)) +
  labs(x = lsmin20$token_length, y = lsmin20$mdd)

#boxplot comparison top token length
ggplot(lstop, aes(x=lstop$token_length, y=lstop$sum_dependency_length, group = lstop$token_length)) + geom_boxplot()



