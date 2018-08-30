library(ggplot2)
#library(ggplot)
dat<- read.csv("summaryData.csv", header = TRUE,sep = ",")

dat$ac_propOpt <- dat$ac_propOpt*100 # Convert to %
dat$ac_switchRate <- dat$ac_switchRate*100 # Convert to %


##### ALASDAIR'S TEMPLATE
#plt <- ggplot(dat, aes(x = ls_mean_log_rt, y = ac_meanlog2rt))
#plt <- plt + geom_point() 
#plt <- plt + geom_smooth(method = lm, colour = "black", fullrange = TRUE, linetype = 0)
#plt <- plt + theme_bw()
#plt <- plt + scale_x_continuous("split-half: mean log rt (s)",
#                                limits = c(0, 14.5 ), breaks = x_breaks, labels = x_labels)
#plt <- plt + scale_y_continuous("adaptive choice: mean log rt (s)",
#                                breaks = y_breaks, labels = y_labels)
#plt <- plt + coord_cartesian(xlim=c(9.9, 13.1), ylim=c(11,12.3))
#plt <- plt + theme(
#  panel.grid.major = element_blank(), 
#  panel.grid.minor = element_blank())
# plt <- plt + ggtitle('Split-half and Attentional Control')
#ggsave("ls_v_ac_mean_log2rt.pdf", width = figYn, height = figYn)
#ggsave("ls_v_ac_mean_log2rt.png", width = figYn, height = figYn)
#plt

figXn <- 3.5
figYn <- 2



###### Percent Opt histogram

#png(file="AC_PercentOpt_Hist.png",width=1600,height=1600,res = 300)
plt<-ggplot(dat,aes(x=ac_propOpt)) + 
  geom_histogram(col = 'black',fill = "grey", alpha = 1,breaks = seq(30,100,2))
plt <- plt + scale_x_continuous(limits = c(30, 100), breaks = seq(30,100,10))
plt <- plt + scale_y_continuous(limits = c(0, 8), breaks = seq(0,8,1))
plt <- plt + theme_classic()
plt <- plt + labs(x = "Optimal Choices (%)", y = "Frequency")
plt <- plt + theme(axis.text.x = element_text(size=8),axis.text.y = element_text(size=8),
                   axis.title.x = element_text(size=10),axis.title.y = element_text(size=10))
                   #legend.position = c(.80, .95))
                   #legend.text = element_text(size=22))
legend.box.background = element_rect()
#plt <- plt + ggtitle('A) Percent Optimal Choices')
ggsave("ac_percentOpt_hist.pdf", width = figXn, height = figYn)
plt
#ggsave("ls_v_ac_mean_log2rt.png", width = figYn, height = figYn)
#print(plt)
#dev.off()

###### Switch rate histogram

#png(file="AC_SwitchRate_Hist.png",width=1600,height=1600,res = 300)
plt<-ggplot(dat,aes(x=ac_switchRate)) + 
  geom_histogram(col = 'black',fill = "grey", alpha = 1,breaks = seq(0,60,2))
plt <- plt + scale_x_continuous(limits = c(0, 60), breaks = seq(0,60,10))
plt <- plt + scale_y_continuous(limits = c(0, 8), breaks = seq(0,8,1))
plt <- plt + theme_classic()
plt <- plt + labs(x = "Switch Rate (%)", y = "Frequency")
plt <- plt + theme(axis.text.x = element_text(size=8),axis.text.y = element_text(size=8),
                   axis.title.x = element_text(size=10),axis.title.y = element_text(size=10))
#legend.position = c(.80, .95))
#legend.text = element_text(size=22))
legend.box.background = element_rect()
#plt <- plt + ggtitle('B) Switch Rate')
ggsave("ac_switchRate_hist.pdf", width = figXn, height = figYn)
plt




###### FG task

dat<- read.csv("Data_MultiTargForag_allsubs.txt", header = TRUE,sep = "\t")

###### Feature Foraging

#png(file="FG_feat_runLength_Hist.png",width=1600,height=1600,res = 300)
plt<-ggplot(dat,aes(x=Feature_RunLength)) + 
  geom_histogram(col = 'black',fill = "grey", alpha = 1,breaks = seq(0,35,.5))
plt <- plt + scale_x_continuous(limits = c(0, 20), breaks = seq(0,20,2))
plt <- plt + scale_y_continuous(limits = c(0, 35), breaks = seq(0,40,10))
plt <- plt + theme_classic()
plt <- plt + labs(x = "Run Length", y = "Frequency")
plt <- plt + theme(axis.text.x = element_text(size=8),axis.text.y = element_text(size=8),
                   axis.title.x = element_text(size=10),axis.title.y = element_text(size=10))
#legend.position = c(.80, .95))
#legend.text = element_text(size=22))
legend.box.background = element_rect()
#plt <- plt + ggtitle('A) Feature foraging')
ggsave("fg_featureRunLength_hist.pdf", width = figXn, height = figYn)
plt
#ggsave("ls_v_ac_mean_log2rt.png", width = figYn, height = figYn)
#print(plt)
#dev.off()

###### Conjunction Foraging 

plt<-ggplot(dat,aes(x=Conj_RunLength)) + 
  geom_histogram(col = 'black',fill = "grey", alpha = 1,breaks = seq(0,35,.5))
plt <- plt + scale_x_continuous(limits = c(0, 20), breaks = seq(0,20,2))
plt <- plt + scale_y_continuous(limits = c(0, 35), breaks = seq(0,40,10))
plt <- plt + theme_classic()
plt <- plt + labs(x = "Run Length", y = "Frequency")
plt <- plt + theme(axis.text.x = element_text(size=8),axis.text.y = element_text(size=8),
                   axis.title.x = element_text(size=10),axis.title.y = element_text(size=10))
#legend.position = c(.80, .95))
#legend.text = element_text(size=22))
legend.box.background = element_rect()
#plt <- plt + ggtitle('B) Conjunction foraging')
ggsave("fg_conjRunLength_hist.pdf", width = figXn, height = figYn)
plt
