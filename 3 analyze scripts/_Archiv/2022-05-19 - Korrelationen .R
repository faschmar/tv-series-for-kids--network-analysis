sub_marinette <- subset(sentimentTable, sentimentTable$Name=='Marinette')

plot(sub_marinette$Sentiment~sub_marinette$NA.1, type="l")
plot(sub_marinette$Sentiment~sub_marinette$NA.1)
sub_lb <- subset(sentimentTable, sentimentTable$Name=='Ladybug')
plot(sub_lb$Sentiment~sub_lb$NA.1)
points(sub_marinette$Sentiment~sub_marinette$NA.1)
plot(sub_lb$Sentiment~sub_lb$NA.1)
points(sub_marinette$Sentiment~sub_marinette$NA.1, col = 3)
cor(sub_lb$Sentiment, sub_marinette$Sentiment)

sub_cn <- subset(sentimentTable, sentimentTable$Name=='Cat Noir')
sub_a <- subset(sentimentTable, sentimentTable$Name=='Adrien')

episodes_cn_a <- which(sub_cn$NA.1%in%sub_a$NA.1==T, arr.ind=T)

cor(sub_cn$Sentiment[episodes_cn_a], sub_a$Sentiment[episodes_cn_a], use="complete.obs")
episodes_a_m <- which(sub_a$NA.1%in%sub_marinette$NA.1==T, arr.ind=T)
episodes_lb_m <- which(sub_lb$NA.1%in%sub_marinette$NA.1==T, arr.ind=T)
episodes_cn_lb <- which(sub_cn$NA.1%in%sub_lb$NA.1==T, arr.ind=T)
corCN_LB<-cor(sub_cn$Sentiment[episodes_cn_lb], sub_lb$Sentiment[episodes_cn_lb], use="complete.obs")

corA_M<-cor(sub_a$Sentiment[episodes_a_m], sub_marinette$Sentiment[episodes_a_m], use="complete.obs")

corCB_A<-cor(sub_cn$Sentiment[episodes_cn_a], sub_a$Sentiment[episodes_cn_a], use="complete.obs")
corM_LB<-cor(sub_marinette$Sentiment[episodes_lb_m], sub_lb$Sentiment[episodes_lb_m], use="complete.obs")
