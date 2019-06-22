library(dplyr)
library(ggplot2)
library('corrplot') 
library('car')  
library(dummies)

setwd('C:/Users/요우용/Desktop/온라인상에의 사회활동.csv/온라인 커뮤니티 활동')
getwd()

# csv파일을 메모장을 통해 UTF-8로 인코딩한 후 다시 메모장을 통해 UTF-8로 인코딩한 csv를 ANSI로 인코딩
com1 <- read.csv('온라인 커뮤니티 활동2.csv',header=T,stringsAsFactors = F)
com2 <- read.csv('온라인 커뮤니티 활동_커뮤니티용2.csv',header=T, stringsAsFactors=F)
com <- read.csv('온라인 커뮤니티 활동_통합.csv',header=T, stringsAsFactors = F)
head(com)

com2 <- com
head(com2)
## com2에서 friends / appointment / culture / cul_friends / offline / offline_content 처리
# 1. friends 처리
com2$friends <- gsub('3개월에 0-2명',1,com2$friends)
com2$friends <- gsub('한 달에 1-2명',2,com2$friends)
com2$friends <- gsub('일주일에 1-2명',3,com2$friends)
com2$friends <- gsub('일주일에 3-6명',4,com2$friends)
com2$friends <- gsub('일주일에 7-10명',5,com2$friends)
com2$friends <- gsub('일주일에 11명 이상',6,com2$friends)
com2$friends

# 2. appointment 처리
com2$appointment <- gsub('3개월에 0-2회',1,com2$appointment)
com2$appointment <- gsub('한 달에 1-2회',2,com2$appointment)
com2$appointment <- gsub('일주일에 1-2회',3,com2$appointment)
com2$appointment <- gsub('일주일에 3-5회',4,com2$appointment)
com2$appointment <- gsub('일주일에 6-10회',5,com2$appointment)
com2$appointment <- gsub('일주일에 11회 이상',6,com2$appointment)
com2$appointment

# 3. culture 처리
com2$culture <- gsub('없음',1,com2$culture)
com2$culture <- gsub('3개월에 1-2회 정도',2,com2$culture)
com2$culture <- gsub('한 달에 1-2회 정도',3,com2$culture)
com2$culture <- gsub('1주일에 1-2회 정도',4,com2$culture)
com2$culture <- gsub('1주일에 3-4 회 정도',5,com2$culture)
com2$culture <- gsub('1주일에 5회 이상',6,com2$culture)
com2$culture

# 4. cul_friends 처리
com2$cul_friends <- gsub('혼자',0,com2$cul_friends)
com2$cul_friends <- gsub('친구/지인',1,com2$cul_friends)
com2$cul_friends

# 5. offline 처리
com2$offline <- gsub('아니오',0,com2$offline)
com2$offline <- gsub('예',1,com2$offline)
com2$offline

# 6. offline_content
com2$offline_content <- gsub('매우 불만족',1,com2$offline_content)
com2$offline_content <- gsub('불만족',2,com2$offline_content)
com2$offline_content <- gsub('매우 만족',3,com2$offline_content)
com2$offline_content <- gsub('만족',4,com2$offline_content)
com2$offline_content

# ag20: 20대만 포함 - 102명
g20 <- com2 %>% filter(age == '20대') ; head(ag20) ; nrow(ag20) 

# ag20 - 20대만(102명)
ag20 <- ag20[,-c(2,4)]
ag20 <- cbind(dummy.data.frame(ag20[,c(1,2)]),ag20[,3:ncol(ag20)]) ; head(ag20)
colnames(ag20)[c(2,3)] <- c('edu_high','edu_univ') ; head(ag20)

## com_read / com_comment / com_like / com_post를 연속형으로 바꿔주기 (한달기준)
# ag20
head(ag20)
ag20_1 <- ag20

# com_read 처리
ag20_1$com_read <- gsub('한 달에 1~3개',6,ag20_1$com_read)
ag20_1$com_read <- gsub('일주일에 1~3개',36,ag20_1$com_read)
ag20_1$com_read <- gsub('하루에 1-5개',255,ag20_1$com_read)
ag20_1$com_read <- gsub('하루에 6-15개',1500,ag20_1$com_read)
ag20_1$com_read <- gsub('하루에 16-30개',2250,ag20_1$com_read)
ag20_1$com_read <- gsub('하루에 31개 이상',3600,ag20_1$com_read)
ag20_1$com_read

# com_comment 처리
ag20_1$com_comment <- gsub('없음',0,ag20_1$com_comment)
ag20_1$com_comment <- gsub('한 달에 1-3개',6,ag20_1$com_comment)
ag20_1$com_comment <- gsub('일주일에 1-3개',36,ag20_1$com_comment)
ag20_1$com_comment <- gsub('하루에 1-4개',240,ag20_1$com_comment)
ag20_1$com_comment <- gsub('하루에 5-9개',630,ag20_1$com_comment)
ag20_1$com_comment <- gsub('하루에 10개 이상',1350,ag20_1$com_comment)
ag20_1$com_comment

# com_like 처리
ag20_1$com_like <- gsub('없음',0,ag20_1$com_like)
ag20_1$com_like <- gsub('한 달에 1-3개',6,ag20_1$com_like)
ag20_1$com_like <- gsub('일주일에 1-3개',36,ag20_1$com_like)
ag20_1$com_like <- gsub('하루에 1-4개',240,ag20_1$com_like)
ag20_1$com_like <- gsub('하루에 5-9개',630,ag20_1$com_like)
ag20_1$com_like <- gsub('하루에 10개 이상',1350,ag20_1$com_like)
ag20_1$com_like

# com_post 처리
ag20_1$com_post <- gsub('없음',0,ag20_1$com_post)
ag20_1$com_post <- gsub('한 달에 1-2개',3,ag20_1$com_post)
ag20_1$com_post <- gsub('일주일에 1-2개',12,ag20_1$com_post)
ag20_1$com_post <- gsub('하루에 1-2개',90,ag20_1$com_post)
ag20_1$com_post <- gsub('하루에 3-5개',360,ag20_1$com_post)
ag20_1$com_post <- gsub('하루에 6개 이상',900,ag20_1$com_post)
ag20_1$com_post

head(ag20_1)


# 뉴스관련 변수 제외 (필요없음)

ag20_1_df <- ag20_1[,-c(8,9,10)]
for (i in 1:ncol(ag20_1_df)) {
  ag20_1_df[,i] <- as.numeric(ag20_1_df[,i])
}
summary(ag20_1_df)


head(ag20_1_df)
cor(ag20_1_df)
corrplot(cor(ag20_1_df))
df_1_h <- ag20_1_df[,-c(2,8)]


#####################################################################################
# female / male 나눈 버전

head(ag20_1)
ag20_m1 <- ag20_1 %>% filter(sex == 0)
ag20_f1 <- ag20_1 %>% filter(sex == 1)
head(ag20_m1)
head(ag20_f1)

ag20_m1 <- ag20_m1[,-c(1,2,8,9,10,11)] # sex & edu_high & friends 제외
ag20_f1 <- ag20_f1[,-c(1,2,8,9,10,11)]

for(i in 1:ncol(ag20_m1)){
  ag20_m1[,i] <- as.numeric(ag20_m1[,i])
}

for(i in 1:ncol(ag20_f1)){
  ag20_f1[,i] <- as.numeric(ag20_f1[,i])
}

########################################################################################################
########################################################################################################
####culture / cul_friends / offline 빼고 척도####

ag20_mo <- ag20_1_df[,-c(2,8,10,11,12)]
head(ag20_mo)


###########################################################################################################
### appointment + offline # ag20

head(ag20_1_df)
df20 <- ag20_1_df[,-c(2,10,11,16)]
head(df20)
df20$ap_of <- (df20$appointment+df20$offline*3) # 사회성에 대한 척도 만들기 - 대외적인 사회성에 더 큰 비중을 둠
head(df20)

df20_mo <- df20[,-c(7,8,9)]
mode1 <- lm(ap_of ~ .,data=df20_mo)
summary(mode1)

ggplot(data=df20_mo, aes(x=com_post, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=com_comment, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=com_read, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=com_like, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=attention, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=talk, y=ap_of))+geom_point()+geom_smooth()
ggplot(data=df20_mo, aes(x=with_many, y=ap_of))+geom_point()+geom_smooth()

corrplot(cor(df20_mo))
cor(df20_mo)

ggplot(data=df20_mo, aes(x=ap_of))+geom_bar()
ggplot(data=df20_mo, aes(x=com_read))+geom_bar()
ggplot(data=df20_mo, aes(x=com_comment))+geom_bar()
ggplot(data=df20_mo, aes(x=com_post))+geom_bar()
ggplot(data=df20_mo, aes(x=com_like))+geom_bar()


########## 전제 검정: 커뮤니티 활동이 사회성에 영향을 미친다.

mode1 <- lm(ap_of ~ .,data=df20_mo)
summary(mode1) # 0.2085
vif(mode1)

mode1 <- lm(ap_of ~ sex+edu_univ+attention+with_many+talk,data=df20_mo) # 통제변수들만
summary(mode1) # 0.1232
vif(mode1)

f = ((0.2085-0.1232)/4)/(0.1232/(102-10)) ; f # F0 = 15.9245 ==> 커뮤니티 활동이 사회성에 영향을 미침

########### 가설1 검정: 각각의 커뮤니티 활동 방식(post/comment/read/like)가 사회성에 영향을 미치는가
head(df20_mo)

## com_read -> 유의미X
df20_mor <- df20_mo[,-c(4,5,6)]
model <- lm(ap_of ~ com_read, data=df20_mor) ; summary(model) 
model1 <- lm(ap_of ~ ., data=df20_mor) ; summary(model) 
vif(model)

ggplot(data=df20_mor, aes(x=com_read, y=ap_of)) + geom_smooth(se=F)+
  theme_bw() + geom_point()

## com_comment -> 유의미
df20_moc <- df20_mo[,-c(3,5,6)]
model2_0 <- lm(ap_of ~ com_comment, data=df20_moc) ; summary(model) # **
model2 <- lm(ap_of ~ ., data=df20_moc) ; summary(model2) # **
model2_2 <- lm(ap_of ~ .+I(com_comment^2), data=df20_moc) ; summary(model2_2) # '
vif(model)
# ap_of = 0.000003179*com_comment^2 -0.005509*com_comment + other_terms
# ap_of = 3.179e-06*com_comment^2 -5.509e-03*com_comment + other_terms
(0.005509)/(2*0.000003179) # 866
ggplot(data=df20_moc, aes(x=com_comment, y=ap_of)) + geom_smooth(se=F, size=1.4,color='#008FC4') + theme_bw()+
  xlab('댓글 단 수') + ylab('사회성') + theme(axis.title.x=element_text(size=18), axis.title.y=element_text(size=18))+
  ggtitle('(20대 전체) 댓글 쓴 정도가 사회성에 미치는 영향') + theme(plot.title=element_text(size=20,hjust=0.5,vjust=4,face='bold'))

## com_like -> 유의미
df20_mol <- df20_mo[,-c(4,3,6)]
model3_0 <- lm(ap_of ~ com_like, data=df20_mol) ; summary(model3_0) # '
model3 <- lm(ap_of ~ ., data=df20_mol) ; summary(model3) # '
model3_2 <- lm(ap_of ~ .+I(com_like^2), data=df20_mol) ; summary(model3_2) # *
# ap_of = 0.000002936*com_like^2 - 0.004699*com_like
0.004699/(2*0.000002936) # 800
vif(model)
ggplot(data=df20_mol,aes(x=com_like,y=ap_of))+geom_smooth(se=F)+geom_point()+
  theme_bw()
# ap_of = -0.25*com_like + other_terms


## com_post -> 유의미
df20_mop <- df20_mo[,-c(4,5,3)]
model4_0 <- lm(ap_of ~ com_post, data=df20_mop) ; summary(model) # ***
model4 <- lm(ap_of ~ ., data=df20_mop) ; summary(model4) # **
model4_2 <- lm(ap_of ~ .+I(com_post^2), data=df20_mop) ; summary(model4_2) # *
vif(model)
# ap_of = 0.00002142*com_post^2-0.0255*com_post + other_terms
curve(0.00002142*x^2-0.0255*x,from=0,to=900)
(0.0255)/(2*0.00002142) # 595
# ap_of = 2.142e-05*com_post^2-2.255e-02*com_post + other_terms

ggplot(data=df20_mop,aes(x=com_post, y=ap_of))+geom_smooth(se=F, size=1.4, color ='#008FC4')+theme_bw()+
  xlab('게시물 올린 수') + ylab('사회성') + theme(axis.title.x=element_text(size=18), axis.title.y=element_text(size=18))+
  ggtitle('(20대 전체) 게시물 올린 정도가 사회성에 미치는 영향') + theme(plot.title=element_text(size=20,hjust=0.5,vjust=4,face='bold'))


############ 가설2 검정: 성별에 따라 각각의 커뮤니티 활동방식(post/comment/read/like)이 사회성에 미치는 영향이 다르다. == sex의 조절효과가 있다.

# com_post
model4s <- lm(ap_of~.+com_post*sex,data=df20_mop) 
summary(model4s) # 0.2318
vif(mode1)

ggplot(data=df20_mo, aes(x=com_post, y=ap_of, color = as.factor(sex)))+geom_smooth(aes(group = as.factor(sex)),se=F, size=1.4)+
  theme_classic()+ylab('사회성')+xlab('게시물 올린 수')+scale_color_discrete(limits=c('1','0'), labels=c('Female','Male')) + labs(color ='Female/Male')

# com_comment
model2s <- lm(ap_of~.+com_comment*sex,data=df20_moc) 
summary(model2s) # 0.123
vif(mode1)

ggplot(data=df20_mo, aes(x=com_comment, y=ap_of, color = as.factor(sex)))+geom_smooth(aes(group = as.factor(sex)),se=F, size=1.4)+
  theme_classic()+ylab('사회성')+xlab('댓글 남긴 수')+scale_color_discrete(limits=c('1','0'), labels=c('Female','Male')) + labs(color ='Female/Male')

# com_like
model3s <- lm(ap_of~.+com_like*sex,data=df20_mol) 
summary(model3s) # 0.0950
vif(mode1)

ggplot(data=df20_mo, aes(x=com_like, y=ap_of, color = as.factor(sex)))+geom_smooth(aes(group = as.factor(sex)),se=F, size=1.4)+
  theme_classic()+ylab('사회성')+xlab('좋아요 눌린 수')+scale_color_discrete(limits=c('1','0'), labels=c('Female','Male')) + labs(color ='Female/Male')

# com_read
## *
model1s <- lm(ap_of~.+com_read*sex,data=df20_mor) ##########와 있다 ㅠㅠ com_read랑!!!!!!! sex 조절효과 있따 
vif(mode1)

ggplot(data=df20_mo, aes(x=com_read, y=ap_of, color = as.factor(sex)))+geom_smooth(aes(group = as.factor(sex)),se=F, size=1.4)+
  theme_bw()+ylab('사회성')+xlab('게시물 읽은 수')+scale_color_discrete(limits=c('1','0'), labels=c('Female','Male')) + labs(color ='Female/Male') +
  theme(axis.title.x=element_text(size = 18), axis.title.y=element_text(size=18))+ggtitle('게시물을 읽는 정도가 사회성에 미치는 영향') +
  theme(plot.title = element_text(size=20, vjust = 4, hjust= 0.5, face ='bold'))

nrow(df20_mo %>% filter(sex == 1)) # 59
nrow(df20_mo %>% filter(sex == 0)) # 43


m <- lm(ap_of ~ sex, data = df20_mo) ; summary(m)

######## 가설2++ sex로 나눠 com_read와 사회성 관계 검정 (위의 결과를 바탕으로)

##com_read

df20_morm <- df20_mor %>% filter(sex==0)
df20_morm <- df20_morm[,-1]
df20_morf <- df20_mor %>% filter(sex==1)
df20_morf <- df20_morf[,-1]

head(df20_morm)
head(df20_morf)

ggplot(data=df20_morm, aes(x=com_read, y=ap_of)) + geom_smooth(se = F) + geom_point()
model1s_m <- lm(ap_of ~.,data = df20_morm) ; summary(model1s_m) # '
model1s_m2 <- lm(ap_of ~.+I(com_read^2),data = df20_morm) ; summary(model1s_m2) # **
vif(model)
# ap_of = -0.0000007382*com_read^2 + 0.002339*com_read + other_terms
# 해석1: 20대 남성의 경우 3.8부분(하루에 15개 미만)까지는 게시물을 읽을수록 사회성이 증가하지만 그 후부터는 사회성이 감소한다. # BY 회귀식 
0.002339/(2*0.0000007382) # 1584
# 해석2: 20대 남성의 경우 하루에 30개 미만까지는 게시물을 읽을수록 사회성이 증가하지만 그 후부터는 감소한다. # BY 도표

ggplot(data=df20_morf, aes(x=com_read, y=ap_of)) + geom_smooth(se=F)+
  geom_point()+theme_bw()
model1s_f <- lm(ap_of ~.,data = df20_morf) ; summary(model1s_f) # '
# ap_of = -0.-0003646com_read + other_terms
# 해석: 20대 여성의 경우 게시물을 한 단위씩 더 읽을수록, 사회성이 0.26씩 감소한다. => practically 유의미???
vif(model)


head(df20_mo)
ggplot(data=df20_mo, aes(x=com_read,y=ap_of,color=as.factor(sex)))+geom_smooth(se=F)


fit1 <- lm(ap_of ~ com_like + I(com_like^2), data = df20_morm)
fit2 <- lm(ap_of ~ com_like, data = df20_morf)
prd <- data.frame(hp = seq(from = range(mtcars$hp)[1], to = range(mtcars$hp)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

ggplot(prd, aes(x = hp, y = fit)) +
  theme_bw() +
  geom_line() +
  geom_smooth(stat = "identity") +
  geom_point(data = mtcars, aes(x = hp, y = mpg))

## 도표들

# com_read / com_comment / com_like / com_post 비교
library(stargazer)
stargazer(model1,model2,model3,model4,
          type="latex",
          out = 'com_compare_conti3.txt',
          column.labels = c('Read','Comment','Like','Post'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table1',
          covariate.labels = c('성별','교육수준','게시물 읽은 수','댓글 단 수',
                               '좋아요 누른 수','게시물 올린 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도'),
          omit = 'Constant',
          dep.var.labels='사회성') 

# com_comment / com_comment^2

stargazer(model2_0,model2,model2_2,
          type="html",
          out = 'com_comment_compare_conti2.htm',
          column.labels = c('Model1','Model2','Model3'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table2',
          covariate.labels = c('성별','교육수준','댓글 단 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도',
                               '댓글 단 수**2'),
          omit = 'Constant',
          dep.var.labels='사회성') 


# com_like / com_like^3

stargazer(model3_0,model3,model3_2,
          type="html",
          out = 'com_like_compare_conti2.htm',
          column.labels = c('Model1','Model2','Model3'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table3',
          covariate.labels = c('성별','교육수준','좋아요 누른 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도',
                               '좋아요 누른 수**2'),
          omit = 'Constant',
          dep.var.labels='사회성')

# com_post / com_post^2

stargazer(model4_0,model4,model4_2,
          type="html",
          out = 'com_post_compare_conti2.htm',
          column.labels = c('Model1','Model2','Model3'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table4',
          covariate.labels = c('성별','교육수준','게시물 올린 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도',
                               '게시물 올린 수**2'),
          omit = 'Constant',
          dep.var.labels='사회성') 


# sex의 조절변인확인

stargazer(model1s,model2s,model3s,model4s,
          type="html",
          out = 'sex_compare_conti2.htm',
          column.labels = c('Read','Comment','Like','Post'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table5',
          covariate.labels = c('성별','교육수준','게시물 읽은 수','댓글 단 수',
                               '좋아요 누른 수','게시물 올린 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도',
                               '게시물 읽은 수*성별','댓글 단 수*성별',
                               '좋아요 누른 수*성별','게시물 올린 수*성별'),
          omit = 'Constant',
          dep.var.labels='사회성') 

# com_read * sex

stargazer(model1s_m,model1s_m2,model1s_f,
          type="html",
          out = 'com_like_sex_conti2.htm',
          column.labels = c('Male1','Male2','Female'),
          model.numbers = FALSE,
          intercept.bottom = FALSE, 
          single.row=FALSE,     
          notes.append = FALSE, 
          header=FALSE,
          title='Table6',
          covariate.labels = c('교육수준','게시물 읽은 수','관심을 즐기는 정도',
                               '다른 사람들과 있는 것을 즐기는 정도','대화를 즐기는 정도',
                               '게시물 읽은 수**2'),
          omit = 'Constant',
          dep.var.labels='사회성')  



nrow(df20_mo %>% filter(com_comment == 1350))
4/102 # 3.9%
nrow(df20_mo %>% filter(com_post == 900))
3/102
