install.packages("stringr")
install.packages("dplyr")
install.packages("rvest")
install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')

library(stringr)
library(dplyr)
library(rvest)
library(rJava)
library(memoise)
library(KoNLP)

install.packages('RColorBrewer')
install.packages('wordcloud')

library(RColorBrewer)
library(wordcloud)

useNIADic()

data=read.csv('sample.csv', stringsAsFactors = F)
data

data=str_replace_all(data[,1], '\n', '')
data=str_replace_all(data, '\t','')
data=str_replace_all(data, '[A-Za-z]','')
data=str_replace_all(data, '[:punct:]',' ')
data=str_replace_all(data, '오류를 우회하기 위한 함수 추가', '')
data=str_replace_all(data, '[0-9]', '')
data=str_replace_all(data, '무단전재 및 재배포 금지','')
data=str_replace_all(data, '[가-힣]씨', '')
#data=str_replace_all(data, '졸음운전', '')
#data=str_replace_all(data, '경부고속도로', '')
data

nouns=extractNoun(data)
wordcount=table(unlist(nouns))
df_word=as.data.frame(wordcount,stringsAsFactors = F)

df_word=rename(df_word, word=Var1, freq=Freq)
df_word=filter(df_word, nchar(word)>=2)


top200=df_word%>%arrange(desc(freq))%>%head(200)
#top200

pal=brewer.pal(8,"Dark2")
set.seed(154)

wordcloud(words=df_word$word, 
          freq=df_word$freq,
          min.freq=2,
          max.words=200,
          random.order=F,
          rot.per=0.1,
          scale=c(2.0,0.05),
          colors=pal)
