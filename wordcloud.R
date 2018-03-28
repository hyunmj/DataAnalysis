install.packages('rJava')
install.packages('memoise')
install.packages('KoNLP')


library(rJava)
library(memoise)
library(KoNLP)

useNIADic()

install.packages(c("wordcloud", 'RColorBrewer'))
install.packages("stringr")
library(stringr)
install.packages('dplyr')

data=readLines("titles.csv")
data

data=str_replace_all(data, '\"','')
data=str_replace_all(data, '^[0-9]*,','')
data

nouns=extractNoun(data)
nouns

wordcount=table(unlist(nouns))
wordcount
df_word=as.data.frame(wordcount,stringsAsFactors = F)
df_word


library(dplyr)

df_word=rename(df_word, word=Var1, freq=Freq)
df_word=filter(df_word, nchar(word)>=2)


#top202=df_word%>%arrange(desc(freq))%>%head(202)
#top202
#top200=top202[-c(1,3),]
#top200

library(wordcloud)
library(RColorBrewer)

pal=brewer.pal(8,"Dark2")
set.seed(154)



wordcloud(words=df_word$word, 
          freq=df_word$freq,
          min.freq=2,
          max.words=200,
          random.order=F,
          rot.per=0.1,
          scale=c(3,0.3),
          colors=pal)
