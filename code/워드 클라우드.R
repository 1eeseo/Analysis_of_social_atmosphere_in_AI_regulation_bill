# 필요한 패키지 로드
library(dplyr)
library(tidyr)
library(tidytext)
library(topicmodels)
library(ggplot2)
library(ldatuning)
library(reshape2)
library(showtext)
library(ggwordcloud)
library(RColorBrewer)
library(wordcloud2)
library(syuzhet)
library(KoNLP)
library(sentimentr)
library(readr)

# Apple SD Gothic Neo 폰트 적용
theme(text = element_text(family = "Apple SD Gothic Neo"))


#tokenized_df <- read.csv('Desktop/AI 법안/data/')


# 파일 읽기
# 텍스트 파일 경로 설정
file_path <- "Desktop/AI 법안/data/refinedStr.txt"  # 경로를 따옴표로 감쌉니다.
text_data <- readLines(file_path, encoding = "UTF-8")

# NIA 사전 사용
useNIADic()


# 간단한 감성 사전 예시
sentiment_dict <- data.frame(
  word = c("우려", "연기", "추진", "가속", "졸속", "우수", "확장", "기대"),
  sentiment = c("negative", "negative", "neutral", "positive", "negative", "positive", "positive", "positive")
)

# 형태소 분석 및 토큰화
tokenized_words <- sapply(text_data, extractNoun, USE.NAMES = FALSE) %>%
  unlist() %>%
  table() %>%
  as.data.frame()

colnames(tokenized_words) <- c("word", "freq")

# 감성 사전과 매칭
sentiment_data <- tokenized_words %>%
  left_join(sentiment_dict, by = "word") %>%
  filter(!is.na(sentiment))  # 감성 단어만 필터링

# 긍정/부정 비율 계산
sentiment_summary <- sentiment_data %>%
  group_by(sentiment) %>%
  summarise(total_freq = sum(freq)) %>%
  mutate(percentage = total_freq / sum(total_freq) * 100)

# 감성 비율 시각화
ggplot(sentiment_summary, aes(x = sentiment, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("negative" = "pink", "positive" = "skyblue", "neutral" = "grey")) +
  labs(title = "AI 법안 관련 감성 분석", x = "감성", y = "비율(%)") +
  theme_minimal() +
  theme(text = element_text(family = "AppleGothic"))
