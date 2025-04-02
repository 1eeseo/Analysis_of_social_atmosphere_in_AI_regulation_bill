library(tm)
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(reshape2)
library(tidyr)
library(wordcloud)
library(Matrix)
library(slam)

# 패키지 설치 및 로드
install.packages("showtext")
library(showtext)

# 원하는 폰트 추가 (예: 나눔고딕)
font_add_google("Nanum Gothic", "nanumgothic")

# showtext 활성화
showtext_auto()


# 데이터 불러오기
comments <- read.csv("data/youtube_cleaned_text_1000.csv", 
                     stringsAsFactors = FALSE)
head(comments)

comments <- comments %>% filter(nchar(word) > 0)
comments

# AI 제외 영어 단어 삭제 함수
remove_english_except_ai <- function(text) {
  gsub("\\b(?!AI\\b)[a-zA-Z]+\\b", "", text, perl = TRUE)
}

# 영어 단어 삭제 (AI 제외)
cleaned_comments <- sapply(comments$word, remove_english_except_ai)

# 결과 출력
cleaned_comments

# Corpus 생성
corpus <- Corpus(VectorSource(comments))

# AI 제외 영어 삭제 적용
corpus <- tm_map(corpus, content_transformer(remove_english_except_ai))

# 결과 확인
inspect(corpus)

# 한국어 불용어 리스트 적용 (예시)
stopwords_ko <- c("이", "그", "저", "등", "더", "는", "에", "을", "의", "도", "로")
corpus_clean <- tm_map(corpus, removeWords, stopwords_ko)

# 3자 이하 단어만 남아 있는지 확인
inspect(corpus_clean) %>% head(10)


# DTM 생성
dtm <- DocumentTermMatrix(corpus_clean)

# DTM 상태 확인
print(dtm)

# DTM이 비어 있으면
if (nrow(dtm) == 0 | ncol(dtm) == 0) {
  stop("DTM이 비어 있습니다. 전처리 과정을 확인하세요.")
}

# 빈 문서 제거
dtm <- dtm[row_sums(dtm) > 0, ]

# DTM 정보 출력
print(dtm)

# 현재의 DTM은:
# 문서 2개와 8737개의 고유 단어를 포함하고 있으며,
# 50%가 값이 0인희소 행렬로 구성되어 있습니다.
# 각 셀의 값은 단순히 해당 단어가 해당 문서에서 등장한 횟수를 의미합니다.

# 문서 수와 단어 수 확인
cat("문서 수:", nrow(dtm), "\n")
cat("단어 수:", ncol(dtm), "\n")


# LDA 모델링
k <- 10 # 주제 개수 (원하는 개수로 설정)
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))
lda_model

# 각 문서의 토픽 확률 추출
topics <- tidy(lda_model, matrix = "beta")
topics

inspect(dtm)  # DocumentTermMatrix 내용 확인


# 주요 키워드 추출
top_terms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)

# 주요 키워드 출력
print(top_terms)


# 토픽별 주요 키워드 시각화
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  theme_minimal(base_family = "nanumgothic") +  # 한글 폰트 적용
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Terms in Each Topic",
    x = NULL, y = "Beta"
  )

# 문서별 토픽 분포 추출
doc_topics <- tidy(lda_model, matrix = "gamma")

# 문서별 토픽 분포 시각화
doc_topics %>%
  ggplot(aes(x = topic, y = gamma, fill = factor(topic))) +
  geom_boxplot() +
  theme_minimal(base_family = "nanumgothic") +  # 한글 폰트 적용
  labs(
    title = "Topic Distribution Across Documents",
    x = "Topic",
    y = "Gamma (Topic Probability)"
  )



# LDA 모델에서 각 토픽별 단어 확률(beta) 추출
topic_terms <- tidy(lda_model, matrix = "beta")

# 결과 확인
head(topic_terms)

# 토픽별 워드클라우드
for (i in 1:k) {
  topic_terms <- filter(top_terms, topic == i)
  wordcloud(
    words = topic_terms$term,
    freq = topic_terms$beta,
    max.words = 50,
    random.order = FALSE,
    colors = brewer.pal(8, "Dark2")
  )
}


# 특정 토픽의 상위 단어 추출 (예: topic 1)
top_terms <- topic_terms %>%
  filter(topic == 1) %>%        # 원하는 토픽 번호
  arrange(desc(beta)) %>%       # beta 값 기준 내림차순 정렬
  head(50)                      # 상위 50개 단어 선택

# 워드클라우드 생성
library(wordcloud)
library(RColorBrewer)

wordcloud(
  words = top_terms$term,       # 단어
  freq = top_terms$beta,       # 빈도 (beta 값 사용)
  max.words = 50,              # 최대 표시 단어 수
  random.order = FALSE,        # 중요 단어를 중앙에 배치
  colors = brewer.pal(8, "Dark2") # 색상 팔레트
)

# 존재하는 토픽 번호 확인
unique(topic_terms$topic)

