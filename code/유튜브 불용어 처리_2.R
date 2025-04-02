library(readr)
library(stringr)  # 문차열 처리 
library(tidytext) # 토큰화 
library(dplyr)  # 데이터 처리 
library(tm) #불용어 제거
library(tidyr)
library(KoNLP)


# 사전 설정
useNIADic()  # KoNLP 기본 사전 사용


# -------------------------------------------------------
df <- read.csv("Desktop/AI 법안/data/youtube_title_description_comments.csv")

# 데이터 클렌징
cleansing <- function(text) {
  text %>% 
    str_replace_all("[^[:alnum:][:space:][:punct:]]", " ") %>% 
    str_to_lower() %>% 
    str_squish()  #중복공백 제거
}


df$description <- sapply(df$description, cleansing)
df$comments <- sapply(df$comments, cleansing)

head(df)


# 불용어 제거(stop words)
korean_stopwords <- c("그리고", "하지만", "그래서", "하다", "있다", "없다", 
                      "의", "은", "는", "이", "가", "을", "를", "과", "와",
                      "에게", "ㅋ","ㅡ","지금", "그냥", "하고", "이런", "것이",
                      "하는", "너무", "합니다", "진짜", "같다")


# 데이터 전처리 함수
clean_text <- function(text) {
  text %>%
    str_remove_all("<[^>]+>") %>%            # HTML 태그 제거
    str_remove_all("[^가-힣\\s]") %>%        # 한글과 공백 제외한 문자 제거 (숫자, 영어, 특수문자)
    str_squish() %>%                        # 공백 정리
    str_split(" ") %>%                      # 단어로 분리
    unlist() %>% 
    .[! . %in% korean_stopwords] %>%        # 불용어 제거
    .[nchar(.) > 1] %>%                     # 한글자 제거
    paste(collapse = " ")                   # 다시 하나의 문자열로 합치기
}



# comments와 description만 선택하고 전처리 적용
cleaned_df <- df %>%
  select(comments, description) %>%
  mutate(
    comments_cleaned = sapply(comments, clean_text),
    description_cleaned = sapply(description, clean_text)
  )

# 결과 확인
head(cleaned_df)

# 토큰화 및 불용어 제거 
tokenized_df <- cleaned_df %>% 
  select(comments_cleaned) %>% 
  pivot_longer(cols = everything(), names_to = "source", values_to = "text") %>% 
  unnest_tokens(word, text) # 단어단위로 분리

# 불용어 제거를 filter()로 처리
tokenized_df <- tokenized_df %>%
  filter(!word %in% korean_stopwords)

# 숫자 제거 
tokenized_df <- tokenized_df %>%
  filter(!str_detect(word, "^[0-9]+$"))

head(tokenized_df)

tokenized_df <- tokenized_df %>% 
  select(word)
tokenized_df

tokenized_df <- tokenized_df %>%
  filter(word != "대통령과내각은니편내편아닌국민에아품을보듬고재산과민생에최선을다해야하는대지금정부는국힘당원들에정부같네요편가르지말고이태원에눈물을닦는대최선을다하시라")
tokenized_df


# 결과 확인
head(tokenized_df)

write.csv(tokenized_df, "Desktop/AI 법안/data/youtube_cleaned_text_1000_final.csv", row.names = FALSE, fileEncoding = "UTF-8")
