# 필요한 패키지
library(rvest)

# 기본 설정
query <- "AI+법안"  # 검색 키워드
start_date <- "2023.11.25"  # 검색 시작 날짜
end_date <- "2024.11.25"    # 검색 종료 날짜
base_url <- paste0(
  "https://search.naver.com/search.naver?where=news&query=", query,
  "&sm=tab_opt&sort=0&ds=", start_date, "&de=", end_date, "&nso=so:r,p:from", 
  gsub("\\.", "", start_date), "to", gsub("\\.", "", end_date), ",a:all"
)


# 데이터 저장 변수
all_titles <- c()
all_dates <- c()
all_links <- c()  # 기사 링크 저장

# 페이지별 크롤링
for (page in seq(1, 500, by = 10)) {
  page_url <- paste0(base_url, "&start=", page)
  webpage <- read_html(page_url)
  
  # 제목, 링크, 날짜 추출
  titles <- webpage %>% html_nodes(".news_tit") %>% html_text()
  dates <- webpage %>% html_nodes(".info_group") %>% html_text()
  links <- webpage %>% html_nodes(".news_tit") %>% html_attr("href")
  
  # 데이터 누적
  all_titles <- c(all_titles, titles)
  all_dates <- c(all_dates, dates)
  all_links <- c(all_links, links)
  
  Sys.sleep(1)  # 서버 부하 방지를 위한 대기 시간
}

# 데이터프레임 생성
news_data <- data.frame(
  Title = all_titles,
  Date = all_dates,
  Link = all_links,
  stringsAsFactors = FALSE
)

# 결과 확인
head(news_data)



library(httr)
library(jsonlite)

# 댓글 크롤링 함수
crawl_comments <- function(news_links) {
  all_comments <- list() # 각 기사별 댓글 저장
  
  for (link in news_links) {
    # media_id와 article_id 추출
    media_id <- gsub(".*input=([0-9]+).*", "\\1", link)
    article_id <- gsub(".*view/([A-Za-z0-9]+).*", "\\1", link)
    
    # 댓글 API URL 구성
    base_url <- "https://apis.naver.com/commentBox/cbox/web_naver_list_jsonp.json"
    params <- list(
      ticket = "news",
      templateId = "view_politics",
      pool = "cbox5",
      lang = "ko",
      country = "KR",
      objectId = paste0("news", media_id, ",", article_id),
      pageSize = 100
    )
    
    page <- 1
    article_comments <- c()
    
    repeat {
      params$page <- page
      response <- GET(base_url, query = params)
      content_raw <- content(response, as = "text", encoding = "UTF-8")
      
      # JSON 데이터 파싱
      content_json <- tryCatch({
        fromJSON(gsub("^[^(]*\\(|\\);?$", "", content_raw), flatten = TRUE)
      }, error = function(e) NULL)
      
      # JSON 데이터 유효성 검사
      if (is.null(content_json) || is.null(content_json$result$commentList)) {
        break
      }
      
      # 댓글 리스트 추출
      comments <- content_json$result$commentList$contents
      article_comments <- c(article_comments, comments)
      
      # 마지막 페이지 확인
      if (content_json$result$pageModel$lastPage) {
        break
      }
      
      page <- page + 1
      Sys.sleep(1) # 요청 간 대기
    }
    
    # 기사별 댓글 저장
    all_comments[[link]] <- article_comments
  }
  
  return(all_comments)
}

# 뉴스 링크에서 댓글 크롤링
news_links <- news_data$Link
comments_data <- crawl_comments(news_links)

comments_data

# 데이터프레임으로 변환
comments_df <- data.frame(
  Link = rep(names(comments_data), sapply(comments_data, length)),
  Comment = unlist(comments_data, use.names = FALSE),
  stringsAsFactors = FALSE
)
comments_df
news_data

colnames(news_data)
colnames(comments_df)

colnames(news_data)[which(colnames(news_data) == "Link")] <- "Link"
colnames(comments_df)[which(colnames(comments_df) == "Link")] <- "Link"

colnames(news_data)
colnames(comments_df)

head(all_comments)

# 뉴스 데이터와 댓글 병합
final_data <- merge(news_data, comments_df, by.x = "Link", by.y = "Link", all.x = TRUE)

# 데이터 저장
write.csv(final_data, "naver_news_with_comments.csv", row.names = FALSE)

# 결과 확인
head(final_data)

