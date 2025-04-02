# 필요한 패키지 로드
library(httr)
library(jsonlite)

# API 키 및 기본 설정
api_key <- "AIzaSyA27Rue05_6IKgq8V6UF5XEZUdY3heRBYc"  # 여기에 API 키를 입력하세요
query <- "AI 법안"
start_date <- "2023-01-01T00:00:00Z"
end_date <- "2024-01-01T00:00:00Z"
max_results <- 50  # 한 번 요청에 최대 50개
total_videos <- 200  # 테스트 목적으로 100개의 영상으로 제한 (수량 조정 가능)
video_count <- 0
next_page_token <- NULL
video_data <- data.frame(
  title = character(),
  description = character(),
  comments = character(),
  stringsAsFactors = FALSE
)

# 댓글 가져오는 함수
get_comments <- function(video_id) {
  comment_url <- paste0(
    "https://www.googleapis.com/youtube/v3/commentThreads?part=snippet&videoId=", 
    video_id, 
    "&maxResults=5&key=", 
    api_key
  )
  comment_response <- GET(comment_url)
  
  if (status_code(comment_response) == 200) {
    comment_content <- content(comment_response, "text")
    comment_results <- fromJSON(comment_content, flatten = TRUE)
    
    # 댓글 추출
    if (!is.null(comment_results$items)) {
      comments <- sapply(comment_results$items$snippet.topLevelComment.snippet.textDisplay, identity)
      return(paste(comments, collapse = " | "))  # 댓글을 하나의 문자열로 합침
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

# 반복 요청을 통한 데이터 수집
while (video_count < total_videos) {
  # 요청 URL 생성
  search_url <- paste0(
    "https://www.googleapis.com/youtube/v3/search?part=snippet&type=video&q=",
    URLencode(query),
    "&publishedAfter=", start_date,
    "&publishedBefore=", end_date,
    "&maxResults=", max_results,
    if (!is.null(next_page_token)) paste0("&pageToken=", next_page_token),
    "&key=", api_key
  )
  
  # API 요청
  response <- GET(search_url)
  
  if (status_code(response) == 200) {
    raw_content <- content(response, "text")
    search_results <- fromJSON(raw_content, flatten = TRUE)
    
    # 데이터 추출
    if (!is.null(search_results$items)) {
      for (i in 1:length(search_results$items$id.videoId)) {
        video_id <- search_results$items$id.videoId[i]
        title <- search_results$items$snippet.title[i]
        description <- search_results$items$snippet.description[i]
        comments <- get_comments(video_id)
        
        # 데이터프레임에 추가
        video_data <- rbind(video_data, data.frame(
          title = title,
          description = description,
          comments = comments,
          stringsAsFactors = FALSE
        ))
        video_count <- nrow(video_data)
        
        # 500개 도달 시 중단
        if (video_count >= total_videos) {
          break
        }
      }
      
      # 다음 페이지 토큰 업데이트
      next_page_token <- search_results$nextPageToken
      
      if (is.null(next_page_token)) {
        break
      }
    } else {
      cat("검색 결과가 없습니다. 종료합니다.\n")
      break
    }
  } else {
    stop(paste("API 요청 실패: 상태 코드", status_code(response)))
  }
}

# CSV 파일로 저장
write.csv(video_data, "youtube_title_description_comments_1000.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("제목, 설명, 댓글 데이터가 'youtube_title_description_comments_1000.csv'로 저장되었습니다.\n")
          