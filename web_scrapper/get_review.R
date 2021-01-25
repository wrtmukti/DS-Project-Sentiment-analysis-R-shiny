library(rvest)

get_product_reviews <- function(url, size = -1, incProgress = NULL) {
    reviews <- character()
    reviewers <- character()
  
    reviewPage <- read_html(url)
    review <- reviewPage %>%
      html_nodes('#cm_cr-review_list .review-text') %>%
      html_text()
    reviewer <- reviewPage %>%
      html_nodes('#cm_cr-review_list .a-profile-name') %>%
      html_text()
    
    reviews <- c(reviews, review)
    reviewers <- c(reviewers, reviewer)
    
    if(!is.null(incProgress)) {
      incProgress(10/size) 
    }
    
    nextPage <- reviewPage %>%
      html_nodes('.a-last>a') %>%
      html_attr('href')
      
    
    if(is_empty(nextPage)) {
      nextPage = NA
    }
    
    while (!is.na(nextPage) & (length(reviews) < size | size == -1)) {
      print(paste(length(reviews), "data", "collected"))
      
      reviewUrl <- paste(url, nextPage, sep = "")
      reviewPage <- read_html(reviewUrl)
      
      review <- reviewPage %>%
        html_nodes('#cm_cr-review_list .review-text') %>%
        html_text()
      
      reviewer <- reviewPage %>%
        html_nodes('#cm_cr-review_list .a-profile-name') %>%
        html_text()

      reviews <- c(reviews, review)
      reviewers <- c(reviewers, reviewer)
    
      nextPage <- reviewPage %>%
        html_nodes('.a-last>a') %>%
        html_attr('href')
        
      
      if(is_empty(nextPage)) {
        nextPage = NA
      }
      
      if(!is.null(incProgress)) {
        incProgress(10/size) 
      }
    }
    
    totalReviews <- length(reviews)
    if(totalReviews < size || size == -1) {
      size = totalReviews
    }
    
    print(paste(length(reviews), "data", "collected"))
    
    return(data.frame(reviewer = reviewers, review = reviews, stringsAsFactors = FALSE)[1 : size,])
}
