## server.R

# load functions
source('functions/cf_algorithm.R') # collaborative filtering
source('functions/similarity_measures.R') # similarity measures

# define functions
get_user_ratings <- function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"),
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                    Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]

  # get the indices of the ratings
  # add the user ratings to the existing rating matrix
  user_ratings <- sparseMatrix(i = dat$MovieID,
                               j = rep(1,nrow(dat)),
                               x = dat$Rating,
                               dims = c(nrow(ratingmat), 1))
  return (user_ratings)
}


myurl = "https://liangfgithub.github.io/MovieData/"
movies = readLines(paste0('movies.dat'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")
movies$Year = as.numeric(unlist(
  lapply(movies$Title, function(x) substr(x, nchar(x)-4, nchar(x)-1))))
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID,
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
movies = movies[sample(1:nrow(movies)), ]

ratings = read.csv(paste0('ratings.dat'),
                   sep = ':',
                   colClasses = c('integer', 'NULL'),
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL
ratingmat <- sparseMatrix(ratings$MovieID, ratings$UserID, x=ratings$Rating) # book x user matrix
ratingmat <- ratingmat[unique(summary(ratingmat)$i), unique(summary(ratingmat)$j)] # remove users with no ratings
dimnames(ratingmat) <- list(MovieID = as.character(sort(unique(ratings$MovieID))), UserID = as.character(sort(unique(ratings$UserID))))

tmp = ratings %>%
  group_by(MovieID) %>%
  summarize(ratings_per_movie = n(), ave_ratings = mean(Rating)) %>%
  inner_join(movies, by = 'MovieID') %>%
  filter(ratings_per_movie >= 1000) %>%
  arrange(desc = -ave_ratings)

shinyServer(function(input, output, session) {

  # show the movies
  output$popular <- renderUI({
    chosen_genre = input$genre
    tmp_filtered = tmp %>% filter(grepl(chosen_genre, Genres, fixed = TRUE))
    num_movies <- min(c(5, length(tmp_filtered))) # movies per row
    num_rows <- min(c(3, floor(length(tmp_filtered) / num_movies)))

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = tmp_filtered$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center; color: #999999; font-size: 80%", round(tmp_filtered$ave_ratings[(i - 1) * num_movies + j], digits = 2)),
                 div(style = "text-align:center", strong(tmp_filtered$Title[(i - 1) * num_movies + j]))))
      })))
    })
  })

  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 5 # movies per row

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })

  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
        # hide the rating container
        useShinyjs()
        jsCode <- "document.querySelector('[data-widget=collapse]').click();"
        runjs(jsCode)

        # get the user's rating data
        value_list <- reactiveValuesToList(input)
        user_ratings <- get_user_ratings(value_list)

        # add user's ratings as first column to rating matrix
        rmat <- cbind(user_ratings, ratingmat)

        # get the indices of which cells in the matrix should be predicted
        # predict all books the current user has not yet rated
        items_to_predict <- which(rmat[, 1] == 0)
        prediction_indices <- as.matrix(expand.grid(items_to_predict, 1))

        # run the ubcf-alogrithm
        res <- predict_cf(rmat, prediction_indices, "ubcf", TRUE, cal_cos, 20, FALSE, 2000, 1000)

        # sort, organize, and return the results
        Rank = 1:20
        user_results <- sort(res[, 1], decreasing = TRUE)[Rank]
        user_predicted_ids <- as.numeric(names(user_results))
        recom_result <- data.table(Rank = Rank,
                                    MovieID = movies$MovieID[user_predicted_ids],
                                    Title = movies$Title[user_predicted_ids],
                                    Predicted_rating =  user_results)

    }) # still busy

  }) # clicked on button


  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()

    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),

          div(style = "text-align:center",
              a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
             ),
          div(style="text-align:center; font-size: 100%",
              strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
             )

        )
      }))) # columns
    }) # rows

  }) # renderUI function

}) # server function
