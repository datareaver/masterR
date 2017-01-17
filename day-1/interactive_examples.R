library(purrr)
map_dbl()

x <- list(1, 10, 100)
y <- list(1, 2, 3)
map2(x, y, ~ .x + .y)
# Or just
map2(x, y, `+`)
data("mtcars")
data('iris')
map_dbl(mtcars, mean)
mu <- c(-10,0,10,100)
map(mu, rnorm, n = 10)

map_df(mu, rnorm, n = 10)



col_means <- function(df) { 
  numeric <- sapply(df, is.numeric) 
  numeric_cols <- df[, numeric] 
  
  data.frame(lapply(numeric_cols, mean)) 
} 

col_means(mtcars) 
col_means(mtcars[, 0]) 
col_means(mtcars[0, ]) 
col_means(mtcars[, "mpg", drop = F]) 
col_means(1:10) 
col_means(as.matrix(mtcars)) 
col_means(mtcars)
colMeans(mtcars[ , 0 ])

by_cyl <- split(mtcars, mtcars$cyl)
model <- function(df) lm(mpg ~ wt, data = df) 
map(by_cyl, model) 
# Shorter 
map(by_cyl, function(df) lm(mpg ~ wt, data = df)) 
# Shortest (only for purrr) 
models <- map(by_cyl, ~ lm(mpg ~ wt, data = .))
map(models, function(x) summary(x)[['coefficients']])
map(models, function(x) names(summary(x)))
map(models, function(x) summary(x)[["r.squared"]])


urls <- c( 
  "http://google.com", 
  "https://en.wikipedia.org", 
  "asdfasdasdkfjlda" 
) 
# Fails 
contents <- map(urls, readLines) 
# Always succeeds 
contents <- urls %>% map(safely(readLines)) %>% transpose()
str(contents)
successes <- map_lgl(contents$error, is.null)
urls[!successes]
contents$result[successes]
map_lgl(contents$error, function(x) inherits(x, "error"))

# is between 0-1, sum to 1, no NA, no NULL, is.double, unique values > 1

rv <- function(x, probs = NULL) { 
  if (is.rv(x)) x <- as.numeric(x) 
  if (is.null(probs)) { 
    probs <- rep(1, length(x)) / length(x) 
  } else { 
    if (length(x) != length(probs)) stop("Values and probability...") 
    check_probs(probs) 
  }
}


is.rv <- function(x) {  
  # equivalent to "rv" %in% class(x) 
  inherits(x, "rv") 
}
test <- rv(c(1,1,2,2,3))
print.rv <- function(x, ...) { 
  cat("Probabilites:", rv)
}
  

