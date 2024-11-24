f.split_data <- function(data, train_ratio = 0.7, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  n <- nrow(data)
  
  train_size <- floor(train_ratio * n)
  
  train_indices <- sample(seq_len(n), size = train_size)
  
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  list(Learn = train_data, Test = test_data)
}
