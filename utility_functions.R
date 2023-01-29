read_csv_to_list_and_drop_na <- function(file_name, add_orig_df_to_list=FALSE) {
  df <- read.csv(file_name, na.strings = c(NA, ''))

  lst <- lapply(df, function(x) {
    x[!is.na(x)] 
    }) 
  if (add_orig_df_to_list) lst$df <- df
  
  return(lst)
}

get_str_cartesian_prod <- function(list_of_sets, sep = ' ') {
  cartesian_df <- expand.grid(list_of_sets, stringsAsFactors = FALSE)
  cartesian_str_vec <- numeric(nrow(cartesian_df))
  for (row in 1:nrow(cartesian_df)){
    cartesian_str_vec[row] <- paste(cartesian_df[row, ], collapse = sep)
  }
  cartesian_str_vec
}

get_zero_data_frame_row <- function(cols) {
  zero <- data.frame(matrix(0, nrow=1, ncol=length(cols)))
  colnames(zero) <- cols
  zero
}

paste_vec <- function(vec, str, where, sep = " ") {
  new_vec <- c()
  for (i in 1:length(vec)) {
    if (where == 'pre') item <- paste(str, vec[i], sep = sep)
    else if (where == 'post') item <- paste(vec[i], str, sep = sep)
    else stop("where should be 'pre' or 'post'.")
    new_vec <- append(new_vec, item)
  }
  new_vec
}

subtract_df_row <- function(df1, df2, row) {
  new_df <- df1
  for (col in colnames(df1)) {
    if (col %in% colnames(df2)) {
      new_df[row, col] <- df1[row, col] - df2[row, col]
    }
  }
  new_df
}