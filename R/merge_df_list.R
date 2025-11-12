#' Merge a list of same-structured data.frames by common columns
#'
#' For example merge a list of five data-frame into one, assuming they all share
#' a common structure and can be merged by the same columns.
#'
#' @param df_list [list] of data.frames to merge
#' @param merge_by [chr] vector of variable names to merge by
#'
#' @returns [data.frame] merged data.frame
#' @export
#'
#' @examples
#' df1 <- data.frame(id = 1:3, value1 = c(10, 20, 30))
#' df2 <- data.frame(id = 2:4, value2 = c(15, 25, 35))
#' df3 <- data.frame(id = 3:5, value3 = c(12, 22, 32))
#' df_list <- list(df1, df2, df3)
#' merge_df_list(df_list, merge_by = "id")
merge_df_list <- function(df_list, merge_by){
  checkmate::assert_list(df_list, min.len = 1)
  checkmate::assert_character(merge_by, any.missing = FALSE, min.len = 1)
  lapply(df_list, function(df){
    checkmate::assert_data_frame(df)
    assert_x_in_y(merge_by, names(df))
  })

  merged_df <- Reduce(function(x, y){
    merge(x, y, by = merge_by, all = TRUE)
  }, df_list)
  return(merged_df)
}
