# get_children_from_parent ---------------

# packages -
library(data.table)
library(dplyr)
library(tidyr)
# library(glue)

# paths -

# PATHS <- list(
#   ROOT = "",
#   HIERARCHIES = "locations"
# )

# load data -
hier <- fread("development_environment/locations/modeling_hierarchy.csv")
hier1 <- copy(hier)
hier2 <- copy(hier)
hier3 <- copy(hier)
hierarchy_df <- copy(hier)
parent_loc_id <- 31
test_location <- 31
result_type <- "child_df"

# testing out different methods -


# new column - character vector of all parents
# new column - T/F logical if a test location is in the path to top parent
# subset data.frame to locations with the test_location in pat to top parent
#
# # data.table
hier1[, parents_all := strsplit(path_to_top_parent, ",")]
hier1[, parents_all := lapply(parents_all, as.numeric)]
hier1[, parentsTF := sapply(parents_all,
                            FUN = function(x){
                              test_location %in% x
                            })]
hier1[which(hier1$parentsTF)]


#
# # dplyr
# hier2 <-
#   hier2 %>%
#   mutate(parents_all = strsplit(path_to_top_parent, ","),
#          parents_all = lapply(parents_all, as.numeric))
# hier2 <-
#   hier2 %>%
#   mutate(parentsTF =
#            sapply(hier2$parents_all,
#                   FUN = function(x){
#                     test_location %in% x
#                   }))
# hier2[which(hier2$parentsTF)]
#
# # testing outside function
#
# hierarchy_df[, parents_all := strsplit(path_to_top_parent, ",")]
# hierarchy_df[, parents_all := lapply(parents_all, as.numeric)]
# hierarchy_df[, parentsTF := sapply(parents_all,
#                                    FUN = function(x){
#                                      parent_loc_id %in% x
#                                    })]
# hierarchy_df[which(hierarchy_df$parentsTF)]

# getting output from multiple parent locations -

# for loop
vec <- c(6,31)

tmp <- list()

for(i in 1:length(vec)){
  tmp[[i]] <-
  get_children_from_parent(
    parent_loc_id = vec[i],
    hierarchy_df = hier
    )
}

tmp <- rbindlist(tmp)

# vectorized

