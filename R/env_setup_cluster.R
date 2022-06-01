# preflight_checks -------------------
#
# rm(list=ls(all.names = T))
# USER <- Sys.info()[['user']]
#
# source(file.path("/ihme/cc_resources/libraries/current/r/get_location_metadata.R"))
# library('ihme.covid', lib.loc = '/ihme/covid-19/.r-packages/current')
#
# VERSIONS <- list(
#   MAIN =    '2022_04_06.05', # most recent fixes from hybrid ETL (or Mon/Tue overnight run)
#   COMPARE = '2022_04_06.01'  # prior model-inputs to compare against  (or last prod "best")
# )
#
# PATHS <- list(
#   DATA_ROOT = "/ihme/covid-19/model-inputs",
#   CODE_ROOT = "/ihme/code/covid-19/user",
#   PLOT_ROOT = "/mnt/share/covid-19/qc-plots",
#   PLOT_DIR = ihme.covid::get_output_dir("/mnt/share/covid-19/qc-plots", "today")
# )
#
# extra_hosp_main <- fread(file.path(PATHS$DATA_ROOT, VERSIONS$MAIN, 'use_at_your_own_risk/full_data_extra_hospital.csv'))
# extra_hosp_compare <- fread(file.path(PATHS$DATA_ROOT, VERSIONS$COMPARE, 'use_at_your_own_risk/full_data_extra_hospital.csv'))
#
# hier_mi <- fread(file.path(PATHS$DATA_ROOT, VERSIONS$MAIN, 'locations/modeling_hierarchy.csv')) # read from model-inputs
# hier_db <- get_location_metadata(location_set_id = 111, location_set_version_id = 1050, release_id = 9) # validate model-inputs version
# library(scriptName, lib = '/mnt/share/code/covid-19/r_packages')
#
# # Testing output, messages, warnings ========
# preflight_checks(hier_db, hier_mi, "hier2hier", v=T, S=F) # testing new user0-friendly output
# preflight_checks(hier_db, hier_mi, "hier2hier", v=T, S=T) # testing STOP behavior with new output
# preflight_checks(hier_db, hier_mi, "hier2hier", v=T, S=F,
#                  user_message = "This is a custom message") # testing new user0-friendly output
# preflight_checks(hier_db, hier_mi, "hier2", v=T, S=F) # see readable method options it method is wrong
# # preflight_checks(hier_db, hier_mi, "hier2hier", v=T, S=T)
# preflight_checks(hier_db, hier_mi, "hier2hier", v=F, S=F)
# # preflight_checks(hier_db, hier_mi, "hier2hier", v=F, S=T)
# preflight_checks(hier_db, hier_db, "hier2hier", v=T, S=F)
# preflight_checks(hier_db, hier_db, "hier2hier", v=F, S=F)
#
#
# test <- "did this stop?"

# print(cat("a", "b", test, sep = "\n"))
# # --- data ---
#
# DFS <- lapply(list.files("data", full.names = T), function(x) as_tibble(get(load(x, verbose = T))))
# names(DFS) <- strsplit(list.files("data"), ".Rdata")
#
# hier_covid <- DFS$modeling_hierarchy
# hier_gbd <- DFS$gbd_analysis_hierarchy
# hier_fh <- DFS$fh_small_area_hierarchy
# hier_covar <- DFS$covariate_with_aggregates_hierarchy
# # bigger data.frame
# full_data <- DFS$full_data_unscaled
#
# # --- sandbox ---
#
# # two equal dataframes for testing
# Equal1 <- hier_covid %>% select(location_id, location_name, path_to_top_parent, most_detailed)
# Equal2 <- copy(Equal1)
#
# # two unequal datasets
# Diff1 <- hier_covid %>% select(location_id, location_name, path_to_top_parent, most_detailed, location_name)
# Diff2 <- hier_gbd %>% select(location_id, location_name, path_to_top_parent, most_detailed, lancet_label)

# get_children_from_parent ---------------

# packages -
# library(data.table)
# library(dplyr)
# library(tidyr)
# library(glue)

# paths -

# PATHS <- list(
#   ROOT = "",
#   HIERARCHIES = "locations"
# )

# load data -
# hier <- fread("development_environment/locations/modeling_hierarchy.csv")
# hier1 <- copy(hier)
# hier2 <- copy(hier)
# hier3 <- copy(hier)
# hierarchy_df <- copy(hier)
# parent_loc_id <- 31
# test_location <- 31
# result_type <- "child_df"

# testing out different methods -


# new column - character vector of all parents
# new column - T/F logical if a test location is in the path to top parent
# subset data.frame to locations with the test_location in pat to top parent
# #
# # # data.table
# hier1[, parents_all := strsplit(path_to_top_parent, ",")]
# hier1[, parents_all := lapply(parents_all, as.numeric)]
# hier1[, parentsTF := sapply(parents_all,
#                             FUN = function(x){
#                               test_location %in% x
#                             })]
# hier1[which(hier1$parentsTF)]
#
#
# #
# # # dplyr
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

# # for loop
# vec <- c(6,31)
#
# tmp <- list()
#
# for(i in 1:length(vec)){
#   tmp[[i]] <-
#   get_children_from_parent(
#     parent_loc_id = vec[i],
#     hierarchy_df = hier
#     )
# }
#
# tmp <- rbindlist(tmp)
#
# # vectorized
#
# # testing
# children_of_parents(parent_loc_ids = parents,
#                     path_to_parent_string = test_col,
#                     include_parent = F)
#
# dfa <- hier %>%
#   mutate(test = children_of_parents(parent_loc_ids = c(32,31),
#                                     path_to_parent_string = path_to_top_parent,
#                                     include_parent = T)) %>%
#   filter(test)
#
# tmp <- hier[, test := children_of_parents(parent_loc_ids = c(6, 31), path_to_parent_string = path_to_top_parent)]
# tmp <- tmp[which(test),]
#
# # development
# any(parents %in% as.numeric(unlist(strsplit(hier$path_to_top_parent[1:20],","))))
#
# test_col <- hier$path_to_top_parent[1:20]
#
# any(parents %in% as.numeric(unlist(strsplit(test_col[20], ","))))
#
# parents <- c(32, 42)
#
# tmp <- c()
#
# for(i in 1:length(test_col)){
#   tmp[i] <-
#     any(parents %in% as.numeric(unlist(strsplit(test_col[i], ","))))
#
# }



library(devtools)
library(withr)
with_libpaths(new = "/mnt/share/code/covid-19/r_packages",
              install_github("epi-sam/SamsElves"))
