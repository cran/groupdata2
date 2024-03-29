library(groupdata2)
context("auto_tune_collapsings")


test_that("testing find_best_group_cols_()", {

  # Set seed
  xpectr::set_test_seed(42)

  # Create data frame
  df <- data.frame(
    "participant" = factor(rep(1:20, 3)),
    "age" = rep(sample(c(1:100), 20), 3),
    "diagnosis" = factor(rep(sample(c(1:3), 20, replace = TRUE), 3)),
    "score" = sample(c(1:100), 20 * 3)
  )
  df <- df %>% dplyr::arrange(participant)

  # Sample rows to get unequal sizes per participant
  df <- dplyr::sample_n(df, size = 27)

  # Create the initial groups (to be collapsed)
  df <- fold(
    data = df,
    k = 8,
    num_fold_cols = 5,
    method = "n_dist",
    id_col = "participant"
  )

  # Ungroup the data frame
  df <- dplyr::ungroup(df)

  best_cols <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 3,
    group_cols_names = paste0(".folds_", 1:5),
    cat_cols = "diagnosis",
    num_cols = "age",
    id_cols = "participant",
    weights = c(
      "diagnosis" = 3,
      "age" = 5,
      "participant" = 2
    ),
    cat_levels = NULL,
    balance_size = TRUE
  )

  expect_equal(
    best_cols,
    c(".folds_4", ".folds_5", ".folds_1"),
    fixed = TRUE
  )

  # With weights for cat_levels
  # With focus on level `1`
  best_cols_lev_1 <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 3,
    group_cols_names = paste0(".folds_", 1:5),
    cat_cols = "diagnosis",
    num_cols = NULL,
    id_cols = NULL,
    weights = c(
      "diagnosis" = 3
    ),
    cat_levels = list("diagnosis" = c("1" = 100, "2" = 1)),
    balance_size = TRUE
  )

  # With focus on level `3`
  best_cols_lev_3 <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 3,
    group_cols_names = paste0(".folds_", 1:5),
    cat_cols = "diagnosis",
    num_cols = NULL,
    id_cols = NULL,
    weights = c(
      "diagnosis" = 3
    ),
    cat_levels = list("diagnosis" = c("3" = 100, "2" = 1)),
    balance_size = TRUE
  )

  # See how they are different, so cat_levels
  # has an effect
  expect_equal(
    best_cols_lev_1,
    c(".folds_4", ".folds_1", ".folds_5"),
    fixed = TRUE
  )
  expect_equal(
    best_cols_lev_3,
    c(".folds_4", ".folds_2", ".folds_1"),
    fixed = TRUE
  )

  # zero-variance
  df$.man_folds = 1

  best_cols <- find_best_group_cols_(
    data = df,
    num_new_group_cols = 2,
    group_cols_names = c(".folds_1", ".man_folds"),
    cat_cols = "diagnosis",
    num_cols = "age",
    id_cols = "participant",
    weights = c(
      "diagnosis" = 3,
      "age" = 5,
      "participant" = 2
    ),
    cat_levels = NULL,
    balance_size = TRUE
  )

  # .man_folds give NAs in ranked_balances
  # so it should rank last
  expect_equal(
    best_cols,
    c(".folds_1", ".man_folds")
  )

})

test_that("testing get_num_cols_to_create_()", {
  xpectr::set_test_seed(42)

  num_combi <- c(seq(from = 1, to = 30, by = 3))
  num_cols <- c(1, 4, 7, 18, 21, 49, 51, 74, 76)

  # Keep number of group cols constant
  # and get the different non-main combination multipliers
  num_non_main_combinations <- plyr::ldply(num_combi, function(nc) {
    get_num_cols_to_create_(num_combinations = nc,
                            num_new_group_cols = 3) %>%
      as.data.frame() %>%
      dplyr::mutate(num_combinations = nc)
  })

  # Keep number of combinations constant
  # and get the different numbers of main-combination
  # columns and random columns
  num_main_and_random_combinations <-
    plyr::ldply(num_cols, function(nc) {
      get_num_cols_to_create_(num_combinations = 6,
                              num_new_group_cols = nc) %>%
        as.data.frame() %>%
        dplyr::mutate(num_new_group_cols = nc)
    })


  ## Testing 'num_non_main_combinations'                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(num_non_main_combinations),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    num_non_main_combinations[["main"]],
    c(9, 9, 9, 9, 9, 9, 9, 9, 9, 9),
    tolerance = 1e-4)
  expect_equal(
    num_non_main_combinations[["non_main"]], # should be the only one depending on num_combinations
    c(4, 4, 3, 3, 2, 2, 2, 2, 2, 1),
    tolerance = 1e-4)
  expect_equal(
    num_non_main_combinations[["random"]],
    c(15, 15, 15, 15, 15, 15, 15, 15, 15, 15),
    tolerance = 1e-4)
  expect_equal(
    num_non_main_combinations[["num_combinations"]],
    c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(num_non_main_combinations),
    c("main", "non_main", "random", "num_combinations"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(num_non_main_combinations),
    c("numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(num_non_main_combinations),
    c("double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(num_non_main_combinations),
    c(10L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(num_non_main_combinations)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'num_non_main_combinations'                           ####



  ## Testing 'num_main_and_random_combinations'                             ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- num_main_and_random_combinations
  # Testing class
  expect_equal(
    class(output_19148),
    "data.frame",
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["main"]],
    c(9, 9, 13, 33, 30, 69, 66, 89, 91),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["non_main"]],
    c(3, 3, 3, 3, 3, 3, 3, 3, 3), # Should not depend on num_new_group_cols
    tolerance = 1e-4)
  expect_equal(
    output_19148[["random"]],
    c(15, 15, 15, 15, 30, 30, 40, 40, 38),
    tolerance = 1e-4)
  expect_equal(
    output_19148[["num_new_group_cols"]],
    c(1, 4, 7, 18, 21, 49, 51, 74, 76),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("main", "non_main", "random", "num_new_group_cols"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(9L, 4L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'num_main_and_random_combinations'                    ####


  ## Testing 'get_num_cols_to_create_(num_combinations = 0...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(get_num_cols_to_create_(num_combinations = 0,
                            num_new_group_cols = 2), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_combinations' failed: Must be >= 1."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'get_num_cols_to_create_(num_combinations = 0...'     ####


  ## Testing 'get_num_cols_to_create_(num_combinations = 2...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(get_num_cols_to_create_(num_combinations = 2,
                            num_new_group_cols = 0), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_new_group_cols' failed: Must be >= 1."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'get_num_cols_to_create_(num_combinations = 2...'     ####


  ## Testing 'get_num_cols_to_create_(num_combinations = 2...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(get_num_cols_to_create_(num_combinations = 2,
                            num_new_group_cols = NA), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_new_group_cols' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'get_num_cols_to_create_(num_combinations = 2...'     ####


  ## Testing 'get_num_cols_to_create_(num_combinations = N...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(get_num_cols_to_create_(num_combinations = NA,
                            num_new_group_cols = 2), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_combinations' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'get_num_cols_to_create_(num_combinations = N...'     ####

})

test_that("testing inform_user_about_autotune_()", {

  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 61,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = 7,
      all_balance_cols = c("a", "b", "c"),
      parallel = FALSE,
      unique_new_group_cols_only = TRUE,
      return_string = TRUE
    )
  # Testing class
  expect_equal(
    class(output_19148),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "character")
  # Testing values
  expect_equal(
    output_19148,
    paste0("-----------------------------------------------------------",
           "-\n  `collapse_groups()` auto-tuning\n----------------------",
           "--------------------------------------\n  Finding 7 unique g",
           "roup collapsings that best balance:\n    a, b, c\n  Will att",
           "empt to create:\n    Extreme pairing balanced splits: 87\n  ",
           "  Extreme triplets balanced splits: 33\n    Random splits: 7",
           "\n    Total number of grouping columns: 127\n---------------",
           "---------------------------------------------\n  Consider en",
           "abling parallelization. See ?collapse_groups\n--------------",
           "----------------------------------------------\n"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning output
  output_19148 <- inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 61,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = 7,
      all_balance_cols = c("a", "b", "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = TRUE
    )
  # Testing class
  expect_equal(
    class(output_19148),
    "character",
    fixed = TRUE)
  # Testing type
  expect_type(
    output_19148,
    type = "character")
  # Testing values
  expect_equal(
    output_19148,
    paste0("-----------------------------------------------------------",
           "-\n  `collapse_groups()` auto-tuning\n----------------------",
           "--------------------------------------\n  Finding 7 group co",
           "llapsings that best balance:\n    a, b, c\n  Will attempt to",
           " create:\n    Extreme pairing balanced splits: 87\n    Extre",
           "me triplets balanced splits: 33\n    Random splits: 7\n    T",
           "otal number of grouping columns: 127\n----------------------",
           "--------------------------------------\n"),
    fixed = TRUE)
  # Testing names
  expect_equal(
    names(output_19148),
    NULL,
    fixed = TRUE)
  # Testing length
  expect_equal(
    length(output_19148),
    1L)
  # Testing sum of element lengths
  expect_equal(
    sum(xpectr::element_lengths(output_19148)),
    1L)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 61,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = 7,
      all_balance_cols = c("a", NA, "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = TRUE
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'all_balance_cols' failed: Contains missing values (element 2)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = NA,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = 7,
      all_balance_cols = c("a", "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = TRUE
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_cols_to_create_settings[['main']]' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 61,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = NA,
      num_new_group_cols = 7,
      all_balance_cols = c("a", "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = TRUE
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_combinations' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 61,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = NA,
      all_balance_cols = c("a", "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = TRUE
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'num_new_group_cols' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


  ## Testing 'inform_user_about_autotune_( num_cols_to_cre...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(inform_user_about_autotune_(
      num_cols_to_create_settings = list(
        "main" = 22,
        "non_main" = 2,
        "random" = 7
      ),
      num_combinations = 15,
      num_new_group_cols = 7,
      all_balance_cols = c("a", "c"),
      parallel = TRUE,
      unique_new_group_cols_only = FALSE,
      return_string = NA
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("Assertion on 'return_string' failed: May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'inform_user_about_autotune_( num_cols_to_cre...'     ####


})

test_that("testing combine_and_fold_combination_()", {

  # Set seed
  xpectr::set_test_seed(42)

  # Create data frame
  df <- data.frame(
    "participant" = factor(rep(1:20, 3)),
    "participant_2" = factor(sample(rep(1:20, 3), 60)),
    "age" = rep(sample(c(1:100), 20), 3),
    "answer" = factor(sample(c("a", "b", "c", "d"), 60, replace = TRUE)),
    "score" = sample(c(1:100), 20 * 3)
  )
  df <- df %>% dplyr::arrange(participant)
  df$session <- rep(c("1", "2", "3"), 20)

  # Sample rows to get unequal sizes per participant
  df <- dplyr::sample_n(df, size = 53)

  # Create the initial groups (to be collapsed)
  df <- fold(
    data = df,
    k = 8,
    method = "n_dist",
    id_col = "participant"
  ) %>%
    dplyr::rename(
      .old_groups = .folds
    )

  # Ungroup the data frame
  # Otherwise `collapse_groups()` would be
  # applied to each fold separately!
  df <- dplyr::ungroup(df)

  summaries <- calculate_summary_collapse_groups_(
    data = df,
    tmp_old_group_var = ".old_groups",
    cat_cols = c("answer"),
    cat_levels = NULL,
    num_cols = c("score", "age"),
    group_aggregation_fn = sum,
    balance_size = TRUE,
    id_cols = c("participant", "participant_2")
  ) %>%
    dplyr::arrange(.old_groups)

  # Ensure it's the expected summary
  # In case the later test fails
  # So we can attribute guilt


  ## Testing 'summaries'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(summaries),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    summaries[[".old_groups"]],
    structure(1:8, .Label = c("1", "2", "3", "4", "5", "6", "7", "8"),
      class = "factor"))
  expect_equal(
    summaries[["answer"]],
    c(-0.73018, 0.3021, -0.47707, 0.63334, -0.16561, 0.39942, -0.20465,
      0.24265),
    tolerance = 1e-4)
  expect_equal(
    summaries[["score"]],
    c(154, 486, 227, 484, 227, 400, 323, 399),
    tolerance = 1e-4)
  expect_equal(
    summaries[["age"]],
    c(136, 348, 268, 477, 348, 107, 186, 568),
    tolerance = 1e-4)
  expect_equal(
    summaries[["size"]],
    c(4, 8, 5, 9, 6, 8, 6, 7),
    tolerance = 1e-4)
  expect_equal(
    summaries[["participant"]],
    c(2, 3, 2, 3, 2, 3, 2, 3),
    tolerance = 1e-4)
  expect_equal(
    summaries[["participant_2"]],
    c(3, 7, 5, 8, 6, 6, 6, 5),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(summaries),
    c(".old_groups", "answer", "score", "age", "size", "participant",
      "participant_2"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(summaries),
    c("factor", "numeric", "integer", "integer", "integer", "integer",
      "integer"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(summaries),
    c("integer", "double", "integer", "integer", "integer", "integer",
      "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(summaries),
    8:7)
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(summaries)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'summaries'                                           ####


  xpectr::set_test_seed(42)
  fold_cols <- combine_and_fold_combination_(
    data = df,
    summaries = summaries,
    tmp_old_group_var = ".old_groups",
    n = 3,
    balance_cols = c(
      "answer",
      "score",
      "age",
      "size",
      "participant",
      "participant_2"
    ),
    col_name = ".coll_g",
    weights = c(
      "answer" = 2,
      "score" = 1,
      "age" = 4,
      "size" = 3,
      "participant" = 2,
      "participant_2" = 3
    ),
    scale_fn = standardize_,
    extreme_pairing_levels = 1,
    num_new_group_cols = 3,
    num_triplet_groupings_as_well = 3,
    unique_new_group_cols_only = TRUE,
    max_iters = 3,
    parallel = FALSE
  )

  ## Testing 'fold_cols'                                                    ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(fold_cols),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    fold_cols[[".coll_g_1"]],
    structure(c(3L, 1L, 1L, 3L, 2L, 2L, 3L, 2L), .Label = c("1", "2",
      "3"), class = "factor"))
  expect_equal(
    fold_cols[[".coll_g_2"]],
    structure(c(1L, 2L, 2L, 1L, 1L, 2L, 3L, 3L), .Label = c("1", "2",
      "3"), class = "factor"))
  expect_equal(
    fold_cols[[".coll_g_3"]],
    structure(c(3L, 2L, 3L, 3L, 2L, 2L, 1L, 1L), .Label = c("1", "2",
      "3"), class = "factor"))
  expect_equal(
    fold_cols[[".coll_g_4"]],
    structure(c(1L, 3L, 1L, 1L, 3L, 2L, 2L, 2L), .Label = c("1", "2",
      "3"), class = "factor"))
  expect_equal(
    fold_cols[[".coll_g_5"]],
    structure(c(1L, 2L, 1L, 1L, 3L, 2L, 2L, 3L), .Label = c("1", "2",
      "3"), class = "factor"))
  expect_equal(
    fold_cols[[".coll_g_6"]],
    structure(c(1L, 3L, 1L, 2L, 3L, 1L, 2L, 2L), .Label = c("1", "2",
      "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(fold_cols),
    c(".coll_g_1", ".coll_g_2", ".coll_g_3", ".coll_g_4", ".coll_g_5",
      ".coll_g_6"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(fold_cols),
    c("factor", "factor", "factor", "factor", "factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(fold_cols),
    c("integer", "integer", "integer", "integer", "integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(fold_cols),
    c(8L, 6L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(fold_cols)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'fold_cols'                                           ####


  # Check all groupings are unique

  comparisons <- expand.grid(names(fold_cols), names(fold_cols)) %>%
    dplyr::filter(Var1 != Var2)
  comparisons[["same_groups"]] <- purrr::map(seq_len(nrow(comparisons)), ~{
    all_groups_identical(
      fold_cols[[comparisons[[.x, "Var1"]]]],
      fold_cols[[comparisons[[.x, "Var2"]]]]
    )
  })

  # Check all comparisons were FALSE (not identical group-member-wise)
  expect_true(all(!isTRUE(comparisons$same_groups)))

  # No triplet cols
  xpectr::set_test_seed(42)
  fold_cols <- combine_and_fold_combination_(
    data = df,
    summaries = summaries,
    tmp_old_group_var = ".old_groups",
    n = 3,
    balance_cols = c(
      "answer",
      "score",
      "age",
      "size",
      "participant",
      "participant_2"
    ),
    col_name = ".coll_g",
    weights = c(
      "answer" = 2,
      "score" = 1,
      "age" = 4,
      "size" = 3,
      "participant" = 2,
      "participant_2" = 3
    ),
    scale_fn = standardize_,
    extreme_pairing_levels = 1,
    num_new_group_cols = 1,
    num_triplet_groupings_as_well = 0,
    unique_new_group_cols_only = TRUE,
    max_iters = 3,
    parallel = FALSE
  )

  expect_equal(ncol(fold_cols), 1)

  # Error with unsorted summaries

  ## Testing 'fold_cols <- combine_and_fold_combination_( ...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(fold_cols <- combine_and_fold_combination_(
      data = df,
      summaries = dplyr::sample_frac(summaries),
      tmp_old_group_var = ".old_groups",
      n = 3,
      balance_cols = c(
        "answer",
        "score",
        "age",
        "size",
        "participant",
        "participant_2"
      ),
      col_name = ".coll_g",
      weights = c(
        "answer" = 2,
        "score" = 1,
        "age" = 4,
        "size" = 3,
        "participant" = 2,
        "participant_2" = 3
      ),
      scale_fn = standardize_,
      extreme_pairing_levels = 1,
      num_new_group_cols = 3,
      num_triplet_groupings_as_well = 3,
      unique_new_group_cols_only = TRUE,
      max_iters = 3,
      parallel = FALSE
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("`summaries` must be ordered by `tmp_old_group_var`. Otherwise the order of the returned group columns won't match the summary order."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)
  ## Finished testing 'fold_cols <- combine_and_fold_combination_( ...'     ####


})

# auto_tune_collapsings


test_that("testing collapse_groups() with auto_tune enabled", {

  testthat::skip_on_cran()

  # Set seed
  xpectr::set_test_seed(42)

  # Create data frame
  df <- data.frame(
    "participant" = factor(rep(1:20, 3)),
    "participant_2" = factor(sample(rep(1:20, 3), 20)),
    "age" = rep(sample(c(1:100), 20), 3),
    "answer" = factor(sample(c("a", "b", "c", "d"), 60, replace = TRUE)),
    "score" = sample(c(1:100), 20 * 3)
  )
  df <- df %>% dplyr::arrange(participant)
  df$session <- rep(c("1", "2", "3"), 20)

  # Sample rows to get unequal sizes per participant
  df <- dplyr::sample_n(df, size = 23)

  # Create the initial groups (to be collapsed)
  df <- fold(
    data = df,
    k = 8,
    method = "n_dist",
    id_col = "participant"
  )

  # Ungroup the data frame
  # Otherwise `collapse_groups()` would be
  # applied to each fold separately!
  df <- dplyr::ungroup(df)

  xpectr::set_test_seed(42)
  df_coll <- collapse_groups(
    data = df,
    n = 3,
    auto_tune = TRUE,
    group_cols = ".folds",
    cat_cols = "answer",
    num_cols = "score",
    id_cols = "participant",
    num_new_group_cols = 1,
    balance_size = TRUE,
    verbose = FALSE
  )



  ## Testing 'df_coll'                                                      ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(df_coll),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    df_coll[["participant"]],
    structure(c(10L, 11L, 11L, 12L, 12L, 13L, 14L, 15L, 16L, 17L, 18L,
      19L, 19L, 3L, 4L, 4L, 5L, 6L, 7L, 7L, 8L, 9L, 9L), .Label = c("1",
      "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
      "14", "15", "16", "17", "18", "19", "20"), class = "factor"))
  expect_equal(
    df_coll[["participant_2"]],
    structure(c(14L, 2L, 2L, 9L, 9L, 4L, 10L, 1L, 2L, 3L, 5L, 15L, 15L,
      13L, 6L, 6L, 12L, 7L, 15L, 15L, 6L, 11L, 11L), .Label = c("2",
      "3", "4", "5", "6", "8", "9", "10", "11", "13", "15", "16",
      "17", "19", "20"), class = "factor"))
  expect_equal(
    df_coll[["age"]],
    c(77, 67, 67, 73, 73, 35, 60, 1, 71, 86, 18, 75, 75, 97, 92, 92,
      8, 49, 37, 37, 85, 42, 42),
    tolerance = 1e-4)
  expect_equal(
    df_coll[["answer"]],
    structure(c(2L, 1L, 3L, 1L, 2L, 2L, 4L, 1L, 3L, 1L, 1L, 3L, 2L,
      1L, 3L, 4L, 2L, 1L, 2L, 1L, 4L, 4L, 3L), .Label = c("a", "b",
      "c", "d"), class = "factor"))
  expect_equal(
    df_coll[["score"]],
    c(88, 64, 36, 42, 75, 67, 19, 57, 35, 53, 30, 6, 51, 99, 78, 38,
      91, 83, 93, 10, 71, 50, 17),
    tolerance = 1e-4)
  expect_equal(
    df_coll[["session"]],
    c("3", "2", "3", "2", "1", "1", "3", "2", "3", "2", "1", "3", "1",
      "1", "3", "1", "1", "2", "2", "3", "2", "1", "2"),
    fixed = TRUE)
  expect_equal(
    df_coll[[".folds"]],
    structure(c(6L, 6L, 6L, 8L, 8L, 4L, 5L, 8L, 4L, 2L, 3L, 3L, 3L,
      8L, 7L, 7L, 7L, 2L, 5L, 5L, 1L, 1L, 1L), .Label = c("1", "2",
      "3", "4", "5", "6", "7", "8"), class = "factor"))
  expect_equal(
    df_coll[[".coll_groups"]],
    structure(c(1L, 1L, 1L, 3L, 3L, 1L, 2L, 3L, 1L, 2L, 1L, 1L, 1L,
      3L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("1", "2",
      "3"), class = "factor"))
  # Testing column names
  expect_equal(
    names(df_coll),
    c("participant", "participant_2", "age", "answer", "score", "session",
      ".folds", ".coll_groups"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(df_coll),
    c("factor", "factor", "integer", "factor", "integer", "character",
      "factor", "factor"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(df_coll),
    c("integer", "integer", "integer", "integer", "integer", "character",
      "integer", "integer"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(df_coll),
    c(23L, 8L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(df_coll)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'df_coll'                                             ####

  summ <- summarize_balances(
    data = df_coll,
    group_cols = ".coll_groups",
    cat_cols = "answer",
    num_cols = "score",
    id_cols = "participant",
    summarize_size = TRUE)


  ## Testing 'summ$Groups'                                                  ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(summ$Groups),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    summ$Groups[[".group_col"]],
    structure(c(1L, 1L, 1L), .Label = ".coll_groups", class = "factor"))
  expect_equal(
    summ$Groups[[".group"]],
    structure(1:3, .Label = c("1", "2", "3"), class = "factor"))
  expect_equal(
    summ$Groups[["# rows"]],
    c(8, 8, 7),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["# participant"]],
    c(6, 6, 5),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["mean(score)"]],
    c(47.125, 58.125, 58.71429),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["sum(score)"]],
    c(377, 465, 411),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["# answ_a"]],
    c(2, 3, 3),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["# answ_b"]],
    c(3, 2, 1),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["# answ_c"]],
    c(3, 1, 1),
    tolerance = 1e-4)
  expect_equal(
    summ$Groups[["# answ_d"]],
    c(0, 2, 2),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(summ$Groups),
    c(".group_col", ".group", "# rows", "# participant", "mean(score)",
      "sum(score)", "# answ_a", "# answ_b", "# answ_c", "# answ_d"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(summ$Groups),
    c("factor", "factor", "integer", "integer", "numeric", "integer",
      "numeric", "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(summ$Groups),
    c("integer", "integer", "integer", "integer", "double", "integer",
      "double", "double", "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(summ$Groups),
    c(3L, 10L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(summ$Groups)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'summ$Groups'                                         ####

})

