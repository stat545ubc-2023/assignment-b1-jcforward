### Exercise 1 + 2: Make a Function + Document your Function

-   I made a function that allows me to take the data frame ‘penguins’
    and create a new data frame with the mean values of specified
    measurements for each species.

<!-- -->

    #' Summarize Measurements by Species
    #'
    #' This function returns a summarized data frame with mean measurements grouped by species. 
    #' The user can specify which variables to summarize.
    #'
    #' @param data A data frame containing at least a "species" column and columns specified in `summary_vars`.
    #' @param summary_vars A character vector indicating which columns in `data` to summarize. 
    #'                     Default is c("bill_length_mm", "flipper_length_mm", "body_mass_g").
    #' @param ... Additional arguments passed on to the `mean` function, such as `na.rm`.
    #' 
    #' @return A data frame with one row for each species and a column for the mean of each `summary_vars`.
    #'
    #' @export

    summarize_by_species <- function(data, summary_vars = c("bill_length_mm", "flipper_length_mm", "body_mass_g"), ...) {
      # Error trap to ensure that the provided data is a data frame
      if (!is.data.frame(data)) {
        stop("Input data must be a data frame.")
      }
      
      if (!all(summary_vars %in% names(data))) {
        stop("Not all specified summary variables are present in the data frame.")
      }

     # Group by species and compute the mean for specified variables
      summarized_data <- data %>%
        group_by(species) %>%
        summarise(across(all_of(summary_vars), mean, na.rm = TRUE, .names = "mean_{.col}"), ...)

      return(summarized_data)
    }

### Exercise 3: Examples

#### Example 1: Default function to see if it works

    #Using the penguins data, I will use the function 
    result <- summarize_by_species(penguins)

    ## Warning: There was 1 warning in `summarise()`.
    ## ℹ In argument: `across(all_of(summary_vars), mean, na.rm = TRUE, .names =
    ##   "mean_{.col}")`.
    ## ℹ In group 1: `species = Adelie`.
    ## Caused by warning:
    ## ! The `...` argument of `across()` is deprecated as of dplyr 1.1.0.
    ## Supply arguments directly to `.fns` through an anonymous function instead.
    ## 
    ##   # Previously
    ##   across(a:b, mean, na.rm = TRUE)
    ## 
    ##   # Now
    ##   across(a:b, \(x) mean(x, na.rm = TRUE))

    print(result)

    ## # A tibble: 3 × 4
    ##   species   mean_bill_length_mm mean_flipper_length_mm mean_body_mass_g
    ##   <fct>                   <dbl>                  <dbl>            <dbl>
    ## 1 Adelie                   38.8                   190.            3701.
    ## 2 Chinstrap                48.8                   196.            3733.
    ## 3 Gentoo                   47.5                   217.            5076.

#### Example 2: Fake Numeric Variable

    # Here I created a variable that is categorical but is actually represented with numbers
    penguins$fake_numeric <- as.factor(sample(1:5, nrow(penguins), replace = TRUE))

    # Trying to get the mean of this fake numeric variable using our function
    result_error <- summarize_by_species(penguins, summary_vars = c("bill_length_mm", "flipper_length_mm", "body_mass_g", "fake_numeric"))

    ## Warning: There were 3 warnings in `summarise()`.
    ## The first warning was:
    ## ℹ In argument: `across(all_of(summary_vars), mean, na.rm = TRUE, .names =
    ##   "mean_{.col}")`.
    ## ℹ In group 1: `species = Adelie`.
    ## Caused by warning in `mean.default()`:
    ## ! argument is not numeric or logical: returning NA
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.

    print(result_error)

    ## # A tibble: 3 × 5
    ##   species   mean_bill_length_mm mean_flipper_length_mm mean_body_mass_g
    ##   <fct>                   <dbl>                  <dbl>            <dbl>
    ## 1 Adelie                   38.8                   190.            3701.
    ## 2 Chinstrap                48.8                   196.            3733.
    ## 3 Gentoo                   47.5                   217.            5076.
    ## # ℹ 1 more variable: mean_fake_numeric <dbl>

### Exercise 4: Test the Function

    test_that("summarize_by_species behaves correctly", {

    # Test 1: Vector that has NAs
      # Assuming 'body_mass_g' contains NAs
      result1 <- summarize_by_species(penguins, summary_vars = "body_mass_g")
      expect_equal(dim(result1), c(3, 2))
      expect_true("species" %in% colnames(result1))
      expect_true("mean_body_mass_g" %in% colnames(result1))
      
      # Test 2: Vector of a different type (using a character vector for numeric_category which should generate a warning or error)
      penguins$char_col <- sample(letters, nrow(penguins), replace = TRUE)
      expect_warning(summarize_by_species(penguins, summary_vars = "char_col"))
      
      # Test 3: Vector of length 0 (like numeric(0))
      expect_error(summarize_by_species(penguins, summary_vars = "nonexistent_variable"))
      
    })

    ## Test passed 🎊
