#' Roll dice
#'
#' @param n_dice e.g. 3 of 3d6
#' @param n_sides e.g 6 of 3d6
#' @param best_of_n if rolling 3d6 n times, choose the highest/lowest sum from n rolls
#' @param method "max" (advantage) or "min" (disadvantage)
#'
#' @returns a barplot showing the distribution of sums
#' @export
#'
#' @examples
#' dice(3, 6, 2)
dice <- function(n_dice, n_sides, best_of_n = 1, method = c("max", "min"), verbose = FALSE) {

  method <- match.arg(method)

  # Set margins and restore on exit
  old_par <- par(mar = c(5.1, 4.1, 4.1, 5.1))
  on.exit(par(old_par))

  # Step 1: Start with a SINGLE die distribution (1-n_sides)
  single_die <- rep(1/n_sides, n_sides)
  names(single_die) <- 1:n_sides

  # Step 2: Convolve to get distribution of sum of n individual dice
  sum_n_dice <- single_die

  if (n_dice > 1) {
    for (i in 2:n_dice) {
      # Convolve using outer product
      temp <- outer(sum_n_dice, single_die)

      # Aggregate by sum
      all_sums <- outer(
        as.numeric(names(sum_n_dice))
        , as.numeric(names(single_die))
        , "+"
      )

      result <- tapply(as.vector(temp), as.vector(all_sums), sum)
      sum_n_dice <- result
    }
  }

  # Step 3: Calculate distribution based on method
  result_dist <- numeric(length(sum_n_dice))
  names(result_dist) <- names(sum_n_dice)

  if (method == "max") {
    # P(max(X1, X2, ...) = k) = P(X ≤ k)^n - P(X ≤ k-1)^n
    cum_prob <- cumsum(sum_n_dice)

    result_dist[1] <- cum_prob[1]^best_of_n
    for (i in 2:length(cum_prob)) {
      result_dist[i] <- cum_prob[i]^best_of_n - cum_prob[i-1]^best_of_n
    }
    method_label <- "Maximum"
    selection_label <- "Highest"

  } else if (method == "min") {
    # P(min(X1, X2, ...) = k) = P(X ≥ k)^n − P(X ≥ k+1)^n
    cum_prob <- cumsum(sum_n_dice)
    surv_prob <- 1 - c(0, cum_prob[-length(cum_prob)])

    n_vals <- length(sum_n_dice)
    for (i in 1:(n_vals - 1)) {
      result_dist[i] <- surv_prob[i]^best_of_n - surv_prob[i+1]^best_of_n
    }
    result_dist[n_vals] <- surv_prob[n_vals]^best_of_n
    method_label <- "Minimum"
    selection_label <- "Lowest"
  } else {
    stop("choose 'max' or 'min' method")
  }

  # Step 4: Calculate cumulative distribution
  cum_result_dist <- cumsum(result_dist) * 100

  # PDF scale (left axis)
  y_max_pdf   <- max(result_dist) * 110
  y_ticks_pdf <- pretty(c(0, y_max_pdf), n = 15)

  # Scale CDF to fit within PDF's y-axis range for plotting
  cum_result_dist_scaled <- cum_result_dist * (y_max_pdf / 100)

  # CDF axis ticks (right axis)
  y_ticks_cdf        <- pretty(c(0, 100), n = 10)
  y_ticks_cdf_scaled <- y_ticks_cdf * (y_max_pdf / 100)

  # Plot CDF first (scaled to fit)
  bp <- barplot(
    cum_result_dist_scaled,
    main = sprintf("Distribution of %s Sum\n(%s of %s Rolls of %sd%s)", method_label, selection_label, best_of_n, n_dice, n_sides),
    xlab = "Sum",
    ylab = "Probability (%)",
    col = "gray70",
    las = 1,
    ylim = c(0, y_max_pdf),
    yaxt = "n"
  )

  # Add horizontal grid lines
  abline(h = y_ticks_pdf, col = "gray80", lty = 1)
  # Add horizontal dashed lines at CDF tick levels
  abline(h = y_ticks_cdf_scaled, col = "gray80", lwd = 1, lty = 3)

  # Redraw CDF bars on top of grid lines
  barplot(
    cum_result_dist_scaled,
    col = "gray70",
    ylim = c(0, y_max_pdf),
    yaxt = "n",
    add = TRUE
  )

  # Add PDF bars on top with transparency
  barplot(
    result_dist * 100,
    col = adjustcolor("steelblue", alpha.f = 0.7),
    ylim = c(0, y_max_pdf),
    yaxt = "n",
    add = TRUE
  )

  # Find median (50% cumulative probability) and mode (most probable)
  # median_idx <- which(cum_result_dist >= 50)[1]
  median_idx <- which.min(abs(cum_result_dist - 50))
  mode_idx <- which.max(result_dist)

  # Jitter if they overlap
  bar_width <- bp[2] - bp[1]
  jitter_offset <- if (median_idx == mode_idx) bar_width * 0.15 else 0

  # Add vertical lines at bar centers
  abline(v = bp[median_idx] - jitter_offset, col = "black",     lwd = 1)
  abline(v = bp[mode_idx]   + jitter_offset, col = "steelblue", lwd = 1)

  # Left axis - PDF scale
  axis(2, at = y_ticks_pdf, las = 1)

  # Right axis - CDF scale (0-100%)
  axis(4, at = y_ticks_cdf_scaled, labels = y_ticks_cdf, las = 1)

  # Right axis label
  mtext("Cumulative Probability (%)", side = 4, line = 3)

  # Step 5: Verify
  result_df <- data.frame(
    sum  = as.numeric(names(result_dist)),
    prob = as.numeric(result_dist)
  )
  if(verbose){
    cat(sprintf("Rolled %sd%s choosing %s of %s rolls", n_dice, n_sides, method, best_of_n))
    cat("\nSum of probabilities:", sum(result_dist), "\n")
    cat("Range:",  min(result_df$sum), "to", max(result_df$sum), "\n")
    cat("Median:", names(result_dist)[median_idx], "\n")
    cat("Mode:",   names(result_dist)[mode_idx], "\n")
  }
}

#' Roll dice and return a sampled value
#'
#' @param n_dice e.g. 3 of 3d6
#' @param n_sides e.g 6 of 3d6
#' @param best_of_n if rolling 3d6 n times, choose the highest/lowest sum from n rolls
#' @param method "max" (advantage) or "min" (disadvantage)
#' @param plot if TRUE, display the distribution via dice()
#'
#' @returns a single integer roll result
#' @export
#'
#' @examples
#' roll(2, 6) # just dice
#' roll(1, 20, best_of_n = 2, method = "max")  # advantage
#' roll(1, 20, best_of_n = 2, method = "min")  # disadvantage
#'
#' stats <- list(
#'   str = integer()
#' , dex = integer()
#' , con = integer()
#' , int = integer()
#' , wis = integer()
#' , chr = integer()
#' )
#'
#' # Roll stats with best of 2 & show plot
#' sapply(stats, function(x) roll(3,6,2,method='max',T))
#' # stat blocks for multiple NPCs/monsters
#' t(sapply(1:10, function(x) sapply(stats, function(x) roll(3,6)))) # normal monsters
#' t(sapply(1:10, function(x) sapply(stats, function(x) roll(3,6,2)))) # hard monsters
#' t(sapply(1:10, function(x) sapply(stats, function(x) roll(3,6,2,'min')))) # easy monsters
roll <- function(n_dice, n_sides, best_of_n = 1, method = c("max", "min"), plot = FALSE, verbose = FALSE) {

  method <- match.arg(method)

  # Optionally show the distribution
  if (plot) {
    dice(n_dice, n_sides, best_of_n, method, verbose)
  }

  rolls <- replicate(best_of_n, sum(sample(1:n_sides, n_dice, replace = TRUE)))
  result <- if (method == "max") max(rolls) else min(rolls)

  result
}


