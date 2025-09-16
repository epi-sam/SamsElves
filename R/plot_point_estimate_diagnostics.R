plot_draw_pe_distribution <- function(
    DT,
    hist_plot_title,
    plot_vs_pe_column = 'mean',
    by_page_var = 'year_id',
    facet_var = 'age_group_id',
    color_scatter_var = 'location_id',
    output_path,
    pdf_height = 10,
    pdf_width = 20,
    verbose = FALSE
) {
  plot_vars <- c('age_group_id', 'sex_id', 'location_id', 'year_id')
  checkmate::assert_data_table(DT)
  checkmate::assert_subset(x = plot_vs_pe_column, choices = c('mean', 'median'))
  checkmate::assert_subset(
    x = c('point_estimate', plot_vs_pe_column, plot_vars),
    choices = colnames(DT)
  )
  checkmate::assert_string(x = hist_plot_title, null.ok = FALSE)
  checkmate::assert_subset(
    x = by_page_var,
    choices = plot_vars,
    empty.ok = TRUE
  )
  checkmate::assert_subset(
    x = facet_var,
    choices = plot_vars,
    empty.ok = TRUE
  )
  checkmate::assert_subset(
    x = color_scatter_var,
    choices = plot_vars,
    empty.ok = TRUE
  )
  checkmate::assert_directory_exists(x = dirname(output_path))
  checkmate::assert_subset(x = tools::file_ext(output_path), choices = 'pdf')
  checkmate::assert_integerish(
    x = pdf_height,
    lower = 1,
    upper = 50,
    any.missing = FALSE
  )
  checkmate::assert_integerish(
    x = pdf_width,
    lower = 1,
    upper = 50,
    any.missing = FALSE
  )
  checkmate::assert_flag(x = verbose, null.ok = FALSE)

  pdf(
    file = output_path,
    height = pdf_height,
    width = pdf_width
  )

  hist_plot <- make_histogram(DT, hist_plot_title)
  plot(hist_plot)

  plot_scatters(DT, plot_vs_pe_column, by_page_var, facet_var, color_scatter_var, plot_vars, verbose)

  dev.off()
}

make_histogram <- function(DT, hist_plot_title) {
  ggplot2::ggplot(
    data = DT,
    mapping = ggplot2::aes(x = pe_percentile)
  ) +
    ggplot2::geom_histogram(binwidth=0.01, fill="blue", color="black", alpha=0.5) +
    ggplot2::labs(
      title = hist_plot_title,
      x = "Percentile of Point Estimate",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()
}

remap_vars <- function(var_name) {
  switch(
    var_name,
    'location_id' = 'super_region_name',
    'age_group_id' = 'age_group_name',
    'sex_id' = 'sex_name',
    'year_id'
  )
}

merge_ihme_metadata <- function(DT) {
  if(!('super_region_name' %in% colnames(DT))) {
    DT <- data.table::merge.data.table(
      x = DT,
      y = location_metadata,
      by = 'location_id'
    )
  }
  if(!('sex_name' %in% colnames(DT))) {
    DT <- data.table::merge.data.table(
      x = DT,
      y = sex_metadata,
      by = 'sex_id'
    )
  }
  if('age_group_name' %in% colnames(DT)) DT$age_group_name <- NULL
  DT <- data.table::merge.data.table(
    x = DT,
    y = age_metadata,
    by = 'age_group_id'
  )
  return(DT)
}

plot_scatters <- function(DT, plot_vs_pe_column, by_page_var, facet_var, color_scatter_var, plot_vars, verbose) {
  DT <- merge_ihme_metadata(DT)

  remaining_var <- setdiff(
    x = plot_vars,
    y = c(facet_var, by_page_var, color_scatter_var)
  ) |>
    remap_vars()
  facet_var <- remap_vars(facet_var)
  by_page_var <- remap_vars(by_page_var)
  color_scatter_var <- remap_vars(color_scatter_var)

  num_plot_cols_per_page <- ceiling(sqrt(length(unique(DT[[remaining_var]]))))
  for(a in sort(unique(DT[[by_page_var]]))) {
    plot_list <- lapply(sort(unique(DT[[remaining_var]])), \(b) {
      i_vec <- which(DT[[by_page_var]] == a & DT[[remaining_var]] == b)
      plot_dat <- DT |> dplyr::slice(i_vec)
      plot_title <- paste(a, b, sep = ' - ')
      if(verbose) print(paste('plotting:', plot_title))

      p <- ggplot2::ggplot(
        data = plot_dat,
        mapping = ggplot2::aes(x = get(plot_vs_pe_column), y = point_estimate)
      ) +
        ggplot2::geom_point(
          mapping = ggplot2::aes(
            colour = get(color_scatter_var)
          )
        ) +
        ggplot2::geom_abline(slope = 1, colour = 'black', linetype = 'dashed') +
        ggplot2::labs(
          title = plot_title,
          x = plot_vs_pe_column,
          y = 'point_estimate',
          colour = color_scatter_var
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.95, hjust=1)
        )
      if (length(unique(DT[[facet_var]])) > 1) {
        p <- p +
          ggplot2::facet_wrap(~get(facet_var))
      }
      return(p)
    })
    do.call(
      getExportedValue('gridExtra', 'grid.arrange'),
      c(plot_list, ncol = num_plot_cols_per_page)
    )
  }
}
