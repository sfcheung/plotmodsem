#' @title Plot the moderation effect in
#' a path model (WIP: Not Ready)
#'
#' @description Plot the moderation
#' effect in a path model
#'
#' @details This function extracts the
#' information stored in the `lavaan`
#' fit object to plot a two-line graph,
#' one for the relation between the
#' focal variable (`x`) and the outcome
#' variable (`y`) when the moderator
#' (`w`) is one standard deviation below
#' mean, and one when the moderator is
#' one standard deviation above mean.
#'
#' @return
#' A [ggplot2] graph.
#'
#' @param fit The fit object. Can be a
#' [lavaan::lavaan-class] object or a
#' list of [lm()] outputs.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
#'
#' @param x Character. The name of the
#' predictor at the start of the path.
#'
#' @param y Character. The name of the
#' outcome variable at the end of the
#' path.
#'
#' @param fit The fit object. Can be a
#' [lavaan::lavaan-class] object or a
#' list of [lm()] outputs.
#' It can also be
#' a `lavaan.mi` object
#' returned by
#' [semTools::runMI()] or
#' its wrapper, such as [semTools::sem.mi()].
#'
#' @param standardized_x Logical.
#' Whether `x` will be standardized.
#' Default is `FALSE`.
#'
#' @param standardized_y Logical.
#' Whether `y` will be standardized.
#' Default is `FALSE`.
#'
#' @param wlevels The output of
#' [manymome::merge_mod_levels()], or the
#' moderator(s) to be passed to
#' [manymome::mod_levels_list()]. If all the
#' moderators can be represented by one
#' variable, that is, each moderator is
#' (a) a numeric variable, (b) a
#' dichotomous categorical variable, or
#' (c) a factor or string variable used
#' in [lm()] in `fit`, then it is a
#' vector of the names of the moderators
#' as appeared in the data frame. If at
#' least one of the moderators is a
#' categorical variable represented by
#' more than one variable, such as
#' user-created dummy variables used in
#' [lavaan::sem()], then it must be a
#' list of the names of the moderators,
#' with such moderators represented by a
#' vector of names. For example:
#' `list("w1", c("gpgp2", "gpgp3")`, the
#' first moderator `w1` and the second
#' moderator a three-categorical
#' variable represented by `gpgp2` and
#' `gpgp3`.
#'
#' @param w_type Character. Whether the
#' moderator is a `"numeric"` variable
#' or a `"categorical"` variable. If
#' `"auto"`, the function will try to
#' determine the type automatically.
#' See [mod_levels_list()] for further
#' information.
#'
#' @param w_method Character, either
#' `"sd"` or `"percentile"`. If `"sd"`,
#' the levels are defined by the
#' distance from the mean in terms of
#' standard deviation. if
#' `"percentile"`, the levels are
#' defined in percentiles.  See
#' [mod_levels_list()] for further
#' information.
#'
#' @param sd_from_mean A numeric vector.
#' Specify the distance in standard
#' deviation from the mean for each
#' level. Default is `c(-1, 0, 1)` when
#' there is only one moderator, and
#' `c(-1, 1)` when there are more than
#' one moderator. Ignored if `w_method`
#' is not equal to `"sd"`. See
#' [mod_levels_list()] for further
#' information.
#'
#' @param percentiles A numeric vector.
#' Specify the percentile (in
#' proportion) for each level. Default
#' is `c(.16, .50, .84)` if there is one
#' moderator, and `c(.16, .84)` when
#' there are more than one moderator.
#' Ignored if `w_method` is not equal to
#' `"percentile"`. See
#' [mod_levels_list()] for further
#' information.
#'
#' @param mod_levels_list_args
#' Additional arguments to be passed to
#' [mod_levels_list()] if it is called
#' for creating the levels of
#' moderators. Default is `list()`.
#'
#' @param standardized_x Logical.
#' Whether `x` will be standardized.
#' Default is `FALSE`.
#'
#' @param standardized_y Logical.
#' Whether `y` will be standardized.
#' Default is `FALSE`.
#'
#' @param x_label The label for the
#' X-axis. Default is the value of the
#' predictor in the output of
#' [manymome::cond_indirect_effects()].
#'
#' @param w_label The label for the
#' legend for the lines. Default is
#' `"Moderator(s)"`.
#'
#' @param y_label The label for the
#' Y-axis. Default is the name of the
#' response variable in the model.
#'
#' @param title The title of the graph.
#' If not supplied, it will be generated
#' from the variable names or labels (in
#' `x_label`, `y_label`, and `w_label`).
#' If `""`, no title will be printed.
#' This can be used when the plot is for
#' manuscript submission and figures are
#' required to have no titles.
#'
#' @param x_from_mean_in_sd How many SD
#' from mean is used to define "low" and
#' "high" for the focal variable.
#' Default is 1.
#'
#' @param x_method How to define "high"
#' and "low" for the focal variable
#' levels. Default is in terms of the
#' standard deviation of the focal
#' variable, `"sd"`. If equal to
#' `"percentile"`, then the percentiles
#' of the focal variable in the dataset
#' is used.
#'
#' @param x_percentiles If `x_method` is
#' `"percentile"`, then this argument
#' specifies the two percentiles to be
#' used, divided by 100. It must be a
#'   vector of two numbers. The default
#' is `c(.16, .84)`, the 16th and 84th
#' percentiles, which corresponds
#' approximately to one SD below and
#' above mean for a normal distribution,
#' respectively.
#'
#' @param x_sd_to_percentiles If
#' `x_method` is `"percentile"` and this
#' argument is set to a number, this
#' number will be used to determine the
#' percentiles to be used. The lower
#' percentile is the percentile in a
#' normal distribution that is
#' `x_sd_to_percentiles` SD below the
#' mean. The upper percentile is the
#' percentile in a normal distribution
#' that is `x_sd_to_percentiles` SD
#' above the mean. Therefore, if
#' `x_sd_to_percentiles` is set to 1,
#' then the lower and upper percentiles
#' are 16th and 84th, respectively.
#' Default is `NA`.
#'
#' @param note_standardized If `TRUE`,
#' will check whether a variable has SD
#' nearly equal to one. If yes, will
#' report this in the plot. Default is
#' `TRUE`.
#'
#' @param no_title If `TRUE`, title will
#' be suppressed. Default is `FALSE`.
#'
#' @param line_width The width of the
#' lines as used in
#' [ggplot2::geom_segment()]. Default is
#' 1.
#'
#' @param point_size The size of the
#' points as used in
#' [ggplot2::geom_point()]. Default is
#' 5.
#'
#' @param graph_type If `"default"`, the
#' typical line-graph with equal
#' end-points will be plotted. If
#' `"tubmle"`, then the tumble graph
#' proposed by Bodner (2016) will be
#' plotted. Default is `"default"`.
#'
#' @param ... Arguments to be passed to
#' the plot method of the output of
#' [manymome::cond_indirect_effects()].
#'
#' @examples
#' \dontrun{
#' # To be prepared
#' }
#' @export

plotmod_new <- function(fit,
                        x,
                        y,
                        wlevels,
                        w_type = "auto",
                        w_method = "sd",
                        sd_from_mean = NULL,
                        percentiles = NULL,
                        mod_levels_list_args = list(),
                        standardized_x = FALSE,
                        standardized_y = FALSE,
                        x_label,
                        w_label = "Moderator(s)",
                        y_label,
                        title,
                        x_from_mean_in_sd = 1,
                        x_method = c("sd", "percentile"),
                        x_percentiles = c(0.16, 0.84),
                        x_sd_to_percentiles = NA,
                        note_standardized = TRUE,
                        no_title = FALSE,
                        line_width = 1,
                        point_size = 5,
                        graph_type = c("default", "tumble"),
                        ...) {
    x_method <- match.arg(x_method)
    graph_type <- match.arg(graph_type)
    cond_out <- manymome::cond_indirect_effects(
                  wlevels = wlevels,
                  x = x,
                  y = y,
                  fit = fit,
                  w_type = w_type,
                  w_method = w_method,
                  sd_from_mean = sd_from_mean,
                  percentiles = percentiles,
                  mod_levels_list_args = mod_levels_list_args,
                  standardized_x = standardized_x,
                  standardized_y = standardized_y)
    plot(cond_out,
         x_label = x_label,
         w_label = w_label,
         y_label = y_label,
         title = title,
         x_from_mean_in_sd = x_from_mean_in_sd,
         x_method = x_method,
         x_percentiles = x_percentiles,
         x_sd_to_percentiles = x_sd_to_percentiles,
         note_standardized = note_standardized,
         no_title = no_title,
         line_width = line_width,
         point_size = point_size,
         graph_type = graph_type,
         ...)
  }
