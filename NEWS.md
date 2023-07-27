# plotmodsem 0.0.4.1

## Miscellaneous

- Customized `linter`.
- Update to use `linewidth` in `ggplot2`
  when appropriate.

# plotmodsem 0.0.4

## Maintenance Release

- Fixed some docs and helps.

# plotmodsem 0.0.3

## New Features

- `plotmod()` accepts unquoted names. E.g., `x = iv` and `x = "iv"` both works.

- Added `x_from_mean_in_sd` and `w_from_mean_in_sd` to `plotmod()`. Users can
  define `high` and `low` in terms of SD. Defaults are 1.

- Added `x_method`, `w_method`, `x_percentiles` and `w_percentiles` to `plotmod()`.
  Users can define "high" and "low" in terms of percentiles for the moderator
  by setting `w_method = "percentile"` (for the focal variable, set
  `x_method = "percentile"`).

- Added `x_sd_to_percentiles` and `w_sd_to_percentiles` to `plotmod()`.
  If percentiles are used to define "high" and "low", users can set
  `x_sd_to_percentiles` and/or `w_sd_to_percentiles` to find the percentiles
  using a number of SD above or below mean in a normal distribution.

- Added `plot_x_vlines` and `x_vlines_unit` to `plotmod()`. Users can add
  one or more vertical lines for selected level of the focal variables.
  The levels can be specified as deviation from mean in SD or percentiles.

## Bug Fix

- Fixed the bug that labels are not used in forming the title.

- Fixed the bug that NAs are not removed in computing percentiles.

# plotmodsem 0.0.2

## New Features

- Can plot the moderation effect in standardized metric. The pattern of lines
  is the same, standardized or not, but the scales are different.

- Print the conditional effects as a subtitle.

## Bug Fix

- Fix the bug that variances instead of standard deviations are retrieved.

- Fix the bug that SDs and means are retrieved from the parameter tables. This
  will not work if the focal variable or the moderator is an endogenous variable
  and so its mean and variance are not free parameters. The implied means are
  SDs are now used.

# plotmodsem 0.0.1

- First alpha release.
