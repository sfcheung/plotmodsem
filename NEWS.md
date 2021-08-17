# plotmodsem 0.0.3

## New Features

- `plotmod` accepts unquoted names. E.g., `x = iv` and `x = "iv"` both works.

# plotmodsem 0.0.2

## New Features

- Can plot the moderation effect in standardized metric. The pattern of lines
  is the same, standardized or not, but the scales are different.

- Print the conditional effects as a subtitle.

## Bug Fix

- Fix the bug that variances instead of standard deivations are retrieved.

- Fix the bug that SDs and means are retrieved from the parameter tables. This 
  will not work if the focal variable or the moderator is an endogenous variable
  and so its mean and variance are not free parameters. The implied means are 
  SDs are now used.

# plotmodsem 0.0.1

- First alpha release.
