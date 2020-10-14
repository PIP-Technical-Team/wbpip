md_check_inputs_compute_poverty_stats <- function(welfare, weight){
  if (!is.numeric(weight))
    rlang::abort(c('`weight` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(weight))))
  if (!is.numeric(welfare))
    rlang::abort(c('`welfare` must be a numeric or integer vector:',
                   x = sprintf('You\'ve supplied an object of class %s.', class(welfare))))
  if (length(weight) != length(welfare))
    rlang::abort(c('`welfare` and `weight` must have compatible lengths:',
                   x = sprintf('`welfare` has length %s.', length(welfare)),
                   x = sprintf('`weight` has length %s.', length(weight))))
}
