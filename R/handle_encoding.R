get_encoding_method <- function( method ) {
  list(
    "mean" = encode_mean,
    "median" = encode_median,
    "deviation" = encode_deviation,
    "lowrank" = encode_lowrank,
    "spca" = encode_spca,
    "mnl" = encode_mnl,
    "dummy" = encode_dummy,
    "difference" = encode_difference,
    "helmert" = encode_helmert,
    "simple_effect" = encode_simple_effect,
    "repeated_effect" = encode_repeated_effect
  )[[method]]
}

validate_encoding_methods <- function( x ) {
  valid <- all(x %in% c("mean", "median", "deviation", "lowrank", "spca",
                        "mnl", "dummy", "difference", "helmert",
                        "simple_effect", "repeated_effect"))
  if(!valid) {
    stop(paste0("Invalid encoding method:\n", x))
  }
}