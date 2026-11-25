# Registered in .onLoad()
#' @exportS3Method NULL
pillar_shaft.sigfig <- function(x, ...) {
  pillar::new_pillar_shaft_simple(prec_format(x), align = "right")
}
