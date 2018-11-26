## Take draws object, cast wide on draws, and tag model number
prep_draw_output <- function(draws, model) {
  return(dcast(draws, iso3 + year ~ draw, value.var = c("yvar", "ydiff"))[, model := model])
}
