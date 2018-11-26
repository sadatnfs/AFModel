## Inv Lemon squeeze logit transformer
inv_logit_trans <- function(x) {
  return(((exp(x) / (1 + exp(x))) - (0.5 / 1000)) * (1000 / 999))
}
