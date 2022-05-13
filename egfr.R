# Clinical #####
# Pediatric eGFR
# age = 0.1
# ht = 0.8
# cre = 1
# sex = "M"

refcre <- function(ht, sex){
  ifelse(sex %in% c("M","Male","male"),
         -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
         -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)}

percre <- function(cre, ht, sex){
  (ifelse(sex %in% c("M","Male","male"),
          -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
          -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778))/cre*100}

pegfr <- function(cre, age, ht, sex){
  R = ifelse(age<(3/12), NA,
             ifelse(0.107*log(age*12)+0.656, 1))
  r_cre = ifelse(sex %in% c("M","Male","male"),
                 -1.259*ht^5+7.815*ht^4-18.57*ht^3+21.39*ht^2-11.71*ht+2.628,
                 -4.536*ht^5+27.16*ht^4-63.47*ht^3+72.43*ht^2-40.06*ht+8.778)
  110.2*R*r_cre/cre+2.93 }