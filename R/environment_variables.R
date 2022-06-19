env <- new.env()

env$adulthood.age <- 18
env$person.maximum.age <- 99
env$person.minimum.age <- 1
env$minimum.age.difference.points <- 1
env$maximum.age.difference.points <- 20
env$cPRA.minimum <- 0
env$cPRA.maximum <- 100
env$dialysis.minimum <- 0
env$dialysis.maximum <- 999

env$valid.tiers = c('A', 'B')
env$valid.rris = c('R1', 'R2', 'R3', 'R4')
env$valid.blood.groups = c('O', 'A', 'B', 'AB')
env$valid.urgent = c(0, 1)