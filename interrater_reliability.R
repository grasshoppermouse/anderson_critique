# library(tidyverse)
# library(readxl)
# library(vcd)

#' # Inter-rater reliability

# Original recoding (preliminary)

hunt_freq <- c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently')

recode_prelim <- 
  read_excel('data/recoding.preliminary.xlsx', skip = 1, na = 'N/A') |> 
  dplyr::select(Society, Subsistence...6:Pseudoreplication) |> # Omit our copy of the Anderson data
  rename(
    Subsistence = Subsistence...6,
    small_game = `Hunt small-medium game (<45kg)`,
    large_game = `Hunt large game (≥45kg)`
  ) |> 
  mutate(
    small_game = str_to_sentence(small_game),
    large_game = str_to_sentence(large_game),
    small_game = factor(small_game, levels = hunt_freq, ordered = T),
    large_game = factor(large_game, levels = hunt_freq, ordered = T)
  )

# Independent rater 2 (SL)
rater2 <- 
  read_excel("data/rater2_SL.xlsx", skip = 1) |>
  slice(-c(60, 61, 62)) |> # Coded twice
  rename(
    small_game2 = `Hunt small-medium game (<45kg)`,
    large_game2 = `Hunt large game (≥45kg)`
  ) |> 
  dplyr::filter(small_game2 != '?' & large_game2 != '?') |> 
  mutate(
    small_game2 = ifelse(small_game2 == 'NA', 'No evidence', small_game2),
    large_game2 = ifelse(large_game2 == 'NA', 'No evidence', large_game2),
    large_game2 = ifelse(large_game2 == 'never', 'Never', large_game2),
    small_game2 = ifelse(small_game2 == 'Rare', 'Rarely', small_game2),
    large_game2 = ifelse(large_game2 == 'Rare', 'Rarely', large_game2),
    small_game2 = factor(small_game2, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently'), ordered = T),
    large_game2 = factor(large_game2, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently'), ordered = T)
  )

# Fix some misspellings in rater2

rater2$Society[rater2$Society == 'Karuareg'] <- 'Kaurareg'
rater2$Society[rater2$Society == 'Fish Lake Valley Northern Paiute'] <- 'Fish Lake Valley North Paiute'
rater2$Society[rater2$Society == 'Torres Straight Islanders'] <- 'Torres Strait Islanders'
setdiff(rater2$Society, recode_prelim$Society)

rater2b <- 
  left_join(rater2, recode_prelim[c('Society', 'small_game', 'large_game')]) |> 
  dplyr::select(Society, small_game, small_game2, large_game, large_game2) |> 
  na.omit()

#+ fig.width=8, fig.height=8
ragg::agg_png("Figures/agree_small.2.png", width = 800, height = 800)
par(mai = c(1.5,1.5,1.5,1.5))
b_small2 <- agreementplot(~small_game2+small_game, data = rater2b, main = 'Small game', ylab = 'SL', xlab = 'VV', ylab_rot = 0, ylab_just = 'right')
dev.off()
b_small2

ragg::agg_png("Figures/agree_large.2.png", width = 800, height = 800)
par(mai = c(1.5,1.5,1.5,1.5))
b_large2 <- agreementplot(~large_game2+large_game, data = rater2b, main = 'Large game', ylab = 'SL', xlab = 'VV', ylab_rot = 0, ylab_just = 'right')
dev.off()
b_large2

# Independent rater 3 (KS)

rater3 <- 
  read_excel("data/rater3_KS.xlsx", skip = 1) |> 
  rename(
    small_game3 = `Hunt small-medium game (<45kg)`,
    large_game3 = `Hunt large game (≥45kg)`
  ) |> 
  mutate(
    large_game3 = ifelse(large_game3 == "Sometimes?", "Sometimes", large_game3),
    small_game3 = factor(small_game3, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently'), ordered = T),
    large_game3 = factor(large_game3, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently'), ordered = T)
  )

rater3b <- 
  left_join(rater3, recode_prelim[c('Society', 'small_game', 'large_game')]) |> 
  left_join(rater2[c('Society', 'small_game2', 'large_game2')]) |> 
  dplyr::select(Society, small_game, small_game2, small_game3, large_game, large_game2, large_game3)

#+ fig.width=8, fig.height=8
ragg::agg_png("Figures/agree_small.3.png", width = 800, height = 800)
par(mai = c(1.5,1.5,1.5,1.5))
b_small3 <- agreementplot(~small_game3+small_game, data = rater3b, main = 'Small game', ylab = 'KS', xlab = 'VV', ylab_rot = 0, ylab_just = 'right')
dev.off()
b_small3

ragg::agg_png("Figures/agree_large.3.png", width = 800, height = 800)
par(mai = c(1.5,1.5,1.5,1.5))
b_large3 <- agreementplot(~large_game3+large_game, data = rater3b, main = 'Large game', ylab = 'KS', xlab = 'VV', ylab_rot = 0, ylab_just = 'right')
dev.off()
b_large3

# Major discrepancies between original ratings and rater 3

small_game_diffs <- abs(as.numeric(rater3b$small_game3) - as.numeric(rater3b$small_game))>1
large_game_diffs <- abs(as.numeric(rater3b$large_game3) - as.numeric(rater3b$large_game))>1

sum(small_game_diffs | large_game_diffs)

rater3b[small_game_diffs | large_game_diffs,]

# Rater 2 vs. 3
b_small2_3 <- agreementplot(~small_game2+small_game3, data = rater3b, main = 'Small game 2 vs 3', xlab = 'KS', ylab = 'SL', ylab_rot = 0, ylab_just = 'right')
b_large2_3 <- agreementplot(~large_game2+large_game3, data = rater3b, main = 'Large game 2 vs 3', xlab = 'KS', ylab = 'SL', ylab_rot = 0, ylab_just = 'right')

