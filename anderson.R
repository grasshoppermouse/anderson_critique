#+ message=F

library(tidyverse)
library(knitr)
library(readxl)

# Anderson et al. (2023)
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0287101

# Uncomment to download excel data file from Anderson et al. (2023)
# download.file("https://doi.org/10.1371/journal.pone.0287101.s001", "journal.pone.0287101.s001.xlsx", "auto")

anderson <- 
  read_excel("journal.pone.0287101.s001.xlsx", range = cell_cols("A:Q")) |>  # omit refs
  rename(
    anderson_subsistence = `Subsistence Economy: Most important activity (0=gathering, 1=hunting, 2=fishing, 3=hunt/gather)`,
    anderson_preysize = `small (1), medium (2), large game (3), all (4)`,
    anderson_hunt = `Documentation of women hunting? (0=no, 1=yes)`
  ) |> 
  dplyr::select(anderson_subsistence, anderson_preysize, anderson_hunt)

recode <- 
  read_excel('recoding.xlsx', skip = 1) |> 
  dplyr::select(Society, Subsistence...6:Pseudoreplication) |> # Omit our copy of the Anderson data
  rename(
    Subsistence = Subsistence...6,
    small_game = `Hunt small-medium game? (<45kg)`,
    large_game = `Hunt large game? (≥45kg)`
  ) |> 
  bind_cols(anderson) |> # fragile, but the rows are in the same order
  mutate(
    anderson_hunt2 = ifelse(anderson_hunt == 0, 'Anderson et al.: No', 'Anderson et al.: Yes'),
    anderson_preysize = ifelse(Society == 'Tamang', NA, anderson_preysize),
    anderson_preysize2 = case_when(
      anderson_preysize %in% c('1', '2') ~ 'Anderson et al.: Small/medium game',
      anderson_preysize %in% c('3', '4') ~ 'Anderson et al.: Large game',
      is.na(anderson_preysize) & anderson_hunt == 1 ~ 'Anderson et al.: Unknown size',
      is.na(anderson_preysize) & anderson_hunt == 0 ~ 'Anderson et al.: No hunting',
      .default = NA
    ),
    anderson_preysize2 = factor(anderson_preysize2, levels = c('Anderson et al.: Small/medium game', 'Anderson et al.: Large game', 'Anderson et al.: Unknown size', 'Anderson et al.: No hunting')),
    small_game = factor(small_game, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently')),
    large_game = factor(large_game, levels = c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently')),
    women_hunt = small_game %in% c('Sometimes', 'Frequently') | large_game %in% c('Sometimes', 'Frequently'),
    women_hunt2 = small_game %in% c('Rarely', 'Sometimes', 'Frequently') | large_game %in% c('Rarely', 'Sometimes', 'Frequently'),
    false_negative = anderson_hunt == 0 & women_hunt2,
    false_positive1 = anderson_hunt == 1 & !women_hunt,
    false_positive2 = anderson_hunt == 1 & !women_hunt2
  ) |> 
  relocate(
    contains('anderson'),
    .after = Society
  )

# Anderson et al. (2023) results

# Most important activity (0=gathering, 1=hunting, 2=fishing, 3=hunt/gather)
table(anderson$anderson_subsistence, useNA = 'a')

# 0: No, 1: Yes
table(anderson$anderson_hunt, useNA = 'a')

# small (1), medium (2), large game (3), all (4)
table(anderson$anderson_preysize, useNA = 'a')


# Our hunting results

xtabs(~small_game+large_game, recode) |> kable()

#+ fig.width=10
ggplot(recode, aes(small_game, large_game)) +
  geom_count() + 
  scale_size_area(breaks = c(1, 3, 5, 10, 15)) +
  labs(x = '\nSmall/medium game hunting (<45kg)', y = 'Large game hunting (≥45kg)') +
  guides(size = guide_legend('Number of societies')) +
  coord_fixed() +
  theme_bw()

# Table of societies by prey size and frequency

recode |> 
  dplyr::select(Society, small_game, large_game) |> 
  pivot_longer(c(small_game, large_game), names_to = 'Prey_size', values_to = 'Frequency') |> 
  group_by(Prey_size, Frequency) |> 
  summarise(
    Societies = paste0('[',n(), ']: ', str_flatten_comma(sort(Society))),
    .groups = 'drop'
  ) |> 
  pivot_wider(names_from = 'Prey_size', values_from = 'Societies') |> 
  relocate(small_game, .before = large_game) |> 
  kable(col.names = c('Frequency', 'Women hunt small-medium game (<45kg)', 'Women hunt large game (≥45 kg)'))

# Comparing Anderson et al. results to our results

# Subsistence miscoding

# Anderson et al. codes
# Most important activity (0=gathering, 1=hunting, 2=fishing, 3=hunt/gather)

# Our codes
# HH = Hunter-Horticulturalists, A = Agriculturalists, H = Horiculturalists, HG = Hunter Gatherers, F = Fishers

xtabs(~Subsistence+anderson_subsistence, recode, addNA = T) |> kable()

# Anderson non-foragers

sum(recode$Subsistence != 'HG', na.rm = T) # Fishers not counted as foragers

# Do women hunt? False negatives and false positives

xtabs(~small_game+large_game+anderson_hunt, recode)

# How many false negatives according to looser criterion?
sum(recode$false_negative)

# How many false positives according to stricter criterion?
sum(recode$false_positive1)

# How many false positives according to looser criterion?
sum(recode$false_positive2)


#+ fig.width=10
ggplot(recode, aes(small_game, large_game)) +
  geom_count() + 
  scale_size_area(breaks = c(1, 3, 5, 10)) +
  labs(x = '\nSmall/medium game hunting (<45kg)', y = 'Large game hunting (≥45kg)') +
  guides(size = guide_legend('Number of societies')) +
  facet_wrap(~anderson_hunt2) + 
  coord_fixed() +
  theme_bw()

# Prey size inconsistencies

# Small/medium game
xtabs(~anderson_preysize2 + small_game, recode) |> kable()

small_game_consistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: Small/medium game' & recode$small_game %in% c('Sometimes', 'Frequently'))
small_game_consistent
# small game inconsistent
sum(recode$anderson_preysize2 == 'Anderson et al.: Small/medium game') - small_game_consistent

# Large game
xtabs(~anderson_preysize2 + large_game, recode) |> kable()

large_game_consistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: Large game' & recode$large_game %in% c('Sometimes', 'Frequently'))
large_game_consistent
# large game inconsistent
sum(recode$anderson_preysize2 == 'Anderson et al.: Large game') - large_game_consistent

# Anderson et al. unknown prey size
unknown_inconsistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: Unknown size')
unknown_inconsistent

# Anderson et al. No women hunting
no_hunting_consistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: No hunting' & recode$small_game %in% c('No evidence', 'Never') & recode$large_game %in% c('No evidence', 'Never'))
no_hunting_consistent
# No hunting inconsistent
sum(recode$anderson_preysize2 == 'Anderson et al.: No hunting') - no_hunting_consistent

#+ fig.width=10
ggplot(recode, aes(small_game, large_game)) +
  geom_count() + 
  scale_size_area(breaks = c(1, 3, 5, 10)) +
  labs(x = '\nSmall/medium game hunting (<45kg)', y = 'Large game hunting (≥45kg)\n') +
  guides(size = guide_legend('Number of societies')) +
  facet_wrap(~anderson_preysize2) + 
  coord_fixed() +
  theme_bw()

