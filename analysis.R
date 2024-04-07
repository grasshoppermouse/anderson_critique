#+ message=F

library(tidyverse)
library(vcd)
library(readxl)
library(knitr)
library(patchwork)
library(scico)

if (!dir.exists("Figures")){
  dir.create("Figures")
}

hunt_freq <- c('No evidence', 'Never', 'Rarely', 'Sometimes', 'Frequently')

source('interrater_reliability.R')

# Anderson et al. (2023)
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0287101

# Uncomment to download Excel data file from Anderson et al. (2023)
# download.file("https://doi.org/10.1371/journal.pone.0287101.s001", "data/journal.pone.0287101.s001.xlsx", "auto")

anderson <- 
  read_excel("data/journal.pone.0287101.s001.xlsx", range = cell_cols("A:Q")) |>  # omit refs
  rename(
    anderson_subsistence = `Subsistence Economy: Most important activity (0=gathering, 1=hunting, 2=fishing, 3=hunt/gather)`,
    anderson_preysize = `small (1), medium (2), large game (3), all (4)`,
    anderson_hunt = `Documentation of women hunting? (0=no, 1=yes)`
  ) |> 
  mutate(
    anderson_subsistence2 = case_when(
      anderson_subsistence == '0' ~ 'Gathering',
      anderson_subsistence == '1' ~ 'Hunting',
      anderson_subsistence == '2' ~ 'Fishing',
      anderson_subsistence == '1,2' ~ 'Hunting/Fishing',
      anderson_subsistence == '3' ~ 'Hunting/Gathering',
      .default = NA
    ),
    anderson_hunt2 = ifelse(anderson_hunt == 0, 'Anderson et al.: No', 'Anderson et al.: Yes'),
    anderson_smallgame = anderson_preysize %in% c('1', '2'),
    anderson_largegame = anderson_preysize %in% c('3', '4'),
    anderson_preysize3 = case_when(
      anderson_preysize == 1 ~ 'Small',
      anderson_preysize == 2 ~ 'Medium',
      anderson_preysize == 3 ~ 'Large',
      anderson_preysize == 4 ~ 'All',
      is.na(anderson_preysize) & anderson_hunt ~ 'Unknown',
      .default = 'No hunting'
    ),
    anderson_preysize3 = factor(anderson_preysize3, levels = c('No hunting', 'Unknown', 'Small', 'Medium', 'Large', 'All')),
    anderson_preysize2 = case_when(
      anderson_smallgame ~ 'Anderson et al.: Small/medium game',
      anderson_largegame ~ 'Anderson et al.: Large game',
      anderson_preysize3 == 'Unknown' ~ 'Anderson et al.: Unknown size',
      anderson_preysize3 == 'No hunting' ~ 'Anderson et al.: No hunting',
      .default = NA
    ),
    anderson_preysize2 = factor(anderson_preysize2, levels = c('Anderson et al.: Small/medium game', 'Anderson et al.: Large game', 'Anderson et al.: Unknown size', 'Anderson et al.: No hunting')),
  ) |> 
  dplyr::select(starts_with("anderson"))

# Final recoding (after independent recoding and review)
recode <- 
  read_excel('data/recoding.final.xlsx', skip = 1, na = 'N/A') |> 
  dplyr::select(Society, Subsistence...6:`Logic of frequency coding`) |> # Omit our copy of the Anderson data
  rename(
    Subsistence = Subsistence...6,
    small_game = `Hunt small-medium game (<45kg)`,
    large_game = `Hunt large game (≥45kg)`,
    Rationale = `Logic of frequency coding`
  ) |> 
  bind_cols(anderson) |> # fragile, but the rows are in the same order
  mutate(
    psuedo_rep = str_detect(Pseudoreplication, "Yes"),
    small_game = str_to_sentence(small_game),
    large_game = str_to_sentence(large_game),
    small_game = factor(small_game, levels = hunt_freq, ordered = T),
    large_game = factor(large_game, levels = hunt_freq, ordered = T),
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

#' # Anderson et al. (2023) results

# Most important activity (0=gathering, 1=hunting, 2=fishing, 3=hunt/gather)
table(anderson$anderson_subsistence2, useNA = 'a')

# 0: No, 1: Yes
table(anderson$anderson_hunt, useNA = 'a')

# small (1), medium (2), large game (3), all (4)
table(anderson$anderson_preysize, useNA = 'a')


# Our hunting results

xtabs(~small_game+large_game, recode) |> kable()

#+ fig.width=10
plot_recode <-
  ggplot(recode, aes(small_game, large_game)) +
  geom_count() + 
  scale_size_area(breaks = c(1, 3, 5, 10, 15)) +
  labs(x = '\nSmall/medium game hunting (<45kg)', y = 'Large game hunting (≥45kg)') +
  guides(size = guide_legend('Number of societies')) +
  coord_fixed() +
  theme_bw()
plot_recode
ggsave("Figures/plot_recode.svg", plot_recode)

# Table of societies by prey size and frequency

recode_table <-
  recode |> 
  dplyr::select(Society, small_game, large_game, anderson_preysize) |> 
  pivot_longer(c(small_game, large_game), names_to = 'Prey_size', values_to = 'Frequency') |> 
  group_by(Prey_size, Frequency) |> 
  mutate(
    Society = case_when(
      anderson_preysize %in% c(1,2,4) & Prey_size == 'small_game' ~ paste0('**', Society, '**'),
      anderson_preysize %in% c(3,4) & Prey_size == 'large_game' ~ paste0('**', Society, '**'),
      .default = Society
    )
  ) |> 
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

xtabs(~Subsistence+anderson_subsistence2, recode, addNA = T) |> kable()

# Anderson non-foragers (included in Figure 1)

sum(!str_detect(recode$Subsistence, 'HG')) # Fishers, HH not counted as foragers
sum(recode$Subsistence %in% c('A', 'H', 'HH')) # Fishers counted as foragers

# Do women hunt? False negatives and false positives

xtabs(~small_game+large_game+anderson_hunt, recode)

# How many false negatives according to looser criterion?
sum(recode$false_negative)

# How many false positives according to stricter criterion?
sum(recode$false_positive1)

# How many false positives according to looser criterion?
sum(recode$false_positive2)

recode$sg <- as.numeric(recode$small_game)
recode$lg <- as.numeric(recode$large_game)
pad <- 0.2

anderson_no <- recode[recode$anderson_hunt==0,]
anderson_fn <- anderson_no[anderson_no$false_negative,]
xmin <- min(anderson_fn$sg) - pad
xmax <- max(anderson_fn$sg) + pad
ymin <- min(anderson_fn$lg) - pad
ymax <- max(anderson_fn$lg) + pad

lbl <- str_glue("False negatives (N={sum(anderson_no$false_negative)})")
plot_false_neg <-
  ggplot(anderson_no, aes(small_game, large_game)) +
  geom_count() +
  annotate("rect", xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, colour = "blue", fill = NA) +
  annotate("text", label = lbl, x = xmin, y = ymax+0.15, hjust=0) +
  scale_size_area(breaks = c(1, 2, 5, 10), limits = c(1, 10)) +
  scale_x_discrete(limits = levels(recode$small_game)) +
  scale_y_discrete(limits = levels(recode$small_game)) +
  labs(
    title = "Anderson et al. (2023)",
    subtitle = "No women hunting",
    x = '\nSmall/medium game hunting (<45kg)',
    y = 'Large game hunting (≥45kg)'
    ) +
  guides(size = guide_none()) +
  coord_fixed() +
  theme_bw(15)
plot_false_neg

anderson_yes <- recode[recode$anderson_hunt==1,]
anderson_fp <- anderson_yes[anderson_yes$false_positive1,]
anderson_fp2 <- anderson_yes[anderson_yes$false_positive2,]
xmin <- min(anderson_fp$sg) - pad
xmax <- max(anderson_fp$sg) + pad
xmax2 <- max(anderson_fp2$sg) + pad
ymin <- min(anderson_fp$lg) - pad
ymax <- max(anderson_fp$lg) + pad
ymax2 <- max(anderson_fp2$lg) + pad

lbl <- str_glue("False positives (N={sum(anderson_yes$false_positive1)})")
lbl2 <- str_glue("False positives (N={sum(anderson_yes$false_positive2)})")
plot_false_pos <-
  ggplot(anderson_yes, aes(small_game, large_game)) +
  geom_count() +
  annotate("rect", xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, colour='blue', fill = NA) +
  annotate("text", label = lbl, x = xmin+0.1, y = ymax+0.15, hjust=0) +
  annotate("rect", xmin = xmin, ymin = ymin, xmax = xmax2, ymax = ymax2, colour='blue', fill = NA) +
  annotate("text", label = lbl2, x = xmin+0.1, y = ymax2+0.15, hjust=0) +
  scale_size_area(breaks = c(1, 2, 5, 10), limits = c(1, 10)) +
  scale_y_discrete(limits = levels(recode$small_game)) +
  labs(
    subtitle = "Women hunting",
    x = '\nSmall/medium game hunting (<45kg)',
    y = 'Large game hunting (≥45kg)'
    ) +
  guides(size = guide_legend('Number of societies')) +
  coord_fixed() +
  theme_bw(15) + 
  theme(axis.title.y = element_blank(), axis.text.y = element_blank())
plot_false_pos

plot_false_both <- plot_false_neg + plot_false_pos + plot_layout(axis_titles = 'collect')
ggsave("Figures/plot_false_both.pdf", width = 14, height = 8, device = cairo_pdf)
ggsave("Figures/plot_false_both.png", width = 14, height = 8)

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

# large game inconsistent (included in Figure 1)
sum(recode$anderson_preysize2 == 'Anderson et al.: Large game') - large_game_consistent

# Anderson et al. unknown prey size
unknown_inconsistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: Unknown size')
unknown_inconsistent

# Anderson et al. No women hunting
no_hunting_consistent <- sum(recode$anderson_preysize2 == 'Anderson et al.: No hunting' & recode$small_game %in% c('No evidence', 'Never') & recode$large_game %in% c('No evidence', 'Never'))
no_hunting_consistent
# No hunting inconsistent
sum(recode$anderson_preysize2 == 'Anderson et al.: No hunting') - no_hunting_consistent

consistent <- tibble(
  anderson_preysize2 = factor(levels(recode$anderson_preysize2), levels = levels(recode$anderson_preysize2)),
  xmin = c(4, 1, NA, 1) - pad,
  xmax = c(5, 5, NA, 2) + pad,
  ymin = c(1, 4, NA, 1) - pad,
  ymax = c(5, 5, NA, 2) + pad,
  small_game = factor(hunt_freq[-1], levels = hunt_freq),
  large_game = factor(hunt_freq[-1], levels = hunt_freq),
  consistent = c(
    sum(recode$small_game %in% c('Sometimes', 'Frequently') & recode$anderson_preysize2 == 'Anderson et al.: Small/medium game'),
    sum(recode$large_game %in% c('Sometimes', 'Frequently') & recode$anderson_preysize2 == 'Anderson et al.: Large game'),
    0,
    sum(recode$small_game %in% c('No evidence', 'Never') & recode$large_game %in% c('No evidence', 'Never') & recode$anderson_preysize2 == 'Anderson et al.: No hunting')
  ),
  inconsistent = c(table(recode$anderson_preysize2) - consistent),
  text = str_glue("Consistent: {consistent} Inconsistent: {inconsistent}")
)

#+ fig.width=12
plot_preysize <-
  ggplot(recode, aes(small_game, large_game)) +
  geom_count() + 
  scale_size_area(breaks = c(1, 3, 5, 10)) +
  geom_rect(data = consistent, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), colour = 'orange', fill = NA) +
  geom_text(data = consistent, aes(label = text), x = 3, y = 5.4, hjust = 0.5) +
  labs(x = '\nSmall/medium game hunting (<45kg)', y = 'Large game hunting (≥45kg)\n') +
  guides(size = guide_legend('Number of societies')) +
  facet_wrap(~anderson_preysize2) + 
  coord_fixed() +
  theme_bw()
plot_preysize
ggsave("Figures/plot_preysize.pdf", plot_preysize, width = 10, height = 10, device = cairo_pdf)
ggsave("Figures/plot_preysize.png", plot_preysize, width = 10, height = 10)

plot_anderson_preysize <-
  ggplot(recode, aes(x="", fill = fct_rev(anderson_preysize3))) +
  geom_bar(position = 'stack') +
    scale_fill_scico_d(direction = -1, palette = 'lapaz', end = 0.9) +
    guides(fill = guide_legend('Prey size')) +
    labs(title = 'Anderson et al. (2023)', x = '', y = 'Number of\nsocieties') +
    theme_minimal(15) +
    theme(axis.title.y = element_text(angle = 0, hjust = 1))
# plot_anderson_preysize

plot_preysize_recode <- 
  recode |> 
  dplyr::select(small_game, large_game) |> 
  rename(
    `<45kg` = small_game,
    `≥45kg` = large_game
  ) |> 
  pivot_longer(1:2) |> 
  ggplot(aes(name, fill = fct_rev(value))) +
  geom_bar(position = 'stack') +
  scale_fill_scico_d(direction = -1, palette = 'lajolla', end = 0.9) +
  guides(fill = guide_legend('Hunting frequency')) +
  labs(title = 'Our recoding', x = 'Prey size', y = '') +
  theme_minimal(15) +
  theme(axis.title.y = element_text(angle = 0, hjust = 1))

ggsave("Figures/plot_raw.pdf", plot_anderson_preysize + plot_preysize_recode, width = 12, height = 9, device = cairo_pdf)
ggsave("Figures/plot_raw.png", plot_anderson_preysize + plot_preysize_recode, width = 12, height = 9)

sessionInfo()