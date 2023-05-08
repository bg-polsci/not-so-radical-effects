### FIGURES ###
# Not so Radical Effects: Estimating the Causal Impact of Radical Right Representation on Political Support #
# The following analysis has been conducted using RStudio Version 2023.03.0+386 #

# SETUP ------------------------------------------------------------------------

# Load packages
library(rgdal)
library(sf)
library(tmap)
library(ggplot2)
library(gridExtra)

# Load constituency shapefiles
g17 <- readOGR('~/Dissertation data analysis/Data/Shapefiles/const17 shapefile', 'Geometrie_Wahlkreise_19DBT_VG250_geo')
g21 <- readOGR('~/Dissertation data analysis/Data/Shapefiles/const21 shapefile', 'Geometrie_Wahlkreise_20DBT_VG250_geo')

# Clean environment
rm(list = setdiff(ls(), c('d', 'd_raw', 'g17', 'g21')))

# FIGURE 1: TREATMENT AND CONTROL CONSTITUENCIES -------------------------------

### 2017 treatment
# Define treatment assignment
g17$group <- ifelse(g17$WKR_NR %in% c(156, 157, 158), 'Treatment',
                     ifelse(g17$WKR_NR %in% c(24, 39, 97, 122, 126, 127, 139, 150, 165, 204, 234, 235), 'Excluded', 'Control'))

# Convert to sf object
g17_sf <- st_as_sf(g17)

# Plot 2017 treatment assignment
map17 <- ggplot(g17_sf) +
  geom_sf(aes(fill = group)) +
  scale_fill_manual(values = c('Treatment' = 'deepskyblue3', 'Control' = 'snow3', 'Excluded' = 'white')) +
  labs(fill = 'Treatment assignment 2017') +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

### 2021 
# Define 2021 treatment assignment
g21$group <- ifelse(g21$WKR_NR %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 'Treatment',
                    ifelse(g21$WKR_NR %in% c(24, 34, 53, 140, 156:158, 223, 251), 'Excluded', 'Control'))

# Convert to sf object
g21_sf <- st_as_sf(g21)

# Plot 2021 treatment assignment
map21 <- ggplot(g21_sf) +
  geom_sf(aes(fill = group)) +
  scale_fill_manual(values = c('Treatment' = 'deepskyblue3', 'Control' = 'snow3', 'Excluded' = 'white')) +
  labs(fill = 'Treatment assignment 2021') +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

### Plot together and save
fig1 <- grid.arrange(map17, map21, ncol = 2)

# save the plots as png
ggsave('fig1.png', fig1, width = 10, height = 5, dpi = 300)

# FIGURE 2: PARALLEL TRENDS ASSUMPTION MAINSTREAM VOTERS -----------------------

## Parallel trends assumption for political trust (H0)
# Create testing dataset
d_plac_pt <- d

# Eliminate respondents with non-responses in constituency variable
d_plac_pt <- d_plac_pt[d_plac_pt$const21 > 0, ]

# Eliminate respondents from constituencies where AfD did not field candidates
d_plac_pt <- d_plac_pt %>% filter(!(const21 %in% c(24, 34, 53, 140, 223, 251)))

# Eliminate respondents from pre-treated constituencies
d_plac_pt <- d_plac_pt %>% filter(!(const21 %in% c(156, 157, 158)))

# Subset to waves of interest
d_plac_pt <- d_plac_pt %>% filter((wave %in% c(2, 8, 9, 10, 12, 14, 15, 16, 20)))

# Create treatment variable 
d_plac_pt$treat21 <- ifelse(d_plac_pt$const21 %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 1, 0)

# Compute average political trust among the treated group (treated in 2021)
pt_tr <- c(mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 2 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 8 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 9 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 10 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 12 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 14 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 15 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 16 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 20 & d_plac_pt$treat21 == 1], na.rm = TRUE))

pt_ct <- c(mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 2 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 8 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 9 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 10 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 12 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 14 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 15 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 16 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 20 & d_plac_pt$treat21 == 0], na.rm = TRUE))

means_pt <- data.frame(pt_tr, pt_ct, period = c(-7, -6, -5, -4, -3, -2, -1, 0, 1))

# Plot parallel trends for political trust
pt_all <- ggplot(means_pt, aes(x = period)) +
  geom_point(aes(y = pt_tr, color = 'Treatment')) +
  geom_point(aes(y = pt_ct, color = 'Control')) +
  geom_line(aes(y = pt_tr, color = 'Treatment')) +
  geom_line(aes(y = pt_ct, color = 'Control')) +
  labs(color = 'Group') +
  xlab('Period') +
  ylab('Political trust') +
  theme(axis.text.x = element_text(size = 6),  axis.text.y = element_text(size = 6), panel.background = element_rect(fill = 'gray96')) +
  scale_color_manual(values = c('Treatment' = 'grey20', 'Control' = 'grey75')) +
  scale_x_discrete(limits = unique(means_pt$period)) +
  scale_y_continuous(limits = c(1, 5))

## Parallel trends assumption for satisfaction with democracy (H0)
# Create testing dataset
d_plac_swd <- d

# Eliminate respondents with non-responses in constituency variable
d_plac_swd <- d_plac_swd[d_plac_swd$const21 > 0, ]

# Eliminate respondents from constituencies where AfD did not field candidates
d_plac_swd <- d_plac_swd %>% filter(!(const21 %in% c(24, 34, 53, 140, 223, 251)))

# Eliminate respondents from pre-treated constituencies
d_plac_swd <- d_plac_swd %>% filter(!(const21 %in% c(156, 157, 158)))

# Subset to waves of interest
d_plac_swd <- d_plac_swd %>% filter((wave %in% c(5, 8, 9, 10, 12, 14, 15, 16, 20)))

# Create treatment variable 
d_plac_swd$treat21 <- ifelse(d_plac_swd$const21 %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 1, 0)

# Compute average satisfaction with democracy among the treated group (treated in 2021)
swd_tr <- c(mean(d_plac_swd$swd[d_plac_swd$wave == 5 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 8 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 9 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 10 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 12 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 14 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 15 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 16 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 20 & d_plac_swd$treat21 == 1], na.rm = TRUE))

swd_ct <- c(mean(d_plac_swd$swd[d_plac_swd$wave == 5 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 8 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 9 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 10 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 12 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 14 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 15 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 16 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 20 & d_plac_swd$treat21 == 0], na.rm = TRUE))

means_swd <- data.frame(swd_tr, swd_ct, period = c(-7, -6, -5, -4, -3, -2, -1, 0, 1))

# Plot parallel trends for satisfaction with democracy
swd_all <- ggplot(means_swd, aes(x = period)) +
  geom_point(aes(y = swd_tr, color = 'Treatment')) +
  geom_point(aes(y = swd_ct, color = 'Control')) +
  geom_line(aes(y = swd_tr, color = 'Treatment')) +
  geom_line(aes(y = swd_ct, color = 'Control')) +
  labs(color = 'Group') +
  xlab('Period') +
  ylab('Satisfaction with democracy') +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6), panel.background = element_rect(fill = 'gray96')) +
  scale_color_manual(values = c('Treatment' = 'grey20', 'Control' = 'grey75')) +
  scale_x_discrete(limits = unique(means_swd$period)) +
  scale_y_continuous(limits = c(1, 5))

# Plot side by side
fig2 <- grid.arrange(pt_all, swd_all, ncol = 2)
ggsave('fig2.png', fig2, width = 10, height = 5, dpi = 300)

# FIGURE 3:  PARALLEL TRENDS ASSUMPTION RADICAL RIGHT VOTERS -------------------

## Parallel trends assumption for political trust (H1)
# Create testing dataset
d_plac_pt <- d

# Eliminate respondents with non-responses in constituency variable
d_plac_pt <- d_plac_pt[d_plac_pt$const21 > 0, ]

# Eliminate respondents from constituencies where AfD did not field candidates
d_plac_pt <- d_plac_pt %>% filter(!(const21 %in% c(24, 34, 53, 140, 223, 251)))

# Eliminate respondents from pre-treated constituencies
d_plac_pt <- d_plac_pt %>% filter(!(const21 %in% c(156, 157, 158)))

# Subset to AfD respondents
d_plac_pt <- d_plac_pt %>% 
  group_by(lfdn) %>% 
  filter(any(first_vote_a == 322 | first_vote_b == 322)) %>% 
  ungroup()

# Subset to waves of interest
d_plac_pt <- d_plac_pt %>% filter((wave %in% c(2, 8, 9, 10, 12, 14, 15, 16, 20)))

# Create treatment variable 
d_plac_pt$treat21 <- ifelse(d_plac_pt$const21 %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 1, 0)

# Compute average political trust among the treated group (treated in 2021)
pt_tr <- c(mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 2 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 8 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 9 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 10 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 12 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 14 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 15 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 16 & d_plac_pt$treat21 == 1], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 20 & d_plac_pt$treat21 == 1], na.rm = TRUE))

pt_ct <- c(mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 2 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 8 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 9 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 10 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 12 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 14 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 15 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 16 & d_plac_pt$treat21 == 0], na.rm = TRUE),
           mean(d_plac_pt$bundestag_trust[d_plac_pt$wave == 20 & d_plac_pt$treat21 == 0], na.rm = TRUE))

means_pt <- data.frame(pt_tr, pt_ct, period = c(-7, -6, -5, -4, -3, -2, -1, 0, 1))

# Plot parallel trends for political trust
pt_all <- ggplot(means_pt, aes(x = period)) +
  geom_point(aes(y = pt_tr, color = 'Treatment')) +
  geom_point(aes(y = pt_ct, color = 'Control')) +
  geom_line(aes(y = pt_tr, color = 'Treatment')) +
  geom_line(aes(y = pt_ct, color = 'Control')) +
  labs(color = 'Group') +
  xlab('Period') +
  ylab('Political trust') +
  theme(axis.text.x = element_text(size = 6),  axis.text.y = element_text(size = 6), panel.background = element_rect(fill = 'gray96')) +
  scale_color_manual(values = c('Treatment' = 'grey20', 'Control' = 'grey75')) +
  scale_x_discrete(limits = unique(means_pt$period)) +
  scale_y_continuous(limits = c(1, 5))

## Parallel trends assumption for satisfaction with democracy (H1)
# Create testing dataset
d_plac_swd <- d

# Eliminate respondents with non-responses in constituency variable
d_plac_swd <- d_plac_swd[d_plac_swd$const21 > 0, ]

# Eliminate respondents from constituencies where AfD did not field candidates
d_plac_swd <- d_plac_swd %>% filter(!(const21 %in% c(24, 34, 53, 140, 223, 251)))

# Eliminate respondents from pre-treated constituencies
d_plac_swd <- d_plac_swd %>% filter(!(const21 %in% c(156, 157, 158)))

# Subset to AfD respondents
d_plac_swd <- d_plac_swd %>% 
  group_by(lfdn) %>% 
  filter(any(first_vote_a == 322 | first_vote_b == 322)) %>% 
  ungroup()

# Subset to waves of interest
d_plac_swd <- d_plac_swd %>% filter((wave %in% c(5, 8, 9, 10, 12, 14, 15, 16, 20)))

# Create treatment variable 
d_plac_swd$treat21 <- ifelse(d_plac_swd$const21 %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 1, 0)

# Compute average satisfaction with democracy among the treated group (treated in 2021)
swd_tr <- c(mean(d_plac_swd$swd[d_plac_swd$wave == 5 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 8 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 9 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 10 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 12 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 14 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 15 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 16 & d_plac_swd$treat21 == 1], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 20 & d_plac_swd$treat21 == 1], na.rm = TRUE))

swd_ct <- c(mean(d_plac_swd$swd[d_plac_swd$wave == 5 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 8 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 9 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 10 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 12 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 14 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 15 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 16 & d_plac_swd$treat21 == 0], na.rm = TRUE),
            mean(d_plac_swd$swd[d_plac_swd$wave == 20 & d_plac_swd$treat21 == 0], na.rm = TRUE))

means_swd <- data.frame(swd_tr, swd_ct, period = c(-7, -6, -5, -4, -3, -2, -1, 0, 1))

# Plot parallel trends for satisfaction with democracy
swd_all <- ggplot(means_swd, aes(x = period)) +
  geom_point(aes(y = swd_tr, color = 'Treatment')) +
  geom_point(aes(y = swd_ct, color = 'Control')) +
  geom_line(aes(y = swd_tr, color = 'Treatment')) +
  geom_line(aes(y = swd_ct, color = 'Control')) +
  labs(color = 'Group') +
  xlab('Period') +
  ylab('Satisfaction with democracy') +
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6), panel.background = element_rect(fill = 'gray96')) +
  scale_color_manual(values = c('Treatment' = 'grey20', 'Control' = 'grey75')) +
  scale_x_discrete(limits = unique(means_swd$period)) +
  scale_y_continuous(limits = c(1, 5))

# Plot side by side
fig3 <- grid.arrange(pt_all, swd_all, ncol = 2)
ggsave('fig3.png', fig3, width = 10, height = 5, dpi = 300)