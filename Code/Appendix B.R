### APPENDIX A - Descriptive statistics ###
# Not so Radical Effects: Estimating the Causal Impact of Radical Right Representation on Political Support #
# The following analysis has been conducted using RStudio Version 2023.03.0+386 #

### ENVIRONMENT SETUP ----------------------------------------------------------

# Set working directory
setwd('~/Dissertation data analysis')
getwd()

# Load packages
library(dplyr)
library(ggplot2)
library(gridExtra)

### DISTRIBUTION OF POLITICAL SUPPORT COMPONENTS, MAINSTREAM VOTERS (FIG B1) ------

# Political trust among the mainstream electorate, 2017
d17_pt <- d17[d17$wave == c(2, 8),]

pt17_all <- ggplot(d17_pt, aes(x = bundestag_trust)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Political trust', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))


# Satisfaction with democracy among the mainstream electorate, 2017
d17_swd <- d17[d17$wave == c(5, 8),]

swd17_all <- ggplot(d17_swd, aes(x = swd)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Satisfaction with democracy', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Political trust among the mainstream electorate, 2021
pt21_all <- ggplot(d21, aes(x = bundestag_trust)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Political trust', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Satisfaction with democracy among the mainstream electorate, 2017
swd21_all <- ggplot(d21, aes(x = swd)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Satisfaction with democracy', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Save figure
figb1 <- grid.arrange(pt17_all, pt21_all, swd17_all, swd21_all, ncol = 2)
ggsave('figb1.png', figb1, width = 10, height = 5, dpi = 300)


### DISTRIBUTION OF POLITICAL SUPPORT COMPONENTS, RADICAL RIGHT VOTERS (FIG B2) -------

# Political trust among the radical right electorate, 2017
afd17_pt <- afd17[afd17$wave == c(2, 8),]

pt17_afd <- ggplot(afd17_pt, aes(x = bundestag_trust)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Political trust', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))


# Satisfaction with democracy among the radical right electorate, 2017
afd17_swd <- afd17[afd17$wave == c(5, 8),]

swd17_afd <- ggplot(afd17_swd, aes(x = swd)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Satisfaction with democracy', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Political trust among the radical right electorate, 2021
pt21_afd <- ggplot(afd21, aes(x = bundestag_trust)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Political trust', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Satisfaction with democracy among the mainstream electorate, 2017
swd21_afd <- ggplot(afd21, aes(x = swd)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25, fill = 'grey20') +
  scale_x_continuous(breaks = seq(1, 5, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.25)) +
  labs(x = 'Satisfaction with democracy', y = 'Percentage of respondents') +
  theme(axis.text = element_text(size = 6),
        axis.title = element_text(size = 8)) +
  facet_wrap(~ wave, ncol = 2,  labeller = labeller(wave = function(x) paste0('Wave ', x)))

# Save figure
figb2 <- grid.arrange(pt17_afd, pt21_afd, swd17_afd, swd21_afd, ncol = 2)
ggsave('figb2.png', figb2, width = 10, height = 5, dpi = 300)
### MEAN AND STANDARD DEVIATION OF POLITCAL SUPPORT (TAB B1) -------------------
# Mean and standard deviation among mainstream voters
aggregate(d17$bundestag_trust, list(d17$wave), function(x) c(mean = mean(x), sd = sd(x)))
aggregate(d21$bundestag_trust, list(d21$wave), function(x) c(mean = mean(x), sd = sd(x)))

aggregate(d17$swd, list(d17$wave), function(x) c(mean = mean(x), sd = sd(x)))
aggregate(d21$swd, list(d21$wave), function(x) c(mean = mean(x), sd = sd(x)))

# Mean and standard deviation among radical right voters
aggregate(afd17$bundestag_trust, list(afd17$wave), function(x) c(mean = mean(x), sd = sd(x)))
aggregate(afd21$bundestag_trust, list(afd21$wave), function(x) c(mean = mean(x), sd = sd(x)))

aggregate(afd17$swd, list(afd17$wave), function(x) c(mean = mean(x), sd = sd(x)))
aggregate(afd21$swd, list(afd21$wave), function(x) c(mean = mean(x), sd = sd(x)))


