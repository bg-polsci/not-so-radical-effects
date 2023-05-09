### APPENDIX D - Attrition analysis and within-year baalnced panel robustness check ###
# Not so Radical Effects: Estimating the Causal Impact of Radical Right Representation on Political Support #
# The following analysis has been conducted using RStudio Version 2023.03.0+386 #

### ENVIRONMENT SETUP ----------------------------------------------------------
library(dplyr)

### DATA CLEANING AND TIDYING  -------------------------------------------------
# Remove duplicate variables
d <- d_raw[, !duplicated(names(d_raw))]

# Create new variable with re-arranged string
d$wave_participation <- ifelse(grepl('a1|a2', d$p_participation.y), {
  temp <- gsub('a1-?', '', d$p_participation.y)
  temp <- gsub('a2-?', '', temp)
  paste0('a2-a1-', temp, sep = '')
}, d$p_participation.y)

# Create wave variable for additional wave a2-a1
d$wavea2a1 <- ifelse(grepl('a2-a1-', d$wave_participation), 1, 0)

# Remove string 'a2-a1'
d$wave_participation <- sub('a2-a1-', '', d$wave_participation)

# Create wave variables
d <- d %>% separate(wave_participation, into = paste0('wave', 1:15), sep = '-')

# Subset to respondents in wave of interest (pre- and post-2017 and 2021 elections)
d <- d %>% filter((wave2 == 1 & wave5 == 1) | wave8 == 1 | wave16 == 1 | wave20 == 1) # imbalanced panel, 16634 respondents

# Subset to variables of interest
d <- subset(d, select = c('lfdn', 'newest_elecdist17_', 'newest_elecdist21_', 'wave1', 'wave2', 'wave3', 'wave4', 'wave5', 'wave6', 'wave7', 'wave8', 'wave9', 'wave10', 'wave12', 'wave14', 'wave15', 'wave16', 'wave20', 'wave21',
                          
                          # Background covariates                         
                          'kp1_2601', 'kpx_2290s', 'kpx_2280', 'kp1_2320', 'kp1_2591', 'kp1_2480', 'kp1_2600', 'kpx_2520', 'kp1_110',
                          
                          # Time-varying covariates 
                          'kp16_4045a', 'kp16_4045b', 'kp16_4045c', 'kp16_4045d', 'kp20_4045a', 'kp20_4045b', 'kp20_4045c', 'kp20_4045d', 'kp21_4045a', 'kp21_4045b', 'kp21_4045c', 'kp21_4045d', 
                          
                          # Outcome variables - satisfaction with democracy                                  
                          'kp5_020', 'kp8_020', 'kp9_020', 'kp10_020', 'kp12_020', 'kp14_020', 'kp15_020', 'kp16_020', 'kp20_020', 'kp21_020',
                          
                          # Outcome variable - political trust
                          'kp2_160a', 'kp2_160b', 'kp2_160j', 'kp2_160q', 'kp8_160a', 'kp8_160b', 'kp8_160j', 'kp8_160q', 'kp9_160a', 'kp9_160b', 'kp9_160j', 'kp9_160q', 'kp10_160a', 'kp10_160b', 'kp10_160j', 'kp10_160q', 'kp12_160a', 'kp12_160b', 'kp12_160j', 'kp12_160q', 'kp14_160a', 'kp14_160b', 'kp14_160j', 'kp14_160q', 'kp15_160a', 'kp15_160b', 'kp15_160j', 'kp15_160q', 'kp16_160a', 'kp16_160b', 'kp16_160j', 'kp16_160q', 'kp20_160a', 'kp20_160b', 'kp20_160j', 'kp20_160q', 'kp21_160a', 'kp21_160b', 'kp21_160j', 'kp21_160q',
                          
                          # Explanatory variable
                          'kp8_191aa', 'kp8_191ab', 'kp8_191ba', 'kp8_191bb', 'kp8_200aa', 'kp8_200ab', 'kp8_200ba', 'kp8_200bb', 'kp20_191aa', 'kp20_191ab', 'kp20_191ba', 'kp20_191bb', 'kp20_200aa', 'kp20_200ab', 'kp20_200ba', 'kp20_200bb'))

# Rename
colnames(d) <- c('lfdn', 'const17', 'const21', 'w_1', 'w_2', 'w_3', 'w_4', 'w_5', 'w_6', 'w_7', 'w_8', 'w_9', 'w_10', 'w_12', 'w_14', 'w_15', 'w_16', 'w_20', 'w_21',
                 
                 # Background covariates
                 'land', 'year_birth', 'gender', 'education', 'household_income', 'religion', 'urban', 'german_citizen', 'pol_know',
                 
                 # Time-varying covariates
                 'w16_care_covid', 'w16_lock_covid', 'w16_econ_covid', 'w16_comm_covid', 'w20_care_covid', 'w20_lock_covid', 'w20_econ_covid', 'w20_comm_covid', 'w21_care_covid', 'w21_lock_covid', 'w21_econ_covid', 'w20_comm_covid', 
                 
                 # Outcome variables - satisfaction with democracy                                 
                 'w5_swd', 'w8_swd', 'w9_swd', 'w10_swd', 'w12_swd', 'w14_swd', 'w15_swd', 'w16_swd', 'w20_swd', 'w21_swd',
                 
                 # Outcome variable - political trust
                 'w2_bundestag_trust', 'w2_fcc_trust', 'w2_army_trust', 'w2_police_trust', 'w8_bundestag_trust', 'w8_fcc_trust', 'w8_army_trust', 'w8_police_trust', 'w9_bundestag_trust', 'w9_fcc_trust', 'w9_army_trust', 'w9_police_trust', 'w10_bundestag_trust', 'w10_fcc_trust', 'w10_army_trust', 'w10_police_trust', 'w12_bundestag_trust', 'w12_fcc_trust', 'w12_army_trust', 'w12_police_trust', 'w14_bundestag_trust', 'w14_fcc_trust', 'w14_army_trust', 'w14_police_trust', 'w15_bundestag_trust', 'w15_fcc_trust', 'w15_army_trust', 'w15_police_trust', 'w16_bundestag_trust', 'w16_fcc_trust', 'w16_army_trust', 'w16_police_trust', 'w20_bundestag_trust', 'w20_fcc_trust', 'w20_army_trust', 'w20_police_trust', 'w21_bundestag_trust', 'w21_fcc_trust', 'w21_army_trust', 'w21_police_trust',
                 
                 # Vote variable
                 'w8_mail_first_vote_a', 'w8_mail_first_vote_b', 'w8_mail_second_vote_a', 'w8_mail_second_vote_b', 'w8_first_vote_a', 'w8_first_vote_b', 'w8_second_vote_a', 'w8_second_vote_b', 'w20_mail_first_vote_a', 'w20_mail_first_vote_b', 'w20_mail_second_vote_a', 'w20_mail_second_vote_b', 'w20_first_vote_a', 'w20_first_vote_b', 'w20_second_vote_a', 'w20_second_vote_b')

#### Pivot to long-format panel
# Variable - wave
d_wave <- subset(d, select = c('lfdn', 'w_1', 'w_2', 'w_3', 'w_4', 'w_5', 'w_6', 'w_7', 'w_8', 'w_9', 'w_10', 'w_12', 'w_14', 'w_15', 'w_16', 'w_20', 'w_21'))
d_wave <- d_wave %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w_(.*)', values_to = 'responded', values_drop_na = FALSE)

# Variable - swd
d$w1_swd <- NA
d$w2_swd <- NA
d$w3_swd <- NA
d$w4_swd <- NA
d$w6_swd <- NA
d$w7_swd <- NA
d_swd <- subset(d, select = c('lfdn', 'w1_swd', 'w2_swd', 'w3_swd', 'w4_swd', 'w5_swd', 'w6_swd', 'w7_swd', 'w8_swd', 'w9_swd', 'w10_swd', 'w12_swd', 'w14_swd', 'w15_swd', 'w16_swd', 'w20_swd', 'w21_swd'))
d_swd <- d_swd %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_swd', values_to = 'swd', values_drop_na = FALSE)

# Variable - bundestag trust
d$w1_bundestag_trust <- NA
d$w3_bundestag_trust <- NA
d$w4_bundestag_trust <- NA
d$w5_bundestag_trust <- NA
d$w6_bundestag_trust <- NA
d$w7_bundestag_trust <- NA
d_bundestag_trust <- subset(d, select = c('lfdn', 'w1_bundestag_trust', 'w2_bundestag_trust', 'w3_bundestag_trust', 'w4_bundestag_trust', 'w5_bundestag_trust', 'w6_bundestag_trust', 'w7_bundestag_trust', 'w8_bundestag_trust', 'w9_bundestag_trust', 'w10_bundestag_trust', 'w12_bundestag_trust', 'w14_bundestag_trust', 'w15_bundestag_trust', 'w16_bundestag_trust', 'w20_bundestag_trust', 'w21_bundestag_trust'))
d_bundestag_trust <- d_bundestag_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_bundestag_trust', values_to = 'bundestag_trust', values_drop_na = FALSE)

# Variable - fcc trust
d$w1_fcc_trust <- NA
d$w3_fcc_trust <- NA
d$w4_fcc_trust <- NA
d$w5_fcc_trust <- NA
d$w6_fcc_trust <- NA
d$w7_fcc_trust <- NA
d_fcc_trust <- subset(d, select = c('lfdn', 'w1_fcc_trust', 'w2_fcc_trust', 'w3_fcc_trust', 'w4_fcc_trust', 'w5_fcc_trust', 'w6_fcc_trust', 'w7_fcc_trust', 'w8_fcc_trust', 'w9_fcc_trust', 'w10_fcc_trust', 'w12_fcc_trust', 'w14_fcc_trust', 'w15_fcc_trust', 'w16_fcc_trust', 'w20_fcc_trust', 'w21_fcc_trust'))
d_fcc_trust <- d_fcc_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_fcc_trust', values_to = 'fcc_trust', values_drop_na = FALSE)

# Variable - army trust
d$w1_army_trust <- NA
d$w3_army_trust <- NA
d$w4_army_trust <- NA
d$w5_army_trust <- NA
d$w6_army_trust <- NA
d$w7_army_trust <- NA
d_army_trust <- subset(d, select = c('lfdn', 'w1_army_trust', 'w2_army_trust', 'w3_army_trust', 'w4_army_trust', 'w5_army_trust', 'w6_army_trust', 'w7_army_trust', 'w8_army_trust', 'w9_army_trust', 'w10_army_trust', 'w12_army_trust', 'w14_army_trust', 'w15_army_trust', 'w16_army_trust', 'w20_army_trust', 'w21_army_trust'))
d_army_trust <- d_army_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_army_trust', values_to = 'army_trust', values_drop_na = FALSE)

# Variable - police trust
d$w1_police_trust <- NA
d$w3_police_trust <- NA
d$w4_police_trust <- NA
d$w5_police_trust <- NA
d$w6_police_trust <- NA
d$w7_police_trust <- NA
d_police_trust <- subset(d, select = c('lfdn', 'w1_police_trust', 'w2_police_trust', 'w3_police_trust', 'w4_police_trust', 'w5_police_trust', 'w6_police_trust', 'w7_police_trust', 'w8_police_trust', 'w9_police_trust', 'w10_police_trust', 'w12_police_trust', 'w14_police_trust', 'w15_police_trust', 'w16_police_trust', 'w20_police_trust', 'w21_police_trust'))
d_police_trust <- d_police_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_police_trust', values_to = 'police_trust', values_drop_na = FALSE)

# Variable - medical care (covid19)
d$w1_care_covid <- NA
d$w2_care_covid <- NA
d$w3_care_covid <- NA
d$w4_care_covid <- NA
d$w5_care_covid <- NA
d$w6_care_covid <- NA
d$w7_care_covid <- NA
d$w8_care_covid <- NA
d$w9_care_covid <- NA
d$w10_care_covid <- NA
d$w12_care_covid <- NA
d$w14_care_covid <- NA
d$w15_care_covid <- NA
d$w21_care_covid <- NA
d_care_covid <- subset(d, select = c('lfdn', 'w1_care_covid', 'w2_care_covid', 'w3_care_covid', 'w4_care_covid', 'w5_care_covid', 'w6_care_covid', 'w7_care_covid', 'w8_care_covid', 'w9_care_covid', 'w10_care_covid', 'w12_care_covid', 'w14_care_covid', 'w15_care_covid', 'w16_care_covid', 'w20_care_covid', 'w21_care_covid'))
d_care_covid <- d_care_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_care_covid', values_to = 'care_covid', values_drop_na = FALSE)

# Variable - public closures (covid19)
d$w1_lock_covid <- NA
d$w2_lock_covid <- NA
d$w3_lock_covid <- NA
d$w4_lock_covid <- NA
d$w5_lock_covid <- NA
d$w6_lock_covid <- NA
d$w7_lock_covid <- NA
d$w8_lock_covid <- NA
d$w9_lock_covid <- NA
d$w10_lock_covid <- NA
d$w12_lock_covid <- NA
d$w14_lock_covid <- NA
d$w15_lock_covid <- NA
d$w21_lock_covid <- NA
d_lock_covid <- subset(d, select = c('lfdn', 'w1_lock_covid', 'w2_lock_covid', 'w3_lock_covid', 'w4_lock_covid', 'w5_lock_covid', 'w6_lock_covid', 'w7_lock_covid', 'w8_lock_covid', 'w9_lock_covid', 'w10_lock_covid', 'w12_lock_covid', 'w14_lock_covid', 'w15_lock_covid', 'w16_lock_covid', 'w20_lock_covid', 'w21_lock_covid'))
d_lock_covid <- d_lock_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_lock_covid', values_to = 'lock_covid', values_drop_na = FALSE)

# Variable - protection economy (covid19)
d$w1_econ_covid <- NA
d$w2_econ_covid <- NA
d$w3_econ_covid <- NA
d$w4_econ_covid <- NA
d$w5_econ_covid <- NA
d$w6_econ_covid <- NA
d$w7_econ_covid <- NA
d$w8_econ_covid <- NA
d$w9_econ_covid <- NA
d$w10_econ_covid <- NA
d$w12_econ_covid <- NA
d$w14_econ_covid <- NA
d$w15_econ_covid <- NA
d$w21_econ_covid <- NA
d_econ_covid <- subset(d, select = c('lfdn', 'w1_econ_covid', 'w2_econ_covid', 'w3_econ_covid', 'w4_econ_covid', 'w5_econ_covid', 'w6_econ_covid', 'w7_econ_covid', 'w8_econ_covid', 'w9_econ_covid', 'w10_econ_covid', 'w12_econ_covid', 'w14_econ_covid', 'w15_econ_covid', 'w16_econ_covid', 'w20_econ_covid', 'w21_econ_covid'))
d_econ_covid <- d_econ_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_econ_covid', values_to = 'econ_covid', values_drop_na = FALSE)

# Variable - public communication (covid19)
d$w1_comm_covid <- NA
d$w2_comm_covid <- NA
d$w3_comm_covid <- NA
d$w4_comm_covid <- NA
d$w5_comm_covid <- NA
d$w6_comm_covid <- NA
d$w7_comm_covid <- NA
d$w8_comm_covid <- NA
d$w9_comm_covid <- NA
d$w10_comm_covid <- NA
d$w12_comm_covid <- NA
d$w14_comm_covid <- NA
d$w15_comm_covid <- NA
d$w21_comm_covid <- NA
d_comm_covid <- subset(d, select = c('lfdn', 'w1_comm_covid', 'w2_comm_covid', 'w3_comm_covid', 'w4_comm_covid', 'w5_comm_covid', 'w6_comm_covid', 'w7_comm_covid', 'w8_comm_covid', 'w9_comm_covid', 'w10_comm_covid', 'w12_comm_covid', 'w14_comm_covid', 'w15_comm_covid', 'w16_comm_covid', 'w20_comm_covid', 'w21_comm_covid'))
d_comm_covid <- d_comm_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_comm_covid', values_to = 'comm_covid', values_transform = list(comm_covid = ~replace_na(.)))

# Variable - first vote (version a)
d$w8_first_vote_a <- ifelse(d$w8_first_vote_a == -97, d$w8_mail_first_vote_a, d$w8_first_vote_a)
d$w20_first_vote_a <- ifelse(d$w20_first_vote_a == -97, d$w20_mail_first_vote_a, d$w20_first_vote_a)
d$w1_first_vote_a <- NA
d$w2_first_vote_a <- NA
d$w3_first_vote_a <- NA
d$w4_first_vote_a <- NA
d$w5_first_vote_a <- NA
d$w6_first_vote_a <- NA
d$w7_first_vote_a <- NA
d$w9_first_vote_a <- NA
d$w10_first_vote_a <- NA
d$w12_first_vote_a <- NA
d$w14_first_vote_a <- NA
d$w15_first_vote_a <- NA
d$w16_first_vote_a <- NA
d$w21_first_vote_a <- NA
d_first_vote_a <- subset(d, select = c('lfdn', 'w1_first_vote_a', 'w2_first_vote_a', 'w3_first_vote_a', 'w4_first_vote_a', 'w5_first_vote_a', 'w6_first_vote_a', 'w7_first_vote_a', 'w8_first_vote_a', 'w9_first_vote_a', 'w10_first_vote_a', 'w12_first_vote_a', 'w14_first_vote_a', 'w15_first_vote_a', 'w16_first_vote_a', 'w20_first_vote_a', 'w21_first_vote_a'))
d_first_vote_a <- d_first_vote_a %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_first_vote_a', values_to = 'first_vote_a', values_drop_na = FALSE)

# Variable - first vote (version b)
d$w8_first_vote_b <- ifelse(d$w8_first_vote_b == -97, d$w8_mail_first_vote_b, d$w8_first_vote_b)
d$w20_first_vote_b <- ifelse(d$w20_first_vote_b == -97, d$w20_mail_first_vote_b, d$w20_first_vote_b)
d$w1_first_vote_b <- NA
d$w2_first_vote_b <- NA
d$w3_first_vote_b <- NA
d$w4_first_vote_b <- NA
d$w5_first_vote_b <- NA
d$w6_first_vote_b <- NA
d$w7_first_vote_b <- NA
d$w9_first_vote_b <- NA
d$w10_first_vote_b <- NA
d$w12_first_vote_b <- NA
d$w14_first_vote_b <- NA
d$w15_first_vote_b <- NA
d$w16_first_vote_b <- NA
d$w21_first_vote_b <- NA
d_first_vote_b <- subset(d, select = c('lfdn', 'w1_first_vote_b', 'w2_first_vote_b', 'w3_first_vote_b', 'w4_first_vote_b', 'w5_first_vote_b', 'w6_first_vote_b', 'w7_first_vote_b', 'w8_first_vote_b', 'w9_first_vote_b', 'w10_first_vote_b', 'w12_first_vote_b', 'w14_first_vote_b', 'w15_first_vote_b', 'w16_first_vote_b', 'w20_first_vote_b', 'w21_first_vote_b'))
d_first_vote_b <- d_first_vote_b %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_first_vote_b', values_to = 'first_vote_b', values_drop_na = FALSE)

# Variable - second vote (version a)
d$w8_second_vote_a <- ifelse(d$w8_second_vote_a == -97, d$w8_mail_second_vote_a, d$w8_second_vote_a)
d$w20_second_vote_a <- ifelse(d$w20_second_vote_a == -97, d$w20_mail_second_vote_a, d$w20_second_vote_a)
d$w1_second_vote_a <- NA
d$w2_second_vote_a <- NA
d$w3_second_vote_a <- NA
d$w4_second_vote_a <- NA
d$w5_second_vote_a <- NA
d$w6_second_vote_a <- NA
d$w7_second_vote_a <- NA
d$w9_second_vote_a <- NA
d$w10_second_vote_a <- NA
d$w12_second_vote_a <- NA
d$w14_second_vote_a <- NA
d$w15_second_vote_a <- NA
d$w16_second_vote_a <- NA
d$w21_second_vote_a <- NA
d_second_vote_a <- subset(d, select = c('lfdn', 'w1_second_vote_a', 'w2_second_vote_a', 'w3_second_vote_a', 'w4_second_vote_a', 'w5_second_vote_a', 'w6_second_vote_a', 'w7_second_vote_a', 'w8_second_vote_a', 'w9_second_vote_a', 'w10_second_vote_a', 'w12_second_vote_a', 'w14_second_vote_a', 'w15_second_vote_a', 'w16_second_vote_a', 'w20_second_vote_a', 'w21_second_vote_a'))
d_second_vote_a <- d_second_vote_a %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_second_vote_a', values_to = 'second_vote_a', values_drop_na = FALSE)

# Variable - second vote (version b)
d$w8_second_vote_b <- ifelse(d$w8_second_vote_b == -97, d$w8_mail_second_vote_b, d$w8_second_vote_b)
d$w20_second_vote_b <- ifelse(d$w20_second_vote_b == -97, d$w20_mail_second_vote_b, d$w20_second_vote_b)
d$w1_second_vote_b <- NA
d$w2_second_vote_b <- NA
d$w3_second_vote_b <- NA
d$w4_second_vote_b <- NA
d$w5_second_vote_b <- NA
d$w6_second_vote_b <- NA
d$w7_second_vote_b <- NA
d$w9_second_vote_b <- NA
d$w10_second_vote_b <- NA
d$w12_second_vote_b <- NA
d$w14_second_vote_b <- NA
d$w15_second_vote_b <- NA
d$w16_second_vote_b <- NA
d$w21_second_vote_b <- NA
d_second_vote_b <- subset(d, select = c('lfdn', 'w1_second_vote_b', 'w2_second_vote_b', 'w3_second_vote_b', 'w4_second_vote_b', 'w5_second_vote_b', 'w6_second_vote_b', 'w7_second_vote_b', 'w8_second_vote_b', 'w9_second_vote_b', 'w10_second_vote_b', 'w12_second_vote_b', 'w14_second_vote_b', 'w15_second_vote_b', 'w16_second_vote_b', 'w20_second_vote_b', 'w21_second_vote_b'))
d_second_vote_b <- d_second_vote_b %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_second_vote_b', values_to = 'second_vote_b', values_drop_na = FALSE)

# All other variables
d_other <- subset(d, select = c('lfdn', 'const17', 'const21', 'land', 'year_birth', 'gender', 'education', 'household_income', 'religion', 'urban', 'german_citizen', 'pol_know'))
d_other <- d_other[rep(seq_len(nrow(d_other)), each = 16), ]

# Merge variables
d <- cbind(d_other, d_wave, d_swd, d_bundestag_trust, d_fcc_trust, d_army_trust, d_police_trust, d_care_covid, d_lock_covid, d_econ_covid, d_comm_covid, d_first_vote_a, d_first_vote_b, d_second_vote_a, d_second_vote_b)

# Eliminate duplicate variables
d <- d[, !duplicated(names(d))]

# Reverse satisfaction with democracy scale to match the scale used for political trust
d$swd <- 6 - d$swd

# Eliminate respondents with non-responses in vote variables  (in the waves of interest)
d <- subset(d, !(first_vote_a < 0 & wave %in% c(8, 20)))
d <- subset(d, !(first_vote_b < 0 & wave %in% c(8, 20)))
d <- subset(d, !(second_vote_a < 0 & wave %in% c(8, 20)))
d <- subset(d, !(second_vote_b < 0 & wave %in% c(8, 20)))

# Impute vote variable to be measured in all core waves
d <- d %>%
  group_by(lfdn) %>%
  mutate(first_vote_a = ifelse(wave == 8, first_vote_a,
                               ifelse(wave %in% c(2, 5), first_vote_a[wave == 8],
                                      ifelse(wave == 20, first_vote_a,
                                             ifelse(wave == 16, first_vote_a[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(first_vote_b = ifelse(wave == 8, first_vote_b,
                               ifelse(wave %in% c(2, 5), first_vote_b[wave == 8],
                                      ifelse(wave == 20, first_vote_b,
                                             ifelse(wave == 16, first_vote_b[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(second_vote_a = ifelse(wave == 8, second_vote_a,
                                ifelse(wave %in% c(2, 5), second_vote_a[wave == 8],
                                       ifelse(wave == 20, second_vote_a,
                                              ifelse(wave == 16, second_vote_a[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(second_vote_b = ifelse(wave == 8, second_vote_b,
                                ifelse(wave %in% c(2, 5), second_vote_b[wave == 8],
                                       ifelse(wave == 20, second_vote_b,
                                              ifelse(wave == 16, second_vote_b[wave == 20], NA)))))

# Recode age variable
d$year_birth <- ifelse(d$year_birth == '1955 und frueher', '1955', d$year_birth)
d$year_birth <- as.numeric(d$year_birth)
d$year_birth <- cut(d$year_birth, breaks = c(0, 1959, 1969, 1979, 1989, 1999, Inf), labels = c('1959 and earlier', '1960-69', '1970-79', '1980-89', '1990-99', '1999 and later'))

# Recode political knowledge variable
d$pol_know <- ifelse(d$pol_know == -98 | d$pol_know == 1 | d$pol_know == 3, 0,
                     ifelse(d$pol_know == 2, 1, d$pol_know))

# Recode vote variables
d$first_vote_a <- ifelse(d$first_vote_a == -84, 'no vote', d$first_vote_a)
d$first_vote_b <- ifelse(d$first_vote_b == -84, 'no vote', d$first_vote_b)
d$second_vote_a <- ifelse(d$second_vote_a == -84, 'no vote', d$second_vote_a)
d$second_vote_b <- ifelse(d$second_vote_b == -84, 'no vote', d$second_vote_b)

# Recode non-responses
d$education <- ifelse(d$education == -93, 'not asked, terminated', 
                      ifelse(d$education == -95, 'not participated',
                             ifelse(d$education == -99, 'no answer', d$education)))

d$household_income <- ifelse(d$household_income == -93, 'not asked, terminated', 
                             ifelse(d$household_income == -95, 'not participated',
                                    ifelse(d$household_income == -99, 'no answer', d$household_income)))

d$religion <- ifelse(d$religion == -93, 'not asked, terminated', 
                     ifelse(d$religion == -95, 'not participated',
                            ifelse(d$religion == -99, 'no answer', d$religion)))

d$urban <- ifelse(d$urban == -93, 'not asked, terminated', 
                  ifelse(d$urban == -95, 'not participated',
                         ifelse(d$urban == -99, 'no answer', d$urban)))

d$german_citizen <- ifelse(d$german_citizen == -93, 'not asked, terminated', 
                           ifelse(d$german_citizen == -95, 'not participated',
                                  ifelse(d$german_citizen == -99, 'no answer', d$german_citizen)))

d$pol_know <- ifelse(d$pol_know == -93, 'not asked, terminated', 
                     ifelse(d$pol_know == -95, 'not participated',
                            ifelse(d$pol_know == -99, 'no answer', d$pol_know)))

d$first_vote_a <- ifelse(d$first_vote_a == -93, 'not asked, terminated', 
                         ifelse(d$first_vote_a == -95, 'not participated',
                                ifelse(d$first_vote_a == -99, 'no answer', 
                                       ifelse(d$first_vote_a == -97, 'not applicable', d$first_vote_a))))

d$first_vote_b <- ifelse(d$first_vote_b == -93, 'not asked, terminated', 
                         ifelse(d$first_vote_b == -95, 'not participated',
                                ifelse(d$first_vote_b == -99, 'no answer',
                                       ifelse(d$first_vote_b == -97, 'not applicable', d$first_vote_b))))

d$second_vote_a <- ifelse(d$second_vote_a == -93, 'not asked, terminated', 
                          ifelse(d$second_vote_a == -95, 'not participated',
                                 ifelse(d$second_vote_a == -99, 'no answer',
                                        ifelse(d$second_vote_a == -97, 'not applicable', d$second_vote_a))))

d$second_vote_b <- ifelse(d$second_vote_b == -93, 'not asked, terminated', 
                          ifelse(d$second_vote_b == -95, 'not participated',
                                 ifelse(d$second_vote_b == -99, 'no answer',
                                        ifelse(d$second_vote_b == -97, 'not applicable', d$second_vote_b))))

d$first_vote_a <- factor(d$first_vote_a)
d$first_vote_b <- factor(d$first_vote_b)
d$second_vote_a <- factor(d$second_vote_a)
d$second_vote_b <- factor(d$second_vote_b)

# Clean environment
rm(list = setdiff(ls(), c('d', 'd_raw', 'pop_den17', 'pop_den21', 'foreign17', 'foreign21', 'unemp17', 'unemp21')))

# Hot-deck imputation for Land (2021) variable from constituency (2021) variable
d <- hotdeck(d, variable = 'land', domain_var = 'const21')

# Create datasets
d17 <- d[d$wave == 2 | d$wave == 5 | d$wave == 8, ]
d21 <- d[d$wave == 16 | d$wave == 20, ]

### ATTRITION ANALYSIS ---------------------------------------------------------
### 2017 treatment
# Create responded variable
d17$responded <- ifelse((d17$wave == 2 & d17$bundestag_trust == -99) | 
                          (d17$wave == 8 & d17$bundestag_trust == -99) |
                          (d17$wave == 5 & d17$swd == -99) |
                          (d17$wave == 8 & d17$swd == -99), 0, 1)

# Calculate attrition rate
n_distinct(d17$lfdn[d17$responded == 0])/ nrow(d17) # 0.001 % attrition

### 2021 treatment
# Create responded variable
d21$responded <- ifelse((d21$wave == 16 & d21$bundestag_trust == -99) | 
                          (d21$wave == 20 & d21$bundestag_trust == -99) |
                          (d21$wave == 16 & d21$swd == -99) |
                          (d21$wave == 20 & d21$swd == -99), 0, 1)

# Calculate attrition rate
n_distinct(d21$lfdn[d21$responded == 0])/ nrow(d21) # 0.002 % attrition

### SET UP WITHIN-ELECTION BALANCED PANEL --------------------------------------

# Clean environment
rm(list = setdiff(ls(), c('d_raw', 'pop_den17', 'pop_den21', 'foreign17', 'foreign21', 'unemp17', 'unemp21')))

## Clean data
# Remove duplicate variables
d <- d_raw[, !duplicated(names(d_raw))]

# Create new variable with re-arranged string
d$wave_participation <- ifelse(grepl('a1|a2', d$p_participation.y), {
  temp <- gsub('a1-?', '', d$p_participation.y)
  temp <- gsub('a2-?', '', temp)
  paste0('a2-a1-', temp, sep = '')
}, d$p_participation.y)

# Create wave variable for additional wave a2-a1
d$wavea2a1 <- ifelse(grepl('a2-a1-', d$wave_participation), 1, 0)

# Remove string 'a2-a1'
d$wave_participation <- sub('a2-a1-', '', d$wave_participation)

# Create wave variables
d <- d %>% separate(wave_participation, into = paste0('wave', 1:15), sep = '-')

# Subset to respondents in wave of interest (pre- and post-2017 and 2021 elections)
d <- d %>% filter((wave2 == 1 & wave5 == 1 & wave8 == 1) | (wave16 == 1 & wave20 == 1)) # within-year balanced panel, 13598 respondents

# Subset to variables of interest
d <- subset(d, select = c('lfdn', 'newest_elecdist17_', 'newest_elecdist21_', 'wave1', 'wave2', 'wave3', 'wave4', 'wave5', 'wave6', 'wave7', 'wave8', 'wave9', 'wave10', 'wave12', 'wave14', 'wave15', 'wave16', 'wave20', 'wave21',
                          
                          # Background covariates                         
                          'kp1_2601', 'kpx_2290s', 'kpx_2280', 'kp1_2320', 'kp1_2591', 'kp1_2480', 'kp1_2600', 'kpx_2520', 'kp1_110',
                          
                          # Time-varying covariates 
                          'kp16_4045a', 'kp16_4045b', 'kp16_4045c', 'kp16_4045d', 'kp20_4045a', 'kp20_4045b', 'kp20_4045c', 'kp20_4045d', 'kp21_4045a', 'kp21_4045b', 'kp21_4045c', 'kp21_4045d', 
                          
                          # Outcome variables - satisfaction with democracy                                  
                          'kp5_020', 'kp8_020', 'kp9_020', 'kp10_020', 'kp12_020', 'kp14_020', 'kp15_020', 'kp16_020', 'kp20_020', 'kp21_020',
                          
                          # Outcome variable - political trust
                          'kp2_160a', 'kp2_160b', 'kp2_160j', 'kp2_160q', 'kp8_160a', 'kp8_160b', 'kp8_160j', 'kp8_160q', 'kp9_160a', 'kp9_160b', 'kp9_160j', 'kp9_160q', 'kp10_160a', 'kp10_160b', 'kp10_160j', 'kp10_160q', 'kp12_160a', 'kp12_160b', 'kp12_160j', 'kp12_160q', 'kp14_160a', 'kp14_160b', 'kp14_160j', 'kp14_160q', 'kp15_160a', 'kp15_160b', 'kp15_160j', 'kp15_160q', 'kp16_160a', 'kp16_160b', 'kp16_160j', 'kp16_160q', 'kp20_160a', 'kp20_160b', 'kp20_160j', 'kp20_160q', 'kp21_160a', 'kp21_160b', 'kp21_160j', 'kp21_160q',
                          
                          # Explanatory variable
                          'kp8_191aa', 'kp8_191ab', 'kp8_191ba', 'kp8_191bb', 'kp8_200aa', 'kp8_200ab', 'kp8_200ba', 'kp8_200bb', 'kp20_191aa', 'kp20_191ab', 'kp20_191ba', 'kp20_191bb', 'kp20_200aa', 'kp20_200ab', 'kp20_200ba', 'kp20_200bb'))

# Rename
colnames(d) <- c('lfdn', 'const17', 'const21', 'w_1', 'w_2', 'w_3', 'w_4', 'w_5', 'w_6', 'w_7', 'w_8', 'w_9', 'w_10', 'w_12', 'w_14', 'w_15', 'w_16', 'w_20', 'w_21',
                 
                 # Background covariates
                 'land', 'year_birth', 'gender', 'education', 'household_income', 'religion', 'urban', 'german_citizen', 'pol_know',
                 
                 # Time-varying covariates
                 'w16_care_covid', 'w16_lock_covid', 'w16_econ_covid', 'w16_comm_covid', 'w20_care_covid', 'w20_lock_covid', 'w20_econ_covid', 'w20_comm_covid', 'w21_care_covid', 'w21_lock_covid', 'w21_econ_covid', 'w20_comm_covid', 
                 
                 # Outcome variables - satisfaction with democracy                                 
                 'w5_swd', 'w8_swd', 'w9_swd', 'w10_swd', 'w12_swd', 'w14_swd', 'w15_swd', 'w16_swd', 'w20_swd', 'w21_swd',
                 
                 # Outcome variable - political trust
                 'w2_bundestag_trust', 'w2_fcc_trust', 'w2_army_trust', 'w2_police_trust', 'w8_bundestag_trust', 'w8_fcc_trust', 'w8_army_trust', 'w8_police_trust', 'w9_bundestag_trust', 'w9_fcc_trust', 'w9_army_trust', 'w9_police_trust', 'w10_bundestag_trust', 'w10_fcc_trust', 'w10_army_trust', 'w10_police_trust', 'w12_bundestag_trust', 'w12_fcc_trust', 'w12_army_trust', 'w12_police_trust', 'w14_bundestag_trust', 'w14_fcc_trust', 'w14_army_trust', 'w14_police_trust', 'w15_bundestag_trust', 'w15_fcc_trust', 'w15_army_trust', 'w15_police_trust', 'w16_bundestag_trust', 'w16_fcc_trust', 'w16_army_trust', 'w16_police_trust', 'w20_bundestag_trust', 'w20_fcc_trust', 'w20_army_trust', 'w20_police_trust', 'w21_bundestag_trust', 'w21_fcc_trust', 'w21_army_trust', 'w21_police_trust',
                 
                 # Vote variable
                 'w8_mail_first_vote_a', 'w8_mail_first_vote_b', 'w8_mail_second_vote_a', 'w8_mail_second_vote_b', 'w8_first_vote_a', 'w8_first_vote_b', 'w8_second_vote_a', 'w8_second_vote_b', 'w20_mail_first_vote_a', 'w20_mail_first_vote_b', 'w20_mail_second_vote_a', 'w20_mail_second_vote_b', 'w20_first_vote_a', 'w20_first_vote_b', 'w20_second_vote_a', 'w20_second_vote_b')


## Pivot to long-format panel
# Variable - wave
d_wave <- subset(d, select = c('lfdn', 'w_1', 'w_2', 'w_3', 'w_4', 'w_5', 'w_6', 'w_7', 'w_8', 'w_9', 'w_10', 'w_12', 'w_14', 'w_15', 'w_16', 'w_20', 'w_21'))
d_wave <- d_wave %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w_(.*)', values_to = 'responded', values_drop_na = FALSE)

# Variable - swd
d$w1_swd <- NA
d$w2_swd <- NA
d$w3_swd <- NA
d$w4_swd <- NA
d$w6_swd <- NA
d$w7_swd <- NA
d_swd <- subset(d, select = c('lfdn', 'w1_swd', 'w2_swd', 'w3_swd', 'w4_swd', 'w5_swd', 'w6_swd', 'w7_swd', 'w8_swd', 'w9_swd', 'w10_swd', 'w12_swd', 'w14_swd', 'w15_swd', 'w16_swd', 'w20_swd', 'w21_swd'))
d_swd <- d_swd %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_swd', values_to = 'swd', values_drop_na = FALSE)

# Variable - bundestag trust
d$w1_bundestag_trust <- NA
d$w3_bundestag_trust <- NA
d$w4_bundestag_trust <- NA
d$w5_bundestag_trust <- NA
d$w6_bundestag_trust <- NA
d$w7_bundestag_trust <- NA
d_bundestag_trust <- subset(d, select = c('lfdn', 'w1_bundestag_trust', 'w2_bundestag_trust', 'w3_bundestag_trust', 'w4_bundestag_trust', 'w5_bundestag_trust', 'w6_bundestag_trust', 'w7_bundestag_trust', 'w8_bundestag_trust', 'w9_bundestag_trust', 'w10_bundestag_trust', 'w12_bundestag_trust', 'w14_bundestag_trust', 'w15_bundestag_trust', 'w16_bundestag_trust', 'w20_bundestag_trust', 'w21_bundestag_trust'))
d_bundestag_trust <- d_bundestag_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_bundestag_trust', values_to = 'bundestag_trust', values_drop_na = FALSE)

# Variable - fcc trust
d$w1_fcc_trust <- NA
d$w3_fcc_trust <- NA
d$w4_fcc_trust <- NA
d$w5_fcc_trust <- NA
d$w6_fcc_trust <- NA
d$w7_fcc_trust <- NA
d_fcc_trust <- subset(d, select = c('lfdn', 'w1_fcc_trust', 'w2_fcc_trust', 'w3_fcc_trust', 'w4_fcc_trust', 'w5_fcc_trust', 'w6_fcc_trust', 'w7_fcc_trust', 'w8_fcc_trust', 'w9_fcc_trust', 'w10_fcc_trust', 'w12_fcc_trust', 'w14_fcc_trust', 'w15_fcc_trust', 'w16_fcc_trust', 'w20_fcc_trust', 'w21_fcc_trust'))
d_fcc_trust <- d_fcc_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_fcc_trust', values_to = 'fcc_trust', values_drop_na = FALSE)

# Variable - army trust
d$w1_army_trust <- NA
d$w3_army_trust <- NA
d$w4_army_trust <- NA
d$w5_army_trust <- NA
d$w6_army_trust <- NA
d$w7_army_trust <- NA
d_army_trust <- subset(d, select = c('lfdn', 'w1_army_trust', 'w2_army_trust', 'w3_army_trust', 'w4_army_trust', 'w5_army_trust', 'w6_army_trust', 'w7_army_trust', 'w8_army_trust', 'w9_army_trust', 'w10_army_trust', 'w12_army_trust', 'w14_army_trust', 'w15_army_trust', 'w16_army_trust', 'w20_army_trust', 'w21_army_trust'))
d_army_trust <- d_army_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_army_trust', values_to = 'army_trust', values_drop_na = FALSE)

# Variable - police trust
d$w1_police_trust <- NA
d$w3_police_trust <- NA
d$w4_police_trust <- NA
d$w5_police_trust <- NA
d$w6_police_trust <- NA
d$w7_police_trust <- NA
d_police_trust <- subset(d, select = c('lfdn', 'w1_police_trust', 'w2_police_trust', 'w3_police_trust', 'w4_police_trust', 'w5_police_trust', 'w6_police_trust', 'w7_police_trust', 'w8_police_trust', 'w9_police_trust', 'w10_police_trust', 'w12_police_trust', 'w14_police_trust', 'w15_police_trust', 'w16_police_trust', 'w20_police_trust', 'w21_police_trust'))
d_police_trust <- d_police_trust %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_police_trust', values_to = 'police_trust', values_drop_na = FALSE)

# Variable - medical care (covid19)
d$w1_care_covid <- NA
d$w2_care_covid <- NA
d$w3_care_covid <- NA
d$w4_care_covid <- NA
d$w5_care_covid <- NA
d$w6_care_covid <- NA
d$w7_care_covid <- NA
d$w8_care_covid <- NA
d$w9_care_covid <- NA
d$w10_care_covid <- NA
d$w12_care_covid <- NA
d$w14_care_covid <- NA
d$w15_care_covid <- NA
d$w21_care_covid <- NA
d_care_covid <- subset(d, select = c('lfdn', 'w1_care_covid', 'w2_care_covid', 'w3_care_covid', 'w4_care_covid', 'w5_care_covid', 'w6_care_covid', 'w7_care_covid', 'w8_care_covid', 'w9_care_covid', 'w10_care_covid', 'w12_care_covid', 'w14_care_covid', 'w15_care_covid', 'w16_care_covid', 'w20_care_covid', 'w21_care_covid'))
d_care_covid <- d_care_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_care_covid', values_to = 'care_covid', values_drop_na = FALSE)

# Variable - public closures (covid19)
d$w1_lock_covid <- NA
d$w2_lock_covid <- NA
d$w3_lock_covid <- NA
d$w4_lock_covid <- NA
d$w5_lock_covid <- NA
d$w6_lock_covid <- NA
d$w7_lock_covid <- NA
d$w8_lock_covid <- NA
d$w9_lock_covid <- NA
d$w10_lock_covid <- NA
d$w12_lock_covid <- NA
d$w14_lock_covid <- NA
d$w15_lock_covid <- NA
d$w21_lock_covid <- NA
d_lock_covid <- subset(d, select = c('lfdn', 'w1_lock_covid', 'w2_lock_covid', 'w3_lock_covid', 'w4_lock_covid', 'w5_lock_covid', 'w6_lock_covid', 'w7_lock_covid', 'w8_lock_covid', 'w9_lock_covid', 'w10_lock_covid', 'w12_lock_covid', 'w14_lock_covid', 'w15_lock_covid', 'w16_lock_covid', 'w20_lock_covid', 'w21_lock_covid'))
d_lock_covid <- d_lock_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_lock_covid', values_to = 'lock_covid', values_drop_na = FALSE)

# Variable - protection economy (covid19)
d$w1_econ_covid <- NA
d$w2_econ_covid <- NA
d$w3_econ_covid <- NA
d$w4_econ_covid <- NA
d$w5_econ_covid <- NA
d$w6_econ_covid <- NA
d$w7_econ_covid <- NA
d$w8_econ_covid <- NA
d$w9_econ_covid <- NA
d$w10_econ_covid <- NA
d$w12_econ_covid <- NA
d$w14_econ_covid <- NA
d$w15_econ_covid <- NA
d$w21_econ_covid <- NA
d_econ_covid <- subset(d, select = c('lfdn', 'w1_econ_covid', 'w2_econ_covid', 'w3_econ_covid', 'w4_econ_covid', 'w5_econ_covid', 'w6_econ_covid', 'w7_econ_covid', 'w8_econ_covid', 'w9_econ_covid', 'w10_econ_covid', 'w12_econ_covid', 'w14_econ_covid', 'w15_econ_covid', 'w16_econ_covid', 'w20_econ_covid', 'w21_econ_covid'))
d_econ_covid <- d_econ_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_econ_covid', values_to = 'econ_covid', values_drop_na = FALSE)

# Variable - public communication (covid19)
d$w1_comm_covid <- NA
d$w2_comm_covid <- NA
d$w3_comm_covid <- NA
d$w4_comm_covid <- NA
d$w5_comm_covid <- NA
d$w6_comm_covid <- NA
d$w7_comm_covid <- NA
d$w8_comm_covid <- NA
d$w9_comm_covid <- NA
d$w10_comm_covid <- NA
d$w12_comm_covid <- NA
d$w14_comm_covid <- NA
d$w15_comm_covid <- NA
d$w21_comm_covid <- NA
d_comm_covid <- subset(d, select = c('lfdn', 'w1_comm_covid', 'w2_comm_covid', 'w3_comm_covid', 'w4_comm_covid', 'w5_comm_covid', 'w6_comm_covid', 'w7_comm_covid', 'w8_comm_covid', 'w9_comm_covid', 'w10_comm_covid', 'w12_comm_covid', 'w14_comm_covid', 'w15_comm_covid', 'w16_comm_covid', 'w20_comm_covid', 'w21_comm_covid'))
d_comm_covid <- d_comm_covid %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_comm_covid', values_to = 'comm_covid', values_transform = list(comm_covid = ~replace_na(.)))

# Variable - first vote (version a)
d$w8_first_vote_a <- ifelse(d$w8_first_vote_a == -97, d$w8_mail_first_vote_a, d$w8_first_vote_a)
d$w20_first_vote_a <- ifelse(d$w20_first_vote_a == -97, d$w20_mail_first_vote_a, d$w20_first_vote_a)
d$w1_first_vote_a <- NA
d$w2_first_vote_a <- NA
d$w3_first_vote_a <- NA
d$w4_first_vote_a <- NA
d$w5_first_vote_a <- NA
d$w6_first_vote_a <- NA
d$w7_first_vote_a <- NA
d$w9_first_vote_a <- NA
d$w10_first_vote_a <- NA
d$w12_first_vote_a <- NA
d$w14_first_vote_a <- NA
d$w15_first_vote_a <- NA
d$w16_first_vote_a <- NA
d$w21_first_vote_a <- NA
d_first_vote_a <- subset(d, select = c('lfdn', 'w1_first_vote_a', 'w2_first_vote_a', 'w3_first_vote_a', 'w4_first_vote_a', 'w5_first_vote_a', 'w6_first_vote_a', 'w7_first_vote_a', 'w8_first_vote_a', 'w9_first_vote_a', 'w10_first_vote_a', 'w12_first_vote_a', 'w14_first_vote_a', 'w15_first_vote_a', 'w16_first_vote_a', 'w20_first_vote_a', 'w21_first_vote_a'))
d_first_vote_a <- d_first_vote_a %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_first_vote_a', values_to = 'first_vote_a', values_drop_na = FALSE)

# Variable - first vote (version b)
d$w8_first_vote_b <- ifelse(d$w8_first_vote_b == -97, d$w8_mail_first_vote_b, d$w8_first_vote_b)
d$w20_first_vote_b <- ifelse(d$w20_first_vote_b == -97, d$w20_mail_first_vote_b, d$w20_first_vote_b)
d$w1_first_vote_b <- NA
d$w2_first_vote_b <- NA
d$w3_first_vote_b <- NA
d$w4_first_vote_b <- NA
d$w5_first_vote_b <- NA
d$w6_first_vote_b <- NA
d$w7_first_vote_b <- NA
d$w9_first_vote_b <- NA
d$w10_first_vote_b <- NA
d$w12_first_vote_b <- NA
d$w14_first_vote_b <- NA
d$w15_first_vote_b <- NA
d$w16_first_vote_b <- NA
d$w21_first_vote_b <- NA
d_first_vote_b <- subset(d, select = c('lfdn', 'w1_first_vote_b', 'w2_first_vote_b', 'w3_first_vote_b', 'w4_first_vote_b', 'w5_first_vote_b', 'w6_first_vote_b', 'w7_first_vote_b', 'w8_first_vote_b', 'w9_first_vote_b', 'w10_first_vote_b', 'w12_first_vote_b', 'w14_first_vote_b', 'w15_first_vote_b', 'w16_first_vote_b', 'w20_first_vote_b', 'w21_first_vote_b'))
d_first_vote_b <- d_first_vote_b %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_first_vote_b', values_to = 'first_vote_b', values_drop_na = FALSE)

# Variable - second vote (version a)
d$w8_second_vote_a <- ifelse(d$w8_second_vote_a == -97, d$w8_mail_second_vote_a, d$w8_second_vote_a)
d$w20_second_vote_a <- ifelse(d$w20_second_vote_a == -97, d$w20_mail_second_vote_a, d$w20_second_vote_a)
d$w1_second_vote_a <- NA
d$w2_second_vote_a <- NA
d$w3_second_vote_a <- NA
d$w4_second_vote_a <- NA
d$w5_second_vote_a <- NA
d$w6_second_vote_a <- NA
d$w7_second_vote_a <- NA
d$w9_second_vote_a <- NA
d$w10_second_vote_a <- NA
d$w12_second_vote_a <- NA
d$w14_second_vote_a <- NA
d$w15_second_vote_a <- NA
d$w16_second_vote_a <- NA
d$w21_second_vote_a <- NA
d_second_vote_a <- subset(d, select = c('lfdn', 'w1_second_vote_a', 'w2_second_vote_a', 'w3_second_vote_a', 'w4_second_vote_a', 'w5_second_vote_a', 'w6_second_vote_a', 'w7_second_vote_a', 'w8_second_vote_a', 'w9_second_vote_a', 'w10_second_vote_a', 'w12_second_vote_a', 'w14_second_vote_a', 'w15_second_vote_a', 'w16_second_vote_a', 'w20_second_vote_a', 'w21_second_vote_a'))
d_second_vote_a <- d_second_vote_a %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_second_vote_a', values_to = 'second_vote_a', values_drop_na = FALSE)

# Variable - second vote (version b)
d$w8_second_vote_b <- ifelse(d$w8_second_vote_b == -97, d$w8_mail_second_vote_b, d$w8_second_vote_b)
d$w20_second_vote_b <- ifelse(d$w20_second_vote_b == -97, d$w20_mail_second_vote_b, d$w20_second_vote_b)
d$w1_second_vote_b <- NA
d$w2_second_vote_b <- NA
d$w3_second_vote_b <- NA
d$w4_second_vote_b <- NA
d$w5_second_vote_b <- NA
d$w6_second_vote_b <- NA
d$w7_second_vote_b <- NA
d$w9_second_vote_b <- NA
d$w10_second_vote_b <- NA
d$w12_second_vote_b <- NA
d$w14_second_vote_b <- NA
d$w15_second_vote_b <- NA
d$w16_second_vote_b <- NA
d$w21_second_vote_b <- NA
d_second_vote_b <- subset(d, select = c('lfdn', 'w1_second_vote_b', 'w2_second_vote_b', 'w3_second_vote_b', 'w4_second_vote_b', 'w5_second_vote_b', 'w6_second_vote_b', 'w7_second_vote_b', 'w8_second_vote_b', 'w9_second_vote_b', 'w10_second_vote_b', 'w12_second_vote_b', 'w14_second_vote_b', 'w15_second_vote_b', 'w16_second_vote_b', 'w20_second_vote_b', 'w21_second_vote_b'))
d_second_vote_b <- d_second_vote_b %>% pivot_longer(cols = -lfdn, names_to = c('wave'), names_pattern = 'w(.*)_second_vote_b', values_to = 'second_vote_b', values_drop_na = FALSE)

# All other variables
d_other <- subset(d, select = c('lfdn', 'const17', 'const21', 'land', 'year_birth', 'gender', 'education', 'household_income', 'religion', 'urban', 'german_citizen', 'pol_know'))
d_other <- d_other[rep(seq_len(nrow(d_other)), each = 16), ]

# Merge variables
d <- cbind(d_other, d_wave, d_swd, d_bundestag_trust, d_fcc_trust, d_army_trust, d_police_trust, d_care_covid, d_lock_covid, d_econ_covid, d_comm_covid, d_first_vote_a, d_first_vote_b, d_second_vote_a, d_second_vote_b)

# Eliminate duplicate variables
d <- d[, !duplicated(names(d))]


## Tidy data
# Eliminate respondents with non-responses in political trust
d <- subset(d, !(bundestag_trust < 0 & wave %in% c(2, 8:10, 12, 14:16, 20, 21)))

# Eliminate respondents with non-responses in satisfaction with democracy (in the waves of interest)
d <- subset(d, !(swd < 0 & wave %in% c(5, 8:10, 12, 14:16, 20, 21)))

# Reverse satisfaction with democracy scale to match the scale used for political trust
d$swd <- 6 - d$swd

# Eliminate respondents with non-responses in vote variables  (in the waves of interest)
d <- subset(d, !(first_vote_a < 0 & wave %in% c(8, 20)))
d <- subset(d, !(first_vote_b < 0 & wave %in% c(8, 20)))
d <- subset(d, !(second_vote_a < 0 & wave %in% c(8, 20)))
d <- subset(d, !(second_vote_b < 0 & wave %in% c(8, 20)))

# Impute vote variable to be measured in all core waves
d <- d %>%
  group_by(lfdn) %>%
  mutate(first_vote_a = ifelse(wave == 8, first_vote_a,
                               ifelse(wave %in% c(2, 5), first_vote_a[wave == 8],
                                      ifelse(wave == 20, first_vote_a,
                                             ifelse(wave == 16, first_vote_a[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(first_vote_b = ifelse(wave == 8, first_vote_b,
                               ifelse(wave %in% c(2, 5), first_vote_b[wave == 8],
                                      ifelse(wave == 20, first_vote_b,
                                             ifelse(wave == 16, first_vote_b[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(second_vote_a = ifelse(wave == 8, second_vote_a,
                                ifelse(wave %in% c(2, 5), second_vote_a[wave == 8],
                                       ifelse(wave == 20, second_vote_a,
                                              ifelse(wave == 16, second_vote_a[wave == 20], NA)))))

d <- d %>%
  group_by(lfdn) %>%
  mutate(second_vote_b = ifelse(wave == 8, second_vote_b,
                                ifelse(wave %in% c(2, 5), second_vote_b[wave == 8],
                                       ifelse(wave == 20, second_vote_b,
                                              ifelse(wave == 16, second_vote_b[wave == 20], NA)))))

# Recode age variable
d$year_birth <- ifelse(d$year_birth == '1955 und frueher', '1955', d$year_birth)
d$year_birth <- as.numeric(d$year_birth)
d$year_birth <- cut(d$year_birth, breaks = c(0, 1959, 1969, 1979, 1989, 1999, Inf), labels = c('1959 and earlier', '1960-69', '1970-79', '1980-89', '1990-99', '1999 and later'))

# Recode political knowledge variable
d$pol_know <- ifelse(d$pol_know == -98 | d$pol_know == 1 | d$pol_know == 3, 0,
                     ifelse(d$pol_know == 2, 1, d$pol_know))

# Recode vote variables
d$first_vote_a <- ifelse(d$first_vote_a == -84, 'no vote', d$first_vote_a)
d$first_vote_b <- ifelse(d$first_vote_b == -84, 'no vote', d$first_vote_b)
d$second_vote_a <- ifelse(d$second_vote_a == -84, 'no vote', d$second_vote_a)
d$second_vote_b <- ifelse(d$second_vote_b == -84, 'no vote', d$second_vote_b)

# Recode non-responses
d$education <- ifelse(d$education == -93, 'not asked, terminated', 
                      ifelse(d$education == -95, 'not participated',
                             ifelse(d$education == -99, 'no answer', d$education)))

d$household_income <- ifelse(d$household_income == -93, 'not asked, terminated', 
                             ifelse(d$household_income == -95, 'not participated',
                                    ifelse(d$household_income == -99, 'no answer', d$household_income)))

d$religion <- ifelse(d$religion == -93, 'not asked, terminated', 
                     ifelse(d$religion == -95, 'not participated',
                            ifelse(d$religion == -99, 'no answer', d$religion)))

d$urban <- ifelse(d$urban == -93, 'not asked, terminated', 
                  ifelse(d$urban == -95, 'not participated',
                         ifelse(d$urban == -99, 'no answer', d$urban)))

d$german_citizen <- ifelse(d$german_citizen == -93, 'not asked, terminated', 
                           ifelse(d$german_citizen == -95, 'not participated',
                                  ifelse(d$german_citizen == -99, 'no answer', d$german_citizen)))

d$pol_know <- ifelse(d$pol_know == -93, 'not asked, terminated', 
                     ifelse(d$pol_know == -95, 'not participated',
                            ifelse(d$pol_know == -99, 'no answer', d$pol_know)))

d$first_vote_a <- ifelse(d$first_vote_a == -93, 'not asked, terminated', 
                         ifelse(d$first_vote_a == -95, 'not participated',
                                ifelse(d$first_vote_a == -99, 'no answer', 
                                       ifelse(d$first_vote_a == -97, 'not applicable', d$first_vote_a))))

d$first_vote_b <- ifelse(d$first_vote_b == -93, 'not asked, terminated', 
                         ifelse(d$first_vote_b == -95, 'not participated',
                                ifelse(d$first_vote_b == -99, 'no answer',
                                       ifelse(d$first_vote_b == -97, 'not applicable', d$first_vote_b))))

d$second_vote_a <- ifelse(d$second_vote_a == -93, 'not asked, terminated', 
                          ifelse(d$second_vote_a == -95, 'not participated',
                                 ifelse(d$second_vote_a == -99, 'no answer',
                                        ifelse(d$second_vote_a == -97, 'not applicable', d$second_vote_a))))

d$second_vote_b <- ifelse(d$second_vote_b == -93, 'not asked, terminated', 
                          ifelse(d$second_vote_b == -95, 'not participated',
                                 ifelse(d$second_vote_b == -99, 'no answer',
                                        ifelse(d$second_vote_b == -97, 'not applicable', d$second_vote_b))))

d$first_vote_a <- factor(d$first_vote_a)
d$first_vote_b <- factor(d$first_vote_b)
d$second_vote_a <- factor(d$second_vote_a)
d$second_vote_b <- factor(d$second_vote_b)

# Hot-deck imputation for Land (2021) variable from constituency (2021) variable
d <- hotdeck(d, variable = 'land', domain_var = 'const21')

# Clean environment
rm(list = setdiff(ls(), c('d', 'd_raw', 'pop_den17', 'pop_den21', 'foreign17', 'foreign21', 'unemp17', 'unemp21')))

### TESTING HYPOTHESIS 0 -------------------------------------------------------

### 2017 treatment
# Create 2017 dataset
d17 <- d[d$wave == 2 | d$wave == 5 | d$wave == 8, ]

# Eliminate respondents with non-responses in constituency variable
d17 <- d17[d17$const17 > 0, ]

# Eliminate respondents from constituencies where AfD did not field candidates
d17 <- d17 %>% filter(!(const17 %in% c(24, 39, 97, 122, 126, 127, 139, 150, 165, 204, 234, 235)))

# Add covariates
d17$year <- ifelse(d17$wave == 8, 2017, 2016)
pop_den17$const17 <- pop_den17$const
d17 <- merge(d17, pop_den17, by = c('const17', 'year'))
foreign17$const17 <- foreign17$const
d17 <- merge(d17, foreign17, by = c('const17', 'year'))
unemp17$const17 <- unemp17$const
d17 <- merge(d17, unemp17, by = c('const17', 'year'))
d17 <- d17[order(d17$lfdn, d17$year), ]

# Create treatment variable 
d17$treat17 <- ifelse(d17$const17 %in% c(156:158), 1, 0)

# Create post variable
d17$post17 <- ifelse(d17$wave == 8, 1, 0)

# Check size of treatment and control
n_distinct(d17 %>% filter(treat17 == 1) %>% select(lfdn)) # 112
n_distinct(d17 %>% filter(treat17 == 0) %>% select(lfdn)) # 8,164

# Models without time-varying covariates
summary(lm(bundestag_trust ~ treat17 + post17 + treat17 * post17, data = d17))
summary(lm(swd ~ treat17 + post17 + treat17 * post17, data = d17))

# Models with time-varying covariates
summary(lm(bundestag_trust ~ treat17 + post17 + treat17 * post17 + ln_pop_den + perc_foreign + unemp_rate, data = d17))
summary(lm(swd ~ treat17 + post17 + treat17 * post17 + ln_pop_den + perc_foreign + unemp_rate, data = d17))

### 2021 treatment
# Create 2021 dataset
d21 <- d[(d$wave == 16 | d$wave == 20),]

# Eliminate respondents with non-responses in constituency variable
d21 <- d21[d21$const21 > 0, ]

# Add covariates
d21$year <- ifelse(d21$wave == 20, 2021, 2020)
pop_den21$const21 <- pop_den21$const
d21 <- merge(d21, pop_den21, by = c('const21', 'year'))
foreign21$const21 <- foreign21$const
d21 <- merge(d21, foreign21, by = c('const21', 'year'))
unemp21$const21 <- unemp21$const
d21 <- merge(d21, unemp21, by = c('const21', 'year'))
d21 <- d21[order(d21$lfdn, d21$year), ]

# Eliminate respondents from constituencies where AfD did not field candidates
d21 <- d21 %>% filter(!(const21 %in% c(24, 34, 53, 140, 223, 251)))

# Eliminate respondents from pre-treated constituencies
d21 <- d21 %>% filter(!(const21 %in% c(156, 157, 158)))

# Create treatment variable 
d21$treat21 <- ifelse(d21$const21 %in% c(71, 74, 151, 154, 155, 161, 163:165, 190, 192, 194, 195), 1, 0)

# Create post variable
d21$post21 <- ifelse(d21$wave == 20, 1, 0)

# Check size of treatment and control
n_distinct(d21 %>% filter(treat21 == 1) %>% select(lfdn)) # 581
n_distinct(d21 %>% filter(treat21 == 0) %>% select(lfdn)) # 13,544

# Models without time-varying covariates
summary(lm(bundestag_trust ~ treat21 + post21 + treat21 * post21, data = d21))
summary(lm(swd ~ treat21 + post21 + treat21 * post21, data = d21))

# Models with time-varying covariates
summary(lm(bundestag_trust ~ treat21 + post21 + treat21 * post21 + ln_pop_den + perc_foreign + unemp_rate, data = d21))
summary(lm(swd ~ treat21 + post21 + treat21 * post21 + ln_pop_den + perc_foreign + unemp_rate, data = d21))


### TESTING HYPOTHESIS 1 -------------------------------------------------------

### 2017 treatment
# Subset to AfD respondents
afd17 <- d17 %>% 
  group_by(lfdn) %>% 
  filter(any(first_vote_a == 322 | first_vote_b == 322)) %>% 
  ungroup()

# Check size of treatment and control
n_distinct(afd17 %>% filter(treat17 == 1) %>% select(lfdn)) # 22
n_distinct(afd17 %>% filter(treat17 == 0) %>% select(lfdn)) # 745

# Models without time-varying covariates
summary(lm(bundestag_trust ~ treat17 + post17 + treat17 * post17, data = afd17))
summary(lm(swd ~ treat17 + post17 + treat17 * post17, data = afd17))

# Models with time-varying covariates
summary(lm(bundestag_trust ~ treat17 + post17 + treat17 * post17 + ln_pop_den + perc_foreign + unemp_rate, data = afd17))
summary(lm(swd ~ treat17 + post17 + treat17 * post17 + ln_pop_den + perc_foreign + unemp_rate, data = afd17))

### 2021 treatment
# Subset to AfD respondents
afd21 <- d21 %>% 
  group_by(lfdn) %>% 
  filter(any(first_vote_a == 322 | first_vote_b == 322)) %>% 
  ungroup()

# Check size of treatment and control
n_distinct(afd21 %>% filter(treat21 == 1) %>% select(lfdn)) # 109
n_distinct(afd21 %>% filter(treat21 == 0) %>% select(lfdn)) # 897

## Models without time-varying covariates
summary(lm(bundestag_trust ~ treat21 + post21 + treat21 * post21, data = afd21))
summary(lm(swd ~ treat21 + post21 + treat21 * post21, data = afd21))

# Models with time-varying covariates
summary(lm(bundestag_trust ~ treat21 + post21 + treat21 * post21 + ln_pop_den + perc_foreign + unemp_rate, data = afd21))
summary(lm(swd ~ treat21 + post21 + treat21 * post21 + ln_pop_den + perc_foreign + unemp_rate, data = afd21))
