#A. temporal variation in gender distribution at FSBI symposium -----------


# setup -------------------------------------------------------------------
library(patchwork)
library(gender)
library(dplyr)
library(ggplot2)
library(performance)
library(tidyr)
library(readr)
library(patchwork)


#data harvested from symposia programmes
FSBI_all_bias <- read.csv("C:/Users/jkaufmann/My Drive/Marine Institute/FSBIAS paper/data/names_FSBIAS.csv", comment.char="#",na.strings = c(""))

unique_names <- FSBI_all_bias %>%
  distinct(FN) %>%
  pull()

# ============================================================
# 1. Run gender classifiers
# ============================================================

res_ssa   <- gender(unique_names, method = "ssa",        years = c(1930, 2012))
res_ipums <- gender(unique_names, method = "ipums",      years = c(1850, 1930))
res_napp  <- gender(unique_names, method = "napp",       years = c(1850, 1910))
res_kant  <- gender(unique_names, method = "kantrowitz")

# ============================================================
# 2. Standardise output from each method
#    - fail  = no gender returned
#    - amb   = gender returned but proportions near 50:50
# ============================================================

merge_gender <- function(out, names, method, amb_limit = 0.30) {
  tibble(name = names) %>%
    left_join(out, by = "name") %>%
    mutate(
      fail = is.na(gender),
      amb  = if (all(c("proportion_male", "proportion_female") %in% names(.))) {
        !is.na(gender) &
          proportion_male > amb_limit &
          proportion_male < (1 - amb_limit)
      } else {
        FALSE
      }
    ) %>%
    select(name, gender, fail, amb) %>%
    rename_with(~ paste0(.x, "_", method), -name)
}

# ============================================================
# 3. Combine all methods
# ============================================================

combined <- merge_gender(res_ssa,   unique_names, "ssa") %>%
  left_join(merge_gender(res_ipums, unique_names, "ipums"), by = "name") %>%
  left_join(merge_gender(res_napp,  unique_names, "napp"),  by = "name") %>%
  left_join(merge_gender(res_kant,  unique_names, "kant"),  by = "name")

# ============================================================
# 4. Define "worked" and count successes
#    (worked = non-fail AND non-ambiguous)
# ============================================================

combined <- combined %>%
  mutate(
    worked_ssa   = !fail_ssa   & !amb_ssa,
    worked_ipums = !fail_ipums & !amb_ipums,
    worked_napp  = !fail_napp  & !amb_napp,
    worked_kant  = !fail_kant  & !amb_kant,
    worked_count = rowSums(across(starts_with("worked_")))
  )

# ============================================================
# 5. Disagreement detection
#    Rule:
#      If ≥2 methods worked AND their genders are not identical
# ============================================================

combined <- combined %>%
  rowwise() %>%
  mutate(
    disagreement =
      worked_count >= 2 &&
      n_distinct(
        c(
          if (worked_ssa)   gender_ssa,
          if (worked_ipums) gender_ipums,
          if (worked_napp)  gender_napp,
          if (worked_kant)  gender_kant
        ),
        na.rm = TRUE
      ) > 1
  ) %>%
  ungroup()

# ============================================================
# 6. Final flags and gender assignment
#    - SSA is explicit tiebreaker
# ============================================================

combined <- combined %>%
  mutate(
    gender_flag = case_when(
      disagreement                              ~ "disagreement",
      worked_count == 4                         ~ "worked for 4",
      worked_count == 3                         ~ "worked for 3",
      worked_count == 2                         ~ "worked for 2",
      worked_count == 1                         ~ "worked for 1",
      amb_ssa | amb_ipums | amb_napp | amb_kant ~ "uncertain",
      TRUE                                      ~ "worked for none"
    ),
    
    ssa_superseded = disagreement & worked_ssa,
    
    final_gender = case_when(
      disagreement ~ gender_ssa,   # SSA overrides conflicts
      worked_ssa   ~ gender_ssa,
      worked_ipums ~ gender_ipums,
      worked_napp  ~ gender_napp,
      worked_kant  ~ gender_kant,
      TRUE         ~ NA_character_
    )
  )
# ===============================
# 9. Quick overview
# ===============================
table(combined$gender_flag,combined$final_gender)

# included genderize manuals and overide -----------------------------------

#join with original dataset
combined_to_join=combined %>% dplyr::select(name,final_gender,gender_flag)

SR_temporal=merge(FSBI_all_bias,combined_to_join,all.x = T,by.x="FN",by.y="name") %>% 
  mutate(final_gender=ifelse(!is.na(final_gender),final_gender,GenderByPass)) %>% #bypass
  mutate(Invited=ifelse(Keynote %in% "Y"|JJ.lecture %in% "Y","Yes","No"))

table(SR_temporal$Invited,useNA="a")
table(SR_temporal$final_gender,is.na(SR_temporal$FN),useNA="a")

table(is.na(SR_temporal$final_gender))

dim(table(SR_temporal$Year))

#25 no first name and no gender
#4 no first name but gender
# 23 no gender with FN
# 2 unclear

#fails
SR_temporal %>% filter(is.na(FN))  %>% filter(is.na(final_gender)) %>% dplyr::select(Year,LastName) # 19 to work on. probably mixed first and last name

# Plot ---------------------------------------------------------
ratio_table <- SR_temporal %>%
  mutate(
    classified = final_gender %in% c("male", "female")
  ) %>%
  group_by(Year, Invited) %>%
  dplyr::summarise(
    males   = sum(final_gender == "male", na.rm = TRUE),
    females = sum(final_gender == "female", na.rm = TRUE),
    n_classified = sum(classified, na.rm = TRUE),
    n_total = n(),
    prop_female_coded = females / n_classified,
    name_balance = (females - males) / n_classified,
    prop_unclassified = 1 - (n_classified / n_total),
    .groups = "drop"
  ) %>%
  arrange(Year, Invited)

A=ggplot(
  ratio_table,
  aes(x = Year, y = prop_female_coded, group = Invited)
) +
  geom_point(aes(size = n_classified, shape = Invited), alpha = 0.7) +
  geom_smooth(aes(linetype = Invited), method = "loess", span = 0.7, se = FALSE, color = "black") +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  annotate(
    "text", x = min(ratio_table$Year), y = 0.6,
    label = "Parity (0.5)", hjust = 0
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  scale_size_continuous(
    name = "N",
    breaks = c(25, 50, 100)
  ) +
  scale_shape_manual(
    values = c(16, 17, 15, 18)  # assign different point shapes per group
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash")  # different line types per group
  ) +
  labs(
    x = "Year",
    y = "Proportion female-coded \n(name-based)",title="A"
  ) +
  theme_classic(base_size = 16)  + theme(
    text = element_text(family = "Arial"),  # set font family
    plot.tag = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))


#not that one caveat of only early years have substantial unclassified which could cause a skew if all the unclassified are female ...  
ggplot(ratio_table, aes(x = Year, y = prop_unclassified)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.7, se = FALSE) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Year",
    y = "Proportion of unclassified names"
  ) +
  theme_classic()

