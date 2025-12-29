#B. 2025 overtime analysis- gender/career-----------


# setup -------------------------------------------------------------------
library(patchwork)
library(gender)
library(dplyr)
library(ggplot2)
library(performance)
library(tidyr)
library(readr)
library(patchwork)

FSBI_2025_bias <- read.csv("C:/Users/jkaufmann/My Drive/Marine Institute/FSBIAS paper/data/FSBI_2025_bias.csv", comment.char="#")

FSBI_2025_bias=FSBI_2025_bias %>% filter(allocated==480)
FSBI_2025_bias=FSBI_2025_bias %>% mutate(career2=ifelse(career %in% c("msc","phd","junior"),"junior",
                                                        ifelse(career %in% "senior","senior",NA))) %>% filter(!is.na(career))

FSBI_2025_bias=FSBI_2025_bias%>% filter(!is.na(min)) %>% mutate(time_curtailed=ifelse(diff<=-0,-diff,0))

#ratios
tab <- table(FSBI_2025_bias$gender, FSBI_2025_bias$career2)

prop_senior <- prop.table(tab, margin = 1)[, "senior"]
prop_senior

#probaility overtime -----------------------------------------------------------

FSBI_2025_bias$over_time_flag <- ifelse(FSBI_2025_bias$diff < 0, 1, 0)
table(FSBI_2025_bias$time_curtailed>0,FSBI_2025_bias$career2)

table(FSBI_2025_bias$time_curtailed>0)
table(FSBI_2025_bias$time_curtailed>60)

summary(FSBI_2025_bias[FSBI_2025_bias$diff < 0, ]$time_curtailed)


#model
mod=glm(over_time_flag ~ gender * career2, family = binomial, data = FSBI_2025_bias)
exp(cbind(OR = coef(mod), confint(mod)))
car::Anova(mod,"III")
check_overdispersion(mod)
check_model(mod)

# plot probability  ---------------------------------------------------------------
over <- FSBI_2025_bias  %>%   dplyr::count(over_time_flag=factor(over_time_flag),career2) %>%
      group_by(career2) %>%
    mutate(
      prop = n / sum(n),
      label = paste0(scales::percent(prop, accuracy = 1))
    ) %>%
    ungroup()


PLOT_PROPC <- ggplot(over, aes(x = career2, y = prop, fill = factor(over_time_flag), label = label)) +
  geom_col(color = "black", width = 0.8) +  # bar outline remains black
  scale_fill_grey(
    start = 0.3, end = 0.8,   # darker vs lighter grey
    labels = c("Did Not", "Went Over"),
    name = "Overtime"
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),  # centers the text on each stacked bar
    color = "white",                        # white font
    size = 5
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    theme_classic(base_size = 16)
  ) +
  labs(
    x = "Career stage",
    y = "Proportion"
  )+  theme(
    text = element_text(family = "Arial"),  # set font family
    plot.tag = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))

overG <- FSBI_2025_bias  %>%   dplyr::count(over_time_flag=factor(over_time_flag),gender) %>%
  group_by(gender) %>%
  mutate(
    prop = n / sum(n),
    label = paste0(scales::percent(prop, accuracy = 1))
  ) %>%
  ungroup()

PLOT_PROPG <- ggplot(overG, aes(x = gender, y = prop, fill = factor(over_time_flag), label = label)) +
  geom_col(color = "black", width = 0.8) +  # bar outline remains black
  scale_fill_grey(
    start = 0.3, end = 0.8,   # darker vs lighter grey
    labels = c("Did Not", "Went Over"),
    name = "Overtime"
  ) +
  geom_text(
    position = position_stack(vjust = 0.5),  # centers the text on each stacked bar
    color = "white",                        # white font
    size = 5
  ) +
  theme_classic() +
  theme(
    legend.position = "right",
    theme_classic(base_size = 16)
  ) +
  labs(
    x = "Gender",
    y = NULL,title="B"
  )+theme(legend.position = "none")+  theme(
    text = element_text(family = "Arial"),  # set font family
    plot.tag = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12))



# time taken (curtailed)--------------------------------------------------------------

B=FSBI_2025_bias %>% #filter(over_time_flag==1)  %>%
  ggplot(aes(x = career2, y = time_curtailed,)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, position = position_dodge(width = 0.8)) + 
  geom_point(             position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.8),
                          size = 2, alpha = 0.8) +
  theme_classic(base_size = 16) +
  labs(y = NULL, x = "Career stage", color = "Career stage")+  theme(
    text = element_text(family = "Arial"),  # set font family
    plot.tag = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))

C=FSBI_2025_bias %>%# filter(over_time_flag==1)  %>%
  ggplot(aes(x = gender, y = time_curtailed)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, position = position_dodge(width = 0.8)) + 
  geom_point(position = position_jitterdodge(jitter.width = 0.05, dodge.width = 0.8),
             size = 2, alpha = 0.8) +
  theme_classic(base_size = 16) +
  labs(y = "Overtime (sec)", x = "Gender", color = "Gender",title="C")+  theme(
    text = element_text(family = "Arial"),  # set font family
    plot.tag = element_text(size = 16, face = "bold"), 
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12))


A/(PLOT_PROPG|PLOT_PROPC|C|B)

#wilcoxon tests

WT1=wilcox.test(time_curtailed~gender,data=FSBI_2025_bias )
WT1
WT2=wilcox.test(time_curtailed~career2,data=FSBI_2025_bias)
WT2

FSBI_2025_bias %>% group_by(career2) %>% dplyr::summarise(mean(time_curtailed))
