#These lines load in the previously installed packages. RStudio folks can set these to auto-load in new sessions, allowing them to be skipped.
rm(list = ls())

library(afex)
library(emmeans)
library(lmerTest)
library(r2glmm)
library(pivottabler)
library(gtools)
library(ggplot2)
library(dplyr)



# Finds your csv (comma separated values) file on your computer. Remember, these can only be 1 single sheet. Multi-sheet documents will not work.


DATANAME <- read.csv('/Users/rileyvanmeter/Desktop/ExtravaganceFinalFinal 2.csv',
               stringsAsFactors = FALSE,
               check.names = FALSE) 



DATANAME$Coded_Response <- as.factor(DATANAME$Coded_Response)
DATANAME$Condition <- as.factor(DATANAME$Condition)
DATANAME$Subject <- as.factor(DATANAME$Subject)
DATANAME$Country <- as.factor(DATANAME$Country)
DATANAME$'Age' <- as.factor(DATANAME$'Age')
DATANAME$'Gender' <- as.factor(DATANAME$'Gender')
DATANAME$'Education' <- as.factor(DATANAME$'Education')
DATANAME$Question <- as.factor(DATANAME$Question)

df_sub_Mexico <- subset(DATANAME, Country == "Mexico")
df_sub_Spain <- subset(DATANAME, Country == "Spain")

df_sub_only_reglas <- subset(DATANAME, Question == "Rules")
df_sub_no_reglas <- subset(DATANAME, Question != "Rules")

df_sub_Spain_only_reglas <- subset(df_sub_only_reglas, Country == "Spain")
df_sub_Mexico_only_reglas <- subset(df_sub_only_reglas, Country == "Mexico")

df_sub_Spain_no_reglas <- subset(df_sub_no_reglas, Country == "Spain")
df_sub_Mexico_no_reglas <- subset(df_sub_no_reglas, Country == "Mexico")


################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
#### Model NO Reglas ###########################################################################################

MIXEDMODELNAME_no_reglas = glmer(Coded_Response ~ Condition * Country  + (1|Subject), data=df_sub_no_reglas, family = "binomial")


# Cross your fingers, it's time to see your results! Results appear as β-coefficients per non-intercept IV level (for each IV) that either are or are not statistically significant (i.e., different or not from the intercept). The direction of effect, crucially, depends on your DV reference level, since positive β-coefficients indicate greater frequencies of the DV reference level and negative ones indicate lesser frequencies of the DV reference level. You'll interpret your direction of effect completely backwards if you don't know the DV reference level! (And no, there's no way to confirm what it is other than graphing your results [see line 861/862] and evaluating if the positive or negative β-coefficients are consistent with what DV level visually respectively increases or decreases for the IV in question.) Finally, note that you can find the Model AIC value with this code. The lower the AIC value, the better the model fit.
summary(MIXEDMODELNAME_no_reglas)

# β-coefficients from the summary(MODELNAME) step above are in log-odds (aka logits), which are not immediately interpretable. A log-odds of 0.0 equates to a DV reference level frequency of precisely 50%, to give a reference point. To convert a log-odds β-coefficient into % of DV reference level frequency, run the code below. Alternatively, you can calculate it using the following code [entering the β-coefficient in question for each ###], which doesn't depend on the gtools package:  exp(###)/(1+exp(###))   .  Once you have the resulant decimal quantity, simply move the decimal over 2 spaces to the right, and you've got your model's predicted % of DV reference level occurrence (for example, .71 means 71% predicted DV reference level occurrence).
inv.logit(###)

# Post-hoc test for any significant nominal IV interaction. 
emmeans(MIXEDMODELNAME_no_reglas, list(pairwise ~ Country * Condition), adjust="tukey")

# This code allows you to see the pseudo-R-squared value for the model (as well as individual IVs). This value will range from 0 to 1, with a higher value indicating greater DV variance accounted for by the model you've created.
r2beta(MIXEDMODELNAME_no_reglas)

####################
######PLOTS#########
####################



font_add(family = "century", regular = "/Users/rileyvanmeter/Desktop/centuryschoolbook.ttf")
showtext_auto()

df_plot_Mexico_no_reglas <- df_sub_Mexico_no_reglas %>%
  group_by(Condition, Coded_Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Condition) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot_Mexico_no_reglas, aes(x = Condition, y = prop, fill = Coded_Response)) +
  geom_col(position = "fill") +
  geom_text(aes(
    label = scales::percent(prop, accuracy = 0.1),
    color = Coded_Response              # map label color to response
  ),
  position = position_fill(vjust = 0.5),
  size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("PRET" = "#FDB515",     # yellow
               "PP"   = "#003057")     # dark blue
  ) +
  scale_color_manual(                     # text colors
    values = c("PRET" = "black",         # text for yellow
               "PP"   = "white")         # text for blue
  ) +
  labs(
    title = "Proportion of PRET vs PP by Condition in Mexico (No Reglas)",
    x = "Condition",
    y = "Proportion",
    fill = "Response",
    color = NULL
  ) +
  theme_minimal() +
  guides(color = "none")                  # hides the text-color legend


df_plot_Spain_no_reglas <- df_sub_Spain_no_reglas %>%
  group_by(Condition, Coded_Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Condition) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot_Spain_no_reglas, aes(x = Condition, y = prop, fill = Coded_Response)) +
  geom_col(position = "fill") +
  geom_text(aes(
    label = scales::percent(prop, accuracy = 0.1),
    color = Coded_Response              # map label color to response
  ),
  position = position_fill(vjust = 0.5),
  size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("PRET" = "#FDB515",     # yellow
               "PP"   = "#003057")     # dark blue
  ) +
  scale_color_manual(                     # text colors
    values = c("PRET" = "black",         # text for yellow
               "PP"   = "white")         # text for blue
  ) +
  labs(
    title = "Proportion of PRET vs PP by Condition in Spain (No Reglas)",
    x = "Condition",
    y = "Proportion",
    fill = "Response",
    color = NULL
  ) +
  theme_minimal() +
  guides(color = "none")                  # hides the text-color legend





################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
#### Model ONLY Reglas ###########################################################################################

MIXEDMODELNAME_only_reglas = glmer(Coded_Response ~ Condition * Country  + (1|Subject), data=df_sub_only_reglas, family = "binomial")


# Cross your fingers, it's time to see your results! Results appear as β-coefficients per non-intercept IV level (for each IV) that either are or are not statistically significant (i.e., different or not from the intercept). The direction of effect, crucially, depends on your DV reference level, since positive β-coefficients indicate greater frequencies of the DV reference level and negative ones indicate lesser frequencies of the DV reference level. You'll interpret your direction of effect completely backwards if you don't know the DV reference level! (And no, there's no way to confirm what it is other than graphing your results [see line 861/862] and evaluating if the positive or negative β-coefficients are consistent with what DV level visually respectively increases or decreases for the IV in question.) Finally, note that you can find the Model AIC value with this code. The lower the AIC value, the better the model fit.
summary(MIXEDMODELNAME_only_reglas)

# β-coefficients from the summary(MODELNAME) step above are in log-odds (aka logits), which are not immediately interpretable. A log-odds of 0.0 equates to a DV reference level frequency of precisely 50%, to give a reference point. To convert a log-odds β-coefficient into % of DV reference level frequency, run the code below. Alternatively, you can calculate it using the following code [entering the β-coefficient in question for each ###], which doesn't depend on the gtools package:  exp(###)/(1+exp(###))   .  Once you have the resulant decimal quantity, simply move the decimal over 2 spaces to the right, and you've got your model's predicted % of DV reference level occurrence (for example, .71 means 71% predicted DV reference level occurrence).
inv.logit(###)


# Post-hoc test for any significant nominal IV interaction. 
emmeans(MIXEDMODELNAME_only_reglas, list(pairwise ~ Country * Condition), adjust="tukey")


# This code allows you to see the pseudo-R-squared value for the model (as well as individual IVs). This value will range from 0 to 1, with a higher value indicating greater DV variance accounted for by the model you've created.
r2beta(MIXEDMODELNAME_only_reglas)


plot(
  table(df_sub_Mexico_only_reglas$Condition, df_sub_Mexico_only_reglas$Coded_Response),
  col = TRUE,
  main = "Mexico Responses by Condition (ONLY reglas)"
)


###############
####PLOTS######
###############


df_plot_Mexico_only_reglas <- df_sub_Mexico_only_reglas %>%
  group_by(Condition, Coded_Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Condition) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot_Mexico_only_reglas, aes(x = Condition, y = prop, fill = Coded_Response)) +
  geom_col(position = "fill") +
  geom_text(aes(
    label = scales::percent(prop, accuracy = 0.1),
    color = Coded_Response              # map label color to response
  ),
  position = position_fill(vjust = 0.5),
  size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("PRET" = "#FDB515",     # yellow
               "PP"   = "#003057")     # dark blue
  ) +
  scale_color_manual(                     # text colors
    values = c("PRET" = "black",         # text for yellow
               "PP"   = "white")         # text for blue
  ) +
  labs(
    title = "Proportion of PRET vs PP by Condition in Mexico (Reglas Only)",
    x = "Condition",
    y = "Proportion",
    fill = "Response",
    color = NULL
  ) +
  theme_minimal() +
  guides(color = "none")                  # hides the text-color legend



df_plot_Spain_only_reglas <- df_sub_Spain_only_reglas %>%
  group_by(Condition, Coded_Response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Condition) %>%
  mutate(prop = n / sum(n))

ggplot(df_plot_Spain_only_reglas, aes(x = Condition, y = prop, fill = Coded_Response)) +
  geom_col(position = "fill") +
  geom_text(aes(
    label = scales::percent(prop, accuracy = 0.1),
    color = Coded_Response              # map label color to response
  ),
  position = position_fill(vjust = 0.5),
  size = 4) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c("PRET" = "#FDB515",     # yellow
               "PP"   = "#003057")     # dark blue
  ) +
  scale_color_manual(                     # text colors
    values = c("PRET" = "black",         # text for yellow
               "PP"   = "white")         # text for blue
  ) +
  labs(
    title = "Proportion of PRET vs PP by Condition in Spain (Reglas Only)",
    x = "Condition",
    y = "Proportion",
    fill = "Response",
    color = NULL
  ) +
  theme_minimal() +
  guides(color = "none")                  # hides the text-color legend
