source('treatment_assignment.R')
source('functions.R')
source('redcap-data.R')


library(GGally)
library(plotly)
library(viridis)
library(hrbrthemes)
library(ggpubr)
library(RColorBrewer)
library(ggiraphExtra)
library(gt)

# plots for symptom tracking  ----

  symptom_plot1 <- snr %>%
    filter(stage == 1) %>%
    group_by(treatment) %>%
    summarize(
      day1 = mean(day1), 
      day2 = mean(day2), 
      day3 = mean(day3)
    ) %>%
    pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
    ggplot(aes(x = time, y = CRP, group = treatment, color = treatment)) + 
    geom_line() + 
    geom_point() + 
    theme_bw() + 
    theme(
      legend.position = 'bottom'
    )

  symptom_plot2 <- snr %>%
    filter(stage == 2) %>%
    group_by(treatment) %>%
    summarize(
      day1 = mean(day1), 
      day2 = mean(day2), 
      day3 = mean(day3)
    ) %>%
    pivot_longer(cols = contains('day'), names_to = 'time', values_to = 'CRP') %>%
    ggplot(aes(x = time, y = CRP, group = treatment, color = treatment)) + 
    geom_line() + 
    geom_point() + 
    theme_bw() + 
    theme(
      legend.position = 'bottom'
    )
  

# plots for treatment breakdown ----

  ## stage 1
  stage1_plot <- snr %>%
    filter(stage == 1) %>%
    ggplot(aes(x = treatment, group = response, fill = response)) + 
    geom_bar(position = position_dodge(), color = 'black', width = 0.8) +
    scale_fill_brewer(palette = 'blues') + 
    labs(x = 'Treatment', y = 'Count') + 
    theme_classic()
  
  snr %>%
    filter(stage == 1) %>%
    ggplot(aes(x = treatment, fill = treatment)) + 
    geom_bar(color = 'black', width = 0.5) + 
    geom_text(aes(label=..count..), stat='count', hjust = -0.8) + 
    theme_classic() +  
    theme(
      legend.position = 'none'
    ) + 
    xlab('Treatment') + 
    coord_flip()

  ## 
  treatment_plot <- ggPieDonut(
    data = test, 
    mapping = aes(pies = treat1, donuts = treat2), 
    labelposition = 1
  )
    
# tables for response breakdown ----
  
  treatment_table1 <- snr %>%
    select(PatientID, stage, treatment, response) %>%
    filter(stage == 1) %>%
    group_by(treatment) %>%
    summarize(
      responders = sum(response == 'Y'), 
      non_responders = sum(response == 'N'), 
      in_progress = sum(response == 'U'), 
      total = n(), 
      response_rate = round(responders/total*100, 2)
    )
  