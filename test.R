source('treatment_assignment.R')
source('functions.R')
source('redcap-data.R')

snr %>% 
  #ggplot(aes(x = day, y = CRP, group = PatientID, color = treatment)) + 
  #geom_line()
  group_by(treatment, day) %>%
  summarize(mean_CRP = mean(CRP)) %>% 
  ggplot(aes(x = day, y = mean_CRP, group = treatment, color = treatment)) + 
    geom_line() + 
    geom_point(size = 3) + 
    theme_bw() + 
  xlab('Treatment Day') + ylab('CRP Level (units)')
    
