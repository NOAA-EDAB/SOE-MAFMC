## ----nefsc-biomass-mab----
total_surv <- nefsc_survey %>% 
  filter(EPU == epu_abbr,
         !str_detect(Var, "Other|Apex|managed"),
         Time >= 1968) %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(Var = word(Var, 1,2))
series.col <- rep("black",length(unique(total_surv$Var)))
total_surv$Var <- factor(total_surv$Var,levels = c("Piscivore Fall",
                                                   "Piscivore Spring",
                                                    "Benthivore Fall",
                                                    "Benthivore Spring",
                                                    "Planktivore Fall",
                                                    "Planktivore Spring",
                                                    "Benthos Fall",
                                                    "Benthos Spring"))
#Create dataframe for label locations
label_loc <- total_surv %>%
  group_by(Var) %>%
  dplyr::summarise(yloc = max(Value)*0.95,
                   xloc = min(Time)) %>% 
  mutate(label = LETTERS[1:8])
#facet names for titles
facet_names <- list("Piscivores" = expression("Piscivores"),
                    "Planktivores" = expression("Planktivores"),
                    "Benthivores" = expression("Benthivores"),
                    "Benthos" = expression("Benthos"))

#Get NEAMAP
neamap <- ecodata::mab_inshore_survey %>% 
  mutate(season = str_to_title(word(Var),1),
         feeding.guild = str_to_title(word(Var,2))) %>% 
   filter(str_detect(Var, "index")) %>% 
 unite(.,Var,c("feeding.guild","season"), sep = " ") %>% 
  group_by(Var) %>% 
  mutate(hline = mean(Value))

neamap$Var <- factor(neamap$Var,levels = c("Piscivore Fall",
                                                   "Piscivore Spring",
                                                    "Benthivore Fall",
                                                    "Benthivore Spring",
                                                    "Planktivore Fall",
                                                    "Planktivore Spring",
                                                    "Benthos Fall",
                                                    "Benthos Spring"))

ggplot(data = total_surv, aes(x = Time, y = Value, color = Var)) +
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max ,
      ymin = -Inf, ymax = Inf) +
  
  #Test for trend and add lines
  geom_gls(aes(x = Time, y = Value,
               color = Var),
             alpha = trend.alpha, size = trend.size) +
  
  #Add time series
  geom_line(size = lwd-0.5) +
  geom_point(size = pcex-0.5) +
  scale_color_manual(values = series.col, aesthetics = "color")+
  guides(color = FALSE) +
  geom_hline(aes(yintercept = hline,
                 group = Var),
             size = hline.size,
             alpha = hline.alpha,
             linetype = hline.lty)+
  
  #Add NEAMAP
  geom_line(data = neamap, aes(x = Time, y = Value, color = Var),
            size = lwd-0.5,
            color = "#ca0020")+
  geom_point(data = neamap, aes(x = Time, y = Value, color = Var),
             size = pcex-0.5,
             color = "#ca0020")+

  #Facet 
  facet_wrap(Var~.,scales = "free_y", ncol = 2) +
  
  #Axis and theme
  scale_x_continuous(breaks = seq(1965, 2015, by = 10), expand = c(0.01, 0.01)) +
  ylab(expression("Biomass (kg tow"^-1*")")) +
  theme_facet()+
  theme(strip.text=element_text(hjust=0))
