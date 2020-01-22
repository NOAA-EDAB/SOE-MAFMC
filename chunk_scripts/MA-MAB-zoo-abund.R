
zoo_abund <- ecodata::zoo_anom_sli %>% 
  filter(EPU == epu_abbr,
         !str_detect(Var, "small-large")) %>% 
  mutate(hline = 0,
         Var = str_to_title(str_remove(Var, "anomaly"))) 

zoo_abund %>% 
  ggplot(aes(x = Time, y = Value, group = Var)) +
         annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_gls() +
  geom_line() +
  geom_point() +
  ylab("Abundance anomaly") +
  ggtitle("Zooplankton abundance anomaly") +
  facet_wrap(Var~., ncol = 3) +
  scale_x_continuous(expand = c(0.01, 0.01))+
      geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty)+
  theme_facet() +
  theme(strip.text=element_text(hjust=0,
                                face = "italic"))
