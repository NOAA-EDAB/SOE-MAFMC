## ----MAB-oi-zoo----
facet_names <- list(
  'centropages spring'=expression(paste(italic("Centropages "), "spring")),
  'centropages fall'=expression(paste(italic("Centropages "), "fall")),
  'temora spring'=expression(paste(italic("Temora "), "spring")),
  'temora fall' = expression(paste(italic("Temora "), "fall")),
  'pseudocalanus spring'=expression(paste(italic("Pseudocalanus "), "spring")),
  'pseudocalanus fall' = expression(paste(italic("Pseudocalanus "), "fall")))

zoo_oi_mab <- ecodata::zoo_oi %>% 
  filter(!str_detect(Var,"SD"),
         EPU == "MAB") %>% 
  
  mutate(Var = str_remove(Var, " zoo"),
         Val2 = exp(Value)) %>% 
      group_by(Var) %>% 
  mutate(hline = mean(Val2, na.rm = T)) %>% 
  separate(.,col = Var, into = c("Species","Season"), remove = F)



top <- zoo_oi_mab %>%  
  filter(Species == "centropages") %>% 

ggplot(aes(x = Time, y = Val2, group = Var)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = 0, ymax = Inf) +
  geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
  geom_gls() +
  geom_line()+
  geom_point()+
  ylab("") +
  facet_wrap(Var ~ ., ncol = 2, scales='free_x',labeller = label) +
  ggtitle("Zooplankton abundance (OI)") +
  scale_x_continuous(expand = c(0.01, 0.01))+
  scale_y_continuous(trans = "log10")+
  theme_facet() +
    theme(strip.text=element_text(hjust=0,
                                face = "italic"),
          axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

middle <- zoo_oi_mab %>%  
  filter(Species == "pseudocalanus") %>% 
ggplot(aes(x = Time, y = Val2, group = Var)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = 0, ymax = Inf) +
  geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
  geom_gls() +
  geom_line()+
  geom_point()+
  ylab(expression("Abundance num m"^-3*"")) +
  facet_wrap(Var ~ ., ncol = 2, scales='free_x',labeller = label) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  scale_y_continuous(trans = "log10")+
  theme_facet() +
    theme(strip.text=element_text(hjust=0,
                                face = "italic"),
          axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

bottom <- zoo_oi_mab %>%  
  filter(Species == "temora") %>% 
ggplot(aes(x = Time, y = Val2, group = Var)) +
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = 0, ymax = Inf) +
  geom_hline(aes(yintercept = hline),
     size = hline.size,
     alpha = hline.alpha,
     linetype = hline.lty)+
    geom_gls() +
  geom_line()+
  geom_point()+
    ylab("") +
  facet_wrap(Var ~ ., ncol = 2,labeller = label) +
  scale_x_continuous(breaks = seq(1980,2010,10),
                     expand = c(0.01, 0.01))+

  theme_facet() +
    theme(strip.text=element_text(hjust=0,
                                face = "italic")) +
   scale_y_log10()

top + middle + bottom + plot_layout(ncol = 1) & theme(plot.margin = margin(0,0,0,0,"cm"))
