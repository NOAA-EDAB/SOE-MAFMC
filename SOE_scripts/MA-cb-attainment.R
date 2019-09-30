## ----cb-attainment----
minlab <- seq(1985,2015,5)
maxlab <- seq(1987,2017,5)



ches_bay_wq %>% 
  mutate(hline = mean(Value)) %>% 
  ggplot(aes(x = Time, y = Value)) +
       annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf) +
  geom_line() +
  geom_point() +
  geom_gls() +
      ylab("Estimated attainment (%)") +
  ggtitle("Chesapeake Bay Estimated Water Quality Standards Attainment") +
  scale_x_continuous(breaks = minlab,labels = paste0(minlab,"-",maxlab),expand = c(0.01, 0.01)) +
    geom_hline(aes(yintercept = hline),
           size = hline.size,
           alpha = hline.alpha,
           linetype = hline.lty) +
  theme_ts() +
  theme(axis.text.x = element_text(angle = 20, vjust = 0.65),
          plot.title = element_text(size = 10))
