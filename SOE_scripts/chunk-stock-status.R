## ----stock-status----
#Get data, spread for plotting, and filter
stock_status <- ecodata::stock_status %>%
  spread(.,Var,Value) %>% 
  filter(Council %in% c(council_abbr,"Both")) %>% 
  group_by(Stock) %>% 
  mutate(score = case_when(
    (F.Fmsy > 1 & B.Bmsy < 1) ~ "a",  
    (F.Fmsy > 1 & B.Bmsy > 1) ~ "b",
    (F.Fmsy < 1 & B.Bmsy < 1) ~ "c",
    (F.Fmsy < 1 & B.Bmsy > 1) ~ "d"))

#Plot constants
y.max <- 2.0 #1.75 mackerel cut off F/Fmsy is 1.8
x.max <- 2.6

#A dataframe that defines custom legend for stocks with unknown status

unknown <- data.frame(text = c("Unknown Status", "Longfin",
                              "Illex", "N. Goosefish", "S. Goosefish"),
                    x = rep(0.9*x.max,5),
                    y = seq(0.93*y.max,1.4,-.1))


#Plotting code
p1<-ggplot(data = stock_status) +
  geom_vline(xintercept = 1, linetype = "dotted")+
  geom_vline(xintercept = 0.5, linetype = "dashed")+
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_point(aes(x = B.Bmsy,
                 y = F.Fmsy,
                 shape = Council,
                 color = score)) +
  geom_text_repel(aes(x = B.Bmsy, #geom_text_repel auto-jitters text around points
                      y = F.Fmsy,
                      label = Code,
                      shape = Council), show.legend = FALSE,nudge_y = 0.05, nudge_x = 0.05) +
  scale_color_manual(values = c("orangered2","gold2", "green3"), #Change legend labels for clarity
                     breaks = stock_status$score) +
  ylim(0,y.max) +
  xlim(0,x.max) +
  geom_text(data = unknown, aes(x = x, y = y, label = text), #Custom legend for unknown stock status
            size = c(4.75,rep(4,4))) +
  annotate("rect", xmin = 0.8*x.max,
           xmax = x.max,
           ymin = 0.65*y.max,
           ymax = 0.90*y.max,
           alpha = 0.1) +
  xlab(expression(~B/B[msy])) +
  ylab(expression(~F/F[msy])) +
  guides(color = FALSE) +
  theme_ts()
