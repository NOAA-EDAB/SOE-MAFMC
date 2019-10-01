### Bennet
library(tidyverse)
library(ecodata)
source(pipe(paste("wget -O -", "https://github.com/NOAA-EDAB/SOE-MAFMC/blob/master/SOE_scripts/MA-setup.R")))

#Filter data into two dataframes for plotting
indicators <- ecodata::bennet %>% 
  filter(EPU == epu_abbr,
         Var %in% c("VI EPU aggregate",
                    "PI EPU aggregate")) %>% 
  mutate(Var, Var = plyr::mapvalues(Var, from = c("VI EPU aggregate","PI EPU aggregate"),
                                    to = c("Volume","Price")))

revchange <- ecodata::bennet %>% 
  filter(EPU == epu_abbr,
         Var %in% c("REVCHANGE EPU aggregate"))

#custom bar fill color (color-blind friendly)
ind_fill <- c("#a6cee3", "#b2df8a")

#limits
y.lim <- c(-450,450)


#plot
ggplot(data = indicators)+
  
  #Highlight last ten years
  annotate("rect", fill = shade.fill, alpha = shade.alpha,
      xmin = x.shade.min , xmax = x.shade.max,
      ymin = -Inf, ymax = Inf)+
  
  geom_bar(aes(x=Time, y= Value, fill = Var), stat="identity")+
  scale_fill_manual(name = "Indicators", values = ind_fill) +
  geom_line(data = revchange, aes(x = Time, y = Value, colour="$"))+
  scale_colour_grey(name ="Revenue Change") +
  ggtitle("Bennet Indicator")+
  labs(y="Value $1,000,000 ($2015)",
       caption = "Revenue change from the long-term mean in 2015 dollars (black), Price (PI), and Volume Indicators (VI) for commercial landings in the Mid-Atlantic Bight.") +
  scale_x_continuous(breaks = seq(1985, 2015, by = 5), expand = c(0.01, 0.01)) +
  scale_y_continuous(breaks = seq(y.lim[1], y.lim[2], by = 100), limits = y.lim, expand = c(0.01, 0.01)) +
  theme_ts() +
  theme(aspect.ratio = 0.45,
        title = element_text(size = 10),
        plot.caption =  element_text(hjust = 0))

