#burada satirlar ekleniyor birbirine
foreign_sum <- foreign_agg  %>% group_by(country_name) %>% mutate(kFrekans=cumsum(value))

#rename columns
foreign_sum <- foreign_sum %>% rename(keyword=country_name, week=year, frequency=value, cumsum=kFrekans)

#burada eklemeler yapiyor
foreign_sum <- foreign_sum %>% 
  group_by(week) %>% 
  arrange(desc(frequency),.by_group = TRUE,asc=TRUE) %>% 
  mutate(rank=seq.int(length(frequency)),Value_rel = frequency/frequency[rank==1],Value_lbl=paste0(frequency)) %>% ungroup()

#once sabit plotlari cikariyor
anim <- ggplot(foreign_sum, aes(rank, group = keyword, 
                                fill = as.factor(keyword), color = as.factor(keyword))) +
  geom_tile(aes(y = cumsum/2,
                height = cumsum,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(keyword, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=cumsum,label = cumsum, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(week, transition_length = 6, state_length = 2, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(x= "Haftalık sıklıklar (Birbirine eklenerek ilerlemektedir)", y= "Anahtar Kelimeler", title = '{closest_state}. Hafta ', subtitle = "Dış Siyaset", 
       caption  = "Veri: BBC-Turkce Twitter") 


#sonra onlari animate gif yapiyor
animate(anim, 1000, fps = 20, width = 1200, height = 1000, 
        renderer = gifski_renderer("foreign.gif"), end_pause = 15, start_pause =  15, duration = 300) 

#son animasyonu kaydet
anim_save(dış_siyaset, animation = last_animation())

#ya da mp4 yapiyor
animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4