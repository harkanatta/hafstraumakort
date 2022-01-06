
Rpakkar <- c("tidyverse", "ggOceanMaps", "pdftools", "tabulizer", "plyr", "ggrepel")
pacman::p_load(Rpakkar, character.only = TRUE)

f <- file.path("skjol/Logemann et al. - 2013 - The circulation of Icelandic waters – a modelling study.pdf")

tafla <- tabulizer::extract_tables(f, pages = 9)

dfs <- do.call(rbind,tafla) %>% 
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) 

one <- dfs[,1:5 ]
two <- dfs[,6:10 ]

dfs <- bind_rows(one, two) %>%
  slice(-c(1, 24, 46)) %>% 
  readr::type_convert() %>% 
  mutate(T=as.double(c("6.98","6.93","6.98","5.89","6.82","6.35","5.93","6.16","1.18","5.95","5.34","0.43","0.09","5.66","2.56","0.21","5.14","2.32","0.09","4.75","4.04","2.98","1.45","-0.22","4.05","2.32","-0.16","3.88","2.55","-0.46","4.28","2.29","-0.19","7.24","7.53","6.74","7.49","7.04","6.74","7.87","7.62","6.86","7.73"))) %>% 
  ddply(.(Current),summarize,T=mean(T))

sam <- readr::read_csv("skjol/currents.csv") %>%
  left_join(dfs, by = c("Current"="Current")) #%>% 
#drop_na(angle) # There wss an empty geometry in the shape file.

sam$type <- recode_factor(as.factor(sam$type), "local"= "Strandsjór", "Atlantic" = "Hlýir", "Arctic"= "Kaldir")
#sam$flow <- recode_factor(as.factor(sam$flow), "o"= "solid")
#sam$flow <- recode_factor(as.factor(sam$flow), "u"= "dashed")

linur <- sf::read_sf("skjol/linur/nokkrarlinur.shp") %>% 
  #linur <- sf::read_sf("C:/Users/valty/Documents/vinna/github/NMARgrein/kort/nokkrarlinur.shp") %>% 
  #filter(!sf::st_is_empty(.)) %>% 
  sf::as_Spatial() %>% 
  fortify()

linur2 <- transform_coord(linur)

linur <- linur %>% 
  mutate(styrkur = sam$styrkur,
         T = sam$T,
         type = sam$type,
         flow = sam$flow,
         current = sam$Current,
         long = linur2[,1],
         lat = linur2[,2])


#dt <- data.frame(lon = c(-30, -30, 15, 15), lat = c(55, 70, 70, 55))


sysfonts::font_add_google("PT Serif", "PT Serif")
showtext::showtext_auto()


linurU <- linur %>% filter(flow == "u", type != "Coastal")
linurY <- linur %>% filter(flow != "u", type != "Coastal")
linurCU <- linur %>% filter(flow == "u", type == "Coastal")
linurCY <- linur %>% filter(flow != "u", type == "Coastal")

lim <- c(-31,-4,62.57 , 68.72)
st=.6
p <- basemap(limits = lim, bathymetry = T, glaciers = T, legends = F, base_size = 2, bathy.style = "poly_greys",land.size = .06, gla.size = .06) +
  geom_path(data = linurU, 
            aes(x = long, y = lat, group = id, color = "black"),
            size = st,
            linetype = 1,
            arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.3, "lines"))) +
  geom_path(data = linurU, 
            aes(x = long, y = lat, group = id, color = type),
            size = linurU$styrkur*st,
            linetype = 11,
            arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.3, "lines"))) + 
  geom_path(data = linurY,
            aes(x = long, y = lat, group = id, color = type),
            size = linurY$styrkur*st,
            linetype = 1,
            arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.3, "lines"))) +
  geom_path(data = linurCY, 
            aes(x = long, y = lat, group = id, color = type),
            size = st,
            linetype = 1,
            arrow = arrow(type = "open", angle = 15, ends = "last", length = unit(0.1, "lines")))


heitipunktar <- sf::read_sf("skjol/punktar/heitiIsland.shp") 

p2 <- p + scale_color_manual(name = "Hafstraumar:",
                             values = c( "Kaldir" = "blue","Hlýir" = "red","Strandsjór" = "green"),
                             guide = guide_legend(order = 3, override.aes = list(fill = NA)))  + 
  geom_text_repel( data = heitipunktar,   aes(geometry = geometry, label = heiti, family="PT Serif", fontface="italic"),
                   colour = "black",
                   size = 30,
                   show.legend = FALSE,
                   stat = "sf_coordinates")

p3 <- p2 + labs(caption = "Unnið upp úr Logeman et al. (2013)") +
  theme_void() + theme_void(base_size = 60) +theme(legend.key.size = unit(.5, "cm"), legend.key.width = unit(.86, "cm"), legend.text.align = unit(.4, "cm"), legend.spacing.y = unit(0.4, "cm"))


ggsave("straumar.png", p3, device = "png", height=16, width=16, units = "cm", dpi=800, bg = "white")
