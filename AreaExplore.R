artworks <- read.csv("~/MakeoverMonday/collection/artwork_data.csv") %>%
  filter(units == "mm") %>%
  mutate(width.size = as.numeric(as.character(width)),
         height.size = as.numeric(as.character(height)),
         year.year = as.numeric(as.character(year)),
         acquisitionYear.year = as.numeric(as.character(acquisitionYear)),
         area = height.size * width.size,
         ratio = height.size / width.size,
         portrait = ratio > 1
         )

artworks %>% select(height.size, width.size) %>% ggplot(., aes(x=width.size, y=height.size, alpha=0.01)) + geom_point()
artworks %>% select(height.size, width.size) %>% summary()

ggplot(artworks %>% filter(width.size < quantile(width.size, c(0.99), na.rm=T)), aes(x=width.size)) + geom_histogram()
ggplot(artworks %>% filter(height.size < quantile(height.size, c(0.99), na.rm=T)), aes(x=height.size)) + geom_histogram()

ggplot(artworks, aes(x=acquisitionYear.year)) + geom_histogram()


ggplot(
  artworks %>% 
    filter(height.size < quantile(height.size, c(0.99), na.rm=T)) %>% 
    filter(width.size < quantile(width.size, c(0.99), na.rm=T))
  , aes(x=area)) + geom_histogram()

ggplot(
  artworks %>% 
    filter(height.size < quantile(height.size, c(0.99), na.rm=T)) %>% 
    filter(width.size < quantile(width.size, c(0.99), na.rm=T)), 
  aes(x=area, fill=portrait)) + geom_histogram(position="dodge")


ggplot(
  artworks %>% 
    filter(height.size < quantile(height.size, c(0.99), na.rm=T)) %>% 
    filter(width.size < quantile(width.size, c(0.99), na.rm=T)), 
  aes(x=year.year, y=area)) + geom_point() + geom_smooth() + stat_quantile(quantiles = c(0.25,0.75))


ggplot(
  artworks %>% 
    filter(height.size < quantile(height.size, c(0.99), na.rm=T)) %>% 
    filter(width.size < quantile(width.size, c(0.99), na.rm=T)) %>% 
    mutate(areaAspect = if_else(portrait, area, -area)), 
  aes(x=year.year, y=areaAspect/7140, color=portrait)) + geom_point(alpha=0.1, size=0.5) + geom_smooth() 

areaOverTime <- artworks %>% filter(!is.na(acquisitionYear.year)) %>% filter(!is.na(area)) %>% arrange(acquisitionYear.year) %>% group_by(acquisitionYear.year) %>% dplyr::summarise(n=n(), sumArea=sum(area) ) %>% mutate(sumN = cumsum(n), cumsumArea =cumsum(sumArea))
ggplot(areaOverTime, aes(x=acquisitionYear.year, y=cumsumArea/7140)) + geom_line() + ggtitle("Number of football pitches needed to display the Tate Artworks")


