# 0.0. setup ---------------------------------------------------------------------------------------

options(scipen=999)

library(extrafont)
loadfonts(device = "win")

lib <- c('tidyr','plyr', 'ggplot2','viridis','dplyr',
         'forcats','hrbrthemes','data.table','curl',
         'readxl','foreign','ggalt','Hmisc',
         'tidyverse','fastDummies','RCurl','httr','rio','alluvial','ggalluvial')

lapply(lib, library, character.only = TRUE);rm(lib)

# set download paths for files in the github folder
gb_data <- paste0('L:/99.LSE studies/01.PB4A7/99.Summative/',
                  'Original Data and Do file -20211220/113691-V1/')

setwd(gb_data)

d1 <- haven::read_dta(paste0(gb_data, '99.main_data plots.dta'))

d1$count <- 1
d1$GMAA <- d1$guest_black * d1$guest_male
d1$GMwh <- d1$guest_white * d1$guest_male

d1$GFAA <- d1$guest_black * d1$guest_female
d1$GFwh <- d1$guest_white * d1$guest_female

d1$GMAA[d1$GMAA == 1] <- 999
d1$GMwh[d1$GMwh == 1] <- 888
d1$GFAA[d1$GFAA == 1] <- 777
d1$GFwh[d1$GFwh == 1] <- 666

d1$race_gender_g <- rowSums(d1[c('GMAA','GMwh','GFAA','GFwh')])

table(d1$race_gender_g)
d1$race_gender_g[d1$race_gender_g == 999] <- 'AA male'
d1$race_gender_g[d1$race_gender_g == 888] <- 'White male'
d1$race_gender_g[d1$race_gender_g == 777] <- 'AA female'
d1$race_gender_g[d1$race_gender_g == 666] <- 'White female'


# 0.1. alluvial ------------------------------------------------------------------------------------

p1 <- d1 %>% group_by(guest_black,guest_male,host_race_black,yes,race_gender_g,host_gender_M) %>% summarise(n = sum(count,na.rm = T))

table(is.na(p1$yes))

p1 <- p1[is.na(p1$yes) == FALSE,]; sum(p1$n)

# Sub categories

# Guest names label

p1$guest_black[p1$guest_black == 1] <- "AA"
p1$guest_black[p1$guest_black == 0] <- "White"

# Guest gender label

p1$guest_male[p1$guest_male == 1] <- "Male"
p1$guest_male[p1$guest_male == 0] <- "Female"

# Host names label

p1$host_race_black[p1$host_race_black == 1] <- "AA"
p1$host_race_black[p1$host_race_black == 0] <- "White"

# Host gender label

p1$host_gender_M[p1$host_gender_M == 1] <- "Male"
p1$host_gender_M[p1$host_gender_M == 0] <- "Female"

# Yes

p1$yes[p1$yes == 1] <- "Yes"
p1$yes[p1$yes == 0] <- "No"

# factors

p1$guest_black <- as.factor(p1$guest_black)
p1$guest_male <- as.factor(p1$guest_male)
p1$host_race_black <- as.factor(p1$host_race_black)
p1$host_gender_M <- as.factor(p1$host_gender_M)
p1$yes <- as.factor(p1$yes)

p1$ry <- 0
p1$ry[p1$guest_black == 'AA' & p1$yes == 'Yes'] <- 1
p1$ry[p1$guest_black == 'AA' & p1$yes == 'No'] <- 2

p1$race_gender_h <- paste(p1$host_race_black, tolower(p1$host_gender_M))
p1$race_gender_hg <-  paste0(p1$race_gender_h, p1$race_gender_g)
p1$race_gender_hg1  <- 0
p1$race_gender_hg1[p1$race_gender_h == p1$race_gender_g] <- 1
p1$race_gender_hg2  <- as.integer(as.factor(p1$race_gender_hg))
p1$race_gender_hg2 <- p1$race_gender_hg2 *p1$race_gender_hg1

p1$ry <- as.character(p1$ry)

pg1 <- ggplot(data = p1,
       aes(axis1 = race_gender_g, axis2 = race_gender_h, axis4 = yes,
           y = n))
pg1 <- pg1 +  scale_x_discrete(limits = c("G.race gender", "H.race gender", "Response"))
pg1 <- pg1 +  xlab("")
pg1 <- pg1 +  ylab("")
pg1 <- pg1 +  geom_alluvium(aes(fill =  as.factor(race_gender_hg2)))
pg1 <- pg1 +  geom_stratum( fill = "#0A292E", color = "#0A292E", alpha = 0.7,size= 0)
pg1 <- pg1 +  geom_text(stat = "stratum", aes(label = after_stat(stratum)),color = "white",size= 4.5, family = 'serif')
pg1 <- pg1 +  theme_minimal() 
pg1 <- pg1 +  guides(fill = FALSE) + scale_fill_manual(values = c("gray",RColorBrewer::brewer.pal(4,'RdYlBu')))

pg1 <-pg1 + theme( axis.line = element_line(colour = "black", family = 'serif',size = 1, linetype = "solid"))
pg1 <-pg1 + theme(axis.text.y = element_text(colour="black", family = 'serif',size=12))
pg1 <-pg1 + theme(axis.text.x = element_text(colour="black", family = 'serif',size=12))

pg1 <-pg1 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pg1

png("alluvial_poster", width = 40, height=15, units="cm", res=1000, bg="transparent")
pg1
dev.off()

# 0.2. tile ----------------------------------------------------------------------------------------
p2 <- openxlsx::read.xlsx('homphily.xlsx')

pg2 <- ggplot(p2, aes(x=A, y=B, fill=percent))
pg2 <- pg2 + geom_tile(color="white", size=0.1)
pg2 <- pg2 + labs(x= "Host", y= 'Guest',size=18,)
pg2 <- pg2 + scale_fill_gradientn("value", colours = rev(brewer.pal(9, "OrRd")), na.value = "white")
pg2 <- pg2 + coord_equal()
pg2 <- pg2 +theme_minimal()
pg2 <- pg2 + theme(plot.title=element_text(hjust=0))
pg2 <- pg2 + theme(axis.ticks=element_blank())
pg2 <- pg2 + theme(text=element_text(size=18,colour="white" ))
pg2 <- pg2 + theme(axis.text=element_text(size=18,colour="white"))
pg2 <- pg2 + theme(legend.title=element_text(size=18,colour="white"))
pg2 <- pg2 + theme(legend.text=element_text(size=18,colour="white"))
pg2 <- pg2 + theme(legend.position="top")
pg2 <- pg2 + theme(legend.position="bottom")

pg2 <-pg2 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png("tile_poster.png", width = 20, height=20, units="cm", res=1000, bg="transparent")
pg2
dev.off()
