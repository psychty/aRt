# This is a piece of work inspired by Chisato (@Chisatini)

# I have used code presented here: https://chichacha.netlify.app/2019/01/19/extracting-colours-from-your-images-with-image-quantization/ and here: https://chichacha.netlify.app/2018/12/22/bubble-packed-chart-with-r-using-packcircles-package/

# I have used circle packing before to show the scale and proportions of different groups within populations but had never considered using R to extract data from photos to then piece back together using circles.

# This started as a 'do anything but COVID-19 work' project for my real work and I really enjoyed it.

# TODO # Make something that can be scaled up for wall art.


easypackages::libraries(c('tidyverse', 'imager', 'scales', 'packcircles', 'deldir', 'colourlovers', 'mgcv', 'sp', 'rgeos', 'ambient', 'svglite', 'generativeart','jasmines', 'flametree'))

github_repo_dir <- "./aRt"
output_directory <- './aRt/outputs'

# Load images ####
img <- load.image(file.choose())
plot(img)

# strip out the colours from the image
im.df.colour <- img %>%
  as.data.frame(wide="c") %>% 
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3))

# Generate circle packing layout using rbeta distribution as size of circles
pack_layout <- circleProgressiveLayout(rbeta(150,1,7), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

# Using the pack_layout above create a data frame using circleLayoutVertices function so that you can plot circle using ggplot2
data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id, hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +
  theme_void() 

# This time there are a larger number of circles
pack_layout <- circleProgressiveLayout(rbeta(1000,2,3), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

pack_layout <- circleProgressiveLayout(rbeta(10000,.1,3), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

pack_layout <- circleProgressiveLayout(rbeta(1000,.1,3), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

pack_layout <- circleProgressiveLayout(rbeta(5000,.4,2), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 

pack_layout <- circleProgressiveLayout(rbeta(15000,1,1), sizetype='area') %>% 
  mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
         im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
         id=row_number()) %>% 
  inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

data_gg <- circleLayoutVertices(pack_layout) %>% 
  inner_join(pack_layout %>% select(id,hex), by=c("id"))

data_gg %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  geom_polygon(aes(fill=hex)) +  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  theme_void() 


png(paste0('./supertat.png'),
    width = 1480,
    height = 1480,
    res = 130)
print(colours_ext)
dev.off()


params <- list(im=list(img_v), 
               n= 3, ## number of colour you want 
               cs=colorspace_types()[-5]) ## gray fails so I've removed it...

my_colors <- pmap_df(params,get_colorPal)

## Let's see what got spitted out as results for different colourspace specifiction in image_quantize function.

## I want to view reduced colours by different colourspaces all at once! 
# my_colors %>%  
#   group_by(colorspace) %>%
#   mutate(ypos=row_number(value)) %>%  ## I decided to stack colours by value. 
#   ggplot(aes(x=fct_infreq(colorspace),y=ypos, fill=hex)) +  
#   geom_tile() +
#   geom_text(aes(label=hex), color="#ffffffbe", 
#             size=4, family="Roboto Condensed") +
#   scale_fill_identity() +
#   scale_y_continuous(breaks=NULL) +
#   theme_void(base_family="Roboto Condensed") +
#   coord_flip(ylim=c(1,12)) +
#   theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
#   labs(caption="Using different colourspce to reduce the colour used in images")


colours_ext <- my_colors %>%  
  group_by(colorspace) %>%
  mutate(ypos=row_number(hue)) %>%  ## alter stacking order
  ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
  geom_tile() +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  theme_void() +
  coord_polar() +
  expand_limits(y=3) 

png(paste0('./supertat_2.png'),
    width = 1480,
    height = 1480,
    res = 130)
print(colours_ext)
dev.off()
