# https://blog.djnavarro.net/posts/2021-09-07_water-colours/

# COLOUR EXTRACTION ####
easypackages::libraries(c('tidyverse', 'imager', 'scales', 'packcircles', 'deldir', 'colourlovers', 'mgcv', 'sp', 'rgeos', 'ambient', 'svglite', 'generativeart','jasmines', 'flametree', 'magick'))

img_x <- image_read(file.choose()) %>% image_resize(500)

img_v <- image_read(paste0(github_repo_dir, '/images/img5.jpg')) %>% 
  image_resize(500)

img_x %>% 
  image_quantize(max = 50)

get_colorPal <- function(im, n=8, cs="RGB"){
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  # making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
  
}

get_colorPal(img_x) %>% 
  pull(hex)

params <- list(im=list(img_x), 
               n= 33, ## number of colour you want 
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
#   labs(caption="Using different colourspace to reduce the colour used in images")

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

colours_ext
