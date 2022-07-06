library(easypackages)

# devtools::install_github("cutterkom/generativeart")
# remotes::install_github("djnavarro/jasmines")
libraries(c('dplyr', 'ambient', 'svglite', 'generativeart', 'jasmines', 'flametree'))

output_directory <- './aRt/outputs'
# https://towardsdatascience.com/getting-started-with-generative-art-in-r-3bc50067d34b
# https://blog.djnavarro.net/posts/2021-10-19_rtistry-posts/
# https://jiwanheo.rbind.io/post/2021-09-17-how-to-work-with-flow-fields-in-r/

# using generative package ####

# set the paths
IMG_DIR <- "./aRt/generative/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "./aRt/generative/logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)

# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^2)),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

generate_img(formula = my_formula,
             nr_of_img = 1,
             polar = FALSE,
             color = '#999999',
             filetype = 'png',
             background_color = 'white')


# flametree ####

# pick some colours
shades <- c("#1b2e3c", "#0c0c1e", "#74112f", "#f3e3e2", '#13c3f6')

# data structure defining the trees
dat <- flametree_grow(time = 7, trees = 35)

# draw the plot
attempt_1 <- dat %>% 
  flametree_plot(
    background = "antiquewhite",
    palette = shades, 
    style = "nativeflora"
  )

svg(paste0(output_directory, '/Flame1.svg'), width = 7)
attempt_1
dev.off()

# jasmines ####

# The use_seed() function is a convenience function that sets the random number generator seed, and stores that value for later use. The seed is then piped to an entity_ function (or a scene_ function) that creates the initial object that will be operated upon. In this case this initial object is a circle of diameter 2 rendered using 1000 points. This is then passed to an unfold_ function, that iteratively applies some operation (in this case the “warp”) operation for some number of steps (in this case 100). The object is converted to a ggplot2 image using one of the style_ functions. In this and most cases the style_ribbon() is used and has several customisable finctions.

# https://jasmines.djnavarro.net/reference/index.html

use_seed(7787) %>%
  entity_circle(grain = 500) %>%
  unfold_tempest(iterations = 10) %>%
  style_ribbon(background = "white",
               type = 'curve',
               palette = 'inferno')

# It is possible to create “scenes” comprised of multiple entities as the initial starting point, apply multiple unfolding operations, and modify the way transparency and colouring is done.

palette.pals()
palette_named()
colours()

attempt_2 <- use_seed(1) %>%
  scene_discs(rings = 7, 
              points = 4000, 
              size = 2) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(iterations = 1,
              scale = .5, 
              output = "layer") %>%
  unfold_tempest(iterations = 8,
                 scale = .01) %>%
  style_ribbon(palette = palette_named("cork"),
               colour = "violetred",
               alpha = c(.1,.1),
               background = "white")

svg(paste0(output_directory, '/Disc1.svg'), width = 7)
attempt_2
dev.off()

attempt_3 <- use_seed(1) %>%
  scene_discs(
    rings = 3, 
    points = 5000, 
    size = 5) %>%
  mutate(ind = 1:n()) %>%
  unfold_warp(
    iterations = 1,
    scale = .5, 
    output = "layer") %>%
  unfold_tempest(
    iterations = 20,
    scale = .01) %>%
  style_ribbon(
    palette = palette_named("vik"),
    colour = "ind",
    alpha = c(.1,.1),
    background = "oldlace"
  )

svg(paste0(output_directory, '/Disc2.svg'), width = 7)
attempt_3
dev.off()
