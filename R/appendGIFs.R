#' @title Append two GIFs
#'
#' @description The appendGIFs function reads and appends two GIFs, and saves the appended gif in your wd.
#'
#' @param gif_1 First GIF image
#' @param gif_2 Second GIF image
#' @param vertical TRUE: GIFs top-to-bottom, FALSE: left-to-right (default)
#' @keywords append GIFs
#' @export
#' @import purrr
#' @import magick

appendGIFs <- function(gif_1, gif_2, vertical=FALSE){
  # Warning message
  writeLines("Appending GIFs... This might take a while!")

   # Read and append gif_1 and gif_2
   map2(
     gif_1 %>% image_read() %>% as.list(),
     gif_2 %>% image_read() %>% as.list(),
     ~image_append(c(.x, .y),stack = vertical)
   ) %>%
     lift(image_join)(.) %>%
     image_write("combined_gif.gif")

   # clear memory
   gc()

  # Done
  writeLines("Done!")
}
