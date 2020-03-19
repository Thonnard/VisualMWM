#' appendGIFs reads and appends two or three GIFs into one GIF
#'
#' @param gif_1 First GIF image
#' @param gif_2 Second GIF image
#' @param gif_3 Third GIF image (optional)
#' @param vertical TRUE: GIFs top-to-bottom, FALSE: left-to-right (default)
#' @keywords append GIFs
#' @export
#' @import purrr
#' @import magick

appendGIFs <- function(gif_1, gif_2, gif_3=NULL, vertical=FALSE){
  # Warning message
  writeLines("Appending GIFs... This might take a while!")

  # Read and append gif_1 and gif_2
  map2(
    gif_1 %>% image_read() %>% as.list(),
    gif_2 %>% image_read() %>% as.list(),
    ~image_append(c(.x, .y),stack = vertical)
  ) %>%
    lift(image_join)(.) %>%
    image_write("appended_gif.gif")

  # Read and append gif_3 (optional)
  if(!is.null(gif_3)){
    map2(
      "appended_gif.gif" %>% image_read() %>% as.list(),
      gif_3 %>% image_read() %>% as.list(),
      ~image_append(c(.x, .y), stack = vertical)
    ) %>%
      lift(image_join)(.) %>%
      image_write("appended_gif.gif")
  }

  # Done
  writeLines("Done!")
}
