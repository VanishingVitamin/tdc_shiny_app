# Load necessary library
library(htmltools)

#' Define a helper function to render SVG from path data Created with the help
#' of MS Copilot
#'
#' @param path character string representing an SVG path
#' @param width output svg width
#' @param height output svg height
#' @param transform transformation matrix of icon (scaling, skewing, flipping,
#'   translating)
#' @param size size of output icon when rendered. width/height are more so used
#'   to define the aspect ratio of the icon
render_icon <- function(path, width, height, transform, size = 20) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    width = size, height = size,
    viewBox = sprintf("0 0 %s %s", width, height),
    tags$g(
      transform = transform,
      tags$path(d = path, fill = "currentColor")
    )
  )
}

# Define icons (example: download, zoom, pan, etc.)
# Source: https://github.com/plotly/plotly.js/blob/master/src/fonts/ploticon.js
icons <- list(
  camera = render_icon(
    path = 'm500 450c-83 0-150-67-150-150 0-83 67-150 150-150 83 0 150 67 150 150 0 83-67 150-150 150z m400 150h-120c-16 0-34 13-39 29l-31 93c-6 15-23 28-40 28h-340c-16 0-34-13-39-28l-31-94c-6-15-23-28-40-28h-120c-55 0-100-45-100-100v-450c0-55 45-100 100-100h800c55 0 100 45 100 100v450c0 55-45 100-100 100z m-400-550c-138 0-250 112-250 250 0 138 112 250 250 250 138 0 250-112 250-250 0-138-112-250-250-250z m365 380c-19 0-35 16-35 35 0 19 16 35 35 35 19 0 35-16 35-35 0-19-16-35-35-35z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 1000, height = 1000
  ),
  zoom_box = render_icon(
    path = 'm1000-25l-250 251c40 63 63 138 63 218 0 224-182 406-407 406-224 0-406-182-406-406s183-406 407-406c80 0 155 22 218 62l250-250 125 125z m-812 250l0 438 437 0 0-438-437 0z m62 375l313 0 0-312-313 0 0 312z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 1000, height = 1000
  ),
  pan = render_icon(
    path = 'm1000 350l-187 188 0-125-250 0 0 250 125 0-188 187-187-187 125 0 0-250-250 0 0 125-188-188 186-187 0 125 252 0 0-250-125 0 187-188 188 188-125 0 0 250 250 0 0-126 187 188z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 1000, height = 1000
  ),
  zoom_plus = render_icon(
    path = 'm1 787l0-875 875 0 0 875-875 0z m687-500l-187 0 0-187-125 0 0 187-188 0 0 125 188 0 0 187 125 0 0-187 187 0 0-125z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 875, height = 1000
  ),
  zoom_minus = render_icon(
    path = 'm0 788l0-876 875 0 0 876-875 0z m688-500l-500 0 0 125 500 0 0-125z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 875, height = 1000
  ),
  home = render_icon(
    path = 'm786 296v-267q0-15-11-26t-25-10h-214v214h-143v-214h-214q-15 0-25 10t-11 26v267q0 1 0 2t0 2l321 264 321-264q1-1 1-4z m124 39l-34-41q-5-5-12-6h-2q-7 0-12 3l-386 322-386-322q-7-4-13-4-7 2-12 7l-35 41q-4 5-3 13t6 12l401 334q18 15 42 15t43-15l136-114v109q0 8 5 13t13 5h107q8 0 13-5t5-13v-227l122-102q5-5 6-12t-4-13z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 928.6, height = 1000
  ),
  tooltip_basic = render_icon(
    path = 'm375 725l0 0-375-375 375-374 0-1 1125 0 0 750-1125 0z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 1500, height = 1000
  ),
  tooltip_compare = render_icon(
    path = 'm187 786l0 2-187-188 188-187 0 0 937 0 0 373-938 0z m0-499l0 1-187-188 188-188 0 0 937 0 0 376-938-1z',
    transform = 'matrix(1 0 0 -1 0 850)',
    width = 1125, height = 1000
  )
)

# Display instructions with icons
HTML(paste0(
  "<ul>",
  "<li>", icons$download, " <strong>Download plot as PNG</strong>: Save the current view of the plot as a high-resolution image.</li>",
  "<li>", icons$zoom, " <strong>Zoom</strong>: Click and drag to zoom into a specific region of the plot.</li>",
  "</ul>"
))
