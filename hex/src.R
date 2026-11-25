# Draws the sigfig hex sticker. Run from the package root.

out_file <- "hex/sigfig.svg"

# ---- Canvas and palette (Blueprint) ------------------------------------------

W <- 520; H <- 600
CX <- 260; CY <- 300; R <- 292

pal <- list(
  bg = "#1d4a86", glass = "#2f64a8", ruler = "#e6eef6", edge = "#ffffff",
  tick = "#173863", point = "#d8402f", rim = "#f8d995", handle = "#e7ae47",
  grip = "#8a5f14", ink = "#f4f8fc"
)

# ---- The number --------------------------------------------------------------

# sigfig(3.2, 1) + sigfig(4.12, 2) stores 7.32, and displays 7.3
value <- 3.2 + 4.12
shown <- sprintf("%.1f", round(value, 1))

# ---- SVG helpers -------------------------------------------------------------

fmt <- function(v) {
  s <- sprintf("%.1f", v)
  sub("\\.0$", "", s)
}
pts <- function(x, y) paste0(fmt(x), ",", fmt(y), collapse = " ")
hexagon <- function(r = R) {
  a <- seq(0, 300, by = 60) * pi / 180
  pts(CX + r * sin(a), CY - r * cos(a))
}
line <- function(x1, y1, x2, y2, col, width, cap = "butt") {
  sprintf('<path d="M%.2f,%.2f L%.2f,%.2f" stroke="%s" stroke-width="%s" stroke-linecap="%s"/>',
          x1, y1, x2, y2, col, width, cap)
}
rect <- function(x, y, w, h, fill) {
  sprintf('<rect x="%s" y="%s" width="%s" height="%s" fill="%s"/>', fmt(x), fmt(y), fmt(w), fmt(h), fill)
}
circle <- function(r, attrs) sprintf('<circle r="%s" %s/>', fmt(r), attrs)
text <- function(x, y, label, size, fill, attrs = "") {
  sprintf(paste0('<text x="%s" y="%s" text-anchor="middle" font-family="Lexend, \'Helvetica Neue\', Arial, sans-serif" ',
                 'font-weight="600" font-size="%s" fill="%s"%s>%s</text>'),
          fmt(x), fmt(y), size, fill, attrs, label)
}
group <- function(content, x, y, rot) {
  sprintf('<g transform="translate(%s %s) rotate(%s)">%s</g>',
          fmt(x), fmt(y), fmt(rot), paste(content, collapse = ""))
}

# ---- Ruler -------------------------------------------------------------------

# `value` is at x = 0, with `unit` px per 1. Ticks are every 10^-digits.
ruler <- function(unit, digits, y0, y1, labels, size, extent) {
  h <- y1 - y0
  i <- seq(ceiling((value - extent / unit) * 10^digits), floor((value + extent / unit) * 10^digits))
  x <- (i / 10^digits - value) * unit
  len <- ifelse(i %% 10 == 0, .5, ifelse(i %% 5 == 0, .38, .24))
  c(
    rect(-extent, y0, 2 * extent, h, pal$ruler),
    rect(-extent, y0, 2 * extent, h * .08, pal$edge),
    line(x, y0, x, y0 + len * h, pal$tick, 3),
    text((as.numeric(labels) - value) * unit, y0 + .86 * h, labels, size, pal$tick)
  )
}

# ---- Loupe: the ruler at 6x, with a tick per hundredth ------------------------

unit <- 130; zoom <- 6; lens_r <- 122
th <- 150 * pi / 180
handle <- function(from, to, col, width) {
  line(from * cos(th), from * sin(th), to * cos(th), to * sin(th), col, width, "round")
}
lens <- c(
  ruler(unit * zoom, 2, -78, 86, shown, 58, lens_r),
  sprintf('<polygon points="%s" fill="%s"/>', pts(c(-11, 11, 0), c(-100, -100, -76)), pal$point),
  line(0, -90, 0, -26, pal$point, 6)
)
loupe <- c(
  ruler(unit, 1, -42, 42, as.character(5:8), 28, 400),
  handle(lens_r, lens_r + 150, pal$grip, 36),
  handle(lens_r + 30, lens_r + 148, pal$handle, 24),
  circle(lens_r, sprintf('fill="%s"', pal$glass)),
  sprintf('<clipPath id="lens"><circle r="%s"/></clipPath><g clip-path="url(#lens)">%s</g>',
          lens_r, paste(lens, collapse = "")),
  circle(lens_r, sprintf('fill="none" stroke="%s" stroke-width="15"', pal$rim)),
  '<path d="M-84,-60 A103,103 0 0 1 -30,-99" fill="none" stroke="#fff" stroke-width="8" stroke-linecap="round" opacity=".6"/>'
)

# ---- Wordmark along the lower-right edge --------------------------------------

# 2.821 em: advance width of "sigfig" in Lexend SemiBold
wordmark <- function(size = 66) {
  text(0, 0, "sigfig", size, pal$ink,
       sprintf(' textLength="%s" lengthAdjust="spacingAndGlyphs"', fmt(2.821 * size)))
}

# ---- Sticker -------------------------------------------------------------------

body <- c(group(loupe, 285, 238, -30), group(wordmark(), 350.4, 457.1, -30))
svg <- paste0(
  sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 %s %s" role="img" aria-label="sigfig hex sticker">', W, H),
  sprintf('<defs><clipPath id="hex"><polygon points="%s"/></clipPath></defs>', hexagon()),
  sprintf('<polygon points="%s" fill="%s"/>', hexagon(), pal$bg),
  sprintf('<g clip-path="url(#hex)">%s</g>', paste(body, collapse = "")),
  sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="2" stroke-dasharray="2 7" stroke-linecap="round" opacity=".5"/>',
          hexagon(R - 13), pal$ink),
  sprintf('<polygon points="%s" fill="none" stroke="%s" stroke-width="12" stroke-linejoin="miter"/></svg>', hexagon(), pal$ink)
)
writeLines(svg, out_file)
