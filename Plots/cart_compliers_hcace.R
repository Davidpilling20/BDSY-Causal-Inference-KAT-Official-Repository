## ---- save output next to THIS script (works for Rscript and RStudio) ----
get_script_dir <- function() {
  # 1) Rscript:  --file=/path/to/script.R
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) return(dirname(normalizePath(f[1])))
  # 2) source("script.R"):  ofile is set
  if (!is.null(sys.frames()[[1]]$ofile))
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  # 3) RStudio: Source button / Run
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
    p <- rstudioapi::getSourceEditorContext()$path
    if (nzchar(p)) return(dirname(normalizePath(p)))
  }
  getwd()   # fallback: current working directory
}
setwd(get_script_dir())

## ---- helper: rounded rectangle ----
roundrect <- function(x, y, w, h, r = 0.18,
                      col = "#cfe0f2", border = "#2b3a52", lwd = 2.1) {
  xl <- x - w/2; xr <- x + w/2
  yb <- y - h/2; yt <- y + h/2
  r  <- min(r, w/2, h/2)
  k  <- 18
  px <- c((xr - r) + r*cos(seq(-pi/2, 0,      length.out = k)),
          (xr - r) + r*cos(seq(0,     pi/2,   length.out = k)),
          (xl + r) + r*cos(seq(pi/2,  pi,     length.out = k)),
          (xl + r) + r*cos(seq(pi,    3*pi/2, length.out = k)))
  py <- c((yb + r) + r*sin(seq(-pi/2, 0,      length.out = k)),
          (yt - r) + r*sin(seq(0,     pi/2,   length.out = k)),
          (yt - r) + r*sin(seq(pi/2,  pi,     length.out = k)),
          (yb + r) + r*sin(seq(pi,    3*pi/2, length.out = k)))
  polygon(px, py, col = col, border = border, lwd = lwd)
}

## ---- helper: small white square holding the node number ----
numbox <- function(x, y, lab, s = 0.42, cex = 0.8) {
  rect(x - s/2, y - s/2, x + s/2, y + s/2,
       col = "white", border = "#2b3a52", lwd = 1.8)
  text(x, y, lab, cex = cex)
}

## ---- helper: white "yes"/"no" rounded label box on an edge ----
labbox <- function(x, y, lab, cex = 0.92, padx = 0.14, pady = 0.16) {
  hw <- strwidth(lab, cex = cex, font = 3)/2 + padx
  hh <- strheight(lab, cex = cex, font = 3)/2 + pady
  roundrect(x, y, 2*hw, 2*hh, r = hh*0.7,
            col = "white", border = "#2b3a52", lwd = 1.9)
  text(x, y, lab, cex = cex, font = 3)   # italic
}

## ---- helper: draw one node (box + 3 text lines + number) ----
draw_node <- function(nd, w, h, cex = 1.02) {
  roundrect(nd$x, nd$y, w, h, col = nd$fill)
  l1 <- sprintf("H-CACE: %s", fmt(nd$mean))
  l3 <- sprintf("n=%d", nd$n)
  yc <- nd$y - 0.02          # nudge text block down, away from the number
  dy <- 0.28                 # line spacing
  if (is.na(nd$lo) || is.na(nd$hi)) {          # no CI available: 2 lines
    text(nd$x, yc + dy/2, l1, cex = cex, col = nd$txt, font = 2)
    text(nd$x, yc - dy/2, l3, cex = cex, col = nd$txt, font = 2)
  } else {                                      # full 3-line layout
    l2 <- sprintf("95%% CrI: (%s, %s)", fmt(nd$lo), fmt(nd$hi))
    text(nd$x, yc + dy, l1, cex = cex, col = nd$txt, font = 2)
    text(nd$x, yc,      l2, cex = cex, col = nd$txt, font = 2)
    text(nd$x, yc - dy, l3, cex = cex, col = nd$txt, font = 2)
  }
  numbox(nd$x, nd$y + h/2 + 0.08, nd$id)   # sit mostly above the top border
}

## format with a proper minus sign (U+2212); DIG decimals
DIG <- 3
fmt <- function(v) sub("-", "\u2212", formatC(v, format = "f", digits = DIG), fixed = TRUE)

## ---- helper: horizontal diverging colour-bar legend ----
draw_colorbar <- function(x0, x1, y0, y1, M, ramp,
                          title = "H-CACE", ticks = c(-0.5, -0.25, 0, 0.25, 0.5)) {
  n  <- length(ramp)
  xs <- seq(x0, x1, length.out = n + 1)
  rect(xs[-(n + 1)], y0, xs[-1], y1, col = ramp, border = NA)
  rect(x0, y0, x1, y1, border = "#2b3a52", lwd = 1)
  tx <- x0 + (ticks + M) / (2 * M) * (x1 - x0)
  segments(tx, y0, tx, y0 - 0.06, col = "#2b3a52")
  lab <- sub("-", "\u2212", formatC(ticks, format = "f", digits = 2), fixed = TRUE)
  text(tx, y0 - 0.19, lab, cex = 0.68)
  text((x0 + x1)/2, y1 + 0.20, title, cex = 0.82, font = 2)
}

## ---- helper: draw a split (edges + variable + yes/no + pval) ----
draw_split <- function(px, py_bot, Lx, Rx, child_top, sy,
                       var_lab, pval_lab, var_cex = 1.05, ecol = "#2b3a52",
                       left_lab = "yes", right_lab = "no") {
  # orthogonal connectors (no stub from the parent box: starts at the bar)
  segments(Lx, sy, Rx, sy,     col = ecol, lwd = 2.2)     # horizontal bar
  segments(Lx, sy, Lx, child_top, col = ecol, lwd = 2.2)  # -> left  (yes)
  segments(Rx, sy, Rx, child_top, col = ecol, lwd = 2.2)  # -> right (no)
  
  # ---- central label cluster:  [left_lab]  VAR  [right_lab]  ----
  var_hw <- strwidth(var_lab,  cex = var_cex, font = 2)/2
  l_hw   <- strwidth(left_lab,  cex = 0.92, font = 3)/2 + 0.14
  r_hw   <- strwidth(right_lab, cex = 0.92, font = 3)/2 + 0.14
  lead   <- 0.12                       # gap between variable text and box
  l_cx   <- px - var_hw - lead - l_hw
  r_cx   <- px + var_hw + lead + r_hw
  
  # mask the bar behind the whole cluster, then draw text + boxes
  rect(l_cx - l_hw - 0.03, sy - 0.26,
       r_cx + r_hw + 0.03, sy + 0.26, col = "white", border = NA)
  text(px, sy, var_lab, cex = var_cex, font = 2)
  labbox(l_cx, sy, left_lab)
  labbox(r_cx, sy, right_lab)
  
  # pval below (close to the split variable)
  if (!is.null(pval_lab) && nzchar(pval_lab))
    text(px, sy - 0.42, pval_lab, cex = 0.82)
}

## ---- node data (real data) ----
y1 <- 8.3; y2 <- 5.35; y3 <- 2.4
nodes <- data.frame(
  id   = 1:7,
  x    = c(6.5, 3.0, 10.0, 1.25, 4.75, 8.25, 11.75),
  y    = c(y1,  y2,  y2,  y3,  y3,  y3,  y3),
  #         1(root)  2(icu no) 3(icu yes) 4(hisp yes) 5(hisp no) 6(age>=78.5) 7(age<78.5)
  mean = c( 0.0402,   0.0119,   0.3482,   -0.1841,     0.0706,    0.1080,     0.4661),
  lo   = c(-0.1001,  -0.1331,  -0.0646,   -0.5394,    -0.0645,   -0.4988,     0.0195),
  hi   = c( 0.1895,   0.1558,   0.8954,    0.0846,     0.2215,    0.7234,     1.0545),
  n    = c( 427,      383,      44,        79,         304,       13,         31),
  stringsAsFactors = FALSE
)

## fill colour: SYMMETRIC diverging scale centred at 0
## blue = negative, softened red = positive; anchored at +/- max|estimate|
M      <- max(abs(nodes$mean))
divpal <- colorRampPalette(c("#2166AC", "#5CA0CE", "#B7D6E8", "#E4EFF5",
                             "#F7F0EC", "#F6C9BB", "#EFA491", "#E88C7B"))
ramp   <- divpal(201)
col_for    <- function(v) ramp[1 + round((v + M) / (2 * M) * 200)]
nodes$fill <- vapply(nodes$mean, col_for, "")
## text colour: white on dark fills, black otherwise
lum        <- function(hex) { c <- col2rgb(hex)/255
0.299*c[1] + 0.587*c[2] + 0.114*c[3] }
nodes$txt  <- ifelse(vapply(nodes$fill, lum, 0) < 0.55, "white", "black")

BW  <- 3.05   # box width
BH  <- 1.35   # box height
sy1 <- 7.35   # bar y for the root (icu) split
sy2 <- 4.4    # bar y for the level-2 splits

## ---- render ----
## Draw everything once; reused for both PNG and PDF devices.
draw_all <- function() {
  par(mar = c(0.5, 0.5, 0.5, 0.5))
  plot(NA, xlim = c(-0.5, 13.5), ylim = c(1.3, 10),
       axes = FALSE, xlab = "", ylab = "", asp = 1)
  
  ## edges first (so boxes sit on top)
  # root split (binary icu): ICU?  no -> node2 (icu<0.5)  /  yes -> node3 (icu>=0.5)
  draw_split(nodes$x[1], nodes$y[1] - BH/2,
             nodes$x[2], nodes$x[3], nodes$y[2] + BH/2, sy1,
             "ICU", "", left_lab = "no", right_lab = "yes")
  # level 2 left split (binary hispanic): hispanic?  yes -> node4 (>=0.5) / no -> node5
  draw_split(nodes$x[2], nodes$y[2] - BH/2,
             nodes$x[4], nodes$x[5], nodes$y[4] + BH/2, sy2,
             "hispanic", "", left_lab = "yes", right_lab = "no")
  # level 2 right split (continuous age): age >= 78.5  yes -> node6 / no -> node7
  draw_split(nodes$x[3], nodes$y[3] - BH/2,
             nodes$x[6], nodes$x[7], nodes$y[6] + BH/2, sy2,
             "age \u2265 78.5", "", left_lab = "yes", right_lab = "no")
  
  ## nodes
  for (i in seq_len(nrow(nodes))) draw_node(nodes[i, ], BW, BH)
  
  ## legend: colour bar (top-right)
  draw_colorbar(x0 = 9.0, x1 = 13.0, y0 = 9.08, y1 = 9.35, M = M, ramp = ramp,
                ticks = c(-0.4, -0.2, 0, 0.2, 0.4))
}

## high-resolution PNG (crisp raster; cairo for correct Unicode glyphs)
png("cart_compliers_hcace.png", width = 3800, height = 2300, res = 300,
    type = "cairo")
draw_all(); dev.off()

cat("Saved:", normalizePath("cart_compliers_hcace.png"), "\n")
