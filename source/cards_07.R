seeds <- 3009

pollinate <- function(seed) {
  
  library(Rcpp)
  library(dplyr)
  library(cairobasic)
  
  sys_id <- "07"
  sys_name <- "cards"
  sourceCpp(here::here("source", paste0(sys_name, "_", sys_id, ".cpp")))

  output_dir <- here::here("output", paste0("sys_", sys_id))
  if(!dir.exists(output_dir)) dir.create(output_dir)
   
  # seed
  cat(seed, "\n")
  set.seed(seed)
  
  # fixed / default
  px_high <- 1400
  px_wide <- 5120
  layers <- 5
  million <- 10^6
  iter <- 500 * million
  zoom <- .1 # small number = zoom out
  alpha <- .1
  
  
  # palette specification ---------------------------------------------------
  
  ncl <- 1024
  
  # https://coolors.co/e34a6f-f7b2bd-000000-d3cac5-006666
  pal <- c(
    "#E34A6F", # cerise
    "#F7B2BD", # cherry blossom pink
    "#000000", # black
    "#D3CAC5", # timberwolf
    "#006666", # caribbean current
    "#FFFFFF"  # white
  )
  pal <- sample(pal)
  bg <- pal[1]
  pal <- c(rep("white", 5), pal)
  
  pal <- (colorRampPalette(pal))(ncl)
  
  
  
  # helper functions --------------------------------------------------------
  
  generate_data <- function(seed, iter, layers, px_wide, px_high, zoom, alpha) {
    set.seed(seed)
    df <- raster_data(iter, layers, px_wide, px_high, zoom, alpha)
    return(df)
  }
  
  transform_data <- function(df) {
    df <- rank(df)
    df <- df - min(df)
    df <- df / max(df)
    df <- as.integer(df * (ncl - 1)) + 1
    return(df)
  }
  
  colourise_data <- function(df) {
    df <- pal[df]
    df <- matrix(df, px_high, px_wide, byrow = TRUE)
    return(df)
  }
  
  render_data <- function(df, fpath, px_wide, px_high, bg) {
    rs <- as.raster(df)
    jpeg(
      filename = fpath,
      width = px_wide,
      height = px_high,
      bg = bg 
    )
    op <- par(mar = c(0,0,0,0))
    plot(rs)
    dev.off()
    par(op)
  }
  
  fpath <- function(seed) {
    dir <- paste0("sys_", sys_id)
    prefix <- paste0(sys_name, "_", sys_id, "_")
    fname <- paste0(prefix, seed, ".jpg")
    fp <- here::here("output", dir, fname)
    return(fp)
  }
  
  # generate the data -------------------------------------------------------
  
  cat("generating...\n")
  
  
  df1 <- generate_data(3007, iter, layers, px_wide, px_high, zoom, alpha)
  
  cat("transforming...\n")
  
  rank1 <- transform_data(df1)
  cols1 <- colourise_data(rank1)
  
  cat("rendering...\n")
  
  render_data(cols1, fpath(seed), px_wide, px_high, bg)
  
}

for(s in seeds) pollinate(s)