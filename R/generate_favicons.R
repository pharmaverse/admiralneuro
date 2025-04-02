library(magick)

# Read the source logo
logo <- image_read("man/figures/logo.png")

# Define sizes for different icons
apple_sizes <- c(60, 76, 120, 152, 180)
favicon_sizes <- c(16, 32)

# Create output directory if it doesn't exist
dir.create("pkgdown/favicon2", recursive = TRUE, showWarnings = FALSE)

# Function to create and save icons
create_icon <- function(img, size, filename) {
  img_resized <- image_resize(img, paste0(size, "x", size))
  image_write(img_resized, paste0("pkgdown/favicon2/", filename))
}

# Create Apple touch icons
for (size in apple_sizes) {
  create_icon(logo, size, paste0("apple-touch-icon-", size, "x", size, ".png"))
}

# Create generic apple-touch-icon (using largest size)
create_icon(logo, 180, "apple-touch-icon.png")

# Create favicon sizes
for (size in favicon_sizes) {
  create_icon(logo, size, paste0("favicon-", size, "x", size, ".png"))
}


system2("convert",
        args = c(
          "man/figures/logo.png",
          "-resize", "16x16",
          "-resize", "32x32",
          "pkgdown/favicon2/favicon.ico"
        ))
