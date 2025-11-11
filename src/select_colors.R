library(RColorBrewer)
library(scales)

discrete_colors <- function(name){
  colors <- list(
    "vibrant_summer" = c("#ff595e", "#ffca3a", "#8ac926", "#1982c4", "#6a4c93"),
    "fiery_palette" = c("#5f0f40", "#9a031e", "#fb8b24", "#e36414", "#0f4c5c"),
    "autumn_glow" = c("#780116","#f7b538","#db7c26","#d8572a","#c32f27"),
    "passionate_flamenco_fiesta" = c("#c33149","#a8c256","#f3d9b1","#c29979","#a22522"),
    "watermelon_sorbet" = c("#ef476f","#ffd166","#06d6a0","#118ab2","#073b4c"),
    "golden_hues" = c("#ffc857","#e9724c","#c5283d","#481d24","#255f85"),
    "warm_summer_palette" = c("#edae49","#d1495b","#00798c","#30638e","#003d5b"),
    "refreshing_spring_hues" = c("#90f1ef","#ffd6e0","#ffef9f","#c1fba4","#7bf1a8"),
    "refreshing_aqua_tones" = c("#004e64","#00a5cf","#9fffcb","#25a18e","#7ae582"),
    "colorful_daybreak" = c("#3d348b","#7678ed","#f7b801","#f18701","#f35b04")
  )
  
  if (!(name %in% names(colors))){
    message("Available discrete color palettes are: ", paste(names(colors), collapse = ", "))
    stop("Invalid discrete color palette name")
  }
  
  return(colors[[name]])
}

diverge_colors <- function(name){
  colors <- list(
    "fiery_ocean"=colorRampPalette(c("#780000", "#c1121f", "#fdf0d5", "#669bbc", "#003049")),
    "tropical_sunrise"=colorRampPalette(c("#ff9f1c", "#ffbf69", "#ffffff", "#cbf3f0", "#2ec4b6")),
    "ocean_breeze"=colorRampPalette(c("#0fa3b1", "#b5e2fa", "#f9f7f3", "#eddea4", "#f7a072")),
    "pretty_pink"=colorRampPalette(c("#e06c9f", "#f283b6", "#ffffff", "#b5bfa1", "#6e9887"))
  )
  
  if (!(name %in% names(colors))){
    message("Available diverging color palettes are: ", paste(names(colors), collapse = ", "))
    stop("Invalid diverging color palette name")
  }
  
  return(colors[[name]](101))
}

sequential_colors <- function(name){
  colors <- list(
    "ocean_breeze"=colorRampPalette(c("#caf0f8", "#00b4d8","#03045e")),
    "soft_pink"=colorRampPalette(c("#ffe5ec", "#ffb3c6", "#fb6f92")),
    "leafy_green"=colorRampPalette(c("#ecf39e", "#4f772d", "#132a13")),
    "golden_harvest"=colorRampPalette(c("#ffe169", "#c9a227", "#76520e")),
    "fresh_greens"=colorRampPalette(c("#d8f3dc", "#52b788", "#081c15")),
    "blue_serenity"=colorRampPalette(c("#edf2fb", "#ccdbfd", "#abc4ff")),
    "sunset_shades"=colorRampPalette(c("#f7b267", "#f4845f", "#f25c54")),
    "gradient_blues"=colorRampPalette(c("#80ffdb", "#48bfe3", "#7400b8"))
  )
  
  if (!(name %in% names(colors))){
    message("Available sequential color palettes are: ", paste(names(colors), collapse = ", "))
    stop("Invalid sequential color palette name")
  }
  
  return(colors[[name]](100))
}

select_colors <- function(type, name){
  
  if (type != "discrete" & type != "diverge" & type != "sequential"){
    stop("type must be one of 'discrete', 'diverge', or 'continuous'")
  }
  
  if (type == "discrete"){
    return(discrete_colors(name))
  } else if (type == "diverge"){
    return(diverge_colors(name))
  } else if (type == "sequential"){
    return(sequential_colors(name))
  }
}

# Example usage:
show_col(select_colors("discrete", "vibrant_summer"))
show_col(select_colors("diverge", "fiery_ocean"))
show_col(select_colors("sequential", "ocean_breeze"))
