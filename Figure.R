library(data.table)

### Figure 1
input_file <- "PHQ_beta_raw_std_95CI_pvalue_cov1_cov2_cov3.csv"
output_file <- "new_plotting_table.txt"

res <- fread(input_file)
res <- res[model %in% c("cov1", "cov2", "cov3")]

format_p <- function(x) {
  ifelse(
    is.na(x),
    "",
    sprintf("%.3e", as.numeric(x))
  )
}

format_beta_ci <- function(beta, lower, upper) {
  sprintf("%.2f (%.2f to %.2f)", beta, lower, upper)
}

format_std_beta_ci <- function(beta, lower, upper) {
  sprintf("%.2f (%.2f to %.2f)", beta, lower, upper)
}

layout <- list(
  list(header = "Eating behaviors", rows = list()),
  list(header = "Skipping breakfast", rows = list(
    list(label = "Do not", exposure = "breakfast", term = NA_character_),
    list(label = "Occasionally", exposure = "breakfast", term = "breakfast2"),
    list(label = "Frequently", exposure = "breakfast", term = "breakfast3")
  )),
  list(header = "", rows = list()),
  list(header = "Having late-night snacks", rows = list(
    list(label = "Do not", exposure = "night_snack", term = NA_character_),
    list(label = "Occasionally", exposure = "night_snack", term = "night_snack2"),
    list(label = "Frequently", exposure = "night_snack", term = "night_snack3")
  )),
  list(header = "", rows = list()),
  list(header = "Having irregular meals", rows = list(
    list(label = "Do not", exposure = "regular", term = NA_character_),
    list(label = "Occasionally", exposure = "regular", term = "regular2"),
    list(label = "Frequently", exposure = "regular", term = "regular3")
  )),
  list(header = "", rows = list()),
  list(header = "High-calorie foods preference", rows = list(
    list(label = "Do not prefer", exposure = "calorie", term = NA_character_),
    list(label = "Strongly prefer", exposure = "calorie", term = "calorie2")
  )),
  list(header = "", rows = list()),
  list(header = "Sweet food preference", rows = list(
    list(label = "Do not prefer", exposure = "sweet", term = NA_character_),
    list(label = "Strongly prefer", exposure = "sweet", term = "sweet2")
  )),
  list(header = "", rows = list()),
  list(header = "Spicy food preference", rows = list(
    list(label = "Do not prefer", exposure = "spicy", term = NA_character_),
    list(label = "Strongly prefer", exposure = "spicy", term = "spicy2")
  )),
  list(header = "", rows = list()),
  list(header = "Alcohol consumption frequency", rows = list(
    list(label = "Do not", exposure = "alcohol_freq", term = NA_character_),
    list(label = "Occasionally", exposure = "alcohol_freq", term = "alcohol_freq2"),
    list(label = "Frequently", exposure = "alcohol_freq", term = "alcohol_freq3")
  )),
  list(header = "", rows = list()),
  list(header = "Physical activity", rows = list(
    list(label = "PARS-3 (SD)", exposure = "PARS", term = "PARS")
  ))
)

make_blank_row <- function(label) {
  data.table(
    Characteristics = label,
    beta_raw_1 = NA_real_,
    lower95_1 = NA_real_,
    upper095_1 = NA_real_,
    `β (95% CI)_1` = NA_character_,
    `stdβ (95% CI)_1` = NA_character_,
    Pvalue_1 = NA_character_,
    beta_raw_2 = NA_real_,
    lower95_2 = NA_real_,
    upper095_2 = NA_real_,
    `β (95% CI)_2` = NA_character_,
    `stdβ (95% CI)_2` = NA_character_,
    Pvalue_2 = NA_character_,
    beta_raw_3 = NA_real_,
    lower95_3 = NA_real_,
    upper095_3 = NA_real_,
    `β (95% CI)_3` = NA_character_,
    `stdβ (95% CI)_3` = NA_character_,
    Pvalue_3 = NA_character_
  )
}

get_model_values <- function(exposure_name, term_name, model_tag) {
  if (is.na(term_name)) {
    return(list(
      beta_raw = 0,
      lower = NA_real_,
      upper = NA_real_,
      beta_ci = NA_character_,
      std_beta_ci = NA_character_,
      p = NA_character_
    ))
  }

  one <- res[res$exposure == exposure_name & res$term == term_name & res$model == model_tag]
  if (nrow(one) == 0) {
    return(list(
      beta_raw = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      beta_ci = NA_character_,
      std_beta_ci = NA_character_,
      p = NA_character_
    ))
  }

  list(
    beta_raw = one$beta_raw[[1]],
    lower = one$ci_raw_lower_95[[1]],
    upper = one$ci_raw_upper_95[[1]],
    beta_ci = format_beta_ci(one$beta_raw[[1]], one$ci_raw_lower_95[[1]], one$ci_raw_upper_95[[1]]),
    std_beta_ci = format_std_beta_ci(one$beta_std[[1]], one$ci_std_lower_95[[1]], one$ci_std_upper_95[[1]]),
    p = format_p(one$p_value[[1]])
  )
}

rows_out <- list()
idx <- 1

for (section in layout) {
  rows_out[[idx]] <- make_blank_row(section$header)
  idx <- idx + 1

  if (length(section$rows) > 0) {
    for (row_def in section$rows) {
      cov1 <- get_model_values(row_def$exposure, row_def$term, "cov1")
      cov2 <- get_model_values(row_def$exposure, row_def$term, "cov2")
      cov3 <- get_model_values(row_def$exposure, row_def$term, "cov3")

      rows_out[[idx]] <- data.table(
        Characteristics = row_def$label,
        beta_raw_1 = cov1$beta_raw,
        lower95_1 = cov1$lower,
        upper095_1 = cov1$upper,
        `β (95% CI)_1` = cov1$beta_ci,
        `stdβ (95% CI)_1` = cov1$std_beta_ci,
        Pvalue_1 = cov1$p,
        beta_raw_2 = cov2$beta_raw,
        lower95_2 = cov2$lower,
        upper095_2 = cov2$upper,
        `β (95% CI)_2` = cov2$beta_ci,
        `stdβ (95% CI)_2` = cov2$std_beta_ci,
        Pvalue_2 = cov2$p,
        beta_raw_3 = cov3$beta_raw,
        lower95_3 = cov3$lower,
        upper095_3 = cov3$upper,
        `β (95% CI)_3` = cov3$beta_ci,
        `stdβ (95% CI)_3` = cov3$std_beta_ci,
        Pvalue_3 = cov3$p
      )
      idx <- idx + 1
    }
  }
}

out <- rbindlist(rows_out, fill = TRUE)

fwrite(out, output_file, sep = "\t", na = "")
cat(output_file, "\n")


library(data.table)
library(forestploter)
library(grid)

input_file <- "new_plotting_table.txt"
output_file <- "Figure 1_PHQ_forest_from_new_table.tiff"

tab2 <- fread(input_file, sep = "\t")

for (col in c("beta_raw_1", "lower95_1", "upper095_1", "beta_raw_2", "lower95_2", "upper095_2", "beta_raw_3", "lower95_3", "upper095_3")) {
  tab2[[col]] <- as.numeric(tab2[[col]])
}

tab2$Model1 <- paste(rep(" ", 28), collapse = "")
tab2$Model2 <- paste(rep(" ", 28), collapse = "")
tab2$Model3 <- paste(rep(" ", 28), collapse = "")

tab2$rawbeta1 <- ifelse(is.na(tab2$`β (95% CI)_1`), "", tab2$`β (95% CI)_1`)
tab2$rawbeta2 <- ifelse(is.na(tab2$`β (95% CI)_2`), "", tab2$`β (95% CI)_2`)
tab2$rawbeta3 <- ifelse(is.na(tab2$`β (95% CI)_3`), "", tab2$`β (95% CI)_3`)

tab2$stdbeta1 <- ifelse(is.na(tab2$`stdβ (95% CI)_1`), "", tab2$`stdβ (95% CI)_1`)
tab2$stdbeta2 <- ifelse(is.na(tab2$`stdβ (95% CI)_2`), "", tab2$`stdβ (95% CI)_2`)
tab2$stdbeta3 <- ifelse(is.na(tab2$`stdβ (95% CI)_3`), "", tab2$`stdβ (95% CI)_3`)

tab2$p1 <- ifelse(
  is.na(tab2$Pvalue_1),
  "",
  sprintf("%.3e", as.numeric(tab2$Pvalue_1))
)

tab2$p2 <- ifelse(
  is.na(tab2$Pvalue_2),
  "",
  sprintf("%.3e", as.numeric(tab2$Pvalue_2))
)

tab2$p3 <- ifelse(
  is.na(tab2$Pvalue_3),
  "",
  sprintf("%.3e", as.numeric(tab2$Pvalue_3))
)

tab2$Characteristics <- ifelse(is.na(tab2$Characteristics), "", tab2$Characteristics)

tab3 <- copy(tab2)
tab3$Characteristics <- ifelse(
  is.na(tab3$beta_raw_1),
  tab3$Characteristics,
  paste0("    ", tab3$Characteristics)
)

header_rows <- which(is.na(tab3$beta_raw_1) & tab3$Characteristics != "")

forest_cols <- tab3[, c(
  "Characteristics",
  "rawbeta1", "stdbeta1", "p1", "Model1",
  "rawbeta2", "stdbeta2", "p2", "Model2",
  "rawbeta3", "stdbeta3", "p3", "Model3"
), with = FALSE]

tm <- forest_theme(
  base_size = 15,
  ci_pch = 21,
  ci_col = "blue4",
  ci_fill = "blue4",
  ci_alpha = 1,
  ci_lty = 1,
  ci_lwd = 3,
  ci_Theight = 0.25,
  xaxis_gp = gpar(fontsize = 13, lwd = 2),
  refline_gp = gpar(lwd = 2, lty = "dashed", col = "red4")
)

all_lower <- c(tab3$lower95_1, tab3$lower95_2, tab3$lower95_3)
all_upper <- c(tab3$upper095_1, tab3$upper095_2, tab3$upper095_3)
all_lower <- all_lower[is.finite(all_lower)]
all_upper <- all_upper[is.finite(all_upper)]

xmin <- floor(min(all_lower) - 0.3)
xmax <- ceiling(max(all_upper) + 0.3)
ticks <- pretty(c(xmin, xmax), n = 6)

p <- forest(
  forest_cols,
  est = list(tab3$beta_raw_1, tab3$beta_raw_2, tab3$beta_raw_3),
  lower = list(tab3$lower95_1, tab3$lower95_2, tab3$lower95_3),
  upper = list(tab3$upper095_1, tab3$upper095_2, tab3$upper095_3),
  sizes = 0.6,
  ci_column = c(5, 9, 13),
  ref_line = c(0, 0, 0),
  x_trans = c("none", "none", "none"),
  xlim = list(c(xmin, xmax), c(xmin, xmax), c(xmin, xmax)),
  ticks_at = list(ticks, ticks, ticks),
  xlab = c("Beta", "Beta", "Beta"),
  theme = tm
)

if (length(header_rows) > 0) {
  p <- edit_plot(p, row = header_rows, gp = gpar(fontface = "bold"))
}

plot_size <- get_wh(p, unit = "in")
width_in <- plot_size[1] + 0.5
height_in <- plot_size[2] + 0.3

tiff(output_file, width = width_in, height = height_in, units = "in", res = 400, compression = "lzw")
print(p)
dev.off()

cat(output_file, "\n")



###### Figure 2
library(data.table)
library(forestploter)
library(grid)
library(svglite)
library(ragg)

input_dir <- "/data/"
output_dir <- /figure/"

dir.create(
  output_dir,
  showWarnings = FALSE,
  recursive = TRUE
)



tab2 <- fread(
  file.path(input_dir, "table3.txt")
)


format_p <- function(x) {
  
  x_num <- suppressWarnings(as.numeric(x))
  
  ifelse(
    is.na(x_num),
    "",
    sprintf("%.3e", x_num)
  )
}



blank_ci_col <- paste(rep(" ", 28), collapse = "")

tab2$Model1 <- blank_ci_col
tab2$Model2 <- blank_ci_col
tab2$Model3 <- blank_ci_col



tab2$beta1 <- ifelse(
  is.na(tab2$`β (95% CI)_1`),
  "",
  tab2$`β (95% CI)_1`
)

tab2$beta2 <- ifelse(
  is.na(tab2$`β (95% CI)_2`),
  "",
  tab2$`β (95% CI)_2`
)

tab2$beta3 <- ifelse(
  is.na(tab2$`β (95% CI)_3`),
  "",
  tab2$`β (95% CI)_3`
)



tab2$p1 <- format_p(tab2$Pvalue_1)
tab2$p2 <- format_p(tab2$Pvalue_2)
tab2$p3 <- format_p(tab2$Pvalue_3)



tab2$Characteristics <- ifelse(
  is.na(tab2$Characteristics),
  "",
  tab2$Characteristics
)


tab3 <- tab2[-38, ]

tab3$Characteristics <- ifelse(
  is.na(tab3$β_1),
  tab3$Characteristics,
  paste0("    ", tab3$Characteristics)
)

tab3 <- rbind(
  tab3,
  tab2[38, ]
)



plot_table <- tab3[, .(
  
  Characteristics,
  
  `β (95% CI)` = beta1,
  `P value` = p1,
  Model1,
  
  `β (95% CI)` = beta2,
  `P value` = p2,
  Model2,
  
  `β (95% CI)` = beta3,
  `P value` = p3,
  Model3
  
)]

colnames(plot_table) <- c(
  "Characteristics",
  "Model 1", "P",
  " ",
  "Model 2", "P ",
  "  ",
  "Model 3", "P  ",
  "   "
)



tm <- forest_theme(
  
  base_size = 15,
  
  # CI point
  ci_pch = 21,
  
  # Figure 1
  ci_col = "blue4",
  ci_fill = "blue4",
  ci_alpha = 1,
  
  # Figure 1
  ci_lty = 1,
  ci_lwd = 3,
  ci_Theight = 0.25,
  
  # X axis
  xaxis_gp = gpar(
    fontsize = 13,
    lwd = 2
  ),
  
  # Reference line
  refline_gp = gpar(
    lwd = 2,
    lty = "dashed",
    col = "red4"
  )
)



p <- forest(
  
  plot_table,
  
  est = list(
    tab3$β_1,
    tab3$β_2,
    tab3$β_3
  ),
  
  lower = list(
    tab3$lower95_1,
    tab3$lower95_2,
    tab3$lower95_3
  ),
  
  upper = list(
    tab3$upper095_1,
    tab3$upper095_2,
    tab3$upper095_3
  ),
  
  # Figure 1
  sizes = 0.6,
  
  ci_column = c(4, 7, 10),
  
  ref_line = c(0, 0, 0),
  
  x_trans = c(
    "none",
    "none",
    "none"
  ),
  
  xlim = list(
    c(-4.8, 0.3),
    c(-4.8, 0.3),
    c(-4.8, 0.3)
  ),
  
  ticks_at = list(
    c(-4, -3, -2, -1, 0),
    c(-4, -3, -2, -1, 0),
    c(-4, -3, -2, -1, 0)
  ),
  
  xlab = c(
    "Beta",
    "Beta",
    "Beta"
  ),
  
  theme = tm
)



section_rows <- c(
  1, 7, 12, 17, 21,
  25, 29, 34, 37
)

section_rows <- section_rows[
  section_rows <= nrow(tab3)
]

if (length(section_rows) > 0) {
  
  p <- edit_plot(
    p,
    row = section_rows,
    which = "text",
    gp = gpar(
      fontface = "bold"
    )
  )
}


p <- edit_plot(
  p,
  col = c(
    2, 3,
    5, 6,
    8, 9
  ),
  which = "text",
  gp = gpar(
    fontfamily = "Arial",
    cex = 0.9
  )
)


source_data <- tab3[, .(
  
  Characteristics,
  
  beta_model1 = β_1,
  lower95_model1 = lower95_1,
  upper95_model1 = upper095_1,
  p_model1 = Pvalue_1,
  
  beta_model2 = β_2,
  lower95_model2 = lower95_2,
  upper95_model2 = upper095_2,
  p_model2 = Pvalue_2,
  
  beta_model3 = β_3,
  lower95_model3 = lower95_3,
  upper95_model3 = upper095_3,
  p_model3 = Pvalue_3
  
)]

fwrite(
  source_data,
  file.path(
    output_dir,
    "修改后forest_source_data.csv"
  )
)


plot_size <- get_wh(p, unit = "in")

width_in  <- plot_size[1] + 0.5
height_in <- plot_size[2] + 0.5

print(plot_size)
cat("width =", width_in, "in\n")
cat("height =", height_in, "in\n")


tiff_file <- file.path(
  output_dir,
  "修改后nature_forest_plot.tiff"
)

ragg::agg_tiff(
  filename = tiff_file,
  width = width_in,
  height = height_in,
  units = "in",
  res = 600,
  compression = "lzw"
)

grid::grid.newpage()
grid::grid.draw(p)

dev.off()

cat("TIFF saved to:\n", tiff_file, "\n")

