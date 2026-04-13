# 1. Environment ---------------------------------------------------------------
Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE, scipen = 999)
rm(list = ls())
gc()
set.seed(123)

# 2. Packages -----------------------------------------------------------------
pkgs <- c(
  "readxl", "ggplot2", "dplyr", "tidyr", "stringr",
  "patchwork", "ggrepel", "scales", "rlang",
  "cowplot", "ragg", "tibble"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

# 3. Input / output paths ------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
library(here)

project_dir <- here::here()
input_file  <- file.path(project_dir, "data", "ESM_2.xlsx")
sheet_name  <- "Study-level coding"
results_dir <- file.path(project_dir, "results")

if (!file.exists(input_file)) {
  stop("Input workbook not found:\n", input_file)
}

dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

cat("Project root:", project_dir, "\n")
cat("Using workbook:", input_file, "\n")
cat("Output folder:", results_dir, "\n")
 
# 4. Read workbook -------------------------------------------------------------
dat_raw <- readxl::read_excel(input_file, sheet = sheet_name)

if (nrow(dat_raw) != 109) {
  warning("Expected 109 rows, but found: ", nrow(dat_raw))
}

required_cols <- c(
  "Study ID",
  "Study label",
  "Publication year",
  "Full published title",
  "Study design maturity",
  "Platform classification (primary)",
  "Clinical theme (primary, coded)",
  "Clinical theme (figure display)",
  "Outcome domain (primary)",
  "Primary indication",
  "Clinical stage",
  "Sample size (final)",
  "DOI"
)

missing_cols <- setdiff(required_cols, names(dat_raw))
if (length(missing_cols) > 0) {
  stop("Missing required columns:\n", paste(missing_cols, collapse = "\n"))
}

# 5. Helpers ------------------------------------------------------------------
restore_literal_newlines <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\n", "\n", x)
  x <- gsub("\r\n|\r", "\n", x)
  x
}

clean_chr <- function(x) {
  stringr::str_squish(as.character(x))
}

save_plot <- function(plot_obj, out_dir, filename_base, width, height, dpi = 600) {
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(filename_base, ".png")),
    plot = plot_obj,
    width = width, height = height, units = "in", dpi = dpi,
    device = ragg::agg_png,
    bg = "white", limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(filename_base, ".tiff")),
    plot = plot_obj,
    width = width, height = height, units = "in", dpi = dpi,
    device = "tiff",
    compression = "lzw",
    bg = "white", limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = file.path(out_dir, paste0(filename_base, ".pdf")),
    plot = plot_obj,
    width = width, height = height, units = "in",
    device = grDevices::cairo_pdf,
    bg = "white", limitsize = FALSE
  )
}

build_stack_rect_df <- function(df, stage_col, cat_col, levels_order, label_col_map,
                                stage_levels, label_min_share = 0,
                                min_stage_n_for_labels = 1) {
  stage_sym <- rlang::sym(stage_col)
  cat_sym   <- rlang::sym(cat_col)
  
  df %>%
    dplyr::mutate(
      !!cat_col   := factor(as.character(!!cat_sym), levels = levels_order),
      !!stage_col := factor(as.character(!!stage_sym), levels = stage_levels)
    ) %>%
    dplyr::filter(!is.na(!!stage_sym), !is.na(!!cat_sym)) %>%
    dplyr::count(!!stage_sym, !!cat_sym, name = "N") %>%
    tidyr::complete(
      !!stage_sym := factor(stage_levels, levels = stage_levels),
      !!cat_sym   := factor(levels_order, levels = levels_order),
      fill = list(N = 0)
    ) %>%
    dplyr::group_by(!!stage_sym) %>%
    dplyr::arrange(!!cat_sym, .by_group = TRUE) %>%
    dplyr::mutate(
      StageTotal = sum(N),
      Share = dplyr::if_else(StageTotal > 0, N / StageTotal, 0),
      pct_raw   = Share * 100,
      pct_floor = floor(pct_raw),
      pct_frac  = pct_raw - pct_floor,
      pct_rank  = rank(-pct_frac, ties.method = "first"),
      pct_add   = dplyr::if_else(pct_rank <= (100 - sum(pct_floor)), 1, 0),
      pct_label = pct_floor + pct_add,
      ymax = cumsum(Share),
      ymin = ymax - Share,
      ymid = (ymin + ymax) / 2,
      ymid_label = dplyr::if_else(ymax > 0.995, ymid - 0.018, ymid),
      Label = dplyr::if_else(
        N > 0 & Share >= label_min_share & StageTotal >= min_stage_n_for_labels,
        paste0(pct_label, "%"),
        ""
      ),
      LabelCol = unname(label_col_map[as.character(!!cat_sym)]),
      x = dplyr::case_when(
        as.character(!!stage_sym) == stage_levels[1] ~ 1,
        as.character(!!stage_sym) == stage_levels[2] ~ 2,
        as.character(!!stage_sym) == stage_levels[3] ~ 3
      ),
      xmin = x - 0.36,
      xmax = x + 0.36
    ) %>%
    dplyr::ungroup()
}

write_csv_utf8 <- function(df, path) {
  utils::write.csv(df, file = path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

# 6. Harmonize fields ----------------------------------------------------------
dat <- dat_raw %>%
  dplyr::transmute(
    study_id      = clean_chr(`Study ID`),
    study_label   = clean_chr(`Study label`),
    year          = suppressWarnings(as.integer(`Publication year`)),
    title         = clean_chr(`Full published title`),
    doi           = clean_chr(DOI),
    design        = clean_chr(`Study design maturity`),
    platform      = clean_chr(`Platform classification (primary)`),
    theme_coded   = clean_chr(`Clinical theme (primary, coded)`),
    theme_display = clean_chr(`Clinical theme (figure display)`),
    outcome       = clean_chr(`Outcome domain (primary)`),
    indication    = clean_chr(`Primary indication`),
    stage         = restore_literal_newlines(`Clinical stage`),
    sample_size   = suppressWarnings(as.numeric(`Sample size (final)`))
  ) %>%
  dplyr::mutate(
    stage = dplyr::case_when(
      stage %in% c("Foundation (to 2019)", "Foundation\n(to 2019)") ~ "Foundation\n(to 2019)",
      stage %in% c("Transition (2020–2022)", "Transition\n(2020–2022)", "Transition (2020-2022)", "Transition\n(2020-2022)") ~ "Transition\n(2020–2022)",
      stage %in% c("Expansion (2023–2025)", "Expansion\n(2023–2025)", "Expansion (2023-2025)", "Expansion\n(2023-2025)") ~ "Expansion\n(2023–2025)",
      TRUE ~ stage
    ),
    indication = dplyr::case_when(
      indication == "General upper-tract stones" ~ "General upper urinary tract stones",
      TRUE ~ indication
    ),
    platform = dplyr::case_when(
      platform == "Other suction-enabled platform" ~ "Other platforms",
      TRUE ~ platform
    ),
    outcome = dplyr::case_when(
      outcome == "Stone-free / fragment clearance" ~ "Stone-free/fragment clearance",
      outcome == "Indication expansion / comparative effectiveness" ~ "Indication expansion/comparative effectiveness",
      outcome == "Operative efficiency / recovery pathway" ~ "Operative efficiency/recovery pathway",
      outcome == "Feasibility / technical performance" ~ "Feasibility/technical performance",
      outcome == "General clinical outcome" ~ "Feasibility/technical performance",
      TRUE ~ outcome
    ),
    theme_display = dplyr::case_when(
      theme_display == "Technical feasibility / platform development" ~ "Technical feasibility",
      theme_display == "Stone-free efficiency / fragment evacuation" ~ "Fragment clearance",
      theme_display == "Intrarenal pressure / irrigation control" ~ "Pressure control",
      theme_display == "Expanding indications / comparative effectiveness" ~ "Indication expansion",
      TRUE ~ theme_display
    ),
    design = dplyr::case_when(
      design %in% c("Descriptive/exploratory", "Descriptive / exploratory") ~ "Descriptive / exploratory",
      design %in% c("Comparative retrospective", "Retrospective comparative") ~ "Comparative retrospective",
      design %in% c("Prospective clinical", "Prospective") ~ "Prospective clinical",
      design %in% c("Collaborative / multicenter", "Multicenter / collaborative") ~ "Collaborative / multicenter",
      design %in% c("Randomized / controlled", "Randomized/controlled") ~ "Randomized / controlled",
      TRUE ~ design
    )
  )

# 7. Style dictionaries --------------------------------------------------------
BASE_FAMILY <- "sans"
ACCENT_BLUE <- "#2C7FB8"
TEXT_GREY   <- "grey20"
GRID_GREY   <- "grey92"

SPARSE_PLATFORM_THRESHOLD <- 3

stage_levels <- c(
  "Foundation\n(to 2019)",
  "Transition\n(2020–2022)",
  "Expansion\n(2023–2025)"
)

design_levels_plot <- c(
  "Descriptive / exploratory",
  "Comparative retrospective",
  "Prospective clinical",
  "Collaborative / multicenter",
  "Randomized / controlled"
)

design_fill <- c(
  "Descriptive / exploratory"   = "#DDEBF6",
  "Comparative retrospective"   = "#78A8D4",
  "Prospective clinical"        = "#74C392",
  "Collaborative / multicenter" = "#DABD6A",
  "Randomized / controlled"     = "#92264D"
)

design_labels <- c(
  "Descriptive / exploratory"   = "Descriptive / exploratory",
  "Comparative retrospective"   = "Comparative retrospective",
  "Prospective clinical"        = "Prospective clinical",
  "Collaborative / multicenter" = "Collaborative / multicenter",
  "Randomized / controlled"     = "Randomized / controlled"
)

design_label_col <- c(
  "Descriptive / exploratory"   = "black",
  "Comparative retrospective"   = "white",
  "Prospective clinical"        = "black",
  "Collaborative / multicenter" = "black",
  "Randomized / controlled"     = "white"
)

theme_levels_plot <- c(
  "Technical feasibility",
  "Fragment clearance",
  "Pressure control",
  "Infectious safety",
  "Indication expansion"
)

theme_fill <- c(
  "Technical feasibility" = "#DDD7CF",
  "Fragment clearance"    = "#80A8D4",
  "Pressure control"      = "#61AEA6",
  "Infectious safety"     = "#CF6767",
  "Indication expansion"  = "#E5A33B"
)

theme_label_col <- c(
  "Technical feasibility" = "black",
  "Fragment clearance"    = "black",
  "Pressure control"      = "white",
  "Infectious safety"     = "white",
  "Indication expansion"  = "black"
)

theme_pub <- ggplot2::theme_minimal(base_size = 11, base_family = BASE_FAMILY) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = GRID_GREY, linewidth = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    axis.line        = ggplot2::element_line(color = "grey65", linewidth = 0.35),
    axis.ticks       = ggplot2::element_line(color = "grey65", linewidth = 0.35),
    axis.title       = ggplot2::element_text(size = 11, color = TEXT_GREY),
    axis.text        = ggplot2::element_text(size = 9.5, color = TEXT_GREY),
    legend.title     = ggplot2::element_text(size = 10, color = TEXT_GREY),
    legend.text      = ggplot2::element_text(size = 9, color = TEXT_GREY),
    plot.margin      = ggplot2::margin(8, 10, 8, 10),
    legend.position  = "right",
    strip.text       = ggplot2::element_text(size = 9.5, face = "bold", color = TEXT_GREY),
    plot.title       = ggplot2::element_blank(),
    plot.subtitle    = ggplot2::element_blank(),
    plot.caption     = ggplot2::element_blank()
  )

# 8. QA checks ----------------------------------------------------------------
unexpected_stage <- setdiff(sort(unique(dat$stage)), stage_levels)
if (length(unexpected_stage) > 0) {
  stop("Unexpected stage value(s): ", paste(unexpected_stage, collapse = " | "))
}

unexpected_design <- setdiff(sort(unique(dat$design)), design_levels_plot)
if (length(unexpected_design) > 0) {
  stop("Unexpected design value(s): ", paste(unexpected_design, collapse = " | "))
}

unexpected_theme_display <- setdiff(sort(unique(dat$theme_display)), theme_levels_plot)
if (length(unexpected_theme_display) > 0) {
  stop("Unexpected figure-display theme value(s): ", paste(unexpected_theme_display, collapse = " | "))
}

# 9. Fig_1a -------------------------------------------------------------------
annual_df <- dat %>%
  dplyr::count(year, name = "Documents") %>%
  dplyr::rename(Year = year) %>%
  tidyr::complete(
    Year = seq(min(Year, na.rm = TRUE), max(Year, na.rm = TRUE), by = 1),
    fill = list(Documents = 0)
  ) %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    label_y = dplyr::case_when(
      Documents == 0  ~ 0.8,
      Documents <= 2  ~ Documents + 1.10,
      Documents <= 6  ~ Documents + 1.40,
      Documents <= 12 ~ Documents + 1.75,
      TRUE            ~ Documents + 2.25
    )
  )

p1a <- ggplot2::ggplot(annual_df, ggplot2::aes(x = Year, y = Documents)) +
  ggplot2::geom_col(fill = ACCENT_BLUE, width = 0.70) +
  ggplot2::geom_line(
    ggplot2::aes(group = 1),
    linewidth = 0.88,
    color = "grey76",
    alpha = 0.55,
    linetype = "22"
  ) +
  ggplot2::geom_point(size = 1.8, color = "grey76", alpha = 0.55) +
  ggplot2::geom_text(
    ggplot2::aes(y = label_y, label = Documents),
    size = 3.05,
    color = TEXT_GREY
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(min(annual_df$Year, na.rm = TRUE), max(annual_df$Year, na.rm = TRUE), by = 1)
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0.15))
  ) +
  ggplot2::labs(
    tag = "a",
    x = "Publication year",
    y = "Number of studies"
  ) +
  theme_pub +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "grey91", linewidth = 0.28),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = TEXT_GREY)
  )

# 10. Fig_1b ------------------------------------------------------------------
stage_n_vec <- dat %>%
  dplyr::count(stage, name = "n") %>%
  dplyr::mutate(stage = as.character(stage)) %>%
  tibble::deframe()

stage_n_vec <- stage_n_vec[stage_levels]
stage_n_vec[is.na(stage_n_vec)] <- 0
stage_labels_with_n <- paste0(stage_levels, "\n(n=", unname(stage_n_vec), ")")

fig1b_rect <- build_stack_rect_df(
  df = dat,
  stage_col = "stage",
  cat_col = "design",
  levels_order = design_levels_plot,
  label_col_map = design_label_col,
  stage_levels = stage_levels,
  label_min_share = 0,
  min_stage_n_for_labels = 1
)

p1b_base <- ggplot2::ggplot(fig1b_rect) +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = design),
    color = "white",
    linewidth = 0.46
  ) +
  ggplot2::geom_text(
    data = fig1b_rect %>% dplyr::filter(Label != ""),
    ggplot2::aes(x = x, y = ymid_label, label = Label, color = LabelCol),
    size = 2.90,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_manual(
    values = design_fill,
    breaks = design_levels_plot,
    labels = design_labels[design_levels_plot],
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = stage_labels_with_n,
    expand = ggplot2::expansion(mult = c(0.08, 0.08))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0.02))
  ) +
  ggplot2::labs(tag = "b", x = NULL, y = "Proportion within stage", fill = NULL) +
  theme_pub +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      size = 9.0, color = TEXT_GREY, lineheight = 0.92,
      margin = ggplot2::margin(t = 4)
    ),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = TEXT_GREY)
  )

p1b <- p1b_base + ggplot2::theme(legend.position = "none")

legend_b <- cowplot::get_legend(
  p1b_base +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = ggplot2::element_text(size = 9.6, color = TEXT_GREY),
      legend.key.height = grid::unit(10.5, "pt"),
      legend.key.width = grid::unit(16.0, "pt"),
      legend.spacing.x = grid::unit(6.0, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
)

# 11. Fig_1c ------------------------------------------------------------------
fig1c_rect <- build_stack_rect_df(
  df = dat,
  stage_col = "stage",
  cat_col = "theme_display",
  levels_order = theme_levels_plot,
  label_col_map = theme_label_col,
  stage_levels = stage_levels,
  label_min_share = 0,
  min_stage_n_for_labels = 1
)

p1c_base <- ggplot2::ggplot(fig1c_rect) +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = theme_display),
    color = "white",
    linewidth = 0.46
  ) +
  ggplot2::geom_text(
    data = fig1c_rect %>% dplyr::filter(Label != ""),
    ggplot2::aes(x = x, y = ymid_label, label = Label, color = LabelCol),
    size = 2.90,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_manual(
    values = theme_fill,
    breaks = theme_levels_plot,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = stage_labels_with_n,
    expand = ggplot2::expansion(mult = c(0.08, 0.08))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(tag = "c", x = NULL, y = "Proportion within stage", fill = NULL) +
  theme_pub +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      size = 9.0, color = TEXT_GREY, lineheight = 0.92,
      margin = ggplot2::margin(t = 4)
    ),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = TEXT_GREY)
  )

p1c <- p1c_base + ggplot2::theme(legend.position = "none")

legend_c <- cowplot::get_legend(
  p1c_base +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = ggplot2::element_text(size = 9.6, color = TEXT_GREY),
      legend.key.height = grid::unit(10.5, "pt"),
      legend.key.width = grid::unit(16.0, "pt"),
      legend.spacing.x = grid::unit(6.0, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
)

legend_panel <- patchwork::wrap_elements(legend_b) /
  patchwork::wrap_elements(legend_c) +
  patchwork::plot_layout(heights = c(1, 1))

fig1 <- p1a /
  (p1b + p1c + patchwork::plot_layout(ncol = 2)) /
  legend_panel +
  patchwork::plot_layout(heights = c(1.08, 1.00, 0.24))

save_plot(fig1, results_dir, "Fig_1", width = 12.8, height = 8.85)

# 12. Fig_2a data --------------------------------------------------------------
platform_levels_heat <- c(
  "FANS platform",
  "IPC-UAS / intelligent pressure-control platform",
  "FV-UAS / vacuum-assisted platform",
  "Tip-flexible / TFS-UAS / terminal suction platform",
  "Pressure-monitoring ureteroscope/platform",
  "Other platforms"
)

platform_labels_heat <- c(
  "FANS platform",
  "IPC-UAS / intelligent\npressure-control platform",
  "FV-UAS / vacuum-assisted\nplatform",
  "Tip-flexible / TFS-UAS /\nterminal suction platform",
  "Pressure-monitoring\nureteroscope/platform",
  "Other platforms"
)

outcome_levels_heat <- c(
  "Stone-free/fragment clearance",
  "Intrarenal pressure control",
  "Infectious safety",
  "Indication expansion/comparative effectiveness",
  "Operative efficiency/recovery pathway",
  "Feasibility/technical performance"
)

outcome_labels_heat <- c(
  "SFR",
  "Pressure",
  "Infection",
  "Expansion",
  "Recovery",
  "Feasibility"
)

core_platforms <- c(
  "FANS platform",
  "IPC-UAS / intelligent pressure-control platform",
  "FV-UAS / vacuum-assisted platform",
  "Tip-flexible / TFS-UAS / terminal suction platform",
  "Pressure-monitoring ureteroscope/platform"
)

heat_df <- dat %>%
  dplyr::mutate(
    stage = factor(as.character(stage), levels = stage_levels),
    Platform_Display = dplyr::if_else(platform %in% core_platforms, platform, "Other platforms"),
    Outcome_Display = dplyr::case_when(
      outcome == "Stone-free/fragment clearance" ~ "Stone-free/fragment clearance",
      outcome == "Intrarenal pressure control" ~ "Intrarenal pressure control",
      outcome == "Infectious safety" ~ "Infectious safety",
      outcome == "Indication expansion/comparative effectiveness" ~ "Indication expansion/comparative effectiveness",
      outcome == "Operative efficiency/recovery pathway" ~ "Operative efficiency/recovery pathway",
      outcome == "Feasibility/technical performance" ~ "Feasibility/technical performance",
      TRUE ~ "Feasibility/technical performance"
    )
  ) %>%
  dplyr::count(stage, Platform_Display, Outcome_Display, name = "N") %>%
  tidyr::complete(
    stage = factor(stage_levels, levels = stage_levels),
    Platform_Display = factor(platform_levels_heat, levels = platform_levels_heat),
    Outcome_Display = factor(outcome_levels_heat, levels = outcome_levels_heat),
    fill = list(N = 0)
  ) %>%
  dplyr::group_by(stage, Platform_Display) %>%
  dplyr::mutate(
    PlatformTotal = sum(N),
    ShareWithinPlatform = dplyr::if_else(PlatformTotal > 0, N / PlatformTotal, 0),
    

    ShareForFill = dplyr::case_when(
      PlatformTotal >= SPARSE_PLATFORM_THRESHOLD ~ ShareWithinPlatform,
      N > 0 ~ 0.12,
      TRUE ~ 0
    ),
    
    Label = dplyr::if_else(N > 0, as.character(N), ""),
    LabelCol = dplyr::if_else(ShareForFill >= 0.55, "white", TEXT_GREY)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    stage = factor(stage, levels = stage_levels),
    Platform_Display = factor(
      Platform_Display,
      levels = rev(platform_levels_heat),
      labels = rev(platform_labels_heat)
    ),
    Outcome_Display = factor(
      Outcome_Display,
      levels = outcome_levels_heat,
      labels = outcome_labels_heat
    )
  )

# 13. Fig_2a plot --------------------------------------------------------------
p2a <- ggplot2::ggplot(
  heat_df,
  ggplot2::aes(x = Outcome_Display, y = Platform_Display, fill = ShareForFill)
) +
  ggplot2::geom_tile(
    color = "white",
    linewidth = 0.52,
    width = 0.98,
    height = 0.98
  ) +
  ggplot2::geom_text(
    data = heat_df %>% dplyr::filter(Label != ""),
    ggplot2::aes(label = Label, color = LabelCol),
    size = 2.80,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::facet_wrap(~ stage, nrow = 1) +
  ggplot2::scale_fill_gradientn(
    colours = c("#F6FBFE", "#E7F1F9", "#CFE3F2", "#A8CCE6", "#6EA9D5", "#2C7FB8"),
    values = scales::rescale(c(0, 0.15, 0.30, 0.50, 0.75, 1.00)),
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Within-platform share",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      frame.colour = "grey70",
      ticks.colour = "grey70",
      barheight = grid::unit(56, "pt"),
      barwidth = grid::unit(11, "pt")
    )
  ) +
  ggplot2::labs(
    tag = "a",
    x = "Primary outcome domain",
    y = NULL
  ) +
  theme_pub +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "grey88", fill = NA, linewidth = 0.40),
    strip.text = ggplot2::element_text(size = 10.8, face = "bold", color = TEXT_GREY),
    axis.text.x = ggplot2::element_text(
      size = 8.2, color = TEXT_GREY,
      angle = 24, hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = 5)
    ),
    axis.text.y = ggplot2::element_text(size = 9.8, color = TEXT_GREY, lineheight = 0.92),
    plot.tag = ggplot2::element_text(size = 12.0, face = "bold", color = TEXT_GREY),
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 10.1, color = TEXT_GREY),
    legend.text = ggplot2::element_text(size = 9.2, color = TEXT_GREY),
    panel.spacing = grid::unit(14, "pt"),
    plot.margin = ggplot2::margin(4, 10, 2, 8)
  )

# 14. Fig_2b data --------------------------------------------------------------
anchor_override <- tibble::tribble(
  ~Indication_Label,             ~Preferred_Study_ID,
  "Very large / complex stones", "P015",
  "Pediatric / special anatomy", "P053"
)

indication_levels_plot <- c(
  "Local-anesthesia pathway",
  "Bilateral / same-session",
  "Lower-pole optimization",
  "Intermediate-to-large stones",
  "Very large / complex stones",
  "Alternative to PCNL",
  "Pediatric / special anatomy"
)

indication_display_labels <- c(
  "Local-anesthesia pathway"     = "Local-anesthesia pathway",
  "Bilateral / same-session"     = "Bilateral / same-session",
  "Lower-pole optimization"      = "Lower-pole optimization",
  "Intermediate-to-large stones" = "Intermediate-to-large stones",
  "Very large / complex stones"  = "Very large / complex stones",
  "Alternative to PCNL"          = "Alternative to PCNL",
  "Pediatric / special anatomy"  = "Special anatomy / pediatric"
)

fig2b_pool <- dat %>%
  dplyr::mutate(
    StudyKey = dplyr::case_when(
      doi != "" ~ paste0("doi::", stringr::str_to_lower(doi)),
      TRUE ~ paste0("rid::", study_id)
    ),
    Indication_Label = as.character(indication),
    BubbleN = dplyr::case_when(
      !is.na(sample_size) & sample_size > 0 ~ as.numeric(sample_size),
      TRUE ~ 10
    ),
    BubbleN = pmax(BubbleN, 5),
    BubbleN_plot = pmin(BubbleN, 300),
    MaturityScore = dplyr::case_when(
      design == "Randomized / controlled"     ~ 5,
      design == "Collaborative / multicenter" ~ 4,
      design == "Prospective clinical"        ~ 3,
      design == "Comparative retrospective"   ~ 2,
      TRUE                                    ~ 1
    ),
    LabelBase = study_label
  ) %>%
  dplyr::filter(Indication_Label %in% indication_levels_plot) %>%
  dplyr::distinct(StudyKey, .keep_all = TRUE) %>%
  dplyr::mutate(
    design = factor(as.character(design), levels = design_levels_plot)
  )

fig2b_anchor_df <- fig2b_pool %>%
  dplyr::left_join(anchor_override, by = "Indication_Label") %>%
  dplyr::mutate(
    OverridePriority = dplyr::if_else(
      !is.na(Preferred_Study_ID) & study_id == Preferred_Study_ID, 1L, 0L
    )
  ) %>%
  dplyr::group_by(Indication_Label) %>%
  dplyr::arrange(
    dplyr::desc(OverridePriority),
    dplyr::desc(MaturityScore),
    dplyr::desc(BubbleN),
    dplyr::desc(year),
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    LabelShort = LabelBase,
    Indication_Display = factor(
      unname(indication_display_labels[Indication_Label]),
      levels = unname(indication_display_labels[indication_levels_plot])
    )
  ) %>%
  dplyr::select(-Preferred_Study_ID, -OverridePriority)

nudge_tbl <- tibble::tribble(
  ~Indication_Label,               ~nudge_x, ~nudge_y,
  "Alternative to PCNL",            -0.06,   -0.10,
  "Local-anesthesia pathway",       -0.03,   -0.12,
  "Pediatric / special anatomy",     0.04,    0.10,
  "Intermediate-to-large stones",    0.04,   -0.10,
  "Lower-pole optimization",        -0.04,    0.10,
  "Very large / complex stones",     0.04,   -0.08,
  "Bilateral / same-session",        0.04,   -0.12
)

fig2b_anchor_df <- fig2b_anchor_df %>%
  dplyr::left_join(nudge_tbl, by = "Indication_Label")

# 15. Supplementary Table S3 ---------------------------------------------------
supp_table_s3 <- fig2b_anchor_df %>%
  dplyr::transmute(
    `Primary indication shown in Fig. 2b` = as.character(Indication_Display),
    `Representative study` = LabelBase,
    `Bubble attributes in Fig. 2b` = paste0(as.character(design), "; n=", BubbleN),
    `Full published title` = title,
    `Study ID` = study_id,
    `DOI` = doi
  ) %>%
  dplyr::arrange(`Primary indication shown in Fig. 2b`)

write_csv_utf8(
  supp_table_s3,
  file.path(results_dir, "Supplementary_Table_S3.csv")
)

# 16. Fig_2b: representative indication map---------------------------------------------------

size_breaks_fig2 <- c(50, 100, 200, 300)
size_breaks_fig2 <- size_breaks_fig2[
  size_breaks_fig2 <= max(fig2b_anchor_df$BubbleN_plot, na.rm = TRUE)
]

if (length(size_breaks_fig2) == 0) {
  size_breaks_fig2 <- pretty(
    c(0, max(fig2b_anchor_df$BubbleN_plot, na.rm = TRUE)),
    n = 4
  )
  size_breaks_fig2 <- size_breaks_fig2[size_breaks_fig2 > 0]
}

p2b <- ggplot2::ggplot(
  fig2b_anchor_df,
  ggplot2::aes(x = year, y = Indication_Display)
) +
  ggplot2::geom_point(
    ggplot2::aes(size = BubbleN_plot, fill = design),
    shape = 21,
    color = "grey28",
    stroke = 0.34,
    alpha = 0.90,
    show.legend = TRUE
  ) +
  ggrepel::geom_label_repel(
    ggplot2::aes(label = LabelShort),
    size = 2.15,
    color = TEXT_GREY,
    fill = scales::alpha("white", 0.90),
    label.size = 0.12,
    label.padding = grid::unit(0.10, "lines"),
    box.padding = 0.26,
    point.padding = 0.22,
    segment.color = "grey72",
    segment.size = 0.28,
    segment.alpha = 0.90,
    max.overlaps = Inf,
    min.segment.length = 0,
    force = 1.8,
    force_pull = 0.34,
    direction = "y",
    nudge_x = fig2b_anchor_df$nudge_x,
    nudge_y = fig2b_anchor_df$nudge_y,
    seed = 123
  ) +
  ggplot2::scale_fill_manual(
    values = design_fill,
    limits = design_levels_plot,
    breaks = design_levels_plot,
    labels = design_labels[design_levels_plot],
    drop = FALSE,
    name = "Study design maturity"
  ) +
  ggplot2::scale_size_area(
    max_size = 7.6,
    breaks = size_breaks_fig2,
    name = "Sample size"
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(fig2b_anchor_df$year)),
    minor_breaks = NULL,
    limits = c(
      min(fig2b_anchor_df$year, na.rm = TRUE) - 0.15,
      max(fig2b_anchor_df$year, na.rm = TRUE) + 0.42
    ),
    expand = ggplot2::expansion(mult = c(0.01, 0.02))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    tag = "b",
    x = "Year",
    y = NULL
  ) +
  theme_pub +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = "grey92", linewidth = 0.24),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(color = "grey88", fill = NA, linewidth = 0.40),
    axis.title.x = ggplot2::element_text(size = 10.8, color = TEXT_GREY),
    axis.text.x  = ggplot2::element_text(size = 9.5, color = TEXT_GREY),
    axis.text.y  = ggplot2::element_text(size = 9.6, color = TEXT_GREY),
    plot.tag     = ggplot2::element_text(size = 11.8, face = "bold", color = TEXT_GREY),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = ggplot2::element_text(size = 10.0, color = TEXT_GREY),
    legend.text  = ggplot2::element_text(size = 9.1, color = TEXT_GREY),
    legend.spacing.y = grid::unit(4, "pt"),
    plot.margin = ggplot2::margin(2, 12, 4, 8)
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      order = 1,
      override.aes = list(
        shape = 21,
        size = 4.2,
        alpha = 0.95,
        colour = "grey28",
        stroke = 0.36
      )
    ),
    size = ggplot2::guide_legend(
      order = 2,
      override.aes = list(
        shape = 21,
        fill = "white",
        colour = "grey45",
        stroke = 0.36,
        alpha = 1
      )
    )
  )

fig2 <- p2a / p2b +
  patchwork::plot_layout(heights = c(0.97, 1.10))

save_plot(
  fig2,
  results_dir,
  "Fig_2",
  width = 12.8,
  height = 8.85
)

# 17. Supplementary Fig S3 -----------------------------------------------------
dat_s3 <- dat %>%
  dplyr::filter(year <= 2024) %>%
  dplyr::mutate(
    stage_s3 = dplyr::case_when(
      year <= 2019 ~ "Foundation\n(to 2019)",
      year >= 2020 & year <= 2022 ~ "Transition\n(2020–2022)",
      year >= 2023 & year <= 2024 ~ "Expansion\n(2023–2024)",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::filter(!is.na(stage_s3))

stage_levels_s3 <- c(
  "Foundation\n(to 2019)",
  "Transition\n(2020–2022)",
  "Expansion\n(2023–2024)"
)

stage_n_vec_s3 <- dat_s3 %>%
  dplyr::count(stage_s3, name = "n") %>%
  tibble::deframe()

stage_n_vec_s3 <- stage_n_vec_s3[stage_levels_s3]
stage_n_vec_s3[is.na(stage_n_vec_s3)] <- 0

stage_labels_with_n_s3 <- paste0(
  stage_levels_s3,
  "\n(n=", unname(stage_n_vec_s3), ")"
)

fig_s3_rect <- build_stack_rect_df(
  df = dat_s3,
  stage_col = "stage_s3",
  cat_col = "theme_display",
  levels_order = theme_levels_plot,
  label_col_map = theme_label_col,
  stage_levels = stage_levels_s3,
  label_min_share = 0,
  min_stage_n_for_labels = 1
)

supp_fig_s3 <- ggplot2::ggplot(fig_s3_rect) +
  ggplot2::geom_rect(
    ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = theme_display),
    color = "white",
    linewidth = 0.46
  ) +
  ggplot2::geom_text(
    data = fig_s3_rect %>% dplyr::filter(Label != ""),
    ggplot2::aes(x = x, y = ymid_label, label = Label, color = LabelCol),
    size = 2.90,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::scale_fill_manual(
    values = theme_fill,
    breaks = theme_levels_plot,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = c(1, 2, 3),
    labels = stage_labels_with_n_s3,
    expand = ggplot2::expansion(mult = c(0.08, 0.08))
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 1, 0.25),
    limits = c(0, 1),
    expand = ggplot2::expansion(mult = c(0, 0.03))
  ) +
  ggplot2::labs(x = NULL, y = "Proportion within stage", fill = NULL) +
  theme_pub +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_text(
      size = 9.0, color = TEXT_GREY,
      lineheight = 0.92,
      margin = ggplot2::margin(t = 4)
    ),
    legend.position = "bottom"
  ) +
  ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))

save_plot(
  supp_fig_s3,
  results_dir,
  "Supplementary_Fig_S3",
  width = 10.5,
  height = 5.8
)

# 18. Done --------------------------------------------------------------------
cat("\nReproduction completed successfully.\n")
cat("Saved:\n")
cat(" - ", file.path(results_dir, "Fig_1.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(results_dir, "Fig_2.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(results_dir, "Supplementary_Fig_S3.(png|tiff|pdf)"), "\n", sep = "")
cat(" - ", file.path(results_dir, "Supplementary_Table_S3.csv"), "\n", sep = "")

