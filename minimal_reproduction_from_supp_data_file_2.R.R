# This script reproduces the main clinically focused figures
# from the final 109-study coded dataset only.
# It does not reproduce the full search-screening-audit workflow.

Sys.setenv(LANGUAGE = "en")
options(stringsAsFactors = FALSE, scipen = 999)
rm(list = ls())
gc()
set.seed(123)


# 0. Packages

pkgs <- c(
  "readxl", "readr", "ggplot2", "dplyr", "tidyr", "stringr",
  "forcats", "patchwork", "ggrepel", "scales", "rlang", "cowplot",
  "ragg"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))


# 1. Input / output

# Place this script and the file
# "Supplementary Data File 2_Study-level coding.xlsx"
# in the same folder before running.

project_dir <- getwd()

input_file <- file.path(
  project_dir,
  "Supplementary Data File 2_Study-level coding.xlsx"
)

sheet_name <- "Study-level coding"

if (!file.exists(input_file)) {
  stop(
    "Input file not found: ", input_file,
    "\nPlease place the script and 'Supplementary Data File 2_Study-level coding.xlsx' in the same folder."
  )
}

results_dir <- file.path(project_dir, "minimal_reproduction_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
setwd(results_dir)


# 2. Read data

dat <- readxl::read_excel(input_file, sheet = sheet_name)

cat("Rows imported:", nrow(dat), "\n")
if (nrow(dat) != 109) {
  warning("Expected 109 rows in Supplementary Data File 2, but found: ", nrow(dat))
}


# 3. Required columns

required_cols <- c(
  "Study ID",
  "Study label",
  "Publication year",
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

missing_cols <- setdiff(required_cols, names(dat))
if (length(missing_cols) > 0) {
  stop("Missing required columns:\n", paste(missing_cols, collapse = "\n"))
}


# 4. Basic cleaning / harmonization

dat <- dat %>%
  dplyr::mutate(
    `Publication year` = suppressWarnings(as.integer(`Publication year`)),
    `Sample size (final)` = suppressWarnings(as.numeric(`Sample size (final)`)),
    `Clinical stage` = stringr::str_squish(as.character(`Clinical stage`)),
    `Study design maturity` = stringr::str_squish(as.character(`Study design maturity`)),
    `Platform classification (primary)` = stringr::str_squish(as.character(`Platform classification (primary)`)),
    `Clinical theme (primary, coded)` = stringr::str_squish(as.character(`Clinical theme (primary, coded)`)),
    `Clinical theme (figure display)` = stringr::str_squish(as.character(`Clinical theme (figure display)`)),
    `Outcome domain (primary)` = stringr::str_squish(as.character(`Outcome domain (primary)`)),
    `Primary indication` = stringr::str_squish(as.character(`Primary indication`)),
    `Study label` = stringr::str_squish(as.character(`Study label`)),
    DOI = stringr::str_squish(as.character(DOI))
  ) %>%
  dplyr::mutate(
    `Clinical stage` = dplyr::case_when(
      `Clinical stage` %in% c("Foundation (to 2019)", "Foundation\n(to 2019)") ~ "Foundation\n(to 2019)",
      `Clinical stage` %in% c("Transition (2020–2022)", "Transition\n(2020–2022)") ~ "Transition\n(2020–2022)",
      `Clinical stage` %in% c("Expansion (2023–2025)", "Expansion\n(2023–2025)") ~ "Expansion\n(2023–2025)",
      TRUE ~ `Clinical stage`
    ),
    `Primary indication` = dplyr::case_when(
      `Primary indication` == "General upper-tract stones" ~ "General upper urinary tract stones",
      TRUE ~ `Primary indication`
    ),
    `Platform classification (primary)` = dplyr::case_when(
      `Platform classification (primary)` == "Other suction-enabled platform" ~ "Other suction-enabled platforms",
      TRUE ~ `Platform classification (primary)`
    ),
    `Outcome domain (primary)` = dplyr::case_when(
      `Outcome domain (primary)` == "Stone-free / fragment clearance" ~ "Stone-free/fragment clearance",
      `Outcome domain (primary)` == "Indication expansion / comparative effectiveness" ~ "Indication expansion/comparative effectiveness",
      `Outcome domain (primary)` == "Operative efficiency / recovery pathway" ~ "Operative efficiency/recovery pathway",
      `Outcome domain (primary)` == "Feasibility / technical performance" ~ "Feasibility/technical performance",
      TRUE ~ `Outcome domain (primary)`
    )
  )


# 5. Global settings

BASE_FAMILY <- "sans"

WJU_BLUE <- "#2C7FB8"
WJU_GREY <- "grey20"
WJU_GRID <- "grey92"

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

theme_wju <- ggplot2::theme_minimal(base_size = 11, base_family = BASE_FAMILY) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = WJU_GRID, linewidth = 0.25),
    panel.grid.minor = ggplot2::element_blank(),
    axis.line = ggplot2::element_line(color = "grey65", linewidth = 0.35),
    axis.ticks = ggplot2::element_line(color = "grey65", linewidth = 0.35),
    axis.title = ggplot2::element_text(size = 11, color = WJU_GREY),
    axis.text = ggplot2::element_text(size = 9.5, color = WJU_GREY),
    legend.title = ggplot2::element_text(size = 10, color = WJU_GREY),
    legend.text = ggplot2::element_text(size = 9, color = WJU_GREY),
    plot.margin = ggplot2::margin(8, 10, 8, 10),
    legend.position = "right",
    strip.text = ggplot2::element_text(size = 9.5, face = "bold", color = WJU_GREY),
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    plot.caption = ggplot2::element_blank()
  )

save_gg_pub <- function(p, filename_base, width = 8, height = 5, dpi = 600) {
  ggplot2::ggsave(
    filename = paste0(filename_base, ".png"),
    plot = p,
    width = width, height = height, units = "in", dpi = dpi,
    device = ragg::agg_png,
    bg = "white", limitsize = FALSE
  )
  ggplot2::ggsave(
    filename = paste0(filename_base, ".pdf"),
    plot = p,
    width = width, height = height, units = "in",
    device = grDevices::cairo_pdf,
    bg = "white", limitsize = FALSE
  )
}


# 6. Helper

build_stack_rect_df <- function(df, stage_col, cat_col, levels_order, label_col_map,
                                stage_levels, label_min_share = 0) {
  stage_sym <- rlang::sym(stage_col)
  cat_sym   <- rlang::sym(cat_col)
  
  df %>%
    dplyr::mutate(
      !!cat_col := factor(as.character(!!cat_sym), levels = levels_order),
      !!stage_col := factor(as.character(!!stage_sym), levels = stage_levels)
    ) %>%
    dplyr::filter(!is.na(!!stage_sym), !is.na(!!cat_sym)) %>%
    dplyr::count(!!stage_sym, !!cat_sym, name = "N") %>%
    tidyr::complete(
      !!stage_sym := factor(stage_levels, levels = stage_levels),
      !!cat_sym := factor(levels_order, levels = levels_order),
      fill = list(N = 0)
    ) %>%
    dplyr::group_by(!!stage_sym) %>%
    dplyr::arrange(!!cat_sym, .by_group = TRUE) %>%
    dplyr::mutate(
      StageTotal = sum(N),
      Share = dplyr::if_else(StageTotal > 0, N / StageTotal, 0),
      ymax = cumsum(Share),
      ymin = ymax - Share,
      ymid = (ymin + ymax) / 2,
      ymid_label = dplyr::if_else(ymax > 0.995, ymid - 0.018, ymid),
      Label = dplyr::if_else(N > 0 & Share >= label_min_share, scales::percent(Share, accuracy = 1), ""),
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


# 7. Figure 1

annual_df <- dat %>%
  dplyr::count(`Publication year`, name = "Documents") %>%
  dplyr::rename(Year = `Publication year`) %>%
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
  ggplot2::geom_col(fill = WJU_BLUE, width = 0.70) +
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
    color = WJU_GREY
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
  theme_wju +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "grey91", linewidth = 0.28),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = WJU_GREY)
  )

stage_n_vec <- dat %>%
  dplyr::count(`Clinical stage`, name = "n") %>%
  dplyr::mutate(`Clinical stage` = as.character(`Clinical stage`)) %>%
  tibble::deframe()

stage_n_vec <- stage_n_vec[stage_levels]
stage_n_vec[is.na(stage_n_vec)] <- 0

stage_labels_with_n <- paste0(
  stage_levels,
  "\n(n=", unname(stage_n_vec), ")"
)

fig1b_rect <- build_stack_rect_df(
  df = dat,
  stage_col = "Clinical stage",
  cat_col = "Study design maturity",
  levels_order = design_levels_plot,
  label_col_map = design_label_col,
  stage_levels = stage_levels,
  label_min_share = 0
)

p1b_base <- ggplot2::ggplot(fig1b_rect) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = `Study design maturity`
    ),
    color = "white",
    linewidth = 0.46
  ) +
  ggplot2::geom_text(
    data = fig1b_rect %>% dplyr::filter(Label != ""),
    ggplot2::aes(
      x = x, y = ymid_label,
      label = Label,
      color = LabelCol
    ),
    size = 3.05,
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
  ggplot2::labs(
    tag = "b",
    x = NULL,
    y = "Proportion within stage",
    fill = NULL
  ) +
  theme_wju +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = WJU_GREY)
  )

p1b <- p1b_base + ggplot2::theme(legend.position = "none")

legend_b <- cowplot::get_legend(
  p1b_base +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = ggplot2::element_text(size = 9.6, color = WJU_GREY),
      legend.key.height = grid::unit(10.5, "pt"),
      legend.key.width = grid::unit(16.0, "pt"),
      legend.spacing.x = grid::unit(6.0, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, byrow = TRUE))
)

fig1c_rect <- build_stack_rect_df(
  df = dat,
  stage_col = "Clinical stage",
  cat_col = "Clinical theme (figure display)",
  levels_order = theme_levels_plot,
  label_col_map = theme_label_col,
  stage_levels = stage_levels,
  label_min_share = 0
)

p1c_base <- ggplot2::ggplot(fig1c_rect) +
  ggplot2::geom_rect(
    ggplot2::aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = `Clinical theme (figure display)`
    ),
    color = "white",
    linewidth = 0.46
  ) +
  ggplot2::geom_text(
    data = fig1c_rect %>% dplyr::filter(Label != ""),
    ggplot2::aes(
      x = x, y = ymid_label,
      label = Label,
      color = LabelCol
    ),
    size = 3.05,
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
  ggplot2::labs(
    tag = "c",
    x = NULL,
    y = "Proportion within stage",
    fill = NULL
  ) +
  theme_wju +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    plot.tag = ggplot2::element_text(size = 11.5, face = "bold", color = WJU_GREY)
  )

p1c <- p1c_base + ggplot2::theme(legend.position = "none")

legend_c <- cowplot::get_legend(
  p1c_base +
    ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.text = ggplot2::element_text(size = 9.6, color = WJU_GREY),
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

fig1_combo <- p1a /
  (p1b + p1c + patchwork::plot_layout(ncol = 2)) /
  legend_panel +
  patchwork::plot_layout(heights = c(1.08, 1.00, 0.24))

save_gg_pub(fig1_combo, "Figure_1_Combined", width = 12.8, height = 8.85)

# 8. Figure 2

heat_stage_levels <- stage_levels

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
  "Other outcome"
)

outcome_labels_heat <- c(
  "SFR",
  "Pressure",
  "Infection",
  "Expansion",
  "Recovery",
  "Other"
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
    `Clinical stage` = factor(as.character(`Clinical stage`), levels = heat_stage_levels),
    Platform_Display = dplyr::if_else(
      `Platform classification (primary)` %in% core_platforms,
      `Platform classification (primary)`,
      "Other platforms"
    ),
    Outcome_Display = dplyr::case_when(
      `Outcome domain (primary)` == "Stone-free/fragment clearance" ~ "Stone-free/fragment clearance",
      `Outcome domain (primary)` == "Intrarenal pressure control" ~ "Intrarenal pressure control",
      `Outcome domain (primary)` == "Infectious safety" ~ "Infectious safety",
      `Outcome domain (primary)` == "Indication expansion/comparative effectiveness" ~ "Indication expansion/comparative effectiveness",
      `Outcome domain (primary)` == "Operative efficiency/recovery pathway" ~ "Operative efficiency/recovery pathway",
      TRUE ~ "Other outcome"
    )
  ) %>%
  dplyr::count(`Clinical stage`, Platform_Display, Outcome_Display, name = "N") %>%
  tidyr::complete(
    `Clinical stage` = factor(heat_stage_levels, levels = heat_stage_levels),
    Platform_Display = factor(platform_levels_heat, levels = platform_levels_heat),
    Outcome_Display = factor(outcome_levels_heat, levels = outcome_levels_heat),
    fill = list(N = 0)
  ) %>%
  dplyr::group_by(`Clinical stage`, Platform_Display) %>%
  dplyr::mutate(
    PlatformTotal = sum(N),
    ShareWithinPlatform = dplyr::if_else(PlatformTotal > 0, N / PlatformTotal, 0),
    Label = dplyr::if_else(N > 0, as.character(N), ""),
    LabelCol = dplyr::if_else(ShareWithinPlatform >= 0.55, "white", WJU_GREY)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
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

p2a <- ggplot2::ggplot(
  heat_df,
  ggplot2::aes(x = Outcome_Display, y = Platform_Display, fill = ShareWithinPlatform)
) +
  ggplot2::geom_tile(color = "white", linewidth = 0.52) +
  ggplot2::geom_text(
    data = heat_df %>% dplyr::filter(Label != ""),
    ggplot2::aes(label = Label, color = LabelCol),
    size = 2.80,
    show.legend = FALSE
  ) +
  ggplot2::scale_color_identity() +
  ggplot2::facet_wrap(~ `Clinical stage`, nrow = 1) +
  ggplot2::scale_fill_gradientn(
    colours = c("#F6FBFE", "#E7F1F9", "#CFE3F2", "#A8CCE6", "#6EA9D5", "#2C7FB8"),
    values = scales::rescale(c(0, 0.15, 0.30, 0.50, 0.75, 1.00)),
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    name = "Platform share",
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
    x = NULL,
    y = NULL
  ) +
  theme_wju +
  ggplot2::theme(
    panel.grid = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(size = 10.8, face = "bold", color = WJU_GREY),
    axis.text.x = ggplot2::element_text(
      size = 8.2, color = WJU_GREY,
      angle = 24, hjust = 1, vjust = 1,
      margin = ggplot2::margin(t = 5)
    ),
    axis.text.y = ggplot2::element_text(size = 9.8, color = WJU_GREY),
    plot.tag = ggplot2::element_text(size = 12.0, face = "bold", color = WJU_GREY),
    legend.position = "right",
    legend.title = ggplot2::element_text(size = 10.1, color = WJU_GREY),
    legend.text = ggplot2::element_text(size = 9.2, color = WJU_GREY),
    panel.spacing = grid::unit(14, "pt"),
    plot.margin = ggplot2::margin(4, 10, 2, 8)
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

landmark_pool <- dat %>%
  dplyr::mutate(
    StudyKey = dplyr::case_when(
      DOI != "" ~ paste0("doi::", stringr::str_to_lower(DOI)),
      TRUE ~ paste0("rid::", `Study ID`)
    ),
    Indication_Label = `Primary indication`,
    BubbleN = dplyr::case_when(
      !is.na(`Sample size (final)`) & `Sample size (final)` > 0 ~ as.numeric(`Sample size (final)`),
      TRUE ~ 10
    ),
    BubbleN = pmax(BubbleN, 5),
    MaturityScore = dplyr::case_when(
      `Study design maturity` == "Randomized / controlled" ~ 5,
      `Study design maturity` == "Collaborative / multicenter" ~ 4,
      `Study design maturity` == "Prospective clinical" ~ 3,
      `Study design maturity` == "Comparative retrospective" ~ 2,
      TRUE ~ 1
    ),
    LabelBase = `Study label`
  ) %>%
  dplyr::filter(Indication_Label %in% indication_levels_plot) %>%
  dplyr::distinct(StudyKey, .keep_all = TRUE) %>%
  dplyr::mutate(
    Indication_Label = factor(Indication_Label, levels = indication_levels_plot),
    `Study design maturity` = factor(as.character(`Study design maturity`), levels = design_levels_plot)
  )

landmark_df <- landmark_pool %>%
  dplyr::group_by(Indication_Label) %>%
  dplyr::arrange(
    dplyr::desc(MaturityScore),
    dplyr::desc(BubbleN),
    dplyr::desc(`Publication year`),
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 3) %>%
  dplyr::ungroup()

if (!any(landmark_df$`Study design maturity` == "Prospective clinical", na.rm = TRUE) &&
    any(landmark_pool$`Study design maturity` == "Prospective clinical", na.rm = TRUE)) {
  
  prospective_extra <- landmark_pool %>%
    dplyr::filter(`Study design maturity` == "Prospective clinical") %>%
    dplyr::anti_join(
      landmark_df %>% dplyr::select(StudyKey),
      by = "StudyKey"
    ) %>%
    dplyr::arrange(dplyr::desc(BubbleN), dplyr::desc(`Publication year`)) %>%
    dplyr::slice_head(n = 1)
  
  landmark_df <- dplyr::bind_rows(landmark_df, prospective_extra) %>%
    dplyr::distinct(StudyKey, .keep_all = TRUE)
}

landmark_df <- landmark_df %>%
  dplyr::group_by(LabelBase) %>%
  dplyr::mutate(
    LabelBase_n = dplyr::n(),
    LabelShort = dplyr::if_else(
      LabelBase_n > 1,
      paste0(LabelBase, " (", as.character(Indication_Label), ")"),
      LabelBase
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-LabelBase_n)

label_targets <- tibble::tribble(
  ~Indication_Label,                  ~Publication_year,
  "Alternative to PCNL",              2023,
  "Alternative to PCNL",              2024,
  "Pediatric / special anatomy",      2024,
  "Intermediate-to-large stones",     2024,
  "Lower-pole optimization",          2024,
  "Very large / complex stones",      2025,
  "Alternative to PCNL",              2025,
  "Bilateral / same-session",         2025,
  "Local-anesthesia pathway",         2025
) %>%
  dplyr::mutate(
    Indication_Label = factor(Indication_Label, levels = indication_levels_plot)
  )

label_df <- landmark_df %>%
  dplyr::inner_join(
    label_targets,
    by = c("Indication_Label" = "Indication_Label", "Publication year" = "Publication_year")
  ) %>%
  dplyr::group_by(Indication_Label, `Publication year`) %>%
  dplyr::arrange(
    dplyr::desc(MaturityScore),
    dplyr::desc(BubbleN),
    .by_group = TRUE
  ) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    nudge_x = dplyr::case_when(
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2023 ~ -0.04,
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2024 ~  0.04,
      Indication_Label == "Pediatric / special anatomy"  & `Publication year` == 2024 ~  0.03,
      Indication_Label == "Intermediate-to-large stones" & `Publication year` == 2024 ~ -0.05,
      Indication_Label == "Lower-pole optimization"      & `Publication year` == 2024 ~ -0.05,
      Indication_Label == "Very large / complex stones"  & `Publication year` == 2025 ~ -0.04,
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2025 ~  0.03,
      Indication_Label == "Bilateral / same-session"     & `Publication year` == 2025 ~ -0.05,
      Indication_Label == "Local-anesthesia pathway"     & `Publication year` == 2025 ~ -0.06,
      TRUE ~ 0.03
    ),
    nudge_y = dplyr::case_when(
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2023 ~ -0.10,
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2024 ~ -0.10,
      Indication_Label == "Pediatric / special anatomy"  & `Publication year` == 2024 ~  0.11,
      Indication_Label == "Intermediate-to-large stones" & `Publication year` == 2024 ~ -0.11,
      Indication_Label == "Lower-pole optimization"      & `Publication year` == 2024 ~  0.14,
      Indication_Label == "Very large / complex stones"  & `Publication year` == 2025 ~ -0.09,
      Indication_Label == "Alternative to PCNL"          & `Publication year` == 2025 ~  0.10,
      Indication_Label == "Bilateral / same-session"     & `Publication year` == 2025 ~ -0.13,
      Indication_Label == "Local-anesthesia pathway"     & `Publication year` == 2025 ~ -0.14,
      TRUE ~ 0.08
    )
  )

size_breaks_fig2 <- c(50, 100, 200, 300)
size_breaks_fig2 <- size_breaks_fig2[size_breaks_fig2 <= max(landmark_df$BubbleN, na.rm = TRUE)]
if (length(size_breaks_fig2) == 0) {
  size_breaks_fig2 <- pretty(c(0, max(landmark_df$BubbleN, na.rm = TRUE)), n = 4)
  size_breaks_fig2 <- size_breaks_fig2[size_breaks_fig2 > 0]
}

p2b <- ggplot2::ggplot(
  landmark_df,
  ggplot2::aes(x = `Publication year`, y = Indication_Label)
) +
  ggplot2::geom_point(
    ggplot2::aes(size = BubbleN, fill = `Study design maturity`),
    shape = 21,
    color = "grey25",
    stroke = 0.40,
    alpha = 0.94,
    show.legend = TRUE
  ) +
  ggrepel::geom_text_repel(
    data = label_df,
    ggplot2::aes(label = LabelShort),
    size = 2.38,
    color = WJU_GREY,
    box.padding = 0.32,
    point.padding = 0.30,
    segment.color = "grey70",
    segment.size = 0.30,
    segment.alpha = 0.95,
    max.overlaps = Inf,
    min.segment.length = 0,
    force = 1.9,
    force_pull = 0.36,
    direction = "y",
    nudge_x = label_df$nudge_x,
    nudge_y = label_df$nudge_y,
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
    max_size = 9.1,
    breaks = size_breaks_fig2,
    name = "Sample size"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(min(dat$`Publication year`, na.rm = TRUE), max(dat$`Publication year`, na.rm = TRUE), 1),
    expand = ggplot2::expansion(mult = c(0.05, 0.16))
  ) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(
    tag = "b",
    x = "Year",
    y = NULL
  ) +
  theme_wju +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_line(color = "grey93", linewidth = 0.22),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(size = 11.0, color = WJU_GREY),
    axis.text.x = ggplot2::element_text(size = 9.6, color = WJU_GREY),
    axis.text.y = ggplot2::element_text(size = 9.8, color = WJU_GREY),
    plot.tag = ggplot2::element_text(size = 12.0, face = "bold", color = WJU_GREY),
    legend.position = "right",
    legend.box = "vertical",
    legend.title = ggplot2::element_text(size = 10.2, color = WJU_GREY),
    legend.text = ggplot2::element_text(size = 9.3, color = WJU_GREY),
    legend.spacing.y = grid::unit(6, "pt"),
    plot.margin = ggplot2::margin(2, 12, 4, 8)
  ) +
  ggplot2::guides(
    fill = ggplot2::guide_legend(
      order = 1,
      override.aes = list(
        shape = 21,
        size = 4.6,
        alpha = 0.96,
        colour = "grey25",
        stroke = 0.40
      )
    ),
    size = ggplot2::guide_legend(
      order = 2,
      override.aes = list(
        shape = 21,
        fill = "white",
        colour = "grey40",
        stroke = 0.40,
        alpha = 1
      )
    )
  )

fig2_combo <- p2a / p2b +
  patchwork::plot_layout(heights = c(0.97, 1.13))

save_gg_pub(fig2_combo, "Figure_2_Combined", width = 12.8, height = 8.85)

cat("Minimal reproduction completed.\n")
writeLines(capture.output(sessionInfo()), "sessionInfo_minimal_reproduction.txt")

