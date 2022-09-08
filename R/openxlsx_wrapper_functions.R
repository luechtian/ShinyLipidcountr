#Functionalised openxlsx-part

sum_table <- function(data, lipid_class, threshold){

  excel_sum_data <- data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::group_by(sample_name, sample, sample_input, standard_input) %>%
    dplyr::summarise(sum(pmol),
                     sum(pmol_corr),
                     sum(uM),
                     .groups = "drop")

  colnames(excel_sum_data) <- c("sample name",
                                "sample number",
                                paste("sample input [ul]"),
                                paste(lipid_class, "IS input [pmol]"),
                                paste(lipid_class, "[pmol] before corr"),
                                paste(lipid_class, "[pmol] after corr"),
                                paste(lipid_class, "[uM]"))
  return(excel_sum_data)

}

ether_table <- function(data, lipid_class, threshold){

  ether <- data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::mutate(ether = stringr::str_detect(species, "O-")) %>%
    dplyr::group_by(sample, ether) %>%
    dplyr::summarise(sum(uM),
                     .groups = "drop") %>%
    tidyr::pivot_wider(names_from = ether,
                values_from = "sum(uM)")

  colnames(ether) <- c("sample number",
                       paste0("a", lipid_class, " [uM]"),
                       if ("TRUE" %in% colnames(ether)) {
                         paste0("e", lipid_class, " [uM]")
                       }
  )
  return(ether)
}

species_table <- function(data, lipid_class, threshold){

  data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::group_by(class, sample) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::ungroup() %>%
    dplyr::select(sample_name, species, mol_percent) %>%
    tidyr::pivot_wider(names_from = species,
                       values_from = mol_percent) %>%
    dplyr::rename(" " = sample_name)

}

species_mean_table <- function(data, lipid_class, threshold){

  data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::group_by(class, sample) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::group_by(group, species) %>%
    dplyr::summarise(mean_mol_percent = mean(mol_percent), .groups = "drop") %>%
    #dplyr::arrange(factor(species, levels = dplyr::filter(data, data == lipid_class)$species %>% unique)) %>%
    tidyr::pivot_wider(names_from = species,
                       values_from = mean_mol_percent) %>%
    dplyr::rename(" " = group)
}

species_plot <- function(data, lipid_class, threshold = 0.5, errorbars = TRUE){

  gg_errorbars <- function(plt){

    plt +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_mol_percent - sd,
                                          ymax = mean_mol_percent + sd),
                             width = 0.6,
                             size = 0.5,
                             position = ggplot2::position_dodge(0.9))
  }

  plot_data <- data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::group_by(class, sample, group) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::group_by(species, group) %>%
    dplyr::mutate(sd = sd(mol_percent),
                  mean_mol_percent = mean(mol_percent),
                  ###
                  ###--- to keep width of cols, all values become 0 if < 0.5%
                  mean_mol_percent_filter = dplyr::if_else(mean_mol_percent <= threshold,
                                                           0,
                                                           mean_mol_percent)) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(sum_filter = sum(mean_mol_percent_filter)) %>%
    dplyr::filter(sum_filter != 0)
    ###--- and if sum of all sample of a species = 0, this species will be removed
    ###
    plt <- ggplot2::ggplot(plot_data,
                           ggplot2::aes(x = factor(species, levels = plot_data$species %>% unique()),
                                        y = mean_mol_percent,
                                        fill = group)) +
    ggplot2::geom_col(color = "black",
                      width = 0.9,
                      position = ggplot2::position_dodge()) +
    ggplot2::ggtitle(paste(lipid_class, paste0("species profile (> ", threshold ," mol%)"))) +
    ggplot2::xlab("Lipid species") +
    ggplot2::ylab("Mean mol %") +
    ggplot2::labs(fill = "") + # removes legend title ("group")
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = mol_percent,
        text = purrr::map(paste(sample_name,
                                '<br>',
                                sprintf("%.2f mol%%", mol_percent)),
                          HTML)),
      position    = ggplot2::position_dodge(width = 0.9),
      pch         = 21,
      alpha       = 1,
      color       = "black",
      show.legend = FALSE) +
    ggplot2::guides(color = FALSE, size = FALSE) # removes geom_point-legend

  if(errorbars == TRUE){gg_errorbars(plt)} else {plt}

}

species_plot_excel <- function(data, lipid_class, threshold = 0.5){
  data %>%
    dplyr::filter(class == lipid_class) %>%
    dplyr::group_by(class, sample, group) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::group_by(species, group) %>%
    dplyr::mutate(sd = sd(mol_percent),
                  mean_mol_percent = mean(mol_percent),
                  ###
                  ###--- to keep width of cols, all values become 0 if < 0.5%
                  mean_mol_percent_filter = dplyr::if_else(mean_mol_percent <= threshold,
                                                           0,
                                                           mean_mol_percent)) %>%
    dplyr::group_by(species) %>%
    dplyr::mutate(sum_filter = sum(mean_mol_percent_filter)) %>%
    dplyr::filter(sum_filter != 0) %>%
    ###--- and if sum of all sample of a species = 0, this species will be removed
    ###
    ggplot2::ggplot(ggplot2::aes(x = factor(species, levels = .$species %>% unique()),
                                 y = mean_mol_percent,
                                 fill = group)) +
    ggplot2::geom_col(color = "black",
                      width = 0.9,
                      position = ggplot2::position_dodge()) +
    ggplot2::ggtitle(paste(lipid_class, paste0("species profile (> ", threshold ," mol%)"))) +
    ggplot2::xlab("Lipid species") +
    ggplot2::ylab("Mean mol %") +
    ggplot2::labs(fill = "") + # removes legend title ("group")
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = species,
                                     y = mol_percent,
                                     fill = group,
                                     color = group),
                        position = ggplot2::position_dodge(0.9),
                        size = 1,
                        shape = 21,
                        fill = "white") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_mol_percent - sd,
                                        ymax = mean_mol_percent + sd),
                           width = 0.3,
                           size = 0.5,
                           position = ggplot2::position_dodge(0.9)) +
    ggplot2::guides(color = FALSE, size = FALSE) # removes geom_point-legend
}

add_lipidclass_sheet <- function(lipid_class, excel_datalist, workbook){

  clean <- function(lipid_class){

    if(stringr::str_detect(lipid_class, "[:;]")){
      lipid_class <- stringr::str_replace(lipid_class, "[;:]", "_") %>%
        stringr::str_replace("[;:]", "_")
    }
    return(lipid_class)
  }

  # initiate lipidclass-specific excelsheet
  openxlsx::addWorksheet(workbook, clean(lipid_class))

  openxlsx::setColWidths(workbook, clean(lipid_class), col = 1, widths = 20)
  openxlsx::setColWidths(workbook, clean(lipid_class), col = 2:99, widths = 10)
  openxlsx::setRowHeights(workbook, clean(lipid_class), rows = 2, heights = 50)
  openxlsx::setRowHeights(workbook, clean(lipid_class), rows = c(1, 3:99), heights = 15)

  sum_df <- excel_datalist[[lipid_class]]$sum
  ether_df <- excel_datalist[[lipid_class]]$ether
  species_df <- excel_datalist[[lipid_class]]$species
  species_mean_df <- excel_datalist[[lipid_class]]$species_mean

  ggplot2::ggsave(paste0("inst/png/", clean(lipid_class), ".png"), plot = excel_datalist[[lipid_class]]$species_plot, dpi = 300, width = 14, height = 6)

  # First table with class-specific sum data
  openxlsx::writeData(workbook, clean(lipid_class), excel_datalist[[lipid_class]]$sum,
                      startCol = 1, startRow = 2, rowNames = FALSE)

  # Second table with ether-information
  if (any(paste0("e", lipid_class, " [uM]") == colnames(ether_df))) {
    openxlsx::writeData(workbook, clean(lipid_class), ether_df[, -1],
                        startCol = (1 + ncol(sum_df) + 1), startRow = 2, rowNames = FALSE)
  }

  # Third with class-specific mol% values
  openxlsx::writeData(workbook, clean(lipid_class), as.character(paste0(lipid_class, " mol%")),
                      startCol = 1, startRow = (nrow(sum_df) + 5))
  openxlsx::writeData(workbook, clean(lipid_class), species_df,
                      startCol = 1, startRow = (nrow(sum_df) + 6))

  # Fourth with class-specific mean mol% values
  openxlsx::writeData(workbook, clean(lipid_class), as.character(paste0(lipid_class, " average mol%")),
                      startCol = 1, (nrow(sum_df) + nrow(species_df) + 9))
  openxlsx::writeData(workbook, clean(lipid_class), species_mean_df,
                      startCol = 1, startRow = (nrow(sum_df) + nrow(species_df) + 10))

  openxlsx::insertImage(workbook, clean(lipid_class), paste0("inst/png/",clean(lipid_class),".png"), units = "in", width = 14, height = 6,
                        startRow = ((nrow(sum_df) + 2) + (nrow(species_df) + 9) + (nrow(species_mean_df) + 2)))

}

## Summary-sheet
add_summary_sheet <- function(data, workbook){
  openxlsx::addWorksheet(workbook, "summary")

  openxlsx::setColWidths(workbook, "summary", col = 1, widths = 20)
  openxlsx::setColWidths(workbook, "summary", col = 2:99, widths = 10)
  openxlsx::setRowHeights(workbook, "summary", rows = 2, heights = 50)
  openxlsx::setRowHeights(workbook, "summary", rows = c(1, 3:99), heights = 15)

  uM_table <- excel_class_profile_uM(data = data)

  mol_percent_table <- excel_class_profile_percent(data = data)

  ggplot2::ggsave("inst/png/summary.png", plot = class_plot_excel(data), dpi = 300, width = 14, height = 6)

  openxlsx::writeData(workbook, "summary", uM_table, startCol = 1, startRow = 2, rowNames = FALSE)
  openxlsx::writeData(workbook, "summary", mol_percent_table, startCol = 1, startRow = (nrow(uM_table) + 5), rowNames = FALSE)
  openxlsx::insertImage(workbook, "summary", "inst/png/summary.png", units = "in", width = 14, height = 6, startRow = ((2 * nrow(uM_table))+8))

}

excel_class_profile_uM <-  function(data){

  data %>%
    dplyr::group_by(class) %>%
    dplyr::mutate(ether = stringr::str_detect(species, "O-"),
                  diacyl = any(ether)) %>%
    dplyr::group_by(class, sample_name, group, ether, diacyl, sample_input) %>%
    dplyr::summarise(uM = sum(uM), .groups = "drop") %>%
    dplyr::mutate(class = ifelse(ether == TRUE, paste0("e", class), class),
                  class = ifelse(diacyl == TRUE & ether == FALSE,
                                 paste0("a", class), class)) %>%
    dplyr::select(-ether, -diacyl) %>%
    dplyr::group_by(sample_name) %>%
    dplyr:: mutate("sum [uM]" = sum(uM),
                   "sum [pmol]" = `sum [uM]` * sample_input) %>%
    tidyr::pivot_wider(names_from = "class",
                       values_from = uM) %>%
    dplyr::select(lipid_class_order[lipid_class_order %in% colnames(.)])
}

excel_class_profile_percent <- function(data){

  data %>%
    dplyr::group_by(class) %>%
    dplyr::mutate(ether = stringr::str_detect(species, "O-"),
                  diacyl = any(ether)) %>%
    dplyr::group_by(class, sample_name, group, ether, diacyl, sample_input) %>%
    dplyr::summarise(uM = sum(uM), .groups = "drop") %>%
    dplyr::mutate(class = ifelse(ether == TRUE, paste0("e", class), class),
                  class = ifelse(diacyl == TRUE & ether == FALSE,
                                 paste0("a", class), class)) %>%
    dplyr::select(-ether, -diacyl) %>%
    dplyr::group_by(sample_name) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::select(class, sample_name, mol_percent, sample_input) %>%
    tidyr::pivot_wider(names_from = class,
                       values_from = mol_percent) %>%
    dplyr::select(lipid_class_order[lipid_class_order %in% colnames(.)])
}

class_plot <- function(data, errorbars = TRUE){

  gg_errorbars <- function(plt){

    plt +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_mol_percent - sd,
                                          ymax = mean_mol_percent + sd),
                             width = 0.6,
                             size = 0.5,
                             position = ggplot2::position_dodge(0.9))
  }

  plot_data <- data %>%
    dplyr::group_by(class) %>%
    dplyr::mutate(ether = stringr::str_detect(species, "O-"),
                  diacyl = any(ether)) %>%
    dplyr::group_by(class, sample_name, group, ether, diacyl, sample_input) %>%
    dplyr::summarise(uM = sum(uM), .groups = "drop") %>%
    dplyr::mutate(class = ifelse(ether == TRUE, paste0("e", class), class),
                  class = ifelse(diacyl == TRUE & ether == FALSE,
                                 paste0("a", class), class)) %>%
    dplyr::select(-ether, -diacyl) %>%
    dplyr::group_by(sample_name) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::group_by(class, group) %>%
    dplyr::mutate(sd = sd(mol_percent),
                  mean_mol_percent = mean(mol_percent))

  plt <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = class,
                                 y = mean_mol_percent,
                                 fill = group)) +
    ggplot2::geom_col(color = "black",
                      width = 0.9,
                      position = ggplot2::position_dodge()) +
    ggplot2::ggtitle("Lipid class profile") +
    ggplot2::xlab("Lipid classes") +
    ggplot2::ylab("Percentage / total lipids") +
    ggplot2::labs(fill = "") + # removes legend title ("group")
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = mol_percent,
        text = purrr::map(paste(sample_name,
                                '<br>',
                                sprintf("%.2f mol%%", mol_percent)),
                          HTML)),
      position    = ggplot2::position_dodge(width = 0.9),
      pch         = 21,
      alpha       = 1,
      color       = "black",
      show.legend = FALSE) +
    ggplot2::scale_x_discrete(limits = lipid_class_order[lipid_class_order %in% plot_data$class]) +
    ggplot2::guides(color = "none", size = "none") # removes geom_point-legend

  if(errorbars == TRUE){gg_errorbars(plt)} else {plt}
}

class_plot_excel <- function(data){

  plot_data <- data %>%
    dplyr::group_by(class) %>%
    dplyr::mutate(ether = stringr::str_detect(species, "O-"),
                  diacyl = any(ether)) %>%
    dplyr::group_by(class, sample_name, group, ether, diacyl, sample_input) %>%
    dplyr::summarise(uM = sum(uM), .groups = "drop") %>%
    dplyr::mutate(class = ifelse(ether == TRUE, paste0("e", class), class),
                  class = ifelse(diacyl == TRUE & ether == FALSE,
                                 paste0("a", class), class)) %>%
    dplyr::select(-ether, -diacyl) %>%
    dplyr::group_by(sample_name) %>%
    dplyr::mutate(mol_percent = uM * 100 / sum(uM)) %>%
    dplyr::group_by(class, group) %>%
    dplyr::mutate(sd = sd(mol_percent),
                  mean_mol_percent = mean(mol_percent))

  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(x = class,
                                 y = mean_mol_percent,
                                 fill = group)) +
    ggplot2::geom_col(color = "black",
                      width = 0.9,
                      position = ggplot2::position_dodge()) +
    ggplot2::ggtitle("Lipid class profile") +
    ggplot2::xlab("Lipid classes") +
    ggplot2::ylab("Percentage / total lipids") +
    ggplot2::labs(fill = "") + # removes legend title ("group")
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45),
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::geom_point(ggplot2::aes(x = class,
                                     y = mol_percent,
                                     fill = group,
                                     color = group),
                        position = ggplot2::position_dodge(0.9),
                        size = 1,
                        shape = 21,
                        fill = "white") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_mol_percent - sd,
                                        ymax = mean_mol_percent + sd),
                           width = 0.3,
                           size = 0.5,
                           position = ggplot2::position_dodge(0.9)) +
    ggplot2::scale_x_discrete(limits = lipid_class_order[lipid_class_order %in% plot_data$class]) +
    ggplot2::guides(color = "none", size = "none") # removes geom_point-legend
}

## uM-sheet
add_uM_sheet <- function(data, workbook){
  uM_table <- data %>%
    dplyr::select(class, species, sample, uM) %>%
    tidyr::pivot_wider(names_from = sample, values_from = uM) %>%
    dplyr::mutate(species = paste(class, species),
                  species = stringr::str_replace(species, "Chol Chol", "Chol"),
                  species = stringr::str_remove(species, "PE P-16:0 "),
                  species = stringr::str_remove(species, "PE P-18:1 "),
                  species = stringr::str_remove(species, "PE P-18:0 "))

  openxlsx::addWorksheet(workbook, "uM_table")

  openxlsx::setColWidths(workbook, "uM_table", col = 1:99, widths = 10)
  openxlsx::setRowHeights(workbook, "uM_table", rows = c(1:99), heights = 15)

  openxlsx::writeData(workbook, "uM_table", uM_table, startCol = 1, startRow = 1)

}

