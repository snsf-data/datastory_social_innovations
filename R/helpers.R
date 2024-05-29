pivot_data <- function(dat, var){
  
  dat |>
    select(id, matches(var)) |>
    pivot_longer(-id, names_to = "var", values_to = "answer")
  
}

prep_data <- function(dat, answer_type = "yes_no", lang = "en") {
  
  dat <-
    dat |>
    filter(!str_detect(var, "\\[res\\]|\\[acad\\]")) |>
    mutate(
      var =
        case_when(
          str_detect(var, "\\[welfare") ~
            switch(
              lang,
              en = "Welfare or educational institutions",
              de = "Sozial- und Bildungseinrichtungen",
              fr = "Structures sociales et éducatives"
            ),
          str_detect(var, "\\[policy") ~
              switch(
                lang,
                en = "Public administration and governmental organizations",
                de = "Administrative Institutionen und staatliche Organisationen",
                fr = "Institutions administratives et organisations étatiques"
              ),
          str_detect(var, "\\[pub") ~
              switch(
                lang,
                en = "General population",
                de = "Allgemeine Bevölkerung",
                fr = "Population générale"
              ),
          str_detect(var, "\\[civsoc") ~
              switch(
                lang,
                en = "NGO",
                de = "NGO",
                fr = "ONG"
              ),
          str_detect(var, "\\[socgr") ~
              switch(
                lang,
                en = "Specific social groups",
                de = "Bestimmte soziale Gruppen",
                fr = "Groupes sociaux précis"
              ),
          str_detect(var, "\\[busi") ~
              switch(
                lang,
                en = "Businesses",
                de = "Unternehmen",
                fr = "Entreprises"
              ),
          str_detect(var, "\\[citiz") ~
              switch(
                lang,
                en = "Individual citizens",
                de = "Einzelpersonen",
                fr = "Personnes privées"
              ),
          str_detect(var, "\\[media") ~
              switch(
                lang,
                en = "Media representatives",
                de = "Medienvertreter:innen",
                fr = "Représentant·es des médias"
              ),
          .default = var
        )
    )
  
  if (answer_type == "yes_no") {
    
    dat <-
      dat |>
      mutate(
        answer =
          fct(if_else(answer == "no", "No", "Yes"), levels = c("No", "Yes"))
      ) |>
      count(var, answer) |>
      drop_na() |>
      mutate(
        prop = n / sum(n),
        .by = var
      ) |>
      mutate(
        order = prop[answer == "Yes"],
        .by = var
      ) |>
      filter(answer == "Yes")
    
  } else if (answer_type == "lmh") {
    
    dat <-
      dat |>
      mutate(
        answer =
          fct(
            case_when(
              answer < 4 ~ "No",
              answer < 7 ~ "Yes",
              .default = "Yes"
            ),
            levels = c("No", "Yes")
          )
      ) |>
      count(var, answer) |>
      drop_na() |>
      mutate(
        prop = n / sum(n),
        .by = var
      ) |>
      mutate(
        order = prop[answer == "Yes"],
        .by = var
      ) |>
      filter(answer == "Yes")
    
  }
  
  dat |>
    arrange(desc(answer)) |>
    mutate(
      lab_x =
        lag(cumsum(prop), default = 0) + ((cumsum(prop) - lag(cumsum(prop), default = 0)) / 2),
      .by = var) |>
    mutate(var = fct_reorder(var, order, .desc = TRUE))
  
}

make_plot <- function(dat, x, y, fill) {
  
  dat |>
    rename(x := {{ x }}, y := {{ y }}, fill := {{ fill }}) |>
    ggplot() +
    aes(x = x, y = y, fill = fill) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(
        label = paste0(round(100 * x), "%"),
        x = lab_x
      ),
      color = "white"
    ) +
    scale_fill_manual(values = rev(get_datastory_scheme(n_col = n_distinct(dat$answer)))) +
    scale_x_continuous(labels = scales::percent, expand = expansion(mult = c(0.0, 0.1))) +
    facet_wrap(~y,scales = "free_y",  ncol = 1) +
    get_datastory_theme(
      text_axis = "x",
      family = "Theinhardt") +
    theme(
      strip.text =
        element_text(
          color = "#4C4C4C",
          face = "plain",
          hjust = 0,
          margin = margin(1, 1, 1),
          size = 10
        ),
      axis.text.x = element_text(size = 9),
      panel.spacing.y = unit(0.25, "lines")
    )
  
}
