# Function for exploring relationship between two categorical variables
make_bi_bar_graph <- function(d, var1, var2, label1, label2, labels=F,voption="plasma"){
  # d <- dsm2
  # var1 <- "sex"
  # var2 <- "sex"
  #
  d1 <- d %>%
    group_by(.dots = c(var1, var2) )%>%
    summarize(
      n_people = n()
    ) %>%
    ungroup() %>%
    mutate(
      total = sum(n_people, na.rm =T)
    ) %>%
    group_by(.dots = var1) %>%
    mutate(
      total_1 = sum(n_people, na.rm = T)
      ,pct_1 = scales::label_percent()(total_1/total)
      ,pct_12 = scales::label_percent()(n_people/total_1)
    )
  n_total = d1 %>% pull(total) %>% unique()

  g1 <- d1 %>%
    ggplot(aes_string(x = var1, y = "n_people", fill = var2 ))+
    geom_col(position = position_dodge())+
    geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
    scale_fill_viridis_d(begin = 0, end = .8, direction = -1, option = voption)+
    labs( title = paste0("Sample size, N = ", n_total))+
    scale_y_continuous(expand=expansion(mult = c(0,.1)))
  # coord_flip()

  if(var1 == var2){
    g1 <- g1 +
      geom_text(aes_string(label = "pct_1"),position = position_dodge(.9), vjust = -.5, color = "black", size = 4)
  }else{
    g1 <- g1 +
      geom_text(aes_string(label = "pct_12"),position = position_dodge(.9), vjust = -.5, color = "black", size = 4)
  }

  g1 + labs(y = "Number of respondents", title = paste0("Bivariate relationship between (",var1,") and (",var2,")"))

}
# How to use
# dsm2 %>% make_bi_bar_graph("sex","class_standing")
# dsm2 %>% make_bi_bar_graph("class_standing","sex")
# dsm2 %>% make_bi_bar_graph("sex","sex")
# dsm2 %>% make_bi_bar_graph("class_standing","class_standing")


make_bi_mosaic <- function(d, var1, var2){

  # d <- dsm2 %>% select(sex, class_standing)
  # var1 <- "class_standing"
  # var2 <- "sex"

  d1 <- d %>%
    dplyr::rename(
      "v1" = var1
      ,"v2" = var2
    ) %>%
    mutate(
      v1 = forcats::fct_drop(v1)
      ,v2 = forcats::fct_drop(v2)
    )

  mosaicplot(~v1 + v2, data = d1,
             main = paste0("Bivariate distribution between (", var1, ") and (", var2,")")
             ,xlab = var1, y = var2
             ,shade = TRUE)
}
# How to use:
# dsm1 %>% make_bi_mosaic("sex", "class_standing")
# dsm2 %>% make_bi_mosaic("sex", "over21")

# var1 <- "class_standing"
# var2 <- "sex"
# Function to conduct an independence test between two categorical variables
# test_independence <- function(d, var1, var2){
#   # browser()
#   # var1 = "sex"; var2 = "religion"; d <- dsm2
#   d1 <- d %>%
#     dplyr::rename(
#       "v1" = var1
#       ,"v2" = var2
#     ) %>%
#     mutate(
#       v1 = forcats::fct_drop(v1)
#       ,v2 = forcats::fct_drop(v2)
#     )
#   # var1_char <- rlang::sym(var1)
#   # var2_char <- rlang::sym(var2)
#   data(d1)
#   d1 %>%
#     dplyr::select(v1, v2) %>%
#     sjPlot::sjtab(
#       fun = "xtab"
#       ,var.labels=c(var1, var2)
#       # ,var.labels=c(var1_char, var2)
#       ,show.row.prc=T
#       ,show.col.prc=T
#       ,show.summary=T
#       ,show.exp=T
#       ,show.legend=T
#     )
#   # see interpretation of Phi and V
#   # http://www.people.vcu.edu/~pdattalo/702SuppRead/MeasAssoc/NominalAssoc.html
# }
# How to use
# dsm2 %>% test_independence("sex", "religion")
# dsm2 %>% test_independence("religion", "class_standing")

scatter_by_groups <- function(
  d,xvar, yvar, groupvar, jitterwidth=0, jitterheight=0,  xlabel = xvar, ylabel=yvar, grouplabel=groupvar
){
  # d <- dsm2
  # xvar = "n_tx_knowledge"
  # xvar = "n_tx_helpful"
  # yvar = "hr_support"
  # groupvar = "sex"

  g1 <- d %>%
    ggplot(aes_string(x = xvar, y = yvar, color = groupvar))+

    geom_point(shape = 21, size = 3, alpha = .4,
               position = position_jitter(width=jitterwidth,height = jitterheight, seed = 42))+
    scale_color_viridis_d(begin = 0, end = .6, option = "plasma")+
    geom_smooth(method="lm", se = F)+
    ggpmisc::stat_poly_eq(formula = y ~ + x ,
                          aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                          parse = TRUE)+
    labs(
      title = paste0("Relationship between (", yvar,") and (", xvar,") for different levels of (",groupvar,")"),
      caption = paste0("N = ", nrow(d)),
      x = xlabel,
      y = ylabel,
      color = grouplabel
    )
  return(g1)

}
# How to use:
# dsm2 %>% scatter_by_groups("n_tx_knowledge","hr_support","sex")
# dsm2 %>% scatter_by_groups("n_tx_knowledge","hr_support","sex",1,0)
# dsm2 %>% scatter_by_groups("n_tx_knowledge","hr_support","sex",1,0,"Knowledge of OUD Tx","HR Policy Support", "Sex")

