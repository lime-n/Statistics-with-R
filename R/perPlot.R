#' Title
#'
#' @param data
#' @param predictors
#' @param type
#'
#' @return
#' @export
#'
#' @examples
perPlot <- function(data, predictors=NULL, type=NULL){
  require(ellipse)
  require(tidyverse)
  require(latex2exp)
  require(glue)

  models <-
    data %>%
    keep( ~ all(length(names(.$model)) > 2))

  if (length(predictors) < 2) {
    stop('You need two predictors only')
  } else if (length(predictors) > 2) {
    stop('You need two predictors only')
  } else{

    models <-
      models %>%
      keep(~all(predictors %in% names(.$model)))

    true_false_list<-
      models %>% sapply(., function(x)
        x$coefficients %>%
          data.frame(.) %>%
          unlist() %>%
          setNames(., names(x$coefficients)) %>%
          .[predictors]  %>%
          is.na(.) %>%
          {
            TRUE %in% .
          } %>% list() )

    true_false <- true_false_list %>% unlist()

    if (all(true_false) == TRUE) {
      stop('All your models has a predictor with NAs; Either pick another predictor or fix the NAs')
    } else if (TRUE %in% true_false) {
      models <-
        mapply(function(x, y) {
          if (FALSE %in% y)
            x}, models, true_false_list, SIMPLIFY = FALSE) %>% Filter(Negate(is.null), .)
    }

    ln <- length(models)

    par(mfrow = n2mfrow(ln, asp=2))

    row_predictor_data <-
      lapply(models, function(x)
        names(x$coefficients) %>%
          seq_along() %>% as.factor() %>%
          data.frame(id = ., name = as.factor(names(x$coefficients))))

    row_predictor_value <-lapply(row_predictor_data, function(x) match(predictors, x$name) %>% setNames(., predictors))

    confint_model <- models %>%
      lapply(., function(x)
        confint(x) %>%
          data.frame(.) %>%
          rownames_to_column(.) %>%
          `colnames<-`(c('predictor', 'X2.5%', 'Y97.5%')) %>%
          .[.$predictor %in% predictors, ])

    confint_model <- lapply(confint_model, function(x) x[match(predictors, x$predictor),])
    #
    confint_model <- confint_model %>%
      keep( ~all(predictors %in% .$predictor))

    confint_model_add <- confint_model %>% lapply(., function(x)
      x[,-1] %>%
        rowid_to_column(.) %>%
        pivot_longer(-c(1)) %>%
        data.frame(.) %>%
        mutate(name = glue("{name}{rowid}")) %>%
        .[,-1] %>%
        pivot_wider(names_from = name, values_from = value) %>% data.frame(.))
    #
    confint_all_models <-
      mapply(function(x, y) {
        if (!'NA' %in% x)
          cbind(x, y)
      }, confint_model_add, row_predictor_value, SIMPLIFY = FALSE) %>% Filter(Negate(is.null), .)

    confint_all_models <-
      lapply(confint_all_models, function(x)
        rownames_to_column(x) %>% pivot_wider(names_from = rowname, values_from = y))
    #
    # #colum names for row_predictor_value
    # #`X2.5%1` `Y97.5%1` `X2.5%2` `Y97.5%2`

    #formula text
    formula_text <-
      lapply(models, function(x)
        formula(x) %>% deparse() %>% str_trim(.)%>% paste(., collapse="") %>% gsub(" ","",.) )

    confint_all_models <-
      mapply(function(x, y)
        cbind(x, y) %>% data.frame(),
        confint_all_models,
        formula_text,
        SIMPLIFY = FALSE)


    lower_conf <- lapply(confint_all_models, function(x) {
      if (x[[1]] < 0 &
          0 < x[[2]]) {
        "Accept"
      } else {
        "Reject"
      }
    })
    total_conf <- lapply(confint_all_models, function(x) {
      if (x[[1]] < 0 &
          0 < x[[2]] & x[[3]] < 0 &
          0 < x[[4]]) {
        "Accept"
      } else  {
        "Reject"
      }
    })

    upper_conf <- lapply(confint_all_models, function(x) {
      if (x[[3]] < 0 &
          0 < x[[4]]) {
        "Accept"
      } else {
        "Reject"
      }
    })

    confint_all_models <-
      mapply(
        function(x, y, z, q)
          cbind(x, y, z, q),
        confint_all_models,
        lower_conf,
        upper_conf,
        total_conf,
        SIMPLIFY = FALSE
      )

    #
    mapply(
      function(x, y)
        plot(ellipse(x, predictors), type = type) %>%
        with(
          points(coef(x)[y[[5]]], coef(x)[y[[6]]], pch = 19)) %>%
        with(
          points(y[[1]], y[[3]], pch = 22)
        ) %>%
        with(
          points(y[[2]], y[[4]], pch = 24)
        ) %>%
        with(
          points(0, 0, pch=19, col='red')
        ) %>%
        with(
          title(sub = y[[7]]))  %>%
        with(
          mtext(text = TeX(paste0('$H_0:$', glue('$\\beta_{1} = 0, {y[[8]]}$') )),cex=0.5,   adj=0)
        ) %>%with(
          mtext(text  = TeX(paste0('$H_0:$', glue('$\\beta_{2} = 0, {y[[9]]}$') )),cex=0.5,  adj=1)
        ) %>%with(
          mtext(text = TeX(paste0('$H_0:$', glue('$\\beta_{1} = \\beta_{2} = 0, {y[[10]]}$') )) , cex=0.5)
        ) %>%
        with(abline(v = confint(x)[y[[5]], ], lty = 2)) %>%
        with(abline(h = confint(x)[y[[6]], ], lty = 2)),
      models,
      confint_all_models
    )
    return('enjoy your plot(s)')
    #
  }
}
