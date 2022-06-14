library(tidyverse)
perModel <- function(data,response,predictors, subs = NULL, pred = NULL) {
  library(tidyverse)
  #library(faraway)
  #set the arguments
  #pr <- list(...)
  response = response
  if (grepl('(', response, fixed = TRUE) == TRUE) {
    response_val <- stringr::str_extract(response, '(?<=\\()[^\\^\\)]+')
    nm <- names(data)
    #get the predictors
    nm <- nm[!nm %in% response_val]
    n <- length(nm)
  } else {
    #substitute data
    subs <- substitute(data)
    #get names
    nm <- names(data)
    #get the predictors
    nm <- nm[!nm %in% response]
    n <- length(nm)
  }

  nm_data <-
    sapply(1:length(nm), function(i)
      c(nm[-(0:i)], nm[0:i])) %>% data.frame()
  comb_predictors <- list()
  for (i in 1:length(nm_data)) {
    comb_predictors[[i]] <- nm_data[1:i, ]
  }

  formula_levels <- function(x) {
    if (nrow(x) < n)
      lapply(x, function(y)
        reformulate(y, response = response))
    else
      lapply(x[1], function(y)
        reformulate(y, response = response))
  }

  if ("all" %in% predictors) {
    predictors_wanted<-comb_predictors


  } else  {
    test_eval <- grepl('(', predictors, fixed=TRUE)
    extract_text<-c()

    for (i in 1:length(test_eval)) {
      if (test_eval[i] == TRUE) {
        #print(dots[[1]][i])
        extract_text[i] <-
          stringr::str_extract(predictors[i], '(?<=\\()[^\\^\\)]+')
      } else {
        extract_text[i] <- predictors[i]
      }
    }
    repl_predictors <- map(comb_predictors, ~ .x %>% mutate(across(everything(), ~ str_replace_all(.x, setNames(predictors, extract_text)))))

    predictors_wanted <-
      lapply(repl_predictors, function(dat)
        Filter(function(x)
          any(predictors %in% x), dat))
  }

  formula_predictors <- map(predictors_wanted, formula_levels)
  formulas_data <-
    formula_predictors %>% unlist() %>% lapply(., paste) %>% lapply(., function(x)
      x[c(2, 1, 3)]) %>% lapply(., function(x)
        paste(x, collapse = " "))

  lm_comb<-formula_predictors %>% unlist() %>% lapply(., function(x)lm(x, data))

  #lm_test <- map(formula_predictors, function(x)eval(parse(text=x)))

  matrix_models <- lapply(lm_comb, model.matrix)
  if(is.null(pred) != TRUE){
    if (is(pred, 'list') == TRUE) {
      type = pred %>% map(., function(dat)
        Filter(function(x)
          any(c(
            'prediction', 'confidence'
          ) %in% x), dat)) %>% unlist()
      if ({
        c(class(pred), sapply(pred, class)) %>% last() == 'character'
      }) {
        pred_type <- pred %>% last()
        summarise_matrix <-
          lapply(matrix_models, function(x)
            apply(x, 2, pred_type))

        if ('all' %in% predictors) {
          prediction_lm <-
            mapply(
              function(a, b)
                predict(a, new = data.frame(t(b)), interval = type),
              lm_comb,
              summarise_matrix
            ) %>% data.frame()
          colnames(prediction_lm) <- formulas_data$formulas
          rownames(prediction_lm) <- c('fit', 'lwr', 'upr')
          return(prediction_lm)
        } else{
          names_replace <-
            map(summarise_matrix, ~ str_replace_all(names(.x), fixed(setNames(extract_text, predictors))))
          clean_matrix <-
            mapply(function(x, y)
              setNames(x, y), summarise_matrix, names_replace)
          prediction_lm <-
            mapply(
              function(a, b)
                predict(a, new = data.frame(t(b)), interval = type),
              lm_comb,
              clean_matrix
            ) %>% data.frame()
          colnames(prediction_lm) <- formulas_data$formulas
          rownames(prediction_lm) <- c('fit', 'lwr', 'upr')
          return(prediction_lm)
        }


      } else if ({
        c(class(pred), sapply(pred, class)) %>% last() == 'numeric'
      }) {
        pred_type <- pred %>% last()
        pred_name <- nm %>%
          data.frame(names = .) %>%
          mutate(p_type = pred_type) %>%
          list()

        if(!('all') %in% predictors){
          named_vec <- setNames(predictors, extract_text)
          pred_name <- map(pred_name, ~ .x %>%
                             mutate(across(
                               everything(), ~ str_replace_all(.x, named_vec)
                             )))
          pred_name<-sapply(pred_name, function(x)as.numeric(x[,2])) %>% data.frame(p_type=.) %>% cbind(pred_name)  %>% select(2, 1) %>% list()
        }
        #
        names_type <-
          map(formulas_data$formulas, function(x)stringr::str_extract(x,'(?<=~ ).*')) %>%
          lapply(., function(x)
            strsplit(x, " + ", fixed = TRUE)) %>%
          lapply(., data.frame) %>%
          lapply(., setNames, 'names')
        #
        names_insert_type <-
          mapply(function(x, y)
            inner_join(x, y, by = 'names'),
            names_type,
            pred_name,
            SIMPLIFY = FALSE)
        pseudo_model_matrix <-
          lapply(names_insert_type, function(x)
            pivot_wider(x, names_from = names, values_from = p_type)) %>%
          lapply(., function(x)
            add_column(x, "(Intercept)" = 1))
        pseudo_model_matrix <-
          pseudo_model_matrix %>% lapply(., function(x)
            x[length(x):0])

        if (!('all') %in% predictors) {
          names_replace <-
            map(pseudo_model_matrix, ~ str_replace_all(names(.x), fixed(setNames(extract_text, predictors))))
          pseudo_model_matrix <-
            mapply(function(x, y)
              setNames(x, y), pseudo_model_matrix, names_replace)
        }

        prediction_lm <-
          mapply(
            function(a, b)
              predict(a, new = data.frame(b), interval = type),
            lm_comb,
            pseudo_model_matrix
          ) %>% data.frame()
        colnames(prediction_lm) <- formulas_data$formulas
        rownames(prediction_lm) <- c('fit', 'lwr', 'upr')

        return(prediction_lm)
      }
    }
    else {
      stop("You need a list object, for example: list('prediction','median)")

    }
  }


  formula_names <- formula_predictors %>% unlist() %>% lapply(., deparse) %>% map(., trimws) %>% map(., function(x)paste(x,collapse=" ")) %>% unlist() %>% data.frame(list_names=.)
  names(lm_comb) <- formula_names$list_names
  return(lm_comb)

}


