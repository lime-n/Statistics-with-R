library(binhf)
library(tidyverse)
require(lmtest)
require(splines)
library(mgcv)
library(latex2exp)
library(glue)
library(faraway)
perDiag <- function(data) {
  input <- NULL
  predictors <- names(data) %>% data.frame(names = .)

  poly66 <- function(data, response_chosen, predictor_chosen){
    i=0
    if(length(predictor_chosen) == 1){
      pmod <- c()
      pmod_I <- c()
      while(i<=5){
        i = i+1
        form <- reformulate(glue('poly({predictor_chosen},{i}, raw=TRUE)'), response=response_chosen)
        pmod_I[[i]] <- lm(form, data)
        pmod[[i]] <- lm(form, data) %>% summary() %>%
          .$coefficients %>%data.frame(.) %>% rownames_to_column() %>%  .[!(.$rowname) %in% "(Intercept)",] %>%
          `colnames<-`(c('predictor', 'Estimate', 'Std.Error', 't-value', 'Pr(>|t|)')) %>% split(., sort(as.character(as.numeric(rownames(.)))))


      }
      pmod <- pmod %>% flatten()
      pmod <- lapply(pmod, function(x){if(x$`Pr(>|t|)` < 0.05)x}) %>% compact() %>% do.call(rbind.data.frame, .) %>% remove_rownames()

      max_n <- stringr::str_extract(pmod$predictor, "[0-9]") %>% unique() %>% as.numeric() %>% max()
      pmod_I <- pmod_I[1:max_n] %>% lapply(., function(x)summary(x))

      op <- c(list(pmod),pmod_I)
      return(op)
    }
    else{
      mpmod<-c()
      while(i <= 4){
        i=i+1
        form_polym <- reformulate(glue('polym({predictor_chosen},degree={i})'), response=response_chosen)
        mpmod[[i]] <- lm(form_polym, data)
      }
      par(mfrow=c(1, 1))
      choose_degree <- menu(c(1, 2, 3, 4, 5), title = "Choose your response surface polynomial degree")
      if(choose_degree == 1){
        lmod <- mpmod[[1]]
        pred1_min <- data[, predictor_chosen[1]] %>% min()
        pred1_max <- data[, predictor_chosen[1]] %>% max()
        pred1_seq <- seq(pred1_min, pred1_max, len=10)
        pred2_min <- data[, predictor_chosen[2]] %>% min()
        pred2_max <- data[, predictor_chosen[2]] %>% max()
        pred2_seq <- seq(pred2_min, pred2_max, len=10)

        pgrid <- expand.grid(pred1_seq, pred2_seq)
        names(pgrid) <- predictor_chosen
        pv <- predict(lmod, pgrid)

        persp(pred1_seq, pred2_seq,matrix(pv, 10, 10), theta=45, xlab=predictor_chosen[1], ylab=predictor_chosen[2], zlab = glue("{substitute(savings)} rate"), ticktype="detailed",
              shade = 0.25)
      }else if(choose_degree == 2){
        lmod <- mpmod[[2]]
        pred1_min <- data[, predictor_chosen[1]] %>% min()
        pred1_max <- data[, predictor_chosen[1]] %>% max()
        pred1_seq <- seq(pred1_min, pred1_max, len=10)
        pred2_min <- data[, predictor_chosen[2]] %>% min()
        pred2_max <- data[, predictor_chosen[2]] %>% max()
        pred2_seq <- seq(pred2_min, pred2_max, len=10)

        pgrid <- expand.grid(pred1_seq, pred2_seq)
        names(pgrid) <- predictor_chosen
        pv <- predict(lmod, pgrid)
        persp(pred1_seq, pred2_seq,matrix(pv, 10, 10), theta=45, xlab=predictor_chosen[1], ylab=predictor_chosen[2], zlab = glue("{substitute(savings)} rate"), ticktype="detailed",
              shade = 0.25)
      }else if(choose_degree == 3){
        lmod <- mpmod[[3]]
        pred1_min <- data[, predictor_chosen[1]] %>% min()
        pred1_max <- data[, predictor_chosen[1]] %>% max()
        pred1_seq <- seq(pred1_min, pred1_max, len=10)
        pred2_min <- data[, predictor_chosen[2]] %>% min()
        pred2_max <- data[, predictor_chosen[2]] %>% max()
        pred2_seq <- seq(pred2_min, pred2_max, len=10)

        pgrid <- expand.grid(pred1_seq, pred2_seq)
        names(pgrid) <- predictor_chosen
        pv <- predict(lmod, pgrid)
        persp(pred1_seq, pred2_seq,matrix(pv, 10, 10), theta=45, xlab=predictor_chosen[1], ylab=predictor_chosen[2], zlab = glue("{substitute(savings)} rate"), ticktype="detailed",
              shade = 0.25)
      }else if(choose_degree == 4){
        lmod <- mpmod[[4]]
        pred1_min <- data[, predictor_chosen[1]] %>% min()
        pred1_max <- data[, predictor_chosen[1]] %>% max()
        pred1_seq <- seq(pred1_min, pred1_max, len=10)
        pred2_min <- data[, predictor_chosen[2]] %>% min()
        pred2_max <- data[, predictor_chosen[2]] %>% max()
        pred2_seq <- seq(pred2_min, pred2_max, len=10)

        pgrid <- expand.grid(pred1_seq, pred2_seq)
        names(pgrid) <- predictor_chosen
        pv <- predict(lmod, pgrid)
        persp(pred1_seq, pred2_seq,matrix(pv, 10, 10), theta=45, xlab=predictor_chosen[1], ylab=predictor_chosen[2], zlab = glue("{substitute(savings)} rate"), ticktype="detailed",
              shade = 0.25)
      }else if(choose_degree == 5){
        lmod <- mpmod[[5]]
        pred1_min <- data[, predictor_chosen[1]] %>% min()
        pred1_max <- data[, predictor_chosen[1]] %>% max()
        pred1_seq <- seq(pred1_min, pred1_max, len=10)
        pred2_min <- data[, predictor_chosen[2]] %>% min()
        pred2_max <- data[, predictor_chosen[2]] %>% max()
        pred2_seq <- seq(pred2_min, pred2_max, len=10)

        pgrid <- expand.grid(pred1_seq, pred2_seq)
        names(pgrid) <- predictor_chosen
        pv <- predict(lmod, pgrid)
        persp(pred1_seq, pred2_seq,matrix(pv, 10, 10), theta=45, xlab=predictor_chosen[1], ylab=predictor_chosen[2], zlab = glue("{substitute(savings)} rate"), ticktype="detailed",
              shade = 0.25)
      }
    }

    return(mpmod)
  }

  perTest_nl <- function(model, ...) {
    pr <- list(...)
    if (is(pr[[1]], 'list') == TRUE) {
      if (length(pr[[1]]) != 3) {
        stop('You need three parameters, i.e. list("data", "column", "value")')
      } else {
        dataset <- pr[[1]][[1]]
        column <- pr[[1]][[2]]
        value <- pr[[1]][[3]]
        #print(paste(dataset %>% data.frame(), column, value))
        dataset <- dataset %>% data.frame()
        variance_test <-
          var.test(residuals(model)[dataset[, column] > value], residuals(model)[dataset[, column] < value])
        return(variance_test)
      }
    } else {
      sumry <-
        parse(text = "summary(lm(sqrt(abs(residuals(model))) ~ fitted(model)))")

      if (pr[[1]] == 'summary') {
        model_diagnostic <-
          #model %>%
          eval(sumry)$coefficients %>%
          data.frame() %>%
          .[2,] %>%
          add_column(
            F_statistic = eval(sumry)$fstatistic[1],
            p_value = 1 - pf(
              eval(sumry)$fstatistic[[1]],
              eval(sumry)$fstatistic[[2]],
              eval(sumry)$fstatistic[[3]]
            ),
            df = eval(sumry)$fstatistic[[3]],
            RSE = eval(sumry)$sigma
          ) %>% `colnames<-`(
            c(
              'Estimate',
              'Std.Error',
              't.value',
              'Pr(>|t|)',
              'F.statistic',
              'P.value',
              'df',
              'RSE'
            )
          )
        return(model_diagnostic)
      }

    }


  }



  while (TRUE) {
    input <-
      menu(
        c(
          "Checking Error Assumptions",
          "Finding Unusual Observations",
          "Checking the Structure of the Model"
        ),
        title = "Which diagnostic test do you want to perform?"
      )
    if (input == 1) {
      sub_input <-
        menu(c(
          "Constant Variance",
          "Normality",
          "Correlated Errors",
          'back'
        ),
        title = "Which method do you need to check for errors??")
      if (sub_input != 'back') {
        if (sub_input == 1) {
          input_names <-
            append(names(data), 'back') %>% data.frame(names = .) %>% rownames_to_column()
          sub1_input <-
            menu(input_names$names, title = "Which is your response variable?")
          chosen_response <-
            input_names[input_names$rowname == sub1_input, 2]
          if (chosen_response != 'back') {
            model_choice <-
              perModel(
                data = data,
                response = chosen_response,
                predictors = 'all'
              )
            x_lmod <- lapply(model_choice, summary)
            print(x_lmod)
            model_names <-
              append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

            sub11_input <-
              menu(model_names$names, title = "Which model do you pick?")

            model_response <-
              model_names[model_names$rowname == sub11_input, 2]
            message(model_response)
            if (model_response != 'back') {
              plot_input <-
                c('predictor(s)', 'fitted', 'back') %>% data.frame(names = .) %>% rownames_to_column()
              sub12_input <-
                menu(plot_input$names, title = "Plot residuals against predictor(s) or fitted?")
              plot_choice <-
                plot_input[plot_input$rowname == sub12_input, 2]
              if (plot_choice != 'back') {
                if (plot_choice == 'predictor(s)') {
                  predictors <- predictors[predictors$names != chosen_response,]
                  print(predictors)
                  n <- length(predictors)

                  #return(model_choice)
                  par(mfrow = n2mfrow(n, asp = 2))
                  #return(model_choice[[sub11_input]])
                  scan_values <- c()

                  for (i in predictors) {
                    plot(
                      residuals(model_choice[[sub11_input]]) ~ data[, i],
                      xlab = glue('Predictor: {i}'),
                      ylab = TeX('$Residuals (\\hat{\\epsilon})$')
                    ) %>%
                      with(abline(h = 0))
                  }
                  sub12_input <-
                    #menu(c('Continue', 'back'), title = "Let's compare and test the variances in these groups")
                    for (i in predictors) {
                      menu(c(i), title = "Choose the cut-off value for the predictors")
                      scan_values[[i]] <- scan(nmax = 1)
                      #perTest(model_choice, list(data))
                    }

                  scan_val <-
                    cbind(names(scan_values), scan_values) %>%
                    data.frame()
                  mapply(
                    function(x, y)
                      plot(
                        residuals(model_choice[[sub11_input]]) ~ data[, x],
                        xlab = glue('Predictor: {i}'),
                        ylab = TeX('$Residuals (\\hat{\\epsilon})$')
                      ) %>%
                      with(abline(h = 0)) %>% with(abline(
                        v = y, col = 'red'
                      )),
                    scan_val$V1,
                    scan_val$scan_values
                  )
                  #variance_results<-mapply(function(x, y)map(lmod1, function(z)perTest_nl(z,list(savings, x, y))), scan_val$V1, scan_val$scan_values, SIMPLIFY=FALSE)
                  variance_results <-
                    mapply(
                      function(x, y)
                        perTest_nl(model_choice[[sub11_input]], list(savings, x, y)),
                      scan_val$V1,
                      scan_val$scan_values,
                      SIMPLIFY = FALSE
                    )

                  observed_input <-
                    c(
                      'Nonlinearity',
                      'Non-constant variance',
                      'Nonlinearity and Non-constant Variance'
                    ) %>% data.frame(names = .) %>% rownames_to_column()
                  sub14_input <-
                    menu(
                      c(
                        'Nonlinearity',
                        'Non-constant variance',
                        'Nonlinearity and Non-constant Variance'
                      ),
                      title = 'What have you observed?'
                    )
                  pred_back1 <-
                    append(predictors, 'back') %>% data.frame(names = .) %>% rownames_to_column()
                  sub15_input <-
                    menu(append(predictors, 'back'), title = 'Which predictor do you want to test for?')

                  #predictor_choice <-
                  #  predictors %>% data.frame(names = .) %>% rownames_to_column()
                  predictor_chosen <-
                    pred_back1[pred_back1$rowname == sub15_input, 2]

                  observed_choice <-
                    observed_input[observed_input$rowname, 2]
                  if (predictor_chosen != 'back') {
                    if (sub14_input == 1 |
                        sub14_input == 3) {
                      sub16_input <-
                        menu(
                          c(
                            'Broken Stick Regression',
                            'Polynomials',
                            'Splines',
                            'Additive Models',
                            'More Complex Models'
                          ),
                          title = 'Which transformation on the predictor do you want to perform?'
                        )
                      if (sub16_input == 1) {
                        subset_val <- lapply(scan_val$V1, function(x)
                          (predictor_chosen %in% x)) %>% mapply(function(x, y)
                            x[unlist(y)], scan_val$scan_values, .) %>%
                          compact() %>%
                          .[[1]]
                        form <-
                          reformulate(predictor_chosen, response = chosen_response)

                        c_predictor <-
                          parse(
                            text = paste(
                              substitute(data),
                              '$',
                              predictor_chosen,
                              collapse = ' '
                            ) %>% gsub(' ', '', .)
                          )
                        mod1 <-
                          lm(form, data, subset = (eval(c_predictor) < subset_val))
                        mod2 <-
                          lm(form, data, subset = (eval(c_predictor) > subset_val))
                        par(mfrow = c(1, 1))
                        plot(
                          form,
                          data,
                          xlab = glue("{predictor_chosen}"),
                          ylab = glue("{chosen_response}")
                        )
                        abline(v = subset_val, lty = 5)
                        min_val <- round(min(eval(c_predictor)))
                        max_val <- round(max(eval(c_predictor)))
                        segments(
                          min_val,
                          mod1$coef[1] + mod1$coef[2] * min_val,
                          subset_val,
                          mod1$coef[1] + mod1$coef[2] * subset_val
                        )
                        segments(
                          max_val,
                          mod2$coef[1] + mod2$coef[2] * max_val,
                          subset_val,
                          mod2$coef[1] + mod2$coef[2] * subset_val
                        )

                        form2 <-
                          reformulate(
                            glue(
                              "lhs({predictor_chosen})+rhs({predictor_chosen})"
                            ),
                            response = chosen_response
                          )
                        #return(subset_val)
                        lhs <-
                          function(x)
                            ifelse(x < subset_val, subset_val - x, 0)
                        rhs <-
                          function(x)
                            ifelse(x > subset_val, subset_val - x, 0)

                        mod3 <- lm(form2, data)

                        x <- seq(min_val, max_val, by = 1)

                        py <-
                          mod3$coef[1] + mod3$coef[2] * lhs(x) + mod3$coef[3] * rhs(x)
                        lines(x, py, lty = 2)
                        #sub16_input <- menu(c(), title = "")
                        mod_list <-
                          list(mod1 %>% summary(),
                               mod2 %>% summary(),
                               mod3 %>% summary())
                        return(mod_list)
                      }
                      else if (sub16_input == 2){
                        message('polynomials')
                        sub17_input <- menu(c('Single', "Multiple"), title="Are you defining polynomial transformation for single or multiple predictors?")
                        if(sub17_input == 1){
                          a_piece <- poly66(data, chosen_response, predictor_chosen)
                          return(a_piece)} else{

                            predictors <- names(data) %>% data.frame(names=.)%>% rownames_to_column() %>% .[.$name != chosen_response, 2]
                            #return(predictors)
                            comb_predictors <- setNames(data.frame(t(combn(predictors, 2))), c("pred1", "pred2")) %>% unite("together",pred1, pred2, sep="+", remove=FALSE) %>% rownames_to_column()

                            poly_menu <- menu(comb_predictors$together, title="Select a combination of two predictors only")
                            comb_choice <- comb_predictors[comb_predictors$rowname == poly_menu, 2]

                            cmb_predictor <- comb_choice %>% strsplit(., "\\+") %>% unlist()

                            poly66(data=data, response_chosen = chosen_response, predictor_chosen = cmb_predictor)


                          }

                      } else if(sub16_input == 3){

                        message("Splines")
                        message("Choose your knots: in the following format - i.e. 0,0,0,0.1, 0.2 ...")
                        #knots <- read_lines()
                        message('Knots still under development')

                      } else if(sub16_input == 4){
                        message("Additive Models still under development")
                        form

                      } else if(sub16_input == 5){
                        message('More Complex Models')
                        image <- raster::stack("https://i.insider.com/61d74a0c95f1b9001837927e?width=750&format=jpeg&auto=webp")
                        raster::plotRGB(image)
                      }


                    }#sub_14 input close
                    else if (sub14_input == 2){
                      message('weighted least squares still under development')


                    }


                    }#predictor_chosen != back close

                }#plot choice predictors close
                else if (plot_choice == 'fitted') {
                  par(mfrow = c(1, 2))
                  plot(
                    fitted(model_choice[[sub11_input]]),
                    residuals(model_choice[[sub11_input]]),
                    xlab = TeX('$Fitted (\\hat{y})$'),
                    ylab = TeX('$Residuals (\\hat{\\epsilon})$')
                  )
                  abline(h = 0)
                  plot(
                    fitted(model_choice[[sub11_input]]),
                    sqrt(abs(residuals(
                      model_choice[[sub11_input]]
                    ))),
                    xlab = TeX('$Fitted (\\hat{y})$'),
                    ylab = TeX('$Residuals (\\sqrt{\\hat{\\epsilon}})$')
                  )
                  nsub1_input <- menu(c('Logarithm', 'Square-root'), title = "Which transformation on the predictor do you want to perform?")
                  if(nsub1_input == 1){

                    ll <- glue("log({chosen_response})")
                    log_mod <-
                      perModel(data = data, ll, predictors = 'all')
                    log_mod_select <- log_mod[[sub11_input]]
                    model_name <- log_mod_select%>% formula() %>% paste() %>% .[c(2, 1, 3)] %>% paste(., collapse=" ")
                    par(mfrow = c(1, 1))
                    plot(
                      fitted(log_mod_select),
                      residuals(log_mod_select),
                      xlab=TeX('$Fitted (\\hat{y})$'),
                      ylab = TeX('$Residuals (\\log{\\hat{\\epsilon}})$')
                    ) %>% with(abline(h = 0))
                    mtext(text=model_name, line=1)
                    log_mod_ <- log_mod_select %>% summary()
                    log_mod_$call <- model_name
                    return(log_mod_)

                    # n <- length(log_mod)
                    # par(mfrow = n2mfrow(n, asp = 2))
                    # lapply(log_mod, function(x)   <- We choose this if we want to include multiple models
                    #   plot(
                    #     fitted(x),
                    #     residuals(x),
                    #     xlab=TeX('$Fitted (\\hat{y})$'),
                    #     ylab = TeX('$Residuals (\\sqrt{\\hat{\\epsilon}})$')
                    #   ) %>% with(abline(h = 0)))
                  } else if(nsub1_input == 2) {
                    se <- glue("sqrt({chosen_response})")
                    sqrt_mod <-
                      perModel(data = data, se, predictors = 'all')
                    sqrt_mod_select <- sqrt_mod[[sub11_input]]
                    model_name <- sqrt_mod_select%>% formula() %>% paste() %>% .[c(2, 1, 3)] %>% paste(., collapse=" ")
                    par(mfrow = c(1, 1))
                    plot(
                      fitted(sqrt_mod_select),
                      residuals(sqrt_mod_select),
                      xlab=TeX('$Fitted (\\hat{y})$'),
                      ylab = TeX('$Residuals (\\sqrt{\\hat{\\epsilon}})$')
                    ) %>% with(abline(h = 0))
                    mtext(text=model_name, line=1)
                    sqrt_mod_ <- sqrt_mod_select %>% summary()
                    sqrt_mod_$call <- mdeol_name
                    return(sqrt_mod_)

                    # n <- length(log_mod)
                    # par(mfrow = n2mfrow(n, asp = 2))  <- If we want multiple models we use this
                    # lapply(log_mod, function(x)
                    #   plot(
                    #     fitted(x),
                    #     residuals(x),
                    #     xlab=TeX('$Fitted (\\hat{y})$'),
                    #     ylab = TeX('$Residuals (\\sqrt{\\hat{\\epsilon}})$')
                    #   ) %>% with(abline(h = 0)))
                  }
                }#plot_choice == fitted close

                  }#plot_choice != back close

                }#model_response != back close

              }#chosen_response - sub_input != 'back close
            }#sub_input == 1 close
        if (sub_input == 2) {
          message('Normality')
          input_names <-
            append(names(data), 'back') %>% data.frame(names = .) %>% rownames_to_column()
          sub1_input <-
            menu(input_names$names, title = "Which is your response variable?")
          chosen_response <-
            input_names[input_names$rowname == sub1_input, 2]
          if(chosen_response != 'back'){
            model_choice <-
              perModel(data = data,
                       response = chosen_response,
                       predictors = 'all')
            print(lapply(model_choice, summary))
            model_names <-
              append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

            sub11_input <-
              menu(model_names$names, title = "Which model do you pick?")

            model_response <-
              model_names[model_names$rowname == sub11_input, 2]
            message(model_response)
            if(model_response != 'back'){
              nm_model <- model_choice[[sub11_input]]
              nm_ <- nm_model %>% summary()
              nm_$call <- model_response

              par(mfrow=c(1, 2))
              qqnorm(residuals(nm_model), ylab = "Residuals", main=model_response)
              qqline(residuals(nm_model))
              hist(residuals(nm_model), xlab="Residuals")

              nmsub2 <- menu(c("Non-Normality", "Skewed Errors", "Long-tailed Errors", "back"), title = "What have you observed?")
              if(nmsub2 == 1){
                shapiro_norm <- shapiro.test(residuals(nm_model))
                ls_nm <- list(nm_, shapiro_norm)
                message("For short-tailed distributions, the consequences of non-normality are not serious and can reasonably be ignored.")
                return(ls_nm)
              } else if(nmsub2==2){
                message("For skewed errors, a transformation of the response may solve the problem.")
                #break
              } else if(nmsub2 == 3){
                message("For long-tailed errors, we might just accept the non-normality and base the inference on the assumption of another distribution or use resampling methods such as the bootstrap or permutation tests.")
                #break
              }
            }
          }#sub_input 2 - chosen_response != 'back'
        }#sub_input ==2 close
        if (sub_input == 3){
          message('Correlated Errors')
          input_names <-
            append(names(data), 'back') %>% data.frame(names = .) %>% rownames_to_column()
          sub1_input <-
            menu(input_names$names, title = "Which is your response variable?")
          chosen_response <-
            input_names[input_names$rowname == sub1_input, 2]
          if(chosen_response != 'back'){
            model_choice <-
              perModel(data = data,
                       response = chosen_response,
                       predictors = 'all')
            modn <- lapply(model_choice,summary)
            print(modn)
            model_names <-
              append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

            sub11_input <-
              menu(model_names$names, title = "Which model do you pick?")

            model_response <-
              model_names[model_names$rowname == sub11_input, 2]
            message(model_response)
            if(model_response != 'back'){
              ce_mod <- model_choice[[sub11_input]]
              print(ce_mod %>% summary())
              ce_sub1 <- menu(c("Yes (name this variable - years)", "No"), title = "Do you have temporality in your data?")
              if(ce_sub1 == 1){
                plot(residuals(ce_mod) ~ years, na.omit(data), ylab="Residuals")
                abline(h=0)
                ce_sub2 <- menu(c("Yes", "No", 'back'), title="Do you observe positive serial correlation?")
                if(ce_sub2 == 1){
                  n <- length(residuals(ce_mod))
                  plot(tail(residuals(ce_mod),n-1) ~ head(residuals(ce_mod),n-1), xlab=
                         expression(hat(epsilon)[i]),ylab=expression(hat(epsilon)[i+1]))
                  abline(h=0,v=0,col=grey(0.75))
                  message("We have performed two tests for serial positive correlation")
                  summary_of_ce <- summary(lm(tail(residuals(ce_mod),n-1) ~ head(residuals(ce_mod),n-1) -1))
                  dw_test_ce <- ce_mod %>% last() %>% formula() %>% dwtest(., data=savings)
                  ls_dw_ce <- list(summary_of_ce, dw_test_ce)
                  return(ls_dw_ce)
                } else if (ce_sub2 == 2){
                  message("Try an alternative metod")
                  break
                }
              } else if (ce_sub1 == 2){
                message('Try an alternative method')
                break
              }
            }#model response - sub_input == 3 close
          }#chosen_response - sub_input == 3 close
        }#sub_input == 3 close
          }#input == 1 != 'back' close
        }#input == 1 close
        else if (input == 2) {
          message('Finding Unusual Observations')
          sub_choice <- c("Leverage", "Outliers", "Influential Observations", 'back') %>% data.frame(names=.) %>% rownames_to_column()
          sub_input <-
            menu(c("Leverage", "Outliers", "Influential Observations", 'back'),
                 title = "Which method do you need to check for unusual observations??")
          chosen_sub <- sub_choice[sub_choice$rowname == sub_input, 2]
          if(chosen_sub != 'back'){
            input_names <-
              append(names(data), 'back') %>% data.frame(names = .) %>% rownames_to_column()
            sub1_input <-
              menu(input_names$names, title = "Which is your response variable?")
            chosen_response <-input_names[input_names$rowname == sub1_input,2]
            predictors <- input_names[!(input_names$names) %in% c(chosen_response, 'back'), 2]
            if(chosen_response != 'back'){
              if(sub_input == 1){
              print(predictors)

                fsub_input <- menu(c(predictors,'rownames', 'back'), title = "Which variable will name the hat values?")
                pred_input<-append(names(data), 'rownames') %>% {ifelse(chosen_response == ., NA, .)} %>% na.exclude(.) %>% data.frame(names=.)%>% rownames_to_column()
                #pred_input <- append(names(data), 'rownames') %>% .[-1] %>% data.frame(names=.) %>% rownames_to_column()
                pred_choice <- pred_input[pred_input$rowname == fsub_input,2]
                if(pred_choice != 'back'){
                  if(pred_choice == 'rownames'){
                    variable <- row.names(data)
                  }
                  else if(pred_choice != 'rownames'){
                    var <- names(data)  %>% data.frame(names=.) %>% rownames_to_column() %>% subset(., .$names %in% pred_choice) %>% .[,1] %>% as.numeric()

                    variable <- data[,pred_choice]
                    data <- data[,-var]
                  }
                  model_choice <-
                    perModel(data = data,
                             response = chosen_response,
                             predictors = 'all')
                  print(lapply(model_choice, summary))
                  model_names <-
                    append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

                  sub11_input <-
                    menu(model_names$names, title = "Which model do you pick?")

                  model_response <-
                    model_names[model_names$rowname == sub11_input, 2]
                  message(model_response)
                  if(model_response != 'back'){

                    hat_model <- model_choice[[sub11_input]]
                    hat_msum <- hat_model %>% summary()
                    hat_msum$call <- model_response
                    hatv <- hatvalues(hat_model)
                    par(mfrow=c(1, 2))
                    halfnorm(hatv, labs=variable, ylab='Leverages')
                    qqnorm(rstandard(hat_model))
                    abline(0, 1)
                    message("Because these residuals have been standardized, we expect the points to approximately follow the y = x line if normality holds. Another advantage of the standardized form is that we can judge the size easily. An absolute value of 2 would be large but not exceptional for a standardized residual whereas a value of 4 would be very unusual under the standard normal.")
                    return(hat_msum)
                  }#model_response != 'back
                }#pred_choice !- 'back' close

              }#sub_input == 1 close
              else if(sub_input == 2){
                model_choice <-
                  perModel(data = data,
                           response = chosen_response,
                           predictors = 'all')
                print(lapply(model_choice, summary))
                model_names <-
                  append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

                sub11_input <-
                  menu(model_names$names, title = "Which model do you pick?")

                model_response <-
                  model_names[model_names$rowname == sub11_input, 2]
                message(model_response)
                if(model_response != 'back'){
                  stud_mod <- model_choice[[sub11_input]]
                  stud <- rstudent(stud_mod)
                  max_t <- stud[which.max(abs(stud))]
                  message("We will use the Bonferroni correction to test for outliers. Its biggest drawback is that it is conservative — it finds fewer outliers than the nominal level of confidence would dictate. The larger that n is, the more conservative it gets")
                  n <- nrow(data)
                  df <- stud_mod %>% summary() %>% .$df %>% split(nchar(.)) %>% last()
                  stud_t <- qt(0.05/(n*2), df)
                  if(max_t <= stud_t){
                    print(paste(round(max_t, 4),'<',round(stud_t,4), ': Accept the Null Hypothesis'))
                  }else if(max_t >= stud_t){
                    print(paste(round(max_t,4),'>',round(stud_t,4), ': Reject the Null Hypothesis'))
                  }
                  lm_select <- model_choice %>% lapply(., function(x)x$coefficients %>% length() %>% {if(. == 2)x}) %>% compact()
                  data_wo_r <- data[,-sub1_input]
                  data_wo_r <- data_wo_r %>% as.list(.)
                  data_wi_r <- data[, sub1_input]
                  lmod_arrange <- names(model_choice) %>% data.frame(names=.) %>% rownames_to_column() %>% arrange(names)
                  lmod_arrange$rowname <- as.numeric(lmod_arrange$rowname)
                  data_arrange <- names(data_wo_r) %>% data.frame(names=.) %>% rownames_to_column() %>% arrange(names)
                  data_arrange$rowname <- as.numeric(data_arrange$rowname)
                  data_wo_r<-data_wo_r[data_arrange$rowname]
                  lm_select<-lm_select[lmod_arrange$rowname] %>% compact()
                  #print(paste(data_wo_r, lm_select))
                  #return(data_wo_r)
                  n <- n2mfrow(length(lm_select), 2)
                  par(mfrow=n)
                  R2 <- lapply(lm_select, function(x)summary(x) %>% .$r.squared) %>% unlist()
                  mapply(function(x, y,z, k, p)plot(data_wi_r ~ y, xlab=z, ylab=chosen_response, main=paste(k,': R-squared = ',round(p, 3))) %>% with(abline(x)),lm_select,  data_wo_r, names(data_wo_r), names(lm_select), R2)
                  return(lm_select)
                }#model_response != 'back' close
              }#sub_input == 2 close
              else if(sub_input ==3){
                pred_labels <- append(predictors,c('none')) %>% data.frame(names=.) %>% rownames_to_column()
                #return(pred_labels)
                sub_labels <- menu(append(predictors,c('none')), title = "choose your column to labels points")
                labels_chosen <- pred_labels[pred_labels$rowname == sub_labels, 2]
                if(labels_chosen == 'none'){
                  data_w_r <- data
                } else if (labels_chosen != 'none'){
                  data_w_r <- data %>% column_to_rownames(labels_chosen)
                }
                model_choice <-
                  perModel(data = data_w_r,
                           response = chosen_response,
                           predictors = 'all')
                print(lapply(model_choice, summary))
                model_names <-
                  append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

                sub11_input <-
                  menu(model_names$names, title = "Which model do you pick?")

                model_response <-
                  model_names[model_names$rowname == sub11_input, 2]
                message(model_response)
                if(model_response != 'back'){
                  cook_mod <- model_choice[[sub11_input]]
                  form <- cook_mod %>% formula() %>% deparse()
                  predictors <-
                    form %>% strsplit(., " ") %>%
                    unlist() %>%
                    data.frame(names = .) %>%
                    slice(., n = seq(1, nrow(.), 2)) %>% .[2:nrow(.), ]
                  re_form <- reformulate(predictors, chosen_response)
                  new_lm <- lm(re_form, data)
                  cook <- cooks.distance(new_lm)
                  lmodi <- lm(re_form, data, subset = (cook < max(cook)))

                  #return(labels_chosen)
                  if(labels_chosen != 'none'){
                    data_w_l <- data[,labels_chosen]
                    halfnorm(cook, 3, labs=data_w_l, ylab="Cook's Distance")
                    p_modi <- lmodi %>% summary()
                    p_cook_m <- cook_mod %>% summary()
                    l_mod <- list(p_modi, p_cook_m)

                    message("We examine the leave-out-one difference in the coefficients")

                    n <- dfbeta(cook_mod) %>% ncol() -1
                    par(mfrow=n2mfrow(n, 2))
                    outlier_val <- c()

                    dfb_mod <- dfbeta(cook_mod)[,-1] %>% data.frame() %>% as.list()
                    xlabel <- names(dfb_mod) %>% split(unique(.))
                    click_results <- mapply(
                      function(x, y) {
                        plot(x, xlab='', ylab=glue('Change in {y} coef') )
                        abline(h=0)
                        identify(x, labels = data_w_l)
                      },
                      dfb_mod, xlabel
                    )
                    data_w_r_I <- data_w_r %>% mutate(ID = 1:nrow(.))
                    label_clicks <- lapply(click_results, function(x){
                      row.names(data_w_r_I)[which(data_w_r_I$ID %in% x)]
                    })


                    lm_influential_p<-lapply(label_clicks, function(x){subset(data_w_r,!row.names(data_w_r) %in% x) %>%
                        lm(re_form, .)
                    })
                    n2 <- n2mfrow(length(lm_influential_p)*4, 1)
                    par(mfrow=n2)
                    mapply(function(x,y){
                      plot(x, sub=names(y))
                    },lm_influential_p, names(lm_influential_p))
                    #lapply(lm_influential_p, function(x)plot(x))
                    lm_influential_p_sum <- lapply(lm_influential_p,summary)
                    return(lm_influential_p_sum)

                  } else if (labels_chosen == "none"){
                    halfnorm(cook,  ylab="Cook's Distance")

                    #data_w_l <- data[,labels_chosen]
                    halfnorm(cook, 3, ylab="Cook's Distance")
                    p_modi <- lmodi %>% summary()
                    p_cook_m <- cook_mod %>% summary()
                    l_mod <- list(p_modi, p_cook_m)

                    message("We examine the leave-out-one difference in the coefficients")

                    n <- dfbeta(cook_mod) %>% ncol() -1
                    par(mfrow=n2mfrow(n, 2))
                    outlier_val <- c()

                    dfb_mod <- dfbeta(cook_mod)[,-1] %>% data.frame() %>% as.list()
                    xlabel <- names(dfb_mod) %>% split(unique(.))
                    click_results <- mapply(
                      function(x, y) {
                        plot(x, xlab='', ylab=glue('Change in {y} coef') )
                        abline(h=0)
                        identify(x)
                      },
                      dfb_mod, xlabel
                    )
                    data_w_r_I <- data_w_r %>% mutate(ID = 1:nrow(.))
                    label_clicks <- lapply(click_results, function(x){
                      row.names(data_w_r_I)[which(data_w_r_I$ID %in% x)]
                    })

                    lm_influential_p<-lapply(label_clicks, function(x){subset(data_w_r,!row.names(data_w_r) %in% x) %>%
                        lm(re_form, .)
                    })
                    n2 <- n2mfrow(length(lm_influential_p)*4, 1)
                    par(mfrow=n2)
                    mapply(function(x,y){
                      plot(x, sub=names(y))
                    },lm_influential_p, names(lm_influential_p))
                    #lapply(lm_influential_p, function(x)plot(x))
                    lm_influential_p_sum <- lapply(lm_influential_p,summary)
                    return(lm_influential_p_sum)
                  }

                }#model_response - sub_input == 3 close
              }#sub_input == 3 close
            }#chosen_response != 'back' close
          }#chosen_sub != 'back' close
        }#input == 2 close
    else if (input == 3) {
      message('input 3')
      input_names <-
        append(names(data), 'back') %>% data.frame(names = .) %>% rownames_to_column()
      sub1_input <-
        menu(input_names$names, title = "Which is your response variable?")
      chosen_response <-input_names[input_names$rowname == sub1_input,2]
      predictors <- input_names[!(input_names$names) %in% c(chosen_response, 'back'), 2]
      if(chosen_response != 'back'){
        model_choice <-
          perModel(data = data,
                   response = chosen_response,
                   predictors = 'all')
        print(lapply(model_choice, summary))
        model_names <-
          append(names(model_choice), 'back') %>% data.frame(names = .) %>% rownames_to_column()

        sub11_input <-
          menu(model_names$names, title = "Which model do you pick?")

        model_response <-
          model_names[model_names$rowname == sub11_input, 2]
        message(model_response)
        if(model_response != 'back'){
          sub3_input <- menu(c("Partial Regression", "Partial Residuals", 'back'), title="Which plots do you want to fit?")
          if(sub3_input == 1){

            lmod <- model_choice[[sub11_input]]

            pm_wo_p <- lapply(predictors,function(x){
              perModel(data %>% dplyr::select(-x), response=chosen_response, predictors='all'
              )})
            pm_wo_p <- pm_wo_p %>% flatten()

            pm_wo_p_t_f <- pm_wo_p %>% lapply(., function(x)formula(x)) %>% unlist() %>% names() %>% strsplit(., ' ') %>% lapply(., function(x)
              length(x)) %>% {
                ifelse(. < which.max(.), FALSE, TRUE)
              } %>% split(., seq_along(.))

            res_comb_pt<- mapply(function(x, y){if(x == TRUE)y}, pm_wo_p_t_f, pm_wo_p) %>% compact()
            nm_1 <- mapply(function(x, y){if(x == TRUE)y}, pm_wo_p_t_f, pm_wo_p) %>% compact()  %>% lapply(., function(z)formula(z) %>% deparse())
            names(res_comb_pt) <- nm_1 %>% unlist()

            pos <- data %>% names %>% {which(. == chosen_response)}
            #we grab all the models with a predictor for response
            pm_wo_r <- lapply(predictors,function(x){
              perModel(data[,-pos], response=x, predictors='all'
              )}) %>% flatten()

            pm_wo_r_t_f <- pm_wo_r %>% lapply(., function(x)formula(x)) %>% unlist() %>% names() %>% strsplit(., ' ') %>% lapply(., function(x)
              length(x)) %>% {
                ifelse(. < which.max(.), FALSE, TRUE)
              } %>% split(., seq_along(.))

            res_comb_rt<- mapply(function(x, y){if(x == TRUE)y}, pm_wo_r_t_f, pm_wo_r) %>% compact()

            nm_2 <- mapply(function(x, y){if(x == TRUE)y}, pm_wo_r_t_f, pm_wo_r) %>% compact()  %>% lapply(., function(z)formula(z) %>% deparse())

            names(res_comb_rt) <- nm_2 %>% unlist()
            # return(res_comb_rt)

            df <- substitute(data)

            res_pt <- lapply(res_comb_pt, function(z)residuals(z))
            res_rt <- lapply(res_comb_rt, function(z)residuals(z))
            #return(res_pt)
            pos <- data %>% names() %>% {which(. == chosen_response)}
            pred_nm <- data[,-pos] %>% names()
            n <- length(res_rt)
            par(mfrow=n2mfrow(n, 2))
            mapply(function(x, y, z){
              plot(x, y, xlab=glue('{z} residuals', ylab='residuals'))
              abline(0, coef(lmod)[z])

            }, res_rt,res_pt, pred_nm)

          }else if(sub3_input == 2){
            pos <- data %>% names %>% {which(. == chosen_response)}

            lmod <- model_choice[[sub11_input]]
            le_nm <- lmod %>% formula() %>% deparse() %>% strsplit(., ' ')
            le_nm<-le_nm %>% unlist() %>% .[seq(3, length(.), 2)]

            termplot(lmod, partial.resid=TRUE, terms=1)

            vf <- c()
            for(i in 1:length(le_nm)){
              vf[[i]] <- shift(le_nm,i, dir='left')
            }
            arrangement_pred <- c(list(le_nm), vf[-length(vf)])

            arr_form <- arrangement_pred %>% lapply(., function(x)reformulate(x, response=chosen_response))
            n <- length(arr_form)
            par(mfrow=n2mfrow(n, 2))
            lapply(arr_form, function(x){
              mod <- lm(x, data)
              termplot(mod, partial.resid=TRUE, terms=1)
            })
            scan_values <- c()
            for (i in predictors) {
              menu(c(i), title = "Choose the cut-off value for the predictors")
              scan_values[[i]] <- scan(nmax = 1)
              #perTest(model_choice, list(data))
            }
            pred_l <- data[,-pos] %>% names() %>% split(unique(.))
            print(scan_values)
            #return(scan_values)

            nn <- length(scan_values)
            #wider format
            group_data <- mapply(function(x, y) {
              savings %>% mutate(group = ifelse(.[, y] > x, "Group 1 (GT)", "Group 2 (LT)"))
            }, val, names(val), SIMPLIFY = FALSE) %>%
              mapply(function(a,z) {
                a %>% `colnames<-`(c(names(.)[-length(.)], glue("{z}_group")))
              }, ., names(.), SIMPLIFY = FALSE) %>%
              Reduce(cbind, .) %>%
              .[, !duplicated(names(.))] %>% pivot_longer(-c(1:(length(.)-nn))) %>% dplyr::select(group=value) %>% cbind.data.frame(savings %>% pivot_longer(-c(1)), .)

            val_hline <- scan_values %>% unlist() %>% data.frame(hline=.) %>% rownames_to_column() %>% `colnames<-`(c('name', 'hline'))
            #return(group_data)
            print(val_hline)

            kop <- inner_join(group_data, val_hline, by='name')

            plot_mapping <- kop %>% ggplot(aes(x = value, y = sr, color = group)) +
              geom_point() + scale_color_manual(values = c("Group 1 (GT)" = "blue", "Group 2 (LT)" = "red")) +
              facet_wrap(name ~ ., scales = "free") + theme_bw() +
              theme(panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    strip.background = element_blank(),
                    panel.border = element_rect(colour = "black", fill = NA),
                    legend.position = "bottom") +
              stat_smooth(method='lm') +
              geom_vline(aes(xintercept=hline)) + xlab("Predictors") + ylab(glue("Response: {chosen_response}"))

            return(plot_mapping)
          }
        }#model_response != 'back' close
      }#chosen_response - input == 3 close
        }#input == 3 close
      }#while loop close
    }#function close
