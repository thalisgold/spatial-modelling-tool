server <- function(input, output, session) {
  
  predictors <- eventReactive(input$sim_predictors, {
    req(input$nlms_for_predictors)
    if (input$set_seed){
      set.seed(input$seed)
    }
    
    print("--------------------Finished generating predictors--------------------")
    return (generate_nlms(input$nlms_for_predictors))
  })
  
  output$predictors <- renderPlot({
    show_landscape(predictors())
  })
  
  target_variable <- eventReactive(input$sim_target_variable, {
    if (input$set_seed){
      set.seed(input$seed)
    }
    nlms <- generate_nlms(input$nlms_for_target_variable)
    target_variable <- raster()
    if (input$expression == ""){
      if (input$set_seed){
        set.seed(input$seed)
      }
      expression <- generate_random_function(nlms)
    }
    else {
      expression <- input$expression
    }
    # print(expression)
    target_variable <- eval(parse(text=expression))
    if (input$r_noise == TRUE){
      r_noise <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
      if (input$set_seed){
        set.seed(input$seed)
      }
      vals <- rnorm(dimgrid*dimgrid, sd=1)
      vals <- vals * 0.05
      r_noise <- setValues(r_noise, vals)
      target_variable <- target_variable + r_noise
    }
    if (input$s_noise == TRUE){
      if (input$set_seed){
        set.seed(input$seed)
      }
      variog_mod <- vgm(model = "Sph", psill = 1, range = 40, nugget = 0)
      gstat_mod <- gstat(formula = z~1, dummy = TRUE, beta = 0, model = variog_mod, nmax = 100)
      s_noise <- predict(gstat_mod, point_grid, nsim = 1)
      s_noise <- rasterFromXYZ(cbind(st_coordinates(s_noise),
                                     as.matrix(as.data.frame(s_noise)[,1], ncol=1)))
      s_noise <- s_noise * 0.05
      target_variable <- target_variable + s_noise
    }
    output$gen_prediction <- renderUI({
      conditionalPanel(condition = "input.cv_method.length > 0",
                       actionButton(
                         inputId = "gen_prediction", label = "Train model and generate prediction"
                       )
      )
    })
    
    target_variable <- normalizeRaster(target_variable)
    names(target_variable) <- "target_variable"
    return(target_variable)
  })
  
  observeEvent(input$sim_target_variable, {
    output$target_variable <- renderPlot({
      print("----------------Finished simulating target_variable-----------------")
      target_variables_with_coords <- stack(coord_stack, target_variable())
      target_variable_surface <- as.data.frame(raster::extract(target_variables_with_coords, point_grid))
      ggplot() +
        geom_raster(aes(x = coord1, y = coord2, fill = target_variable), data = target_variable_surface) +
        geom_sf(fill = "transparent", data = study_area) +
        xlab("") + ylab("") +
        scale_fill_viridis_c(name="", limits= c(0,1)) +
        theme_light()
      })
    })
  
  distInput <- reactive({
    switch(input$dist_sampling_points,
           "random",
           "regular",
           "clustered",
           "nonunif")
  })
  
  output$clustered <- reactive({
    if (input$dist_sampling_points == "clustered")
      return(TRUE)
  })
  
  outputOptions(output, "clustered", suspendWhenHidden = FALSE)
  
  sampling_points <- reactive({
    req(input$n_sampling_points, input$dist_sampling_points)
    if (input$set_seed){
      set.seed(input$seed)
    }
    if (input$dist_sampling_points != "clustered"){
      sampling_points <- generate_sampling_points(input$n_sampling_points, input$dist_sampling_points)
    }
    else{
      sampling_points <- clustered_sample(study_area, input$n_parents, input$n_offsprings, input$radius)
    }
  })
  
  output$sampling_points <- renderPlot({
    ggplot() +
      geom_sf(data = sampling_points(), size = 1) +
      geom_sf(data = study_area,  alpha = 0) +
      theme_light()
  })
  
  observe({
    if (input$variable_selection != "None"){
      updateCheckboxInput(session, "show_aoa", value = TRUE)
      updateCheckboxInput(session, "show_prediction", value = TRUE)
      updateCheckboxInput(session, "show_difference", value = TRUE)
      updateCheckboxInput(session, "show_selected_predictors", value = TRUE)
    }
    else{
      updateCheckboxInput(session, "show_aoa", value = TRUE)
      updateCheckboxInput(session, "show_prediction", value = FALSE)
      updateCheckboxInput(session, "show_difference", value = FALSE)
      updateCheckboxInput(session, "show_selected_predictors", value = FALSE)
    }
  })
  
  observeEvent(input$gen_prediction, {
    print("--------------------Started generating prediction---------------------")
    predictors <- predictors()
    target_variable <- target_variable()
    sampling_points <- sampling_points()
    sampling_points <- st_join(sampling_points, spatial_blocks) # Assign a spatial block to each sampling point
    training_data_stack <- stack(coord_stack, predictors, target_variable) # Create a stack of all predictors, the target_variable and the coordinates (to calculate nndm indices later on)
    training_data <- as.data.frame(extract(training_data_stack, sampling_points, sp = TRUE)) # Extract the informations of the predictors and the target_variable on the positions of the sampling points
    
    # # For distances plot?
    # predictors_df <- as.data.frame(stack(predictors, coord_stack))
    # predictors_as_sfc <- st_as_sf(predictors_df, coords = c("coord1", "coord2"), remove = F)
    # training_data_as_sfc <- st_as_sf(training_data, coords = c("coord1", "coord2"), remove = F)
    # # To plot variogram:
    # training_data_sp_df <- training_data
    # coordinates(training_data_sp_df)=~coord1+coord2
    # empvar <- variogram(outcome~1, data = training_data_sp_df)
    # fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T), fit.sills = TRUE)
    # outrange <- fitvar$range[2]
    # print(outrange)
    # output$test1 <- renderPlot(plot(empvar, fitvar,cutoff = 50, main = "Outcome semi-variogram estimation"))
    # 
    # #For plotting distances:
    # View(training_data_as_sfc)
    # print(class(predictors_and_coords_as_sfc)[1])
    # distribution <- plot_geodist(training_data_as_sfc, predictors_and_coords_as_sfc,
    #                              type = "feature",
    #                              showPlot = FALSE)
    
    # distribution$plot+scale_x_log10(labels=round)+ggtitle("Randomly distributed reference data")
    
    
    
    # Create lists to store all necessary information for each cv method selected! 
    models <- list()
    predictions <- list()
    dif <- list()
    aoa <- list()
    true_errors <- list()
    cv_errors <- list()
    varImp <- list()
    surface <- list()
    
    # Create models and use the different cv_methods passed by the user to estimate the error
    if (input$set_seed){
      set.seed(input$seed)
    }
    for (i in 1:length(input$cv_method)) {
      # Generate models
      models[[i]] <- train_model(input$algorithm, input$cv_method[i], training_data, predictors, input$variable_selection)
      
      # Generate predictions
      predictions[[i]] <- predict(predictors, models[[i]])
      names(predictions[[i]]) <- "prediction"
      
      # Generate aoa and di
      aoa[[i]] <- aoa(predictors, models[[i]])
      
      # Calculate true errors
      dif[[i]] <- target_variable() - predictions[[i]]
      names(dif[[i]]) <- "differences"
      dif_as_numeric <- raster::extract(abs(dif[[i]]), point_grid)
      RMSE <- sqrt(mean((dif_as_numeric)^2))
      Rsquared <- (cor(as.data.frame(target_variable), as.data.frame(predictions[[i]]))^2)[1,1]
      MAE <- mean(abs(dif_as_numeric))
      true_error <- data.frame(RMSE, Rsquared, MAE)
      true_errors[[i]] <- true_error
      
      result_with_coords <- stack(coord_stack, predictions[[i]], dif[[i]], aoa[[i]]$AOA)
      surface[[i]] <- as.data.frame(raster::extract(result_with_coords, point_grid))
      # print(head(surface[[i]]))
      
      
      if (input$variable_selection != "RFE") {
        # Calculate global cv errors
        global_cv_errors <- global_validation(models[[i]])
        RMSE <- global_cv_errors[1]
        Rsquared <- global_cv_errors[2]
        MAE <- global_cv_errors[3]
        cv_error <- data.frame(RMSE, Rsquared, MAE)
        cv_errors[[i]] <- cv_error
        
        # Save importance of the variables
        varImp[[i]] <- varImp(models[[i]], scale = FALSE)
        # View(models[[i]])
        # View(varImp[[i]])
      }
      else if (input$variable_selection == "RFE") {
        # Calculate cv errors
        number_selected_variables <- length(models[[i]]$optVariables)
        print(models[[i]]$results)
        cv_errors[[i]] <- models[[i]]$results[number_selected_variables, c("RMSE", "Rsquared", "MAE")]
        
        # Save importance of the variables
        # View(models[[i]])
        varImp.train <- varImp(models[[i]], scale = FALSE)
        varImp[[i]] <- varImp.train
        # View(varImp[[i]])
        # varImp[[i]] <- models[[i]]$optVariables
        # test <- predictors(models[[i]]) 
        # print(test)
        varimp_data <- data.frame(feature = row.names(varImp(models[[i]])),
                                  importance = varImp(models[[i]])[, 1])
        output$test1 <- renderPlot({
          ggplot(data = varimp_data, aes(x = reorder(feature, -importance), y = importance, fill = feature)) +
          geom_bar(stat="identity") + labs(x = "Features", y = "Variable Importance") + 
          geom_text(aes(label = round(importance, 2)), vjust=1.6, color="white", size=4) + 
          theme_bw() + theme(legend.position = "none")
        })
        # renderPlot(ggplot(data = models[[i]], metric = "RMSE") + theme_bw())
        output$test2 <- renderPlot(plot(models[[i]],type="o"))
        print(models[[i]]$optVariables)
      }
    }
    names(models) <- input$cv_method # Name the models like their cv method!
    # print(models[[1]])
    # print(names(models))
    # print(predictions)
    # print(dif)
    # print(aoa)
    # print(true_errors)
    # print(cv_errors)
    # print(varImp)
    
    # Output the results on the right place:
    for (i in 1:length(input$cv_method)) {
      if (names(models[i]) == "random_10_fold_cv"){
        j <- i
        output$random_10_fold_cv_prediction <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = prediction), data = surface[[j]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(name="", limits= c(0,1)) +
            theme_light()
          })
        output$random_10_fold_cv_difference <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = differences), data = surface[[j]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(option = "H", name="") +
            theme_light()
        })
        output$random_10_fold_cv_true_error <- renderTable(expr = true_errors[[j]], striped = TRUE, digits = 4, width = "100%")
        output$random_10_fold_cv_cv_error <- renderTable(expr = cv_errors[[j]], striped = TRUE, digits = 4, width = "100%" )
        output$random_10_fold_cv_aoa <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[j]]) +
            scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("0","1"), guide = guide_legend(reverse=TRUE)) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            theme_light()
        })
        output$random_10_fold_cv_di <- renderPlot(show_landscape(aoa[[j]]$DI))
        output$random_10_fold_cv_varImp <- renderPlot(plot(varImp[[j]]))
        # output$random_10_fold_cv_varImp <- renderPlot(plot_ffs(models[[j]]))
      }
      if (names(models[i]) == "loo_cv"){
        k <- i
        output$loo_cv_prediction <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = prediction), data = surface[[k]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(name="", limits= c(0,1)) +
            theme_light()
        })
        output$loo_cv_difference <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = differences), data = surface[[k]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(option = "H", name="") +
            theme_light()
        })
        output$loo_cv_true_error <- renderTable(expr = true_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        output$loo_cv_cv_error <- renderTable(expr = cv_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        output$loo_cv_aoa <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[k]]) +
            scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("0","1"), guide = guide_legend(reverse=TRUE)) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            theme_light()
        })
        output$loo_cv_di <- renderPlot(show_landscape(aoa[[k]]$DI))
        output$loo_cv_varImp <- renderPlot(plot(varImp[[k]]))
        # output$loo_cv_varImp <- renderPlot(plot_ffs(models[[k]]))
      }
      if (names(models[i]) == "sb_cv"){
        l <- i
        output$sb_cv_prediction <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = prediction), data = surface[[l]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(name="", limits= c(0,1)) +
            theme_light()
        })
        output$sb_cv_difference <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = differences), data = surface[[l]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(option = "H", name="") +
            theme_light()
        })
        output$sb_cv_true_error <- renderTable(expr = true_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        output$sb_cv_cv_error <- renderTable(expr = cv_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        output$sb_cv_aoa <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[l]]) +
            scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("0","1"), guide = guide_legend(reverse=TRUE)) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            theme_light()
        })
        output$sb_cv_di <- renderPlot(show_landscape(aoa[[l]]$DI))
        output$sb_cv_varImp <- renderPlot(plot(varImp[[l]]))
        # output$sb_cv_varImp <- renderPlot(plot_ffs(models[[l]]))
      }
      if (names(models[i]) == "nndm_loo_cv"){
        m <- i
        output$nndm_loo_cv_prediction <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = prediction), data = surface[[m]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(name="", limits= c(0,1)) +
            theme_light()
        })
        output$nndm_loo_cv_difference <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = differences), data = surface[[m]]) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            scale_fill_viridis_c(option = "H", name="") +
            theme_light()
        })
        output$nndm_loo_cv_true_error <- renderTable(expr = true_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        output$nndm_loo_cv_cv_error <- renderTable(expr = cv_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        output$nndm_loo_cv_aoa <- renderPlot({
          ggplot() +
            geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[m]]) +
            scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("0","1"), guide = guide_legend(reverse=TRUE)) +
            geom_sf(fill = "transparent", data = study_area) +
            xlab("") + ylab("") +
            theme_light()
        })
        output$nndm_loo_cv_di <- renderPlot(show_landscape(aoa[[m]]$DI))
        output$nndm_loo_cv_varImp <- renderPlot(plot(varImp[[m]]))
        # output$nndm_loo_cv_varImp <- renderPlot(plot_ffs(models[[m]]))
      }
    }
    # For the first passed cv-method calculate a prediction the difference between
    # the simulated outcome and the prediction
    
    output$prediction <- renderPlot({
      ggplot() +
        geom_raster(aes(x = coord1, y = coord2, fill = prediction), data = surface[[1]]) +
        geom_sf(fill = "transparent", data = study_area) +
        xlab("") + ylab("") +
        scale_fill_viridis_c(name="", limits= c(0,1)) +
        theme_light()
    })
    
    output$dif <- renderPlot({
      ggplot() +
        geom_raster(aes(x = coord1, y = coord2, fill = differences), data = surface[[1]]) +
        geom_sf(fill = "transparent", data = study_area) +
        xlab("") + ylab("") +
        scale_fill_viridis_c(option = "H", name="") +
        theme_light()
      })
    
    output$cv_methods <- reactive({
      return(names(models))
    })
    outputOptions(output, "cv_methods", suspendWhenHidden = FALSE)
    
    # output$variable_selection <- reactive({
    #   return(input$variable_selection)
    # })
    # outputOptions(output, "variable_selection", suspendWhenHidden = FALSE) 
    
    output$finished_prediction <- reactive({
      return(TRUE)
    })
    
    outputOptions(output, "finished_prediction", suspendWhenHidden = FALSE) 
    
    print("-------------------Finished generating prediction---------------------")
  })
}