server <- function(input, output, session) {
  predictors <- eventReactive(input$generate_predictors, {
    req(input$nlm)
    if (input$set_seed){
      set.seed(input$seed)
    }
    
    print("--------------------Finished generating predictors--------------------")
    return (generate_predictors(input$nlm))
  })
  
  output$predictors <- renderPlot({
    show_landscape(predictors())
  })
  

  
  simulation <- eventReactive(input$sim_outcome, {
    nlms <- generate_predictors(input$nlms_for_outcome)
    simulation <- raster()
    if (input$set_seed){
      set.seed(input$seed)
    }
    if (input$expression == ""){
      expression <- generate_random_function(nlms)
    }
    else {
      expression <- input$expression
    }
    # print(expression)
    simulation <- eval(parse(text=expression))
    if (input$r_noise == TRUE){
      r_noise <- raster(ncols=dimgrid, nrows=dimgrid, xmn=0, xmx=dimgrid, ymn=0, ymx=dimgrid)
      if (input$set_seed){
        set.seed(input$seed)
      }
      vals <- rnorm(dimgrid*dimgrid, sd=1)
      vals <- vals * 0.05
      r_noise <- setValues(r_noise, vals)
      simulation <- simulation + r_noise
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
      simulation <- simulation + s_noise
    }
    output$gen_prediction <- renderUI({
      conditionalPanel(condition = "input.cv_method.length > 0",
                       actionButton(
                         inputId = "gen_prediction", label = "Train model and generate prediction"
                       )
      )
    })
    
    simulation <- normalizeRaster(simulation)
    names(simulation) <- "outcome"
    return(simulation)
  })
  
  observeEvent(input$sim_outcome, {
    output$outcome <- renderPlot({
      print("----------------Finished generating simulated outcome-----------------")
      return(show_landscape(simulation()))
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
  
  observe({
    if (input$variable_selection != "None"){
      updateCheckboxInput(session, "show_prediction", value = TRUE)
      updateCheckboxInput(session, "show_difference", value = TRUE)
      updateCheckboxInput(session, "show_selected_predictors", value = TRUE)
    }
    else{
      updateCheckboxInput(session, "show_prediction", value = FALSE)
      updateCheckboxInput(session, "show_difference", value = FALSE)
      updateCheckboxInput(session, "show_selected_predictors", value = FALSE)
    }
  })
  
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
  
  observeEvent(input$gen_prediction, {
    print("--------------------Started generating prediction---------------------")
    predictors <- predictors()
    sampling_points <- sampling_points()
    # st_write(sampling_points, "./sampling_points.shp")
    simulation <- simulation()
    training_data_stack <- stack(coord_stack, simulation, predictors) # Create a stack of the simulated outcome, the coordinates(to calculate nndm indices later) and all predictors
    # writeRaster(training_data_stack, "./training_data_stack.grd")
    predictors_df <- as.data.frame(stack(predictors, coord_stack))
    predictors_as_sfc <- st_as_sf(predictors_df, coords = c("coord1", "coord2"), remove = F)
    
    sampling_points <- st_join(sampling_points, spatial_blocks) # Assign a spatial block to each sampling point
    training_data <- as.data.frame(extract(training_data_stack, sampling_points, sp = TRUE)) # Extract the informations of the predictors and the outcome on the positions of the sampling points
    training_data_as_sfc <- st_as_sf(training_data, coords = c("coord1", "coord2"), remove = F)
   
    
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
    
    
    
    
    models <- list() # Create list to store all models
    predictions <- list()
    dif <- list()
    aoa <- list()
    cv_errors <- list() #Create list to store the cv results of the models
    true_errors <- list()
    varImp <- list()
    # Create models and use the different cv_methods passed by the user
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
      dif[[i]] <- simulation() - predictions[[i]]
      dif_as_numeric <- raster::extract(abs(dif[[i]]), point_grid)
      RMSE <- sqrt(mean((dif_as_numeric)^2))
      Rsquared <- (cor(as.data.frame(simulation), as.data.frame(predictions[[i]]))^2)[1,1]
      MAE <- mean(abs(dif_as_numeric))
      true_error <- data.frame(RMSE, Rsquared, MAE)
      true_errors[[i]] <- true_error
      if (input$algorithm == "rf") {
        cv_errors[[i]] <- models[[i]]$results[c("RMSE", "Rsquared", "MAE")]
        # print(models[[i]])
        varImp[[i]] <- varImp(models[[i]], scale = FALSE)
        # View(varImp[[i]]["importance"])
      }
      else if(input$algorithm == "svmRadial"){
        cv_errors[[i]] <- models[[i]]$results[c("C", "RMSE", "Rsquared", "MAE")]
      }
    }
    names(models) <- input$cv_method # Name the models like their cv method!
    print(names(models))
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
        output$random_10_fold_cv_true_error <- renderTable(expr = true_errors[[j]], striped = TRUE, digits = 4, width = "100%")
        output$random_10_fold_cv_cv_error <- renderTable(expr = cv_errors[[j]], striped = TRUE, digits = 4, width = "100%" )
        output$random_10_fold_cv_prediction <- renderPlot({
          predictions[[j]][1] <- 0
          predictions[[j]][2] <- 1
          show_landscape(predictions[[j]])})
        output$random_10_fold_cv_difference <- renderPlot(show_landscape(dif[[j]]))
        output$random_10_fold_cv_aoa <- renderPlot(show_landscape(aoa[[j]]$AOA))
        output$random_10_fold_cv_di <- renderPlot(show_landscape(aoa[[j]]$DI))
        output$random_10_fold_cv_varImp <- renderPlot(plot(varImp[[j]]))
        # output$random_10_fold_cv_varImp <- renderPlot(plot_ffs(models[[j]]))
      }
      if (names(models[i]) == "loo_cv"){
        k <- i
        output$loo_cv_true_error <- renderTable(expr = true_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        output$loo_cv_cv_error <- renderTable(expr = cv_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        output$loo_cv_prediction <- renderPlot({
          predictions[[k]][1] <- 0
          predictions[[k]][2] <- 1
          show_landscape(predictions[[k]])})
        output$loo_cv_difference <- renderPlot(show_landscape(dif[[k]]))
        output$loo_cv_aoa <- renderPlot(show_landscape(aoa[[k]]$AOA))
        output$loo_cv_di <- renderPlot(show_landscape(aoa[[k]]$DI))
        output$loo_cv_varImp <- renderPlot(plot(varImp[[k]]))
        # output$loo_cv_varImp <- renderPlot(plot_ffs(models[[k]]))
      }
      if (names(models[i]) == "sb_cv"){
        l <- i
        output$sb_cv_true_error <- renderTable(expr = true_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        output$sb_cv_cv_error <- renderTable(expr = cv_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        output$sb_cv_prediction <- renderPlot({
          predictions[[l]][1] <- 0
          predictions[[l]][2] <- 1
          show_landscape(predictions[[l]])})
        output$sb_cv_difference <- renderPlot(show_landscape(dif[[l]]))
        output$sb_cv_aoa <- renderPlot(show_landscape(aoa[[l]]$AOA))
        output$sb_cv_di <- renderPlot(show_landscape(aoa[[l]]$DI))
        output$sb_cv_varImp <- renderPlot(plot(varImp[[l]]))
        # output$sb_cv_varImp <- renderPlot(plot_ffs(models[[l]]))
      }
      if (names(models[i]) == "nndm_loo_cv"){
        m <- i
        output$nndm_loo_cv_true_error <- renderTable(expr = true_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        output$nndm_loo_cv_cv_error <- renderTable(expr = cv_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        output$nndm_loo_cv_prediction <- renderPlot({
          predictions[[m]][1] <- 0
          predictions[[m]][2] <- 1
          show_landscape(predictions[[m]])})
        output$nndm_loo_cv_difference <- renderPlot(show_landscape(dif[[m]]))
        output$nndm_loo_cv_aoa <- renderPlot(show_landscape(aoa[[m]]$AOA))
        output$nndm_loo_cv_di <- renderPlot(show_landscape(aoa[[m]]$DI))
        output$nndm_loo_cv_varImp <- renderPlot(plot(varImp[[m]]))
        # output$nndm_loo_cv_varImp <- renderPlot(plot_ffs(models[[m]]))
      }
    }
    # For the first passed cv-method calculate a prediction the difference between
    # the simulated outcome and the prediction
    
    output$prediction <- renderPlot({
      # To rescale legend to values between 0 and 1 (same as simulated outcome):
      predictions[[1]][1] <- 0
      predictions[[1]][2] <- 1
      show_landscape(predictions[[1]])
    })
    
    output$dif <- renderPlot({
      show_landscape(dif[[1]])
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