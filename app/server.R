server <- function(input, output, session) {
  
  predictors <- eventReactive(input$sim_predictors, {
    req(input$nlms_for_predictors)
    if (input$set_seed){
      set.seed(input$seed)
    }
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
    
    # When no expression is typed in by the user generate random expression
    if (input$expression == ""){ 
      if (input$set_seed){
        set.seed(input$seed)
      }
      expression <- generate_random_function(nlms)
    }
    else {
      expression <- input$expression
    }
    target_variable <- eval(parse(text=expression))
    
    # Add random noise
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
    
    # Add spatially correlated noise (40%)
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
    
    # Generate predict button after the target variable is simulated
    output$gen_prediction <- renderUI({
      conditionalPanel(condition = "input.cv_method.length > 0",
                       actionButton(
                         inputId = "gen_prediction", label = "Train model and generate prediction"
                       )
      )
    })
    
    # Normalize target variable to get values between 0 and 1
    target_variable <- normalizeRaster(target_variable)
    names(target_variable) <- "target_variable"
    return(target_variable)
  })
  
  observeEvent(input$sim_target_variable, {
    # Link the target variable to the coordinates stack in order to be able to display it
    output$target_variable <- renderPlot({
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
    switch(input$dist_sample_points,
           "random",
           "regular",
           "clustered",
           "nonunif")
  })
  
  output$clustered <- reactive({
    if (input$dist_sample_points == "clustered")
      return(TRUE)
  })
  
  outputOptions(output, "clustered", suspendWhenHidden = FALSE)
  
  sample_points <- reactive({
    req(input$n_sample_points, input$dist_sample_points)
    if (input$set_seed){
      set.seed(input$seed)
    }
    if (input$dist_sample_points != "clustered"){
      sample_points <- simulate_sample_points(input$n_sample_points, input$dist_sample_points)
    }
    else{
      sample_points <- clustered_sample(study_area, input$n_parents, input$n_offsprings, input$radius)
    }
  })
  
  output$sample_points <- renderPlot({
    ggplot() +
      geom_sf(data = sample_points(), size = 1) +
      geom_sf(data = study_area,  alpha = 0) +
      theme_light()
  })
  
  # When variable selection is selected, the prediction, difference and selected predictors are displayed for all chosen CV methods, as they may differ significantly.
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
  
  observeEvent(input$gen_prediction, {
    predictors <- predictors()
    target_variable <- target_variable()
    sample_points <- sample_points()
    sample_points <- st_join(sample_points, spatial_blocks) # Assign a spatial block to each sample point
    training_data_stack <- stack(coord_stack, predictors, target_variable) # Create a stack of all predictors, the target_variable and the coordinates (to calculate nndm indices later on)
    training_data <- as.data.frame(extract(training_data_stack, sample_points, sp = TRUE)) # Extract the informations of the predictors and the target_variable on the positions of the sample points
    
    # To plot sample to sample and sample to prediction points distances it is necessary to assign a CRS
    sample_points_for_distances <- sample_points()
    st_crs(sample_points_for_distances) <- "+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs"
    predictors_for_distances <- study_area
    st_crs(predictors_for_distances) <- "+proj=utm +zone=32 +ellps=WGS84 +units=m +no_defs"
    
    # Create cv folds to compute cv distances
    random_10_fold_cv_folds <- createFolds(training_data, k = 10)
    loo_cv_folds <- createFolds(training_data, k = 50)
    sb_cv_folds <- CreateSpacetimeFolds(training_data,spacevar = "ID",k=length(unique(training_data$ID)))
    
    # For NNDM LOO CV it is firstly necessary to fit a variogram!
    training_data_as_sfc <- st_as_sf(training_data, coords = c("coord1", "coord2"), remove = F) 
    predictors_df <- as.data.frame(stack(predictors, coord_stack))
    predictors_as_sfc <- st_as_sf(predictors_df, coords = c("coord1", "coord2"), remove = F)
    training_data_sp_df <- training_data
    coordinates(training_data_sp_df)=~coord1+coord2
    empvar <- variogram(target_variable~1, data = training_data_sp_df)
    fitvar <- fit.variogram(empvar, vgm(model="Sph", nugget = T), fit.sills = TRUE)
    outrange <- fitvar$range[2]
    
    # Plot variogram if wished
    # output$testPlot <- renderPlot(plot(empvar, fitvar,cutoff = 50, main = "Outcome semi-variogram estimation"))
    
    nndm_loo_cv_folds <- nndm(training_data_as_sfc, predictors_as_sfc, outrange, min_train = 0.5)
    
    
    # Compute distances
    output$random_10_fold_cv_distances <- renderPlot(plot_geodist(sample_points_for_distances, predictors_for_distances, cvfolds =  random_10_fold_cv_folds, type = "geo",showPlot = TRUE))
    output$loo_cv_distances <- renderPlot(plot_geodist(sample_points_for_distances, predictors_for_distances, cvfolds =  loo_cv_folds, type = "geo",showPlot = TRUE))
    output$sb_cv_distances <- renderPlot(plot_geodist(sample_points_for_distances, predictors_for_distances, cvfolds =  sb_cv_folds$indexOut, type = "geo",showPlot = TRUE))
    output$nndm_loo_cv_distances <- renderPlot(plot_geodist(sample_points_for_distances, predictors_for_distances, cvfolds = nndm_loo_cv_folds$indx_test, cvtrain = nndm_loo_cv_folds$indx_train, type = "geo",showPlot = TRUE))

    
    
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
      models[[i]] <- train_model(input$algorithm, input$cv_method[i], training_data, predictors, input$variable_selection, nndm_loo_cv_folds)
      
      # Generate predictions
      predictions[[i]] <- predict(predictors, models[[i]])
      names(predictions[[i]]) <- "prediction"
      
      # Generate AOA and DI
      if (input$variable_selection != "RFE") {
        aoa[[i]] <- aoa(predictors, models[[i]])
      }
      
      # Calculate true errors
      dif[[i]] <- abs(target_variable() - predictions[[i]])
      names(dif[[i]]) <- "differences"
      dif_as_numeric <- raster::extract(dif[[i]], point_grid)
      RMSE <- sqrt(mean((dif_as_numeric)^2))
      Rsquared <- (cor(as.data.frame(target_variable), as.data.frame(predictions[[i]]))^2)[1,1]
      MAE <- mean(abs(dif_as_numeric))
      true_error <- data.frame(RMSE, Rsquared, MAE)
      true_errors[[i]] <- true_error
      
      # Since it is not possible to calculate the AOA and the global CV error for RFE, we need to create two different result objects
      if (input$variable_selection != "RFE") {
        result_with_coords <- stack(coord_stack, predictions[[i]], dif[[i]], aoa[[i]]$AOA, aoa[[i]]$DI)
        
        # Calculate global cv errors
        global_cv_errors <- global_validation(models[[i]])
        RMSE <- global_cv_errors[1]
        Rsquared <- global_cv_errors[2]
        MAE <- global_cv_errors[3]
        cv_error <- data.frame(RMSE, Rsquared, MAE)
        cv_errors[[i]] <- cv_error
        
        # Save importance of the variables (we save it in a data frame, so it can be visualized in the same way as for RFE)
        # Otherwise RFE variable importance could not be plotted!
        importance <- varImp(models[[i]], scale = FALSE)[["importance"]]
        features <- rownames(importance)
        importance <- importance$Overall
        varImpDF <- data.frame(Features = features, Importance = importance)
        varImp[[i]] <- varImpDF
      }
      else if (input$variable_selection == "RFE") {
        result_with_coords <- stack(coord_stack, predictions[[i]], dif[[i]])
        
        # Calculate cv errors
        number_selected_variables <- length(models[[i]]$optVariables)
        cv_errors[[i]] <- models[[i]]$results[number_selected_variables, c("RMSE", "Rsquared", "MAE")]
        
        # Save importance of the variables
        opt_variables <- models[[i]][["optVariables"]]
        true <- varImp(models[[i]], scale = FALSE)
        features <- rownames(true)
        importance <- varImp(models[[i]], scale = FALSE)[,1]
        varImpDF <- data.frame(Features = features, Importance = importance)
        varImpDF <- varImpDF[varImpDF$Features %in% opt_variables,]
        varImp[[i]] <- varImpDF
      }
      
      # Save all informations to plot the the prediction, difference, AOA and DI later on 
      surface[[i]] <- as.data.frame(raster::extract(result_with_coords, point_grid))
    }
    names(models) <- input$cv_method # Name the models like their cv method!
    
    # Output the results on the right place:
    for (i in 1:length(input$cv_method)) {
      
      ################ RANDOM 10-FOLD CV ################
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
            scale_fill_viridis_c(name="") +
            theme_light()
        })
        if (input$variable_selection != "RFE") {
          if (allSame(aoa[[j]]$AOA@data@values) && aoa[[j]]$AOA@data@values[1] == 1){
            output$random_10_fold_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[j]]) +
                scale_fill_manual(name= "", values = c("#3CBB75FF"), labels = c("Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          else {
            output$random_10_fold_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[j]]) +
                scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("Not applicable","Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          output$random_10_fold_cv_di <- renderPlot({
            ggplot() +
              geom_raster(aes(x = coord1, y = coord2, fill = DI), data = surface[[j]]) +
              geom_sf(fill = "transparent", data = study_area) +
              xlab("") + ylab("") +
              scale_fill_viridis_c(name="") +
              theme_light()
          })
        }
        if (input$variable_selection == "None") {
          output$random_10_fold_cv_true_error <- renderTable(expr = true_errors[[1]], striped = TRUE, digits = 4, width = "100%")
        }
        else if (input$variable_selection != "None") {
          output$random_10_fold_cv_true_error <- renderTable(expr = true_errors[[j]], striped = TRUE, digits = 4, width = "100%")
        }
        output$random_10_fold_cv_cv_error <- renderTable(expr = cv_errors[[j]], striped = TRUE, digits = 4, width = "100%" )
        output$random_10_fold_cv_varImp <- renderPlot({
          ggplot(data=varImp[[j]], aes(x=Importance, y= reorder(Features, Importance))) +
            ylab("Features") +
            geom_bar(stat="identity", width = 0.3, color="grey30", fill="grey60") + 
            theme_light()
        })
        }
      
      ################### LOO CV ###################
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
            scale_fill_viridis_c(name="") +
            theme_light()
        })
        if (input$variable_selection != "RFE") {
          if (allSame(aoa[[k]]$AOA@data@values) && aoa[[k]]$AOA@data@values[1] == 1){
            output$loo_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[k]]) +
                scale_fill_manual(name= "", values = c("#3CBB75FF"), labels = c("Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          else {
            output$loo_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[k]]) +
                scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("Not applicable","Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          output$loo_cv_di <- renderPlot({
            ggplot() +
              geom_raster(aes(x = coord1, y = coord2, fill = DI), data = surface[[k]]) +
              geom_sf(fill = "transparent", data = study_area) +
              xlab("") + ylab("") +
              scale_fill_viridis_c(name="") +
              theme_light()
          })
        }
        if (input$variable_selection == "None") {
          output$loo_cv_true_error <- renderTable(expr = true_errors[[1]], striped = TRUE, digits = 4, width = "100%")
        }
        else if (input$variable_selection != "None") {
          output$loo_cv_true_error <- renderTable(expr = true_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        }
        output$loo_cv_cv_error <- renderTable(expr = cv_errors[[k]], striped = TRUE, digits = 4, width = "100%")
        output$loo_cv_varImp <- renderPlot({
          ggplot(data=varImp[[k]], aes(x=Importance, y= reorder(Features, Importance))) +
            ylab("Predictors") +
            geom_bar(stat="identity", width = 0.3, color="grey30", fill="grey60") + 
            theme_light()
        })
      }
      ################ SPATIAL BLOCK CV ################
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
            scale_fill_viridis_c(name="") +
            theme_light()
        })
        if (input$variable_selection != "RFE") {
          if (allSame(aoa[[l]]$AOA@data@values) && aoa[[l]]$AOA@data@values[1] == 1){
            output$sb_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[l]]) +
                scale_fill_manual(name= "", values = c("#3CBB75FF"), labels = c("Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          else {
            output$sb_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[l]]) +
                scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("Not applicable","Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          output$sb_cv_di <- renderPlot({
            ggplot() +
              geom_raster(aes(x = coord1, y = coord2, fill = DI), data = surface[[l]]) +
              geom_sf(fill = "transparent", data = study_area) +
              xlab("") + ylab("") +
              scale_fill_viridis_c(name="") +
              theme_light()
          })
        }
        if (input$variable_selection == "None") {
          output$sb_cv_true_error <- renderTable(expr = true_errors[[1]], striped = TRUE, digits = 4, width = "100%")
        }
        else if (input$variable_selection != "None") {
          output$sb_cv_true_error <- renderTable(expr = true_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        }
        output$sb_cv_cv_error <- renderTable(expr = cv_errors[[l]], striped = TRUE, digits = 4, width = "100%")
        output$sb_cv_varImp <- renderPlot({
          ggplot(data=varImp[[l]], aes(x=Importance, y= reorder(Features, Importance))) +
            ylab("Precitors") +
            geom_bar(stat="identity", width = 0.3, color="grey30", fill="grey60") + 
            theme_light()
        })
      }
      ################ NNDM LOO CV ################
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
            scale_fill_viridis_c(name="") +
            theme_light()
        })
        if (input$variable_selection != "RFE") {
          if (allSame(aoa[[m]]$AOA@data@values) && aoa[[m]]$AOA@data@values[1] == 1){
            output$nndm_loo_cv3_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[m]]) +
                scale_fill_manual(name= "", values = c("#3CBB75FF"), labels = c("Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          else {
            output$nndm_loo_cv_aoa <- renderPlot({
              ggplot() +
                geom_raster(aes(x = coord1, y = coord2, fill = as.character(AOA)), data = surface[[m]]) +
                scale_fill_manual(name= "", values = c("#440164FF", "#3CBB75FF"), labels = c("Not applicable","Applicable"), guide = guide_legend(reverse=TRUE)) +
                geom_sf(fill = "transparent", data = study_area) +
                xlab("") + ylab("") +
                theme_light() + theme(legend.position = "bottom", legend.text = element_text(margin = margin(r = 10, unit = "pt"), size = 12)) 
            })
          }
          output$nndm_loo_cv_di <- renderPlot({
            ggplot() +
              geom_raster(aes(x = coord1, y = coord2, fill = DI), data = surface[[m]]) +
              geom_sf(fill = "transparent", data = study_area) +
              xlab("") + ylab("") +
              scale_fill_viridis_c(name="") +
              theme_light()
          })
        }
        if (input$variable_selection == "None") {
          output$nndm_loo_cv_true_error <- renderTable(expr = true_errors[[1]], striped = TRUE, digits = 4, width = "100%")
        }
        else if (input$variable_selection != "None") {
          output$nndm_loo_cv_true_error <- renderTable(expr = true_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        }
        output$nndm_loo_cv_cv_error <- renderTable(expr = cv_errors[[m]], striped = TRUE, digits = 4, width = "100%")
        output$nndm_loo_cv_varImp <- renderPlot({
          ggplot(data=varImp[[m]], aes(x=Importance, y= reorder(Features, Importance))) +
            ylab("Predictors") +
            geom_bar(stat="identity", width = 0.3, color="grey30", fill="grey60") + 
            theme_light()
        })
      }
    }
    
    # For the first passed cv-method calculate a prediction and the difference between the target variable and the prediction
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
        scale_fill_viridis_c(name="") +
        theme_light()
      })
    
    output$cv_methods <- reactive({
      return(names(models))
    })
    outputOptions(output, "cv_methods", suspendWhenHidden = FALSE)
    
    output$finished_prediction <- reactive({
      return(TRUE)
    })
    
    outputOptions(output, "finished_prediction", suspendWhenHidden = FALSE)
  })
}