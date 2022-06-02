ui <- navbarPage(title = "Spatial modelling tool", theme = shinytheme("flatly"),
                 tabPanel("App",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              
                              # Add spinner so that the user knows that the tool is performing calculations in the background
                              add_busy_spinner(spin = "circle",timeout = 800, color = "#0dc5c1", position = "bottom-right", height = "70px", width = "70px"),
                              
                              
                              # It is possible to plant a seed in order to always achieve the same results and thus comparability.
                              checkboxInput(
                                inputId = "set_seed",
                                label = "Set following seed to make your results reproducible:",
                                value = TRUE
                              ),
                              conditionalPanel(condition = "input.set_seed",
                                               numericInput(
                                                 inputId = "seed",
                                                 label = "",
                                                 value = 1,
                                                 min = 1,
                                                 max = 10000,
                                                 step = 1,
                                                 width = "25%"
                                               )
                              ),
                              br(),
                              
                              
                              h4("Step 1: Simulation of the predictors"),
                              # Choose at least two NLMs to simulate the predictors.
                              selectInput(
                                inputId = "nlms_for_predictors",
                                label = "Choose some NLMs to simulate predictors:",
                                choices = c("fractional_brownian_motion_05",
                                            "fractional_brownian_motion_10",
                                            "fractional_brownian_motion_20",
                                            "fractional_brownian_motion_30",
                                            "fractional_brownian_motion_40",
                                            "gaussian_random_field_05",
                                            "gaussian_random_field_10",
                                            "gaussian_random_field_20",
                                            "gaussian_random_field_30",
                                            "gaussian_random_field_40",
                                            "distance_gradient",
                                            "edge_gradient",
                                            "planar_gradient",
                                            "polygonal_landscapes",
                                            "random",
                                            "random_neighbourhood",
                                            "random_rectangular_cluster"),
                                multiple = TRUE,
                              ),
                              conditionalPanel(condition = "input.nlms_for_predictors.length >= 2",
                                               actionButton(
                                                 inputId = "sim_predictors",
                                                 label = "Simulate selected predictors"
                                               )
                              ),
                              br(),
                              br(),
                              
                              
                              h4("Step 2: Simulation of the target variable"),
                              selectInput(
                                inputId = "nlms_for_target_variable",
                                label = "Simulate the target variable from following NLMs:",
                                choices = c("fractional_brownian_motion_05",
                                            "fractional_brownian_motion_10",
                                            "fractional_brownian_motion_20",
                                            "fractional_brownian_motion_30",
                                            "fractional_brownian_motion_40",
                                            "gaussian_random_field_05",
                                            "gaussian_random_field_10",
                                            "gaussian_random_field_20",
                                            "gaussian_random_field_30",
                                            "gaussian_random_field_40",
                                            "distance_gradient",
                                            "edge_gradient",
                                            "planar_gradient",
                                            "polygonal_landscapes",
                                            "random",
                                            "random_neighbourhood",
                                            "random_rectangular_cluster"),
                                multiple = TRUE),
                              textAreaInput(inputId = "expression", label = "Enter an expression that describes how the target variable is to be calculated (optional):", placeholder = "nlms$distance_gradient^2 - nlms$edge_gradient"),
                              checkboxInput(inputId = "r_noise", label = "Add random noise", value = FALSE),
                              checkboxInput(inputId = "s_noise", label = "Add spatially correlated noise", value = FALSE),
                              conditionalPanel(condition = "input.nlms_for_target_variable.length >= 1",
                                               actionButton(
                                                 inputId = "sim_target_variable",
                                                 label = "Simulate target variable"
                                               )
                              ),
                              br(),
                              br(),
                              
                              
                              h4("Step 3: Simulation of the sample points"),
                              # Select the number and distribution of the sample points.
                              selectInput(
                                inputId = "dist_sample_points",
                                label = "Distribution of sample points:",
                                choices = c("Clustered" = "clustered",
                                            "Non-uniform" = "nonunif",
                                            "Random" = "random",
                                            "Regular" = "regular"),
                                selected = "random"
                              ),
                              conditionalPanel(condition = "!output.clustered",
                                               numericInput(
                                                 inputId = "n_sample_points",
                                                 label = "Number of sample points:",
                                                 value = 50,
                                                 min = 50,
                                                 max = 250,
                                                 step = 50,
                                                 width = "50%"
                                               )
                              ),
                              # If "clustered" is selected as the distribution, three sliders open to determine further parameters.
                              conditionalPanel(condition = "output.clustered",
                                               sliderInput(
                                                 inputId = "n_parents",
                                                 label = "Number of parents:",
                                                 value = 5,
                                                 min = 1,
                                                 max = 20,
                                                 step = 1,
                                                 width = "100%"
                                               ),
                                               sliderInput(
                                                 inputId = "n_offsprings",
                                                 label = "Number of offsprings:",
                                                 value = 45,
                                                 min = 10,
                                                 max = 250,
                                                 step = 1,
                                                 width = "100%"
                                               ),
                                               sliderInput(
                                                 inputId = "radius",
                                                 label = "Radius:",
                                                 value = 5,
                                                 min = 1,
                                                 max = 8,
                                                 step = 1,
                                                 width = "100%"
                                               )
                              ),
                              br(),
                              br(),
                              
                              
                              h4("Step 4: Model training and prediction"),
                              radioButtons(
                                inputId = "algorithm", 
                                label = "Choose algorithm for training:",
                                choices = c("Random Forest" = "rf", 
                                            "Support Vector Machines" = "svmRadial"),
                                selected = "rf"
                              ),
                              selectInput(
                                inputId = "cv_method",
                                label = "Cross-validation method:",
                                choices = c("Random 10-fold CV" = "random_10_fold_cv",
                                            "LOO CV" = "loo_cv",
                                            "Spatial block CV" = "sb_cv",
                                            "NNDM LOO CV" = "nndm_loo_cv"
                                ),
                                multiple = TRUE,
                              ),
                              conditionalPanel(condition = "input.algorithm == 'rf'",
                                               selectInput(
                                                 inputId = "variable_selection", label = "Variable selection:",
                                                 choices = c("None", "FFS", "RFE"),
                                                 selected = "None"
                                               )
                              ),
                              # When the target variable has been calculated, it is possible to train a model and make a prediction.
                              uiOutput("gen_prediction"),
                              conditionalPanel(condition = "output.finished_prediction",
                                               br(),
                                               br(),
                                               h4("Show the following for all selected CV methods:"),
                                               p(),
                                               checkboxInput(
                                                 inputId = "show_prediction",
                                                 label = "Prediction",
                                                 value = FALSE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_difference",
                                                 label = "Absolute difference",
                                                 value = FALSE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_selected_predictors",
                                                 label = "Selected predictors",
                                                 value = FALSE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_aoa",
                                                 label = "AOA",
                                                 value = TRUE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_di",
                                                 label = "DI",
                                                 value = FALSE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_distances",
                                                 label = "Distance distributions",
                                                 value = FALSE
                                               ),
                                               ),
                              ),
                            
                            
                            mainPanel(
                              width = 9,
                              fluidRow(
                                column(6, conditionalPanel(condition = "input.sim_predictors",
                                                           wellPanel(
                                                             h4("Predictors"),
                                                             withSpinner(
                                                             plotOutput(outputId = "predictors"), type = 4, color = "#0dc5c1"
                                                             )
                                                           )
                                )
                                ),
                                column(6, conditionalPanel(condition = "input.sim_target_variable",
                                                           wellPanel(
                                                             h4("Sample points"),
                                                             plotOutput(outputId = "sample_points")
                                                           )
                                )
                                ),
                              ),
                              
                              
                              conditionalPanel(condition = "input.sim_target_variable",
                                               fluidRow(
                                                 column(4, 
                                                        wellPanel(
                                                          h4("Target variable"),
                                                          withSpinner(
                                                          plotOutput(outputId = "target_variable"), type = 4, color = "#0dc5c1"
                                                          )
                                                        ),
                                                 ),
                                                 column(4, conditionalPanel(condition = "output.finished_prediction && !input.show_prediction",
                                                                            wellPanel(
                                                                              h4("Prediction"),
                                                                              plotOutput(outputId = "prediction"),
                                                                            )
                                                 )
                                                 ),
                                                 column(4, conditionalPanel(condition = "output.finished_prediction && !input.show_prediction",
                                                                            wellPanel(
                                                                              h4("Absolute difference"),
                                                                              plotOutput(outputId = "dif"),
                                                                            )
                                                 )
                                                 ),
                                               ),
                              ),
                              
                              
                              conditionalPanel(condition = "output.finished_prediction",
                                               fluidRow(
                                                 column(3,
                                                        wellPanel(
                                                          h4("Random 10-fold CV"),
                                                        ),
                                                        conditionalPanel(condition = "output.cv_methods.includes('random_10_fold_cv')",
                                                                         wellPanel(
                                                                           h5("True error:"),
                                                                           tableOutput(outputId = "random_10_fold_cv_true_error"),
                                                                           h5("CV error (global):"),
                                                                           tableOutput(outputId = "random_10_fold_cv_cv_error"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_prediction)",
                                                                         wellPanel(
                                                                           h5("Prediction:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_prediction"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_difference)",
                                                                         wellPanel(
                                                                           h5("Absolute difference:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_aoa && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_di && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_di"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_distances)",
                                                                         wellPanel(
                                                                           h5("Distance distributions:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_distances"),
                                                                         )
                                                        ),
                                                 ),
                                                 column(3,
                                                        wellPanel(
                                                          h4("LOO CV"),
                                                        ),
                                                        conditionalPanel(condition = "output.cv_methods.includes('loo_cv')", #BUGGY WHEN ONLY NNDM LOO CV IS CHOSEN
                                                                         wellPanel(
                                                                           h5("True error:"),
                                                                           tableOutput(outputId = "loo_cv_true_error"),
                                                                           h5("CV error (global):"),
                                                                           tableOutput(outputId = "loo_cv_cv_error"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_prediction)",
                                                                         wellPanel(
                                                                           h5("Prediction:"),
                                                                           plotOutput(outputId = "loo_cv_prediction"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_difference)",
                                                                         wellPanel(
                                                                           h5("Absolute difference:"),
                                                                           plotOutput(outputId = "loo_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "loo_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_aoa && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "loo_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_di && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "loo_cv_di"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_distances)",
                                                                         wellPanel(
                                                                           h5("Distance distributions:"),
                                                                           plotOutput(outputId = "loo_cv_distances"),
                                                                         )
                                                        ),
                                                 ),
                                                 column(3,
                                                        wellPanel(
                                                          h4("Spatial block CV"),
                                                        ),
                                                        conditionalPanel(condition = "output.cv_methods.includes('sb_cv')",
                                                                         wellPanel(
                                                                           h5("True error:"),
                                                                           tableOutput(outputId = "sb_cv_true_error"),
                                                                           h5("CV error (global):"),
                                                                           tableOutput(outputId = "sb_cv_cv_error"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_prediction)",
                                                                         wellPanel(
                                                                           h5("Prediction:"),
                                                                           plotOutput(outputId = "sb_cv_prediction"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_difference)",
                                                                         wellPanel(
                                                                           h5("Absolute difference:"),
                                                                           plotOutput(outputId = "sb_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "sb_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_aoa && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "sb_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_di && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "sb_cv_di"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_distances)",
                                                                         wellPanel(
                                                                           h5("Distance distributions:"),
                                                                           plotOutput(outputId = "sb_cv_distances"),
                                                                         )
                                                        ),
                                                 ),
                                                 column(3,                                    
                                                        wellPanel(
                                                          h4("NNDM LOO CV"),
                                                        ),
                                                        conditionalPanel(condition = "output.cv_methods.includes('nndm_loo_cv')",
                                                                         wellPanel(
                                                                           h5("True error:"),
                                                                           tableOutput(outputId = "nndm_loo_cv_true_error"),
                                                                           h5("CV error (global):"),
                                                                           tableOutput(outputId = "nndm_loo_cv_cv_error"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_prediction)",
                                                                         wellPanel(
                                                                           h5("Prediction:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_prediction"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_difference)",
                                                                         wellPanel(
                                                                           h5("Absolute difference:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_aoa && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_di && input.variable_selection != 'RFE')",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_di"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_distances)",
                                                                         wellPanel(
                                                                           h5("Distance distributions:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_distances"),
                                                                         )
                                                        ),
                                                 ),
                                               ),
                              ),
                              plotOutput(outputId = "target_variable_variogram"),
                              br(),
                            )
                          )
                 ),
                 tabPanel("Documentation",
                          h5("Click", a(href="https://github.com/thalisgold/spatial-modelling-tool", "here"), "to access the documentation!"),
                          ),
                 
)