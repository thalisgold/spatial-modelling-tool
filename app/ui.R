ui <- navbarPage(title = "Remote Sensing Modeling Tool", theme = shinytheme("flatly"), 
                 tabPanel("App",
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              # It is possible to plant a seed in order to always achieve the same results
                              # and thus comparability.
                              checkboxInput(
                                inputId = "set_seed",
                                label = "Set following seed to make your results reproducible:",
                                value = FALSE
                              ),
                              
                              conditionalPanel(condition = "input.set_seed",
                                               numericInput(
                                                 inputId = "seed",
                                                 label = "",
                                                 value = 1,
                                                 min = 1,
                                                 max = 10000,
                                                 step = 1,
                                                 width = "30%"
                                               )
                              ),
                              br(),
                              h4("Step 1: Generation of the predictors"),
                              
                              # Choose multiple NLMs to generate predictors.
                              selectInput(
                                inputId = "nlm",
                                label = "Choose some NLMs as predictors:",
                                choices = c("Distance gradient" = "distance_gradient",
                                            "Edge gradient" = "edge_gradient",
                                            "Fractional brownian motion" = "fractional_brownian_motion",
                                            "Gaussian random field" = "gaussian_random_field",
                                            "Planar gradient" = "planar_gradient",
                                            "Polygonal landscapes" = "polygonal_landscapes",
                                            "Random" = "random",
                                            # "Random cluster" = "random_cluster",
                                            "Random neighbourhood" = "random_neighbourhood",
                                            "Random rectangular cluster" = "random_rectangular_cluster"),
                                multiple = TRUE,
                                selected = c("distance_gradient", "edge_gradient", "fractional_brownian_motion")
                              ),
                              
                              conditionalPanel(condition = "input.nlm.length >= 1",
                                               actionButton(
                                                 inputId = "generate_predictors",
                                                 label = "Generate selected predictors"
                                               )
                              ),
                              br(),
                              br(),
                              h4("Step 2: Simulation of the outcome"),
                              selectInput(
                                inputId = "nlms_for_outcome",
                                label = "Simulate the outcome from following NLMs:",
                                choices = c("distance_gradient",
                                            "edge_gradient",
                                            "fractional_brownian_motion",
                                            "gaussian_random_field",
                                            "planar_gradient",
                                            "polygonal_landscapes",
                                            "random",
                                            "random_neighbourhood",
                                            "random_rectangular_cluster"),
                                multiple = TRUE, 
                                selected = c("distance_gradient", "edge_gradient")),
                              textAreaInput(inputId = "expression", label = "Enter an expression that describes how the outcome is to be calculated (optional):", placeholder = "nlms$distance_gradient^2 - nlms$edge_gradient"),
                              checkboxInput(inputId = "r_noise", label = "Add random noise", value = FALSE),
                              checkboxInput(inputId = "s_noise", label = "Add spatially correlated noise", value = FALSE),
                              
                              # If more than 2 were chosen, it is possible to simulate the outcome.
                              conditionalPanel(condition = "input.nlms_for_outcome.length >= 1",
                                               actionButton(
                                                 inputId = "sim_outcome",
                                                 label = "Simulate outcome"
                                               )
                              ),
                              
                              br(),
                              br(),
                              # Select the number and distribution of the sampling points.
                              h4("Step 3: Selection of parameters for the sampling points"),
                              selectInput(
                                inputId = "dist_sampling_points",
                                label = "Distribution of sampling points:",
                                choices = c("Clustered" = "clustered",
                                            "Non-uniform" = "nonunif",
                                            "Random" = "random",
                                            "Regular" = "regular"),
                                selected = "random"
                              ),
                              
                              conditionalPanel(condition = "!output.clustered",
                                               numericInput(
                                                 inputId = "n_sampling_points",
                                                 label = "Number of sampling points:",
                                                 value = 50,
                                                 min = 50,
                                                 max = 250,
                                                 step = 50,
                                                 width = "60%"
                                               )
                              ),
                              
                              # If "clustered" is selected as the distribution, three sliders open to
                              # determine further parameters.
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
                                               sliderInput(inputId = "radius",
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
                              
                              # When the result has been calculated, it is possible to make a prediction.
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
                                                 label = "Difference",
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
                                                 value = FALSE
                                               ),
                                               checkboxInput(
                                                 inputId = "show_di",
                                                 label = "DI",
                                                 value = FALSE
                                               ),
                                               ),
                              ),
                            
                            mainPanel(
                              width = 9,
                              fluidRow(
                                column(6, conditionalPanel(condition = "input.generate_predictors",
                                                           wellPanel(
                                                             h4("Predictors"),
                                                             plotOutput(outputId = "predictors")
                                                           )
                                )
                                ),
                                column(6, conditionalPanel(condition = "input.sim_outcome",
                                                           wellPanel(
                                                             h4("Sampling points"),
                                                             plotOutput(outputId = "sampling_points")
                                                           )
                                )
                                ),
                              ),
                              conditionalPanel(condition = "input.sim_outcome",
                                               fluidRow(
                                                 column(4, 
                                                        wellPanel(
                                                          h4("Simulated outcome"),
                                                          plotOutput(outputId = "outcome")
                                                        ),
                                                 ),
                                                 column(4, conditionalPanel(condition = "output.finished_prediction && !input.show_prediction",
                                                                            wellPanel(
                                                                              h4("Prediction"),
                                                                              plotOutput(outputId = "prediction_of_first_model")
                                                                            )
                                                 )
                                                 ),
                                                 column(4, conditionalPanel(condition = "output.finished_prediction && !input.show_prediction",
                                                                            wellPanel(
                                                                              h4("Difference"),
                                                                              plotOutput(outputId = "dif_of_first_model")
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
                                                                           h5("CV error:"),
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
                                                                           h5("Difference:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_aoa)",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('random_10_fold_cv') && input.show_di)",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "random_10_fold_cv_di"),
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
                                                                           h5("CV error:"),
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
                                                                           h5("Difference:"),
                                                                           plotOutput(outputId = "loo_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "loo_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_aoa)",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "loo_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('loo_cv') && input.show_di)",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "loo_cv_di"),
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
                                                                           h5("CV error:"),
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
                                                                           h5("Difference:"),
                                                                           plotOutput(outputId = "sb_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "sb_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_aoa)",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "sb_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('sb_cv') && input.show_di)",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "sb_cv_di"),
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
                                                                           h5("CV error:"),
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
                                                                           h5("Difference:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_difference"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_selected_predictors)",
                                                                         wellPanel(
                                                                           h5("Selected predictors and their importance:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_varImp"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_aoa)",
                                                                         wellPanel(
                                                                           h5("AOA:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_aoa"),
                                                                         )
                                                        ),
                                                        conditionalPanel(condition = "(output.cv_methods.includes('nndm_loo_cv') && input.show_di)",
                                                                         wellPanel(
                                                                           h5("DI:"),
                                                                           plotOutput(outputId = "nndm_loo_cv_di"),
                                                                         )
                                                        ),
                                                 ),
                                               ),
                              ),
                              plotOutput(outputId = "test1"),
                              plotOutput(outputId = "test2"),
                              br(),
                            )
                          )
                 ),
                 tabPanel("Documentation"),
                 tabPanel("Demo"),
)