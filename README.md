# Spatial-modelling-tool

## Current state of research
In many areas of environmental science, it is required to have area-wide information on certain variables in order to identify spatial patterns and changes and to make important decisions based on these. For instance, air pollution maps ([Beelen et al., 2013](https://www.sciencedirect.com/science/article/pii/S0140673613621583?dgcid=api_sd_search-api-endpoint)) can help identify links between pollution and people's health, which could lead to the introduction of new limit values. Further examples are soil maps which may be used to estimate organic carbon and model agricultural productivity or continuous maps of meteorological variables such as air temperature and precipitation ([Fick and Hijmans, 2017](https://rmets.onlinelibrary.wiley.com/doi/10.1002/joc.5086)) to model species distribution and biodiversity.

What all these examples have in common is that the target variables are typically collected only at specific points. Air temperature, precipitation, as well as pollutants such as nitrogen oxides and carbon monoxides are usually measured at fixed climate stations, and information about the soil is commonly obtained from samples collected through fieldwork. The goal of predictive modelling tasks (as described in e.g., [Van den Hoogen et al. (2021)](https://www.biorxiv.org/content/10.1101/2021.07.07.451145v1)) is to derive spatially continuous datasets from this kind of limited field data. To achieve this, field data in combination with predictors (variables that are believed to explain the target variable in some way) are used to train machine learning algorithms.

The performances of this models are typically communicated via cross-validation estimates. Cross-validation is a resampling method that uses different parts of the data to test and train a model in different iterations ([James et al., 2021](https://link.springer.com/book/10.1007/978-1-4614-7138-7)). There is a variety of cross-validation methods that mainly differ in how they divide the data into training and test data. In the current literature, however, there are different views on the suitability of these methods for estimating the quality of models. 

Whenever a model is validated, it is important to ensure that the validation is carried out with independent data. This means that data used for model training should not be used for validation. Otherwise, the model error would indicate how well the model can reproduce the data, not how well it can predict on new data. 

[Wadoux et al. (2021)](https://www.sciencedirect.com/science/article/abs/pii/S0304380021002489) assume that a model-free, design-neutral and valid assessment of map accuracy is achieved with probability sampling and design-based inference. However, when these approaches are not feasible as additional samples for validation purposes cannot be collected or since the sampling locations are often determined from the very beginning (e.g., fixed monitoring stations, samples already available), they propose to apply cross-validation methods in which the data is randomly split (random cross-validation).

In many recent studies ([Meyer et al. (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0304380019303230?via%3Dihub); [Ploton et al., 2020](https://www.nature.com/articles/s41467-020-18321-y); [Pohjankukka et al., 2017](https://www.tandfonline.com/doi/abs/10.1080/13658816.2017.1346255?journalCode=tgis20); [Rocha et al., 2018](https://www.mdpi.com/2072-4292/10/8/1263))., a different opinion has been expressed. The researchers of these studies claim that spatial data always correlate with each other to some degree (see Tobler’s first law of geography), which is why they cannot be independent and basic statistical procedures do not hold. This is especially the case when the data available is clustered. To ensure that the quality of the model can still be reliably estimated, they propose different cross-validation methods that attempt to warrant independence between test and validation data by dividing the data spatially rather than randomly (spatial cross-validation). In their opinion, ignoring spatial autocorrelation leads to overoptimistic models that do not perform nearly as well in practice and can lead to erroneous maps and interpretations.

## Aim of the tool
Following these discussions, there seems to be uncertainty and a lack of understanding of which cross-validation methods to use to approximate the model error as precisely as possible. Within the framework of my bachelor thesis, I therefore developed an interactive tool to quickly and easily test and compare different scenarios for spatial prediction models. It is thus intended to contribute to a better understanding of the challenges of spatial prediction models and their quality assessment. The tool works with simulated data, giving the advantage that the actual error can be calculated and compared with the error estimates of up to four different cross-validation methods, one random (Random 10-fold CV), a special form of k-fold CV (Leave-One-Out-CV), one spatial (Spatial Block CV) and a new developed CV method ([Nearest Neighbour Distance Matching Leave-One-Out CV](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13851)).  The user has the possibility to choose from different predictors and to model the target variable as desired. The number and distribution of sample points can also be chosen at will, allowing the investigation of a variety of scenarios.

## Variable selection
To prevent the overfitting of a model, [Meyer et al. (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0304380019303230?via%3Dihub) assume that not only spatial validation strategies, but also spatial variable selection is essential for reliable predictions. To be able to make statements about the effects of variable selection and to show what impact it has on the quality of spatial predictions, two different methods were implemented in the simulation tool, the first being the forward feature selection method (FFS) developed by [Meyer et al. in 2018](https://rdrr.io/github/HannaMeyer/CAST/man/ffs.html) and secondly the default recursive feature elimination method (RFE).

## Area of applicability (AOA)
In 2021, Meyer and Pebesma even went a step further and published a paper in which they argue that validation of models by cross-validation alone is not sufficient. They believe that models can only be applied to new areas if they are similar to the training data. As spatial mapping requires predictions for a new geographic space, which in many cases are accompanied by new predictive properties, a method is needed to estimate the area to which a predictive model can be reliably applied. To this end, they propose their newly developed method for calculating the "area of applicability" (AOA), defined as the area for which the cross-validation estimates retain their validity. Since the AOA is considered as a relevant addition for spatial model assessment, it is also included in this tool. For more informations on the AOA click [here](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13650)!

## How does the tool work?
The tool was developed with the [R](https://www.r-project.org/about.html) programming language  and uses the [Rshiny](https://shiny.rstudio.com/) package in particular in order to enable the user to explore the data interactively. The user is guided through the tool step by step.

### Step 1: Simulation of the predictors
The simulation of the predictors relies on the [NLMR](https://github.com/ropensci/NLMR) package developed by Sciani et al. (2018). The package is able to create neutral landscape models (NLMs) that simulate landscape patterns based on theoretical distributions. For our purpose, these can be considered as spatially continuous predictors. One can imagine an NLM as an image of a spectral band of a satellite sensor, for example. 

The study area in the tool is defined as a 100x100 pixel square grid (i.e. 10 000 grid cells). 17 different theoretical distributions are used to generate NLMs of this size that can be selected by the user as predictors. After the user has selected at least two of these, he can press the "simulate selected predictors" button.

For  fractional-brownian-motion and gaussian-random-field distributions the autocorrelation range could be specified. Five different ranges were implemented for each of these. The appended number corresponds to the range.

![Generated predictors](https://github.com/thalisgold/Spatial-modelling-tool/blob/main/images/Predictors.jpg)  
Figure 1: Six simulated predictors.

### Step 2: Simulation of the target variable
Since the tool does not work with real data, there is no variable that the model is supposed to predict later on. Hence, before any prediction can be made, it is necessary to simulate a target variable. Once again, the NLMs are utilised. The user must first select the NLMs on the basis of which the target variable is to be simulated. The same NLMs are available as for the selection of the predictors. 

The tool then offers two possibilities to generate the target variable from the chosen NLMs:
1. A random mathematical expression is formed that contains additive and multiplicative terms as well as non-linearities and in which each NLM occurs exactly once. With five NLMs (X1, ..., X5), the target variable Y could be modelled by the tool as follows:
  * Y = X1 * X2 + X3^2 – X4 + X5^3
2. The user can enter any mathematical expression in a text field. For example:
  * Y = ((X3 + X1)^3 / (X3^2 – X4) + X5^3) * X2 

To approximate real-world measured data and make the simulation more realistic, there is also the possibility to add random and/or spatially correlated noise. In a last step, the target variable is normalised to obtain a grid with values between zero and one.

![Simulated target variable](https://github.com/thalisgold/Spatial-modelling-tool/blob/main/images/Simulation%20of%20target%20variable.jpg)  
Figure 2: Simulated target variable generated with a random mathematical expression using fractional_motion_40 and gaussian_random_field_20.  Afterwards spatially correlated noise was added.

### Step 3: Simulation of the sample points
It is necessary to simulate the places where reference data are available or, in other words, where the target variable is known. For the simulation, the tool allows the selection of two parameters: the number of sample points and their spatial distribution. It is possible to choose from four spatial distributions. For the first three, only a fixed number of sample points (50, 100, 150, 200, 250) can be selected.
1. Random: The sample points are randomly distributed in the study area. 
2. Regular: Based  on  the  number  of  sample points a regular  grid  is  formed.
3. Non-uniform: The study area is divided into 20 equal-sized blocks (20x20 pixel), five of which are randomly selected. Then the sample points are randomly distributed within the selected blocks.
4.	Clustered: So called parent points (1-20) are randomly distributed in the study area. Then offsprings (10-250) are divided equally among the parent points and then randonmly spread within a radius (1-8) around the parent.

![Distributions](https://github.com/thalisgold/Spatial-modelling-tool/blob/main/images/distributions.jpg)  
Figure 3: Possible distributions of the sample points.

### Step 4: Model training and prediction
For model training and prediction, the functions of the [caret package](https://topepo.github.io/caret/) (Kuhn et al., 2022) are used. Before a model can be trained, the training data must be prepared. For this purpose, information on both the predictors and the target variable is extracted at the previously defined sample points and subsequently merged. 

After the training data has been prepared, the user has to choose which machine learning algorithm to use for the model training. He can choose between a random forest algorithm and an SVM. Since it is not the aim of the tool to deal with the effects of hyperparameter tuning, it will not be carried out. This reduces the computational effort and decreases the complexity of the tool. For the random forest algorithm the number of random features considered for partitioning at each node (mtry) was set to 2 and the number of trees to grow (ntree) was set to 100. Analogously, for the SVM, the penalty for each misclassified point (C) has been set to 1 and the parameter controlling the distance of the influence of a single point (gamma) was set to 0.5.

If a random forest algorithm is selected, there is also the possibility to perform a variable selection. On the one hand, a FFS can be performed as implemented in the [CAST](https://github.com/HannaMeyer/CAST) package (Meyer, 2018) and on the other hand, a function of the caret package can be used to execute a RFE.
Once a model has been trained, it is used to predict on the entire study area.

### Results


