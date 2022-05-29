# Spatial-modelling-tool

## Current state of research
In many areas of environmental science, it is required to have area-wide information on certain variables in order to identify spatial patterns and changes and to make important decisions based on these. For instance, air pollution maps ([Beelen et al., 2013](https://www.sciencedirect.com/science/article/pii/S0140673613621583?dgcid=api_sd_search-api-endpoint)) can help identify links between pollution and people's health, which could lead to the introduction of new limit values. Further examples are soil maps which may be used to estimate organic carbon and model agricultural productivity or continuous maps of meteorological variables such as air temperature and precipitation ([Fick and Hijmans, 2017](https://rmets.onlinelibrary.wiley.com/doi/10.1002/joc.5086)) to model species distribution and biodiversity.

What all these examples have in common is that the target variables are typically collected only at specific points. Air temperature, precipitation, as well as pollutants such as nitrogen oxides and carbon monoxides are usually measured at fixed climate stations, and information about the soil is commonly obtained from samples collected through fieldwork. The goal of predictive modelling tasks (as described in e.g., [Van den Hoogen et al. (2021)](https://www.biorxiv.org/content/10.1101/2021.07.07.451145v1)) is to derive spatially continuous datasets from this kind of limited field data. To achieve this, field data in combination with predictors (variables that are believed to explain the target variable in some way) are used to train machine learning algorithms.

The performances of this models are typically communicated via cross-validation estimates. Cross-validation is a resampling method that uses different parts of the data to test and train a model in different iterations ([James et al., 2021](https://link.springer.com/book/10.1007/978-1-4614-7138-7)). There is a variety of cross-validation methods that mainly differ in how they divide the data into training and test data. In the current literature, however, there are different views on the suitability of these methods for estimating the quality of models. 

Whenever a model is validated, it is important to ensure that the validation is carried out with independent data. This means that data used for model training should not be used for validation. Otherwise, the model error would indicate how well the model can reproduce the data, not how well it can predict on new data. 

[Wadoux et al. (2021)](https://www.sciencedirect.com/science/article/abs/pii/S0304380021002489) assume that a model-free, design-neutral and valid assessment of map accuracy is achieved with probability sampling and design-based inference. However, when these approaches are not feasible as additional samples for validation purposes cannot be collected or since the sampling locations are often determined from the very beginning (e.g., fixed monitoring stations, samples already available), they propose to apply cross-validation methods in which the data is randomly split to estimate the performance of a model (random cross-validation).

In many recent studies ([Meyer et al. (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0304380019303230?via%3Dihub); [Ploton et al., 2020](https://www.nature.com/articles/s41467-020-18321-y); [Pohjankukka et al., 2017](https://www.tandfonline.com/doi/abs/10.1080/13658816.2017.1346255?journalCode=tgis20); [Rocha et al., 2018](https://www.mdpi.com/2072-4292/10/8/1263))., a different opinion has been expressed. The researchers of these studies claim that spatial data always correlate with each other to some degree (see Tobler’s first law of geography), which is why they cannot be independent and basic statistical procedures do not hold. This is especially the case when the data available is clustered. To ensure that the quality of the model can still be reliably estimated, they propose different cross-validation methods that attempt to warrant independence between test and validation data by dividing the data spatially rather than randomly (spatial cross-validation). In their opinion, ignoring spatial autocorrelation leads to overoptimistic models that do not perform nearly as well in practice and can lead to erroneous maps and interpretations.

## Aim of the tool
Following these discussions, there seems to be uncertainty and a lack of understanding of which cross-validation methods to use to approximate the model error as precisely as possible. Within the framework of my bachelor thesis, I therefore developed an interactive tool to quickly and easily test and compare different scenarios for spatial prediction models. It is thus intended to contribute to a better understanding of the challenges of spatial prediction models and their quality assessment. The tool works with simulated data, giving the advantage that the actual error can be calculated and compared with the error estimates of up to four different cross-validation methods, one random (Random 10-fold CV), a special form of k-fold CV (Leave-One-Out-CV), one spatial (Spatial Block CV) and a new developed CV method ([Nearest Neighbour Distance Matching Leave-One-Out CV](https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13851)).  The user has the possibility to choose from different predictors and to model the target variable as desired. The number and distribution of sample points can also be chosen at will, allowing the investigation of a variety of scenarios.

## Variable selection
To prevent the overfitting of a model, [Meyer et al. (2019)](https://www.sciencedirect.com/science/article/abs/pii/S0304380019303230?via%3Dihub) assume that not only spatial validation strategies, but also spatial variable selection is essential for reliable predictions. To be able to make statements about the effects of variable selection and to show what impact it has on the quality of spatial predictions, two different methods were implemented in the simulation tool, the first being the forward feature selection method (FFS) developed by [Meyer et al. in 2018](https://rdrr.io/github/HannaMeyer/CAST/man/ffs.html) and secondly the default recursive feature elimination method (RFE).

## AOA
In 2021, Meyer and Pebesma even went a step further and published a paper in which they argue that validation of models by cross-validation alone is not sufficient. They believe that models can only be applied to new areas if they are similar to the training data. As spatial mapping requires predictions for a new geographic space, which in many cases are accompanied by new predictive properties, a method is needed to estimate the area to which a predictive model can be reliably applied. To this end, they propose their newly developed method for calculating the "area of applicability" (AOA), defined as the area for which the cross-validation estimates retain their validity. Since the AOA is considered as a relevant addition for spatial model assessment, it is also included in this tool. For more informations on the AOA click [here](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13650)!

## How does the software work?
The tool was developed with the R programming language (R Core Team, 2022) and uses the Rshiny package in particular in order to enable the user to explore the data interactively.

## How to use the tool?
- User guided step by step


### Step 1: Simulation of the predictors
- Mindestens zwei Prediktoren

### Step 2: Simulation of the target variable

### Step 3: Simulation of the sample points

### Step 4: Model training and prediction


