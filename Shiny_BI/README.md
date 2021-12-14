# Shiny_bi
Shiny app for Business intelligence analysis.

This application is a consolidation of data generating, modeling, and post-model analysis process into a single workspace.

Running `app.R` will present an application with 5 tabs:
* **DATA**: Generate the dataset.  Pick and choose customized variables, individually defined with a `source.R` file in the variable library.  Save data in the dataset library as csv.
* **MODEL**: Use H2O to generate a model.  Pick and choose datasets and model parameters.  Score data.
* **ANALYSIS**: Analyze factors from the model.  Pick and choose cuts of data.  Make selects and save as a seperate model.
* **PIVOT**: Select fields to cut the data by.  Create calculated fields to aggregate stats.
* **SEGMENTATION**: Choose variables of the data by which to cut the data.  Retrieve segments containing those variables matching certain criteria.

# Screenshots
![Data tab](screenshots/data_tab_1.png)
![Model tab](screenshots/model_tab_1.png)
![Segmentation tab](screenshots/segmentation_tab_1.png)
![Docs tab](screenshots/docs_tab_1.png)
