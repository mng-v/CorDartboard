# CorDartboard
This project was created to determine and visualize the strength of the linear relationship between numerical variables. The strength of the relationship between a selected target variable and the other variables in a data set is determined through the linear correlation coefficient. 
Linear correlation refers to straight-line relationships between two variables.
This project explores correlation using a chart that resembles a dartboard with radius 1, which is divided into 5 concentric rings of equal width. The position of a point on this dartboard is defined by the correlation coefficient, and the closer to the center, the greater feature influence on the target variable. The correlation sign is shown using color: green for positive and red for negative correlation coefficients.

It works very simply. The main panel consists of four tabs Nav1: File, Nav2: Variables, Nav3: Correlation, Nav4: About project.

1. On the Nav1: File tab upload the data file from your computer. You can specify the columns separator and the encoding and mark header and stringAsFactor options. You can also get a descriptive overview of the dataset here by clicking on the subtab Summary.

2. On the Nav2: Variables tab you see the list of all dataset variables. Remove unnecessary variables before calculating correlations by unchecking their check boxes. You can also see here an overview of types of variables included in the dataset.

3. On the Nav3: Correlation tab visualization of the strength of linear dependence is provided using only numerical data features. You should select your target variable from the list. In addition to visualization, the total numbers of variables with very weak, weak, medium, strong, very strong correlation with target variable are calculated.
Since in the future medium, strong, very strong relationships are more interesting, the names of these features are displayed below the chart.

----------------------------------------------------
This is an interactive Shiny/R web application. To use this application, you need R and RStudio installed on your computer (and to know how to use them). Place app.R file with the application code and www folder with supplementary materials into an application directory in your working directory. You will need the packages commented in the app.R file. You can run the application in RStudio by giving the name of its directory to the function runApp().
