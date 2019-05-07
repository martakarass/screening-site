
The app code is designed in a way that it generates the UI dynamically (including filtering variables and their values) based on a simple schema of an input CSV file. 

## Input CSV file schema 

CSV input file should consist on 2 groups of columns. 

#### Group 1

Group 1 are study meta information columns: 

* study ID/name (`"Study"` column name), 
* study URL link (`"Url"` column name), 
* other meta information variables (currently hard-coded: `"Title"`, `"Info"`, `"Summary"`).

Note: 

* All variables in this grup should be `character` type. 
* All but Study ID/name (`"Study"` column name) can be left blank for one/many/all studies. 
* All but Study ID/name (`"Study"` column name) and URL link (`"Url"` column name) can be modified easily (in app .R file top part). 

#### Group 2

Group 2 are study variable and its value columns. They are build in the following way: 

* `"STUDY_VARIABLE=STUDY_VARIABLE_VALUE"`.

Note: 

* All variables in this grup should be `numeric`/`integer` type and only take value `0` or `1`. 
* The `STUDY_VARIABLE_VALUE` part of column name can contain spaces and special characters (i.e., `+`), but cannot contain `=` sign. 

#### Example 

Example of input CSV file schema is located at  [data/data-foo.csv](https://raw.githubusercontent.com/martakarass/screening-site/master/data/data-foo.csv). It was simulated with the use of R script [R/simulate-foo-data.R](https://github.com/martakarass/screening-site/blob/master/R/simulate-foo-data.R). 


## Filtering logic 

The filtering logic works as follows: for a study to pass the filter, the selected variable value in filter must  
have value `1` in CSV input file for corresponding `"STUDY_VARIABLE=STUDY_VARIABLE_VALUE"` variable, and this agreement must be kept for all variables the filter defines (the CSV file defines). 


## App screenshot 

The properly launched app should look like this: 

![foo](https://i.imgur.com/vCmjBnV.png)

 



