## Parental loss: a global perspective

Scripts for the data preparation can't be run because big data sets (hence, not uploadable on Github) are involved.
You should be able to get the main outputs on a sample of six countries by starting with the script "kin_dynamics.R". Data needed for that script are in the "data" folder. This script will create the object "d_x_t.rda".

You can then run the script "kin_outputs.R" to generate the object "df_results.rda" which contains the outputs of the model for the six selected countries.

Once these two scripts have been successfully run, you should have the required objects to be able to generate the Quarto report and generate figures of interest to you.


