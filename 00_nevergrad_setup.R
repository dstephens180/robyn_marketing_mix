# BUSINESS SCIENCE LEARNING LABS ----
# LAB 61: MARKETING MIX MODELING ----
# ROBYN SETUP ----
# **** ----

# ROBYN SETUP SCRIPTS ----

# Run these scripts to:
#  - Set up a Conda Environment 'lab_61_robyn'
#  - Install the package 'nevergrad' and it's dependencies

library(reticulate)

conda_create('robyn_marketing_mix')
conda_install(envname = 'robyn_marketing_mix', packages = 'nevergrad', pip = TRUE)



# C:\\Users\\DavidStephens\\anaconda3\\envs\\lab_61_robyn/python.exe
