This repository contains scripts that automatically retrieve information from the most recent sources for the Sustainable Wage Tool.

Sources:

Housing Cost API: https://www.huduser.gov/portal/dataset/fmr-api.html

Food Plan List: https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-monthly-reports

Self Sufficiency Standard: https://selfsufficiencystandard.org/Wisconsin/

Job Salary Data: https://download.bls.gov/pub/time.series/oe/

Job Educational Requirements: https://www.bls.gov/emp/tables/education-and-training-by-occupation.htm


The project expects a file named "credentials.R" in the directory "DataFiles/credentials.R" of the project. 

This file should have the environment variable API_KEY_HOUSING, which contains your Huduser API key. This is used to retrieve housing cost data. Refer to the [Housing Cost API](https://www.huduser.gov/portal/dataset/fmr-api.html) source mentioned above to learn how to create an API key.
