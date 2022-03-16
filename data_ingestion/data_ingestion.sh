# Note: This is a pseudo-code that does not really run on the server. 
# It should run on the server on a daily basis and sync the raw_data folder with the latest version of datasets everyday.
# We can use crontab to set up a daily cron job to run it.
# But since the requirement of project2 does not ask students to really ingest live data everyday, so this does not really run.

wget https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/775ca815-5028-4e9b-9dd4-6975ff1be021/download/vaccines_by_age.csv -o ../raw_data/vaccines_by_age.csv

wget https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/8a89caa9-511c-4568-af89-7f2174b4378c/download/vaccine_doses.csv -o ../raw_data/vaccine_doses.csv

wget https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/274b819c-5d69-4539-a4db-f2950794138c/download/vac_status_hosp_icu.csv -o ../raw_data/vac_status_hosp_icu.csv

wget https://data.ontario.ca/dataset/f4f86e54-872d-43f8-8a86-3892fd3cb5e6/resource/ed270bb8-340b-41f9-a7c6-e8ef587e6d11/download/covidtesting.csv -o covidtesting.csv

wget https://data.ontario.ca/dataset/8f3a449b-bde5-4631-ada6-8bd94dbc7d15/resource/e760480e-1f95-4634-a923-98161cfb02fa/download/region_hospital_icu_covid_data.csv -o region_hospital_icu_covid_data.csv
