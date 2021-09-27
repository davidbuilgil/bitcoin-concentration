# bitcoin-concentration
Data and codes used in the paper "Offending concentration on the internet: An exploratory analysis of Bitcoin-related cybercrime", by David Buil-Gil and Patricia Saldaña-Taboada

The script with all codes ('script.R') should be ran from within the 'bitcoin-project.Rproj' R project file.

All data used in this project is available open access. The following steps provide guidance on how to accesss and download the BitcoinAbuse data used in the scripts.

1. Access the BitcoinAbuse website: https://www.bitcoinabuse.com/
2. Click on *Register* and create a new profile. Log in.
3. Go to https://www.bitcoinabuse.com/settings#/api and create a new API token (more information here: https://www.bitcoinabuse.com/api-docs). Copy your API token to clipboard.
4. Replace *{API_TOKEN}* by your API token in this URL: https://www.bitcoinabuse.com/api/download/forever?api_token={API_TOKEN}. Load the URL and the full dataset will download.
5. Save the file in the *Data* folder.
6. Now you can run the script.
