import os
import requests

# Get the Google Sheet ID from env var
sheet_id = os.getenv('SHEET_ID')

# URL to download Google Sheet as XLSX
url = f'https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=xlsx'

# Path where the file will be saved in the repo
output_path = 'home/dashboard_cargas/data/bienestar_jugador_primer_equipo_respuestas.xlsx'

# Download the file
response = requests.get(url)
response.raise_for_status()

with open(output_path, 'wb') as f:
    f.write(response.content)

print(f'Downloaded Google Sheet as {output_path}')
