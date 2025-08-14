import os
import requests

# Get the Google Sheet ID from environment variable
sheet_id = os.getenv('SHEET_ID')

if not sheet_id:
    raise ValueError("SHEET_ID environment variable not set")

# URL to download Google Sheet as XLSX
url = f'https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=xlsx'

# Path where the file will be saved in the repo (relative to repo root)
output_dir = 'data'
output_path = os.path.join(output_dir, 'bienestar_jugador_primer_equipo_respuestas.xlsx')

# Ensure the 'data' folder exists
os.makedirs(output_dir, exist_ok=True)

# Download the file
response = requests.get(url)
response.raise_for_status()

with open(output_path, 'wb') as f:
    f.write(response.content)

print(f'Downloaded Google Sheet as {output_path}')
