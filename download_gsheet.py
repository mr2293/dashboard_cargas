import os
import requests

def must_get(name: str) -> str:
    val = os.getenv(name)
    if not val:
        raise ValueError(f"Missing environment variable: {name}")
    return val

def download_sheet(sheet_id: str, out_path: str, gid: str | None = None) -> None:
    base = f"https://docs.google.com/spreadsheets/d/{sheet_id}/export?format=xlsx"
    url = f"{base}&gid={gid}" if gid else base
    os.makedirs(os.path.dirname(out_path), exist_ok=True)
    r = requests.get(url, timeout=120)
    r.raise_for_status()
    with open(out_path, "wb") as f:
        f.write(r.content)
    print(f"Downloaded: {out_path}")
    
def get_any(*names: str) -> str:
    for n in names:
        v = os.getenv(n)
        if v:
            return v
    raise ValueError(f"Missing environment variable (tried): {', '.join(names)}")

    # ---- Sheet 2 (RPE) ----
    RPE_ID  = get_any("RPE_SHEET_ID", "SHEET_ID_RPE")
    RPE_GID = os.getenv("RPE_SHEET_GID", os.getenv("SHEET_GID_RPE", "1991973790"))
    OUT2    = os.getenv("OUTPUT_NAME_RPE", "rpe_primera.xlsx")

    download_sheet(
      sheet_id=RPE_ID,
      gid=RPE_GID,
      out_path=os.path.join(output_dir, OUT2),
    )

def main():
    # Where to save files (default: data/)
    output_dir = os.getenv("OUTPUT_DIR", "data")

    # ---- Sheet 1 (Cuestionario Bienestar) ----
    SHEET_ID = must_get("SHEET_ID")                # workbook id
    SHEET_GID = os.getenv("SHEET_GID", "87000818")             # optional tab gid
    OUT_NAME_1 = os.getenv(                       # customize filename if you like
        "OUTPUT_NAME_SHEET",
        "bienestar_jugador_primer_equipo_respuestas.xlsx",
    )

    download_sheet(
        sheet_id=SHEET_ID,
        gid=SHEET_GID,
        out_path=os.path.join(output_dir, OUT_NAME_1),
    )

    # ---- Sheet 2 (RPE) ----
    RPE_SHEET_ID = must_get("RPE_SHEET_ID")        # workbook id
    RPE_SHEET_GID = os.getenv("RPE_SHEET_GID", "1991973790")  # tab gid (from your URL)
    OUT_NAME_2 = os.getenv("OUTPUT_NAME_RPE", "rpe_primera.xlsx")

    download_sheet(
        sheet_id=RPE_SHEET_ID,
        gid=RPE_SHEET_GID,
        out_path=os.path.join(output_dir, OUT_NAME_2),
    )

if __name__ == "__main__":
    main()
