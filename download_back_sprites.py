#!/usr/bin/env python3
"""
Script to download back sprites from FireRed/LeafGreen for all Pokemon
defined in game-engine/src/Game/Pokemon.hs
"""

import re
import time
import os
from pathlib import Path
from urllib.request import Request, urlopen
from urllib.error import URLError, HTTPError

# Base URL for FireRed/LeafGreen back sprites
SPRITE_URL_TEMPLATE = (
    "https://img.pokemondb.net/sprites/firered-leafgreen/back-normal/{}.png"
)

# Output directory
OUTPUT_DIR = Path("game-client/assets/pokemon")


def extract_pokemon_from_hs_file(filepath):
    """Extract Pokemon names and IDs from the Haskell file"""
    with open(filepath, "r") as f:
        content = f.read()

    # Pattern to match Pokemon definitions with pId and pName
    pattern = r'Pokemon\s*\{[^}]*pId\s*=\s*(\d+)[^}]*pName\s*=\s*"([^"]+)"'
    matches = re.findall(pattern, content)

    pokemon_list = []
    for pid, pname in matches:
        # Convert Pokemon name to lowercase for URL
        # Handle special cases like "MR. MIME" -> "mr-mime", "NIDORAN F" -> "nidoran-f"
        url_name = pname.lower()
        url_name = url_name.replace(" ", "-")
        url_name = url_name.replace(".", "")
        url_name = url_name.replace("'", "")

        pokemon_list.append(
            {
                "id": int(pid),
                "name": pname,
                "url_name": url_name,
                "filename": f"{int(pid):04d}_back.png",
            }
        )

    return pokemon_list


def download_sprite(pokemon, output_dir):
    """Download a single sprite"""
    url = SPRITE_URL_TEMPLATE.format(pokemon["url_name"])
    output_path = output_dir / pokemon["filename"]

    # Skip if already exists
    if output_path.exists():
        print(f"✓ Skipping {pokemon['name']} (#{pokemon['id']}) - already exists")
        return True

    try:
        # Create request with User-Agent header to avoid 403 errors
        headers = {
            "User-Agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
        }
        req = Request(url, headers=headers)

        with urlopen(req, timeout=10) as response:
            data = response.read()

        # Save the image
        with open(output_path, "wb") as f:
            f.write(data)

        print(
            f"✓ Downloaded {pokemon['name']} (#{pokemon['id']}) -> {pokemon['filename']}"
        )
        return True

    except (URLError, HTTPError) as e:
        print(f"✗ Failed to download {pokemon['name']} (#{pokemon['id']}): {e}")
        return False


def main():
    # Ensure output directory exists
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    # Extract Pokemon from Haskell file
    print("Extracting Pokemon list from Pokemon.hs...")
    pokemon_list = extract_pokemon_from_hs_file("game-engine/src/Game/Pokemon.hs")
    print(f"Found {len(pokemon_list)} Pokemon\n")

    # Download sprites
    successful = 0
    failed = 0

    for pokemon in pokemon_list:
        if download_sprite(pokemon, OUTPUT_DIR):
            successful += 1
        else:
            failed += 1

        # Be nice to the server
        time.sleep(0.5)

    print(f"\n{'=' * 60}")
    print(f"Download complete!")
    print(f"Successful: {successful}")
    print(f"Failed: {failed}")
    print(f"{'=' * 60}")


if __name__ == "__main__":
    main()
