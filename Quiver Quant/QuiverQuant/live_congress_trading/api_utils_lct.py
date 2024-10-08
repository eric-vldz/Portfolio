import requests
import pandas as pd
import json
from typing import Dict, Any

import os
import json

def load_api_key() -> str:
    # Construct the path to auth.json
    current_dir = os.path.dirname(__file__)
    auth_file_path = os.path.join(current_dir, 'auth.json')
    
    with open(auth_file_path, 'r') as file:
        return json.load(file)['Authorization'] 

# Function to fetch data from API
def live_congress_trading() -> pd.DataFrame:
    # Load the API key
    api_key = load_api_key()
    
    # API endpoint
    url = "https://api.quiverquant.com/beta/live/congresstrading"
    
    headers = {
        'Accept': 'application/json',
        'Authorization': f'Bearer {api_key}'
    }

    try:
        # Make the API request
        response = requests.get(url, headers=headers)
        response.raise_for_status()  # Raises an exception for bad status codes

        # Convert the response to a DataFrame
        data = response.json()
        
        # Assuming the API response is directly convertible to a DataFrame
        df = pd.DataFrame(data)
        
    except requests.exceptions.RequestException as e:
        print(f"An error occurred while fetching data: {e}")
        raise

    return df