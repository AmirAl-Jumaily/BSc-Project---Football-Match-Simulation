import json, os, sys

import requests

import api_logger as api_log
import utils as u

cfg = u.get_config()
data_cfg = u.get_config("data")
api_cfg = u.get_config("api")
URL_HEADERS = api_cfg["HEADERS"]
API_DATA_DIR = cfg['DIRS']['API_DATA']

def make_api_request(url, headers, action, request_id, ignore_logs=False):
    """
    Function with padding around it to make an api request under certain conditions.
    Whenever a call is made, everything is automatically logged.
    """
    action = action.lower()

    call_made_previously = False if ignore_logs else api_log.check_if_call_made_previously(action, request_id)
    requests_remaining = api_log.check_if_api_calls_remaining()

    if not requests_remaining:
        print(f"Daily API request limit reached. Action: {action}, RequestID: {request_id}")
        return False
    elif call_made_previously:
        print(f"Call already made previously according to logs. Action: {action}, RequestID: {request_id}.")
        return True

    elif (not call_made_previously) and requests_remaining:
        response = requests.get(url, headers=headers)
        api_log.update_logs_after_request(response, action, request_id)
        return response


def get_general_api_data(action, ignore_logs=False):
    # 0) Define required variables
    action = action.upper()
    filepath = os.path.join(API_DATA_DIR, f"general/{action.lower()}.json")

    # 1) Perform checks
    #   1.1) Valid API action passed.
    if cfg['API_ACTIONS'][action]['group'] != "general":
        print(f"API action '{action}' is not a 'general' call. Check config.")
        return
    #   1.2) Check if file, and therefore data, already exists.
    if os.path.isfile(filepath):
        print(f"Data for '{action.lower()}' may already exist at '{filepath}'.")
        return
    
    # 2) Make API call and extract data to 'response_data'
    url = f"http://v2.api-football.com/{cfg['API_ACTIONS'][action]['name']}"
    response = make_api_request(url, URL_HEADERS, action, "Only one request", ignore_logs=ignore_logs)

    if isinstance(response, bool):
        sys.exit(0)
    else:
        response_data = response.json()['api'][cfg['API_ACTIONS'][action]['response']]

    # 3) Save the data and print a success message.
    with open(filepath, "w") as w:
        json.dump(response_data, w)
    
    print(f"COMPLETE! Data saved at '{filepath}'.")


def get_league_api_data(action, only_leagues_with_match_stats=True, filtered_leagues=True, ignore_logs=False, overwrite=False):
    # 0) Define required variables
    action = action.upper()
    action_name = cfg['API_ACTIONS'][action]['name']
    action_coverage_name = cfg['API_ACTIONS'][action]['coverage_name']
    response_label = cfg['API_ACTIONS'][action]['response']

    # 1) Perform checks
    #   1.1) Valid API action passed.
    if cfg['API_ACTIONS'][action]['group'] != "league":
        print(f"API action '{action}' is not a 'league' call. Check config.")
        return

    # 2) Get all leagueIds and loop through them
    leagueIds = u.get_league_data(filtered=filtered_leagues, ids_only=True, has_match_stats_data=only_leagues_with_match_stats)
    for league in leagueIds:
        # 2.1) Get the country and competition names of this league id to construct the filepath.
        country, league_name = u.get_country_and_league_name_from_id(league)
        league_directory = os.path.join(API_DATA_DIR, f"league/{country}/{league_name}")
        filepath = f"{league_directory}/{action.lower()}.json"

        # 1.2) Check if the league has the right coverage:
        if isinstance(action_coverage_name, str):
            if not u.check_if_league_has_coverage(league, action_coverage_name):
                print(f"League '{league}' does not have coverage for action '{action}'.")
                continue

        # 1.3) Check if data already exists within the file if the file exists
        if (os.path.isfile(filepath) & (not overwrite)):
            with open(filepath, "r") as r:
                data = json.load(r)
            if league in data.keys():
                print(f"'{action}' data for league '{league}' already exists in '{filepath}'.")
                continue
        
        # 3) Make API call and extract data to 'response_data'
        url = f"http://v2.api-football.com/{action_name}/{league}"
        response = make_api_request(url, URL_HEADERS, action, league, ignore_logs=ignore_logs)

        if isinstance(response, bool):
            if response:
                continue
            elif not response:
                sys.exit(0)
        else:
            response_data = response.json()['api'][response_label]

        # 4) Save the data and print a success message.
        try:
            with open(filepath, "r") as r:
                data = json.load(r)
        except FileNotFoundError:
            data = {}
        
        data[league] = response_data

        os.makedirs(league_directory, exist_ok=True)
        with open(filepath, "w") as w:
            json.dump(data, w)
        
        print(f"Data for league '{league}' saved at '{filepath}'.")
    print(f"COMPLETE! All data for '{action.lower()}' saved.")


def get_fixture_api_data(
    action, main_leagues_only=True, only_leagues_with_match_stats=True, filtered_leagues=True, only_selected_fixture_ids=True,
    start_at_league=0, ignore_logs=False, overwrite_league=False, overwrite_fixture=False):
    # 0) Define required variables
    action = action.upper()
    action_name = cfg['API_ACTIONS'][action]['name']
    response_label = cfg['API_ACTIONS'][action]['response']

    # 1) Perform checks
    #   1.1) Valid API action passed.
    if cfg['API_ACTIONS'][action]['group'] != "fixture":
        print(f"API action '{action}' is not a 'fixture' call. Check config.")
        return
    
    selected_fixture_ids = data_cfg["SELECTED_IDS"] if ("SELECTED_IDS" in data_cfg.keys()) else None
    
    # 2) Get all leagueIds and loop through them
    leagueIds = u.get_league_data(filtered=filtered_leagues, ids_only=True, has_match_stats_data=only_leagues_with_match_stats)
    count=0
    for league in leagueIds:

        # To speed things up:
        if int(league) < start_at_league:
            print(f"TEMPORARY: Skipping {league}\r", end="")
            continue

        # 2.1) Get the country and competition names of this league id to construct the filepath.
        country, league_name = u.get_country_and_league_name_from_id(league)
        season = u.get_league_info_from_league_id(leagueId=league, key_name="season")
        filepath = os.path.join(API_DATA_DIR, f"league/{country}/{league_name}/{action.lower()}.json")

        # 1.2) Filter countries and leagues for only those we may want
        if main_leagues_only:
            condition = not ((country in cfg['KEY_COUNTRIES']) and (league_name in cfg['KEY_COMPETITIONS']))
        else:
            condition = (country not in cfg['2ND_KEY_COUNTRIES'] or any(substr in league_name for substr in cfg['AVOID_LEAGUES_THAT_CONTAIN']))
        
        if condition:
            print(f"Skipping {country}, {league_name}, {season}: {league}. League and/or country not important for now.")
            continue

        # 1.3) Check if the league has the right coverage:
        if action == "FIXTURES_ALL":
            if not u.check_if_league_has_coverage(league, "events", fixtures_coverage=True):
                print(f"League '{country}, {league_name}, {season}: {league}' does not have coverage for action '{action}'.")
                continue
        
        # 2.2) Get all fixtureIds to loop through
        allFixtureIds = u.get_fixture_ids_for_league(league)
        
        if isinstance(selected_fixture_ids, list) & (len(selected_fixture_ids) > 0):
            fixtureIds = [FID for FID in allFixtureIds if FID in selected_fixture_ids]
        elif isinstance(selected_fixture_ids, list) & (len(selected_fixture_ids) == 0) & only_selected_fixture_ids:
            print("No selected ID's remaining. Continuing.")
            continue
        else:
            fixtureIds = allFixtureIds

        # 1.4) Check if we already have fixture ids for that league
        if os.path.isfile(filepath) & (not overwrite_league):
            with open(filepath, "r") as r:
                data = json.load(r)
            if league in data.keys():
                try:
                    if len(data[league]['fixtures']) == len(fixtureIds):
                        print(f"Data for league '{league}' already saved at '{filepath}'. Continuing to next league.")
                        continue
                except KeyError:
                    if len(data[league].keys()) == len(fixtureIds):
                        print(f"Data for league '{league}' already saved at '{filepath}'. Continuing to next league.")
                        continue

        # 2.3) Open the file here for this league so we're not opening and closing the file for every request.
        try:
            with open(filepath, "r") as r:
                data = json.load(r)
        except FileNotFoundError:
            data = {}
        
        # 2.4) Ensure there is a key for this league
        if league not in data.keys():
            data[league] = {}
            data[league]['league_id'] = league
            data[league]['country'] = country
            data[league]['type'] = u.get_league_info_from_league_id(leagueId=league, key_name="type")
            data[league]["name"] = league_name
            data[league]['season'] = season
            data[league]["fixtures"] = []

        
        # 3) Loop through the fixture IDs
        for fixture in fixtureIds:
            # 3.1) Make API call and extract data to 'response_data'

            if isinstance(data[league]["fixtures"], list):
                index_dict_of_fixtures_by_fixture_id = u.create_dict_of_indexes_from_dict_values(data[league]["fixtures"], "fixture_id")
                index_of_fixture = index_dict_of_fixtures_by_fixture_id.get(fixture)

                if isinstance(index_of_fixture, list):
                    if not overwrite_fixture:
                        print(f"We already have fixture {fixture} from {country}, {league_name}, {season}: {league}")
                        continue

            url = f"http://v2.api-football.com/{action_name}/{fixture}"
            response = make_api_request(url, URL_HEADERS, action, fixture, ignore_logs=ignore_logs)

            if isinstance(response, bool):
                if response:
                    continue
                elif not response:
                    with open(filepath, "w") as w:
                        json.dump(data, w)
                    with open("config/data_config.json", "w") as w:
                        json.dump(data_cfg, w)
                    print(f"API call for league '{league}' INCOMPLETE as we've reached the API call limit. Data for league '{league}' saved at '{filepath}'. Count: {count}")
                    sys.exit(0)
            else:
                response_data = response.json()['api'][response_label]
            
            # 3.2) Update data with the response.
            if response.json()['api']['results'] == 0:
                print(f"Fixture {fixture} returned nothing from API call. This fixture is omitted from saved data and the fixture id is recorded at logs/omitted_fixtures.json.")
                try:
                    with open("logs/omitted_fixtures.json", "r") as r:
                        omitted_fixtures = json.load(r)
                except FileNotFoundError:
                    omitted_fixtures = {}
                
                if action not in omitted_fixtures.keys():
                    omitted_fixtures[action] = {}
                    omitted_fixtures[action][league] = []
                elif league not in omitted_fixtures[action].keys():
                    omitted_fixtures[action][league] = []
                
                omitted_fixtures[action][league].append(fixture)
                
                with open("logs/omitted_fixtures.json", "w") as w:
                    json.dump(omitted_fixtures, w)
                continue
            
            else:
                if isinstance(data[league]["fixtures"], dict):
                    data[league]["fixtures"][fixture] = response_data[0]

                elif isinstance(data[league]["fixtures"], list):
                    if isinstance(index_of_fixture, list) & overwrite_fixture:
                        data[league]["fixtures"][index_of_fixture[0]] = response_data[0]
                    elif index_of_fixture is None:
                        data[league]["fixtures"].append(response_data[0])
            
            if isinstance(selected_fixture_ids, list):
                data_cfg["SELECTED_IDS"].remove(fixture)

            print(f"Got data saved for fixture {fixture}.  \r", end="")
            count+=1

        # 5) Save the data and print a success message.
        with open(filepath, "w") as w:
            json.dump(data, w)
        with open("config/data_config.json", "w") as w:
            json.dump(data_cfg, w)

        print(f"Data for league '{league}' saved at '{filepath}'. Count: {count}")
        count=0
    print(f"COMPLETE! All data for '{action.lower()}' saved.")


def get_h2h_api_data(action="h2h", count=True, ignore_logs=False):
    # 0) Define required variables
    action = action.upper()
    action_name = cfg['API_ACTIONS'][action]['name']
    response_labels = cfg['API_ACTIONS'][action]['response']

    # 1) Perform checks
    #   1.1) Valid API action passed.
    if cfg['API_ACTIONS'][action]['group'] != "h2h":
        print(f"API action '{action}' is not a 'h2h' call. Check config.")
    
    # 2) Get all combinations of teamIds that have played against each other and loop through them
    teamIdCombos = u.get_all_team_id_combinations_from_fixtures()
    iteration = 0
    for home_team, away_team in teamIdCombos:
        # 3) Make API call and extract data to 'response_data'
        url = f"http://v2.api-football.com/{action_name}/{home_team}/{away_team}"
        response = make_api_request(url, URL_HEADERS, action, [home_team, away_team], ignore_logs=ignore_logs)

        # 3.1) Check the response value
        if isinstance(response, bool):
            if response:
                continue
            elif not response:
                sys.exit(0)
        else:
            response_data = response.json()['api']

        # 3.2) Seperate the response into it's two components
        team_response_data = response_data[response_labels[0]]
        fixtures_response_data = response_data[response_labels[1]]

        # 4) Save the data
        # 4.1.1) Get country for filepath and define the filepath
        home_team_country = u.get_team_info_from_team_id(home_team, "league_country")
        away_team_country = u.get_team_info_from_team_id(away_team, "league_country")

        if isinstance(home_team_country, list) or isinstance(away_team_country, list):
            if isinstance(home_team_country, str):
                home_team_country = [home_team_country]
            elif isinstance(away_team_country, str):
                away_team_country = [away_team_country]


            common_country = [value for value in home_team_country if value in away_team_country]
            if len(common_country) == 2:
                common_country.remove('World')
            common_country = common_country[0]
            
            os.makedirs(os.path.join(API_DATA_DIR, f"league/{common_country}/{action.lower()}", exist_ok=True))

            home_filepath = os.path.join(API_DATA_DIR, f"league/{common_country}/{action.lower()}/{home_team}.json")
            away_filepath = os.path.join(API_DATA_DIR, f"league/{common_country}/{action.lower()}/{away_team}.json")

        else:
            os.makedirs(os.path.join(API_DATA_DIR, f"league/{home_team_country}/{action.lower()}", exist_ok=True))
            os.makedirs(os.path.join(API_DATA_DIR, f"league/{away_team_country}/{action.lower()}", exist_ok=True))

            home_filepath = os.path.join(API_DATA_DIR, f"league/{home_team_country}/{action.lower()}/{home_team}.json")
            away_filepath = os.path.join(API_DATA_DIR, f"league/{away_team_country}/{action.lower()}/{away_team}.json")


        # 4.1) Home team        
        # 4.1.1) Read in file if exists and create json object if not
        try:
            with open(home_filepath, "r") as r:
                data = json.load(r)
        except FileNotFoundError:
            data = {}            
        
        # 4.1.2) Add data to json file object
        if away_team not in data.keys():
            data[away_team] = {}
            data[away_team][response_labels[0]] = team_response_data
            data[away_team][response_labels[1]] = fixtures_response_data
        # 4.1.3) Save file
        with open(home_filepath, "w") as w:
            json.dump(data, w)

        # 4.2) Away team
        # 4.2.1) Read in file if exists and create json object if not
        try:
            with open(away_filepath, "r") as r:
                data = json.load(r)
        except FileNotFoundError:
            data = {}
        
         # 4.2.2) Add data to json file object
        if home_team not in data.keys():
            data[home_team] = {}
            data[home_team][response_labels[0]] = team_response_data
            data[home_team][response_labels[1]] = fixtures_response_data
        # 4.2.3) Save file
        with open(away_filepath, "w") as w:
            json.dump(data, w)
        

        # 5) Check the iteration and print success message when done
        if count:
            iteration+=1
            print(f"Gone through iteration {iteration}. Saved for teams '{home_team}', '{away_team}'. \r", end="")
    print()
    print(f"COMPLETE! All data for '{action.lower()}' saved.")
