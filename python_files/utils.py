from datetime import date, datetime
import json, os

import requests
import pandas as pd

def get_config(cfg_type=None):
    filename = "config.json"
    if cfg_type is not None:
        filename = "_".join([cfg_type, filename])

    with open(f"config/{filename}", "r") as r:
        cfg = json.load(r)
    return cfg

cfg = get_config()
data_cfg = get_config("data")
API_DATA_DIR = cfg['DIRS']['API_DATA']

###################################################################
######################## Generic functions ########################
###################################################################

def item_generator(json_input, lookup_key):
    if isinstance(lookup_key, str):
        lookup_key = [lookup_key]

    if isinstance(json_input, dict):
        for k, v in json_input.items():
            if k in lookup_key:
                yield v
            else:
                yield from item_generator(v, lookup_key)
    elif isinstance(json_input, list):
        for item in json_input:
            yield from item_generator(item, lookup_key)


def convert_all_dict_values_to_str(dictionary, recursive=False, verbose=False):
    for key in dictionary.keys():
        if isinstance(dictionary[key], int):
            dictionary[key] = str(dictionary[key])
        elif dictionary[key] is None:
            dictionary[key] = ''
        elif isinstance(dictionary[key], dict) and recursive:
            dictionary[key] = convert_all_dict_values_to_str(dictionary[key], recursive=recursive)
        elif isinstance(dictionary[key], list):
            if verbose:
                print(f"Function 'convert_all_dict_values_to_str' does not work on lists. Dictionary key: {key}.")
    
    return dictionary


def create_duplicate_row_with_modified_values(dataframe, modification_dictionary):
    df = dataframe.copy()
    for row_index in modification_dictionary.keys():
        mods_to_this_row = modification_dictionary[row_index]
        
        # A loop in case we want to create more than one row copy from this row index
        for row_copy_mods in mods_to_this_row:
            
            row_copy = dataframe.loc[int(row_index),].copy()
            
            # Check if user wants more than one copy of this exact modification of the row
            num_row_repeats = int(row_copy_mods.pop('num_row_repeats', 1))

            for col_name, new_col_value in row_copy_mods.items():
                row_copy.at[col_name] = new_col_value

            df = df.append([row_copy]*num_row_repeats, ignore_index=True)
    
    return df


def find_indexes_by_col_values_dict(dataframe, col_values_dict, max_return=None):
    conditions = dataframe.any(axis=1)
    
    if isinstance(col_values_dict, dict):        
        max_return = col_values_dict.pop("max_return", 1) if (max_return is None) else max_return

        for col, value in col_values_dict.items():
            conditions = conditions & (dataframe[col] == value)
        
        indexes = dataframe.index[conditions].tolist()

        if (len(indexes) > max_return) & (max_return != -1):
            print("ERROR: More indexes returned than expected. Please add/edit the 'max_return' dict key.")
            print("If you don't want to limit the number of index values returned, specify 'max_return: -1' in the dictionary", end = ' ')
            print("or pass 'max_return= -1' to the function call for no limits on any set of conditions.")
        else:
            return indexes
    
    elif isinstance(col_values_dict, list):
        all_indexes = []
        for spec_dict in col_values_dict:
            all_indexes.extend(find_indexes_by_col_values_dict(dataframe, spec_dict, max_return))
        
        return all_indexes
    
    else:
        print("ERROR: You must either pass a dictionary (to specify a single set of conditions) or a list of ", end='')
        print("dictionarys (to specify more than one set of conditions to find )")


def modify_row_values(dataframe, dict_of_row_info_and_changes):
    df = dataframe.copy()

    if isinstance(dict_of_row_info_and_changes, dict):
        index_of_row_to_change = find_indexes_by_col_values_dict(df, dict_of_row_info_and_changes['col_details'])
        
        for col, value in dict_of_row_info_and_changes['values_to_change'].items():
            df.loc[index_of_row_to_change, col] = value
    
    elif isinstance(dict_of_row_info_and_changes, list):
        for row_info_change in dict_of_row_info_and_changes:
            index_of_row_to_change = find_indexes_by_col_values_dict(df, row_info_change['col_details'])

            for col, value in row_info_change['values_to_change'].items():
                df.loc[index_of_row_to_change, col] = value
    
    return df


def create_dict_of_indexes_from_dict_values(list_of_dicts, key):
    dictionary_of_indexes = {}
    for (index, dict_at_index) in enumerate(list_of_dicts):
        if dict_at_index[key] not in dictionary_of_indexes.keys():
            dictionary_of_indexes[dict_at_index[key]] = [index]
        else:
            dictionary_of_indexes[dict_at_index[key]].append(index)
    
    return dictionary_of_indexes

####################################################################
###################### Data request functions ######################
####################################################################

def get_league_data(filtered = True, ids_only = False, has_match_stats_data = True):

    if filtered:
        with open(os.path.join(API_DATA_DIR, "filtered/leagues.json"), "r") as r:
            league_data = json.load(r)
    elif not filtered:
        filepath = os.path.join(API_DATA_DIR, "general/all_leagues.json")

        if os.path.exists(filepath):
            with open(filepath, "r") as r:
                league_data = json.load(r)
        else:
            with open(os.path.join(API_DATA_DIR, "general/leagues.json"), "r") as r:
                all_leagues_raw_data = json.load(r)['api']['leagues']
            
            league_data = {}

            for league in all_leagues_raw_data:

                league_id = str(league['league_id'])
                league_object = {}

                league_object['league_id'] = league_id
                league_object['country'] = league['country']
                league_object['type'] = league['type']
                league_object['name'] = league['name']
                league_object['season'] = league['season']
                league_object['season_start'] = league['season_start']
                league_object['season_end'] = league['season_end']
                league_object['coverage'] = league['coverage']
                league_object['is_current'] = league['is_current']

                # At the end add the league to the overall league_data
                league_data[league_id] = league_object
            
            with open(filepath, "w") as w:
                json.dump(league_data, w)
    
    if has_match_stats_data:
        league_ids = []
        for league in league_data.keys():
            if check_if_league_has_coverage(league, 'statistics', fixtures_coverage=True):
                league_ids.append(league)

        if ids_only:
            return league_ids
        else:
            return {lkey: league_data[lkey] for lkey in league_ids}
    else:
        if ids_only:
            return list(league_data.keys())
        else:
            return league_data


def check_if_league_has_coverage(leagueId, coverage_name, fixtures_coverage=False):
    league_data_coverage = get_league_data(filtered=False, has_match_stats_data=False)[leagueId]['coverage']

    if fixtures_coverage:
        league_data_coverage = league_data_coverage['fixtures']
    
    return league_data_coverage[coverage_name]


def get_country_and_league_name_from_id(leagueId):
    league_data = get_league_data(filtered=False, has_match_stats_data=False)[leagueId]
    
    return league_data['country'], league_data['name']


def get_fixture_ids_for_league(leagueId):
    country, league_name = get_country_and_league_name_from_id(leagueId)

    fixtures_filepath = os.path.join(API_DATA_DIR, f"league/{country}/{league_name}/league_fixtures.json")

    with open(fixtures_filepath, "r") as r:
        fixture_data = json.load(r)
    
    try:
        fixture_data = fixture_data[leagueId]['fixtures']
    except TypeError:
        fixture_data = fixture_data[leagueId]

    return list(item_generator(fixture_data, "fixture_id"))


def get_all_files_with_name(filename, file_dirs=[], key_countries_only=True):
    project_root_folder = cfg['DIRS']['PROJECT_ROOT']
    filepaths = []

    for path, _, files in os.walk(project_root_folder):
        if key_countries_only:
            if not any(x in path for x in cfg['KEY_COUNTRIES']):
                continue
        
        if len(file_dirs) != 0:
            if not any(x in file_dirs for x in path.split('/')):
                continue

        for name in files:
            if name == filename:
                filepaths.append(os.path.join(path, name))
    
    return filepaths


def get_all_team_id_combinations_from_fixtures(key_countries_only=True, key_leagues_only=True):
    if key_leagues_only:
        file_dirs = cfg['ALL_LEAGUES_AND_CUPS']
    else:
        file_dirs = []
    filepaths = get_all_files_with_name("league_fixtures.json", file_dirs=file_dirs, key_countries_only=key_countries_only)

    team_combinations = []

    for fp in filepaths:
        with open(fp, "r") as r:
            data = json.load(r)
        
        teamIds = list(item_generator(data, "team_id"))
        home_teams = teamIds[::2]
        away_teams = teamIds[1::2]
        team_combinations.append(list(zip(home_teams, away_teams)))
    
    team_combinations = [item for sublist in team_combinations for item in sublist]
    team_combinations = list(map(sorted, team_combinations))
    team_combinations = [tuple(team_combo) for team_combo in team_combinations]

    return set(team_combinations)


def get_teams_as_csv(overwrite=False):
    file_location = os.path.join(cfg['DIRS']['CSV_OF_JSON_DATA'], "all_teams.csv")

    # If file exists read from it.
    if os.path.isfile(file_location) and not overwrite:
        teams_csv = pd.read_csv(file_location, dtype={
            "team_id": str, "founded": str, "venue_capacity": str, "league_id": str, "league_season": str})
        return teams_csv
    
    with open(os.path.join(API_DATA_DIR, "filtered/teams.json"), "r") as r:
        teams = json.load(r)
    
    for league in teams.keys():
        for k, v in teams[league].items():
            if isinstance(v, int):
                teams[league][k] = str(v)
            elif isinstance(v, list):
                for index, team in enumerate(teams[league][k]):
                    for inner_k, inner_v in team.items():
                        if isinstance(inner_v, int):
                            teams[league][k][index][inner_k] = str(inner_v)
    
    teams_csv = pd.DataFrame()
    for league in teams.keys():
        one_leagues_teams_csv = pd.json_normalize(data=teams[league], record_path="teams", meta=[
            "league_id", "country", "type", "name", "season"], meta_prefix="league_")
        teams_csv = teams_csv.append(one_leagues_teams_csv)
    
    teams_csv.reset_index(drop=True, inplace=True)

    teams_csv.rename(columns={"league_league_id": "league_id"}, inplace=True)

    teams_csv.to_csv(file_location, index=False)
    print(f"teams_csv saved at location :{file_location}")

    return teams_csv


def get_team_info_from_team_id(teamId, col_name, verbose=False):
    teams_csv = get_teams_as_csv()

    teamId = str(teamId)
    team_value = teams_csv.loc[teams_csv['team_id'] == teamId, col_name].unique()
    
    if len(team_value) == 0:
        if verbose:
            print(f"No team with id {teamId}")
        return None
    if len(team_value) == 1:
        return team_value[0]
    if len(team_value) > 1:
        if verbose:
            print(f"Multiple values returned: {team_value}")
        return list(team_value)


def get_league_info_from_league_id(leagueId, key_name, verbose=False):
    all_leagues = get_league_data(filtered=False, has_match_stats_data=False)
    leagueId = str(leagueId)

    return all_leagues[leagueId][key_name]


def filter_fixtures_by_match_stats(fixture_data, no_missing_stats_in_fixture = True):
    if no_missing_stats_in_fixture:
        filtered_data = fixture_data[~fixture_data[data_cfg['MATCH_STAT_COLUMNS']].isna().any(axis=1)]
    else:
        filtered_data = fixture_data[~fixture_data[data_cfg['MATCH_STAT_COLUMNS']].isna().all(axis=1)]
    
    return filtered_data


def get_ids_of_key_teams():
    teams_csv = get_teams_as_csv()
    teams_csv = teams_csv[teams_csv.league_season.astype(int)  > 2015].copy()
    important_teams = teams_csv[teams_csv.league_type == 'League']

    return important_teams.team_id.unique()
