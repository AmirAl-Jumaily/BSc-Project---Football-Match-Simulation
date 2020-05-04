from datetime import datetime as dt
import os

import pandas as pd
import simplejson as sjson

import utils as u

cfg = u.get_config()
data_cfg = u.get_config("data")

def get_feature_set_data(which_data="clean", just_key_comps=True, season=None):
    feature_set_fp = os.path.join(cfg['DIRS']['CSV_OF_JSON_DATA'], f"{which_data}/match_stats.csv")
    data = pd.read_csv(feature_set_fp)

    if season is not None:
        data = data[data.league_season == int(season)].copy()

    if just_key_comps:
        data = data[(data["league_type"] == "League") | (data["country"] == "World")].copy()
        data = data[data.fixture_round != "Finals"].copy() # One match in Bundesliga
    
    data.sort_values(by="fixture_id", inplace=True, ignore_index=True)
    
    return data


def get_number_of_matches(which_data="clean", just_key_comps=True, season=None):
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)

    return len(data)


def get_number_of_match_stats(which_data="clean", just_key_comps=True, season=None):
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)

    match_stat_cols = data.columns.tolist()
    for col in data_cfg["MATCH_INFO"]["COLUMNS"]:
        match_stat_cols.remove(col)
    
    return len(match_stat_cols)/2


def get_all_team_names(which_data="clean", just_key_comps=True, season=None):
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)
    
    home_teams = data["home_team_name"].unique().tolist()
    away_teams = data["away_team_name"].unique().tolist()

    if sorted(home_teams) == sorted(away_teams):
        return home_teams
    else:
        return {"home_teams": home_teams, "away_teams": away_teams}


def get_number_of_teams(which_data="clean", just_key_comps=True, season=None):
    teams = get_all_team_names(which_data=which_data, just_key_comps=just_key_comps, season=season)

    if isinstance(teams, list):
        return len(teams)
    elif isinstance(teams, dict):
        return {"num_home_teams": len(teams["home_teams"]), "num_away_teams": len(teams["away_teams"])}


def create_team_id_mapper(which_data="clean", just_key_comps=True, season=None):

    teams = sorted(get_all_team_names(which_data=which_data, just_key_comps=just_key_comps, season=season))
    teams_index = range(1, len(teams)+1)
    team_name_to_index_map = {teams[index-1]:index for index in teams_index}
    
    return team_name_to_index_map


def create_team_name_to_country_mapper(which_data="clean", just_key_comps=True, season=None):
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)
    data = data[data.country != "World"].copy()
    data.drop_duplicates(subset="home_team_name", keep="last", ignore_index=True, inplace=True)
    data = data[["home_team_name", "country"]].copy()
    data.sort_values(by=["country", "home_team_name"], inplace=True, ignore_index=True)

    teams_index = range(0, len(data))
    team_name_to_country_map = {data.loc[index, "home_team_name"]:data.loc[index, "country"] for index in teams_index}

    return team_name_to_country_map


def create_game_index_to_team_index_mapper(home_or_away, which_data="clean", just_key_comps=True, season=None):
    team_index_mapper = create_team_id_mapper(which_data=which_data, just_key_comps=just_key_comps, season=season)
    
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)[["fixture_id", f"{home_or_away}_team_name"]].copy()
    data[f"{home_or_away}_index_id"] = data[f"{home_or_away}_team_name"].map(team_index_mapper)
    data = data[[f"{home_or_away}_index_id"]]
    
    return data


def create_match_stats_bugs_data(home_or_away, which_data="clean", just_key_comps=True, season=None):
    data = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps, season=season)
    
    match_stat_cols = data.columns.tolist()
    for col in data_cfg["MATCH_INFO"]["COLUMNS"]:
        match_stat_cols.remove(col)
    columns = [x for x in match_stat_cols if home_or_away in x]

    return data[columns].copy()


def combine_all_bugs_model_data(which_data="clean", just_key_comps=True, season=None):
    num_matches = get_number_of_matches(which_data=which_data, just_key_comps=just_key_comps, season=season)
    num_teams = get_number_of_teams(which_data=which_data, just_key_comps=just_key_comps, season=season)
    num_match_stats = get_number_of_match_stats(which_data=which_data, just_key_comps=just_key_comps, season=season)

    team_id_mapper = create_team_id_mapper(which_data=which_data, just_key_comps=just_key_comps, season=season)
    team_country_mapper = create_team_name_to_country_mapper(which_data=which_data, just_key_comps=just_key_comps, season=season)

    home_team_game_index = create_game_index_to_team_index_mapper("home", which_data=which_data, just_key_comps=just_key_comps, season=season)
    away_team_game_index = create_game_index_to_team_index_mapper("away", which_data=which_data, just_key_comps=just_key_comps, season=season)

    bugs_model_data_dict = {
        "num_matches": num_matches,
        "num_teams": num_teams,
        "num_match_stats": num_match_stats,
        "team_id_mapper": team_id_mapper,
        "team_country_mapper": team_country_mapper,
        "home_team_game_index": home_team_game_index["home_index_id"].tolist(),
        "away_team_game_index": away_team_game_index["away_index_id"].tolist()
    }

    home_match_stats = create_match_stats_bugs_data("home", which_data=which_data, just_key_comps=just_key_comps, season=season)
    away_match_stats = create_match_stats_bugs_data("away", which_data=which_data, just_key_comps=just_key_comps, season=season)

    for df in [home_match_stats, away_match_stats]:
        columns = df.columns.tolist()
        home_or_away = columns[0].split("_")[0]
        bugs_model_data_dict[f"{home_or_away}_match_stats"] = {}
        for col in columns:
            col_data = df[col].tolist()
            clean_col = col.split(f"{home_or_away}_")[-1]
            bugs_model_data_dict[f"{home_or_away}_match_stats"][clean_col] = col_data
    
    return bugs_model_data_dict


def create_file_with_all_data_for_bugs_model(which_data="clean", just_key_comps=False, by_season=True):
    BUGS_DIR = cfg['DIRS']['BUGS_DATA']
    os.makedirs(os.path.join(BUGS_DIR, which_data), exist_ok=True)

    if by_season:
        seasons = get_feature_set_data(which_data=which_data, just_key_comps=just_key_comps).league_season.unique()

        bugs_model_data_dict = {}
        for season in seasons:
            bugs_model_data_dict[str(season)] = combine_all_bugs_model_data(which_data=which_data, just_key_comps=just_key_comps, season=season)
        filename = "bugs_model_data_dict_seasons_split.json"
        with open(os.path.join(BUGS_DIR, which_data, filename), "w") as w:
            sjson.dump(bugs_model_data_dict, w, ignore_nan=True)
    
    else:
        bugs_model_data_dict = combine_all_bugs_model_data(which_data=which_data, just_key_comps=just_key_comps)
        filename = "bugs_model_data_dict_seasons_combined.json"
        with open(os.path.join(BUGS_DIR, which_data, filename), "w") as w:
            sjson.dump(bugs_model_data_dict, w, ignore_nan=True)
    
    print(f"COMPLETE - All bugs data needed for the model saved in file '{filename}' at\n{os.path.join(BUGS_DIR, which_data)}")
    print(dt.today().strftime("%d-%m-%Y %H:%M:%S"))
