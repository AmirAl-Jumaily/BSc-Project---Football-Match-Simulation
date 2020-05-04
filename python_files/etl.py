from datetime import datetime as dt
import json, os

import pandas as pd
import numpy as np

import utils as u

cfg = u.get_config()
data_cfg = u.get_config("data")
csv_json_data_dir = f"{cfg['DIRS']['CSV_OF_JSON_DATA']}"

for dir_type in ["raw", "clean", "transformed"]:
    os.makedirs(os.path.join(cfg['DIRS']['CSV_OF_JSON_DATA'], dir_type), exist_ok=True)

def create_raw_match_stats_csv_from_json(verbose=True):

    fixture_filepaths = u.get_all_files_with_name("fixtures_all.json", file_dirs=cfg['ALL_LEAGUES_AND_CUPS'])
    path_to_save_at = os.path.join(csv_json_data_dir, "raw/match_stats.csv")
    match_stats_csv = pd.DataFrame()

    ##################################### Read in all data and convert to a single dataframe #####################################
    for fp in fixture_filepaths:
        with open(fp, "r") as r:
            data = json.load(r)

        for league in data.keys():
            league_data = u.convert_all_dict_values_to_str(data[league])

            for index, fixture in enumerate(league_data['fixtures']):
                if "statistics" in fixture.keys():
                    league_data['fixtures'][index]['match_statistics'] = league_data['fixtures'][index].pop("statistics", None)

                # Remove the other objects so they don't appear when we apply pd.json_normalise
                for data_object in ['lineups', 'events', 'players']:
                    league_data['fixtures'][index].pop(data_object, None)

            league_csv = pd.json_normalize(data=league_data, record_path='fixtures', meta=['country', 'type', 'name', 'season'])
            match_stats_csv = match_stats_csv.append(league_csv)

            if verbose: print(f"Data for league {league} added.\r", end="")
        if verbose: print(f"All leagues from {fp} added.")
    if verbose: print("All data saved to match_stats_csv.")
    ##################################### Read in all data and convert to a single dataframe #####################################

    ###################### Format dataframe: Filter columns, rename columns, create 2 key columns, sort data #####################
    match_stats_csv['fixture_date'] = match_stats_csv['event_timestamp'].apply(lambda x: dt.utcfromtimestamp(x).date().strftime('%d/%m/%Y'))
    if verbose: print("Column 'fixture_date' created.")

    match_stats_csv.rename(data_cfg["MATCH_STAT"]['COLUMN_RENAME_MAP'], axis='columns', inplace=True)
    if verbose: print("Columns renamed according to ['MATCH_STAT']['COLUMN_RENAME_MAP'] in data_config.json")

    match_stats_csv['has_match_stats'] = ~match_stats_csv[data_cfg['MATCH_STAT']['COLUMNS'][1:]].isna().all(axis=1)
    if verbose: print("Column 'has_match_stats' has been created to indicate which rows have at least 1 match statistic.")

    for column in ['home_passes_pct', 'away_passes_pct', 'home_possession', 'away_possession']:
        match_stats_csv[column] = (match_stats_csv[column].str.strip('%').astype(float)/100).apply(lambda x: round(x, 2))
    if verbose: print("Percentage columns have been converted to float values between 0 and 1.")

    COLUMN_FILTER = data_cfg['MATCH_INFO']['COLUMNS'] + data_cfg["MATCH_STAT"]['COLUMNS']
    match_stats_csv = match_stats_csv[COLUMN_FILTER].copy()
    if verbose: print("Columns filtered.")

    match_stats_csv.sort_values(
        by=['fixture_id'],
        axis=0, ascending=True, inplace=True)
    if verbose: print("Data sorted by fixture_id.")
    ###################### Format dataframe: Filter columns, rename columns, create 2 key columns, sort data #####################

    ######################################################### Save data ##########################################################
    match_stats_csv.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Match stats data saved at {path_to_save_at}")
    ######################################################### Save data ##########################################################

    print("COMPLETE - RAW match stats data created from json data and saved.")


def create_raw_match_events_csv_from_json(verbose=True):

    fixture_filepaths = u.get_all_files_with_name("fixtures_all.json", file_dirs=cfg['ALL_LEAGUES_AND_CUPS'])
    path_to_save_at = os.path.join(csv_json_data_dir, "raw/match_events.csv")
    match_events_csv = pd.DataFrame()

    ########################################## Read in all data and convert to a single dataframe #########################################
    for fp in fixture_filepaths:
        with open(fp, "r") as r:
            data = json.load(r)

        for league in data.keys():
            league_data = u.convert_all_dict_values_to_str(data[league])

            for index, fixture in enumerate(league_data['fixtures']):
                if "statistics" in fixture.keys():
                    league_data['fixtures'][index]['match_statistics'] = league_data['fixtures'][index].pop("statistics", None)

                # Remove the other objects so they don't appear when we apply pd.json_normalise
                for data_object in ['lineups', 'match_statistics', 'players']:
                    league_data['fixtures'][index].pop(data_object, None)
                
                league_data['fixtures'][index] = u.convert_all_dict_values_to_str(league_data['fixtures'][index], recursive=True)
            
            fixtures_data = league_data['fixtures']

            fixtures_csv = pd.json_normalize(data=fixtures_data, record_path='events', record_prefix='events.',
                meta=['fixture_id', 'league_id', 'event_timestamp', 'round', 'status', 'elapsed', 'venue', 'referee', 'goalsHomeTeam', 'goalsAwayTeam',
                      ['league', 'name'], ['league', 'country'],
                      ['homeTeam', 'team_id'], ['homeTeam', 'team_name'], ['awayTeam', 'team_id'], ['awayTeam', 'team_name'],
                      ['score', 'halftime'], ['score', 'fulltime'], ['score', 'extratime'], ['score', 'penalty']])
            
            fixtures_csv = fixtures_csv.assign(league_type = league_data['type'], league_season = league_data['season'])
            
            match_events_csv = match_events_csv.append(fixtures_csv)

            if verbose: print(f"Data for league {league} added.\r", end="")
        if verbose: print(f"All leagues from {fp} added.")
    if verbose: print("All data saved to match_events_csv.")
    ########################################## Read in all data and convert to a single dataframe #########################################

    ########################## Format dataframe: Filter columns, rename columns, create 2 key columns, sort data ##########################
    match_events_csv['fixture_date'] = match_events_csv['event_timestamp'].astype(int).apply(lambda x: dt.utcfromtimestamp(x).date().strftime('%d/%m/%Y'))
    if verbose: print("Column 'fixture_date' created.")

    match_events_csv.rename(data_cfg["MATCH_EVENT"]['COLUMN_RENAME_MAP'], axis='columns', inplace=True)
    if verbose: print("Columns renamed according to ['MATCH_EVENT']['COLUMN_RENAME_MAP'] in data_config.json")

    COLUMN_FILTER = data_cfg['MATCH_INFO']['COLUMNS'] + data_cfg["MATCH_EVENT"]['COLUMNS']
    match_events_csv = match_events_csv[COLUMN_FILTER].copy()
    if verbose: print("Columns filtered.")

    # Removing penalty shootouts as we absolutely do not want them
    match_events_csv = match_events_csv[match_events_csv['event_time_elapsed'] != -1].copy()
    if verbose: print("Removed penalty shootout data.")

    match_events_csv.sort_values(
        by=['fixture_id', 'event_type', 'event_detail', 'event_time_elapsed', 'event_team_name', 'event_player_name'],
        axis=0, ascending=True, inplace=True)
    if verbose: print("Data sorted by fixture_id and event_details.")
    ########################## Format dataframe: Filter columns, rename columns, create 2 key columns, sort data ##########################

    ############################################################## Save data ##############################################################
    match_events_csv.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Match events data saved at {path_to_save_at}")
    ############################################################## Save data ##############################################################

    print("COMPLETE - RAW match events data created from json data and saved.")


def create_clean_match_stats_csv_from_raw(verbose=True):

    match_stats_path = os.path.join(csv_json_data_dir, "raw/match_stats.csv")
    path_to_save_at = os.path.join(csv_json_data_dir, "clean/match_stats.csv")

    match_stats_csv = pd.read_csv(match_stats_path)

    ################################################################## Removing data  ##################################################################

    # Remove rows that have no match stats at all
    match_stats_csv = match_stats_csv[match_stats_csv.has_match_stats].copy()
    match_stats_csv.drop(columns='has_match_stats', inplace=True)
    if verbose: print("Removed rows that have no match stats at all.")

    # Keep only matches from rounds of interest
    match_stats_csv = match_stats_csv[
    (match_stats_csv.league_type == "League") | #((match_stats_csv.league_type == "Cup") & (match_stats_csv.country != "World")) |
    ((match_stats_csv.country == "World") & (match_stats_csv.fixture_round.isin(data_cfg["WORLD_DATA"]["KEY_FIXTURE_ROUNDS"])))]

    # # Remove rows that aren't "Match Finished"
    # match_stats_csv = match_stats_csv[match_stats_csv.fixture_status == "Match Finished"].copy()
    # if verbose: print("Removed rows where match is not finished.")
    # Note: Uneccesary due to removing matches with no match stats just above

    # Remove any irrelevant teams
    key_team_ids = u.get_ids_of_key_teams()
    match_stats_csv = match_stats_csv[
        (match_stats_csv['home_team_id'].isin(key_team_ids)) |
        (match_stats_csv['away_team_id'].isin(key_team_ids))].copy()
    match_stats_csv = match_stats_csv[
        ~((~(match_stats_csv['home_team_id'].isin(key_team_ids)) |
        ~(match_stats_csv['away_team_id'].isin(key_team_ids))) &
        (match_stats_csv["country"] != "World") &
        (match_stats_csv["league_type"] == "Cup"))].copy()
    if verbose: print("Removed rows where teams are not relevant.")

    # # Remove 2 fixtures that are missing most data (can't do anything to fix these and theyre just Coupe de France matches)
    # match_stats_csv = match_stats_csv[~match_stats_csv.home_possession.isnull()].copy()
    # if verbose: print("Removed 2 rows manually: Basically all data missing.")
    # Note: After changes I made to how we filter out by important teams or important rounds, this filter no longer does anything.

    # Manually removing Amiens vs Lille match (35777)
    #   (The data we could compare with the google results are all incorrect. We could manually input this data but can't conform other data is correct. Not worth the effort)
    match_stats_csv = match_stats_csv[(match_stats_csv.fixture_id).astype(int) != 35777].copy()
    if verbose: print("Removed 1 row manually where everything is likely incorrect and we can't check it easily (fixture 35777).")

    ################################################################## Removing data  ##################################################################

    # Manually editing, by fixture_id, one row for match between Sassuolo and Pescara as number of goals of both teams is inconsistent with match_events data
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 21650), 'home_goals'] = 2
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 21650), 'away_goals'] = 1
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 21650), 'fixture_result_ht'] = '1-0'
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 21650), 'fixture_result_ft'] = '2-1'
    if verbose: print("Manually edited row where goals and result columns were incorrect (fixture 21650).")

    ############################################# Use values of related columns to fill NAs where possible #############################################

    # Fill in missing shots_tot
    match_stats_csv['home_shots_tot'].fillna(match_stats_csv['home_shots_ont'] + match_stats_csv['home_shots_offt'] + match_stats_csv['home_shots_bl'], inplace=True)
    match_stats_csv['away_shots_tot'].fillna(match_stats_csv['away_shots_ont'] + match_stats_csv['away_shots_offt'] + match_stats_csv['away_shots_bl'], inplace=True)
    if verbose: print("Filling in shots_tot using shots (ont+offt+bl).")

    # Fill in missing shots_bl - Afternote: Seems they were all NA because they were equal to 0
    match_stats_csv['home_shots_bl'].fillna(match_stats_csv['home_shots_tot'] - (match_stats_csv['home_shots_ont'] + match_stats_csv['home_shots_offt']), inplace=True)
    match_stats_csv['away_shots_bl'].fillna(match_stats_csv['away_shots_tot'] - (match_stats_csv['away_shots_ont'] + match_stats_csv['away_shots_offt']), inplace=True)
    if verbose: print("Filling in shots_bl using shots (tot-(ont+offt)).")

    # Fill in missing home_shots_outb for the matches we have home_shots_inb - Afternote: Again, seems to be a nan because the value is 0. (Index found in 0.1) ipynb at 3.2.1))
    match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'home_shots_outb'] = (
        match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'home_shots_tot'] - match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'home_shots_inb'])
    match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'away_shots_outb'] = (
        match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'away_shots_tot'] - match_stats_csv.loc[match_stats_csv["fixture_id"]==20592,'away_shots_inb'])
    if verbose: print("Filling in shots_outb using shots (tot-inb) for the 1 fixture we can (fixture 20592).")

    # Fill in missing shots_inb as (shots_tot - shots_outb)
    match_stats_csv['home_shots_inb'] = (match_stats_csv['home_shots_tot'] - match_stats_csv['home_shots_outb'])
    match_stats_csv['away_shots_inb'] = (match_stats_csv['away_shots_tot'] - match_stats_csv['away_shots_outb'])
    if verbose: print("Filling in shots_inb using shots (tot-outb).")
    
    # Fill in missing passes_pct where we have passes_acc and passes_tot
    match_stats_csv['home_passes_pct'] = (match_stats_csv['home_passes_acc']/match_stats_csv['home_passes_tot']).apply(lambda x: round(x, 2))
    match_stats_csv['away_passes_pct'] = (match_stats_csv['away_passes_acc']/match_stats_csv['away_passes_tot']).apply(lambda x: round(x, 2))
    if verbose: print("Filling in passes_pct using passes (acc/tot).")

    # Approximate missing gksaves from shots_ont and goals of opposition
    match_stats_csv['home_gksaves'] = match_stats_csv["home_gksaves"].fillna((match_stats_csv['away_shots_ont'] - match_stats_csv['away_goals']))
    match_stats_csv['away_gksaves'] = match_stats_csv['away_gksaves'].fillna((match_stats_csv['home_shots_ont'] - match_stats_csv['home_goals']))

    match_stats_csv.home_gksaves[match_stats_csv['home_gksaves']<0] = 0
    match_stats_csv.away_gksaves[match_stats_csv['away_gksaves']<0] = 0
    
    if verbose: print("Filling in gksaves approximately using shots_ont-goals of opposition.")

    ############################################# Use values of related columns to fill NAs where possible #############################################

    ######################################### Use match_event_counts_clean to correct goals and cards columns #########################################

    match_events_count_clean = pd.read_csv(os.path.join(csv_json_data_dir, "clean/match_events_count.csv"))

    match_events_count_clean = match_events_count_clean.assign(
        home_goals = (match_events_count_clean['home_normal_goal'] + match_events_count_clean['home_own_goal'] + match_events_count_clean['home_penalty']),
        away_goals = (match_events_count_clean['away_normal_goal'] + match_events_count_clean['away_own_goal'] + match_events_count_clean['away_penalty']))
    
    relevant_fixture_ids = match_stats_csv.fixture_id.unique().tolist()

    key_match_counts = match_events_count_clean[['fixture_id', 'home_goals', 'away_goals', 'home_red_card', 'home_yellow_card','away_red_card', 'away_yellow_card']]
    key_match_counts = key_match_counts.loc[key_match_counts.fixture_id.isin(relevant_fixture_ids)].copy().reset_index(drop=True)

    # Make sure all fixtures are on the same row for both dataframes
    key_match_counts.sort_values(by='fixture_id', inplace=True)
    match_stats_csv.reset_index(drop=True, inplace=True)
    match_stats_csv.sort_values(by='fixture_id', inplace=True)

    match_stats_csv = match_stats_csv.assign(
        home_goals=key_match_counts['home_goals'], home_rc=key_match_counts['home_red_card'], home_yc=key_match_counts['home_yellow_card'],
        away_goals=key_match_counts['away_goals'], away_rc=key_match_counts['away_red_card'], away_yc=key_match_counts['away_yellow_card']
    )

    if verbose: print("Corrected Goal and Card columns using match_event_counts_clean.")
    ######################################### Use match_event_counts_clean to correct goals and cards columns #########################################

    # Manually editing, by fixture_id, a couple of rows where shots and passes data is missing and some other rows are incorrect.
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_shots_offt'] = 11
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_shots_bl'] = 1
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_shots_tot'] = 15
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_shots_inb'] = 9
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_shots_inb'] = 7
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_shots_outb'] = 6
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_shots_outb'] = 1
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_passes_acc'] = 349
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_passes_acc'] = 187
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_passes_tot'] = 468
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_passes_tot'] = 293
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_passes_pct'] = 0.75
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_passes_pct'] = 0.64
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'home_possession'] = 0.62
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_possession'] = 0.38
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 27411), 'away_yc'] = 3

    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_shots_bl'] = 6
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_shots_tot'] = 18
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_shots_inb'] = 10
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_shots_inb'] = 5
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_shots_outb'] = 8
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_shots_outb'] = 1
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_passes_acc'] = 266
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_passes_acc'] = 426
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_passes_tot'] = 347
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_passes_tot'] = 516
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_passes_pct'] = 0.77
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_passes_pct'] = 0.83
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'home_possession'] = 0.40
    match_stats_csv.loc[((match_stats_csv.fixture_id).astype(int) == 29264), 'away_possession'] = 0.60
    if verbose: print("Manually edited two rows where shots and passes were missing (fixtures 27411, 29264).")

    ##################################################### Impute values for missing offsides data #####################################################

    match_stats_csv["home_offsides"] = match_stats_csv.groupby("home_team_id")["home_offsides"].transform(lambda grp: grp.fillna(round(np.mean(grp))))
    match_stats_csv["away_offsides"] = match_stats_csv.groupby("away_team_id")["away_offsides"].transform(lambda grp: grp.fillna(round(np.mean(grp))))
    if verbose: print("Imputed Offsides columns using the average value of the team wh had the missing value.")

    ##################################################### Impute values for missing offsides data #####################################################

    ################################################### Removing data with too many missing values ####################################################
    match_stats_csv = match_stats_csv[~match_stats_csv[data_cfg['MATCH_STAT']['COLUMNS'][1:]].isna().any(axis=1)]
    if verbose: print("Finally remove any rows that have any match stats missing.")
    ################################################### Removing data with too many missing values ####################################################

    #################################################################### Save data ####################################################################
    match_stats_csv.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Clean match stats data saved at {path_to_save_at}")
    #################################################################### Save data ####################################################################

    print("COMPLETE - CLEAN match stats data created from raw match stats data and saved.")


def create_clean_match_events_csv_from_raw(verbose=True):

    path_to_save_at = os.path.join(csv_json_data_dir, "clean/match_events.csv")
    raw_match_events_path = os.path.join(csv_json_data_dir, "raw/match_events.csv")

    match_events_csv = pd.read_csv(raw_match_events_path)

    ################################################################## Editing data  ##################################################################

    # Manually editing, by fixture_id, one row for match between Sassuolo and Pescara as number of goals of both teams is inconsistent with match_events data
    match_events_csv.loc[((match_events_csv.fixture_id).astype(int) == 21650), 'home_goals'] = 2
    match_events_csv.loc[((match_events_csv.fixture_id).astype(int) == 21650), 'away_goals'] = 1
    match_events_csv.loc[((match_events_csv.fixture_id).astype(int) == 21650), 'fixture_result_ht'] = '1-0'
    match_events_csv.loc[((match_events_csv.fixture_id).astype(int) == 21650), 'fixture_result_ft'] = '2-1'
    if verbose: print("Manually edited row where goals and result columns were incorrect (fixture 21650).")

    # Manually correcting event_time_elapsed values for all valid rows with negative value for event_time_elapsed
    match_events_csv = u.modify_row_values(match_events_csv, data_cfg['MATCH_EVENT']['DATA_TO_CHANGE'])
    if verbose: print("Manually correcting row values (mostly event_time_elapsed where eventtime_elapsed was negative).")

    ################################################################## Editing data ##################################################################
    if verbose: print("COMPLETE - DATA CORRECTION")

    #################################################################################################################################################
    ################################################################# Deleting data #################################################################
    #################################################################################################################################################

    # Remove rows where match event (Goal or Card) did not exist.
    indexes_to_drop = u.find_indexes_by_col_values_dict(match_events_csv, data_cfg['MATCH_EVENT']['DATA_TO_DELETE'])
    match_events_csv.drop(labels=indexes_to_drop, inplace=True)
    if verbose: print("Removed Goal and Card events that did not actually happen. These were manually identified.")

    # Remove Card events with match_comments indicating it was 'Not on pitch'
    match_events_csv = match_events_csv[match_events_csv.event_comments != "Not on pitch"]
    match_events_csv = match_events_csv[match_events_csv.event_comments != "Not on pitch, Unsportsmanlike conduct"]
    if verbose: print("Removed Card events that occured to players 'Not on pitch'.")

    #################### Removing yellow card events that are duplicated but not red cards and therefore an incorrect duplicate ####################
    duplicate_match_events = match_events_csv[match_events_csv.duplicated(keep=False)].copy()
    duplicate_match_events_without_added_time_goals = duplicate_match_events[~(
        (duplicate_match_events.event_type == 'Goal') & ((duplicate_match_events.event_time_elapsed%45).astype(int) == 0))].copy()
    duplicate_event_ids = duplicate_match_events_without_added_time_goals.fixture_id.unique()
    
    rc_checker = match_events_csv[
        match_events_csv.event_detail == 'Red Card'].loc[match_events_csv.fixture_id.isin(duplicate_event_ids),
        ].copy().assign(event_detail = 'Yellow Card')
    rc_checker = duplicate_match_events_without_added_time_goals.merge(rc_checker, on=list(rc_checker.columns), how='left', indicator='exist')
    
    rc_checker = rc_checker[rc_checker.duplicated()].copy()
    indices_of_duplicates_to_remove = list(rc_checker[rc_checker.exist == 'left_only'].index)

    indexes_to_drop = []

    for rc_index in indices_of_duplicates_to_remove:
        incorrect_row_data = rc_checker.loc[[rc_index]]
        
        col_values_to_identify_row = {
            "fixture_id": incorrect_row_data["fixture_id"].tolist()[0],
            "event_team_name": incorrect_row_data["event_team_name"].tolist()[0],
            "event_time_elapsed": incorrect_row_data["event_time_elapsed"].tolist()[0],
            "event_detail": incorrect_row_data["event_detail"].tolist()[0]
        }
        
        index_value = u.find_indexes_by_col_values_dict(match_events_csv, col_values_to_identify_row, max_return=2)

        indexes_to_drop.extend(index_value)

    duplicate_rows = match_events_csv.loc[indexes_to_drop].copy()
    indexes_to_drop = duplicate_rows.index[duplicate_rows.duplicated()].tolist()

    match_events_csv.drop(labels=indexes_to_drop, inplace=True)
    if verbose: print("Removed any Yellow Card duplicate match events that should not be there. (They should only be there if that player was sent of after a second yellow).")
    #################### Removing yellow card events that are duplicated but not red cards and therefore an incorrect duplicate ####################

    #################################################################################################################################################
    ################################################################# Deleting data #################################################################
    #################################################################################################################################################
    if verbose: print("COMPLETE - DATA DELETION")

    #################################################################### Save data #####################################################################
    match_events_csv.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Clean match events data saved at {path_to_save_at}")
    #################################################################### Save data #####################################################################

    print("COMPLETE - CLEAN match events data created from raw match events data and saved.")


def create_match_events_count_csv(raw_clean="raw", verbose=True):

    path_to_save_at = os.path.join(csv_json_data_dir, f"{raw_clean}/match_events_count.csv")
    match_events_csv = pd.read_csv(os.path.join(cfg['DIRS']['CSV_OF_JSON_DATA'], f"{raw_clean}/match_events.csv"))

    ############################################# Perform necessary manipulations on match_events_csv data #############################################

    match_events_csv = match_events_csv.assign(event_by_home_team = (match_events_csv['event_team_name'] == match_events_csv['home_team_name'])).copy()
    if verbose: print("Formatting match_events (only within function): Created indicator column for if event was by home or away team.")

    goal_and_card_events_conditional = match_events_csv['event_type'].isin(['Goal', 'Card'])
    match_events_csv['event_type'][goal_and_card_events_conditional] = match_events_csv[goal_and_card_events_conditional]['event_detail']
    if verbose: print("Formatting match_events (only within function): Converted event_type column to have event_detail value (only for Goals and Cards).")

    ############################################# Perform necessary manipulations on match_events_csv data #############################################

    ###################################################### Creating match_events_count dataframe ######################################################
    
    match_events_count = match_events_csv.groupby(['fixture_id', 'event_type', 'event_by_home_team']).count()[['home_team_id']]
    match_events_count.rename({"home_team_id":"event_count"}, axis=1, inplace = True)
    match_events_count.reset_index(inplace=True)
    if verbose: print("Creating match_event_counts: Perform group by to get counts for different Card and Goal events.")
    
    # Now we make it show the count for each event for each team (home and away)
    home_match_events_count = match_events_count[match_events_count.event_by_home_team].copy()
    away_match_events_count = match_events_count[~match_events_count.event_by_home_team].copy()

    home_match_events_count.drop(columns='event_by_home_team', inplace=True)
    away_match_events_count.drop(columns='event_by_home_team', inplace=True)
    if verbose: print("Creating match_event_counts: Seperate data into home and away teams data.")
    
    home_match_events_count['event_type'] = home_match_events_count['event_type'].apply(lambda x: f"home_{x}".lower().replace(' ', '_'))
    away_match_events_count['event_type'] = away_match_events_count['event_type'].apply(lambda x: f"away_{x}".lower().replace(' ', '_'))
    if verbose: print("Creating match_event_counts: Change the names of the values to show that they are for home or away team.")
    
    home_match_events_count = home_match_events_count.pivot_table(values='event_count', index='fixture_id', columns='event_type')
    away_match_events_count = away_match_events_count.pivot_table(values='event_count', index='fixture_id', columns='event_type')
    if verbose: print("Creating match_event_counts: Apply pivot_table function to get final count for each event for bot home and away teams.")
    
    # Note: This is just to remove name attribute from the column index
    home_match_events_count.columns = list(home_match_events_count.columns)
    away_match_events_count.columns = list(away_match_events_count.columns)
    
    match_events_count = home_match_events_count.merge(away_match_events_count, on='fixture_id', how='outer')
    if verbose: print("Creating match_event_counts: Remerge home and away data to have a single table of match event counts.")

    match_events_count.fillna(0, inplace=True)
    match_events_count.reset_index(inplace=True)
    if verbose: print("Clean up: Fill any NA's with 0 and reseting index.")

    ###################################################### Creating match_events_count dataframe ######################################################

    #################################################################### Save data ####################################################################
    match_events_count.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Match events count data saved at {path_to_save_at}.")
    #################################################################### Save data ####################################################################

    print(f"COMPLETE - {raw_clean.upper()} match event counts data created from {raw_clean} match events data and saved.")


def create_match_info_data(verbose=True):

    match_stats_path = os.path.join(csv_json_data_dir, "raw/match_stats.csv")
    path_to_save_at = os.path.join(csv_json_data_dir, "match_info.csv")
    match_info_columns = data_cfg['MATCH_INFO']['COLUMNS'] + ['has_match_stats']
    
    match_stats_csv = pd.read_csv(match_stats_path)
    match_info_csv = match_stats_csv[match_info_columns]
    if verbose: print(f"Filtered out all columns except match_info_columns: {match_info_columns}")

    match_info_csv.to_csv(path_to_save_at, index=False)

    print("COMPLETE - All match info data saved.")


def create_transformed_match_stats_csv_from_clean(verbose=True):

    match_stats_path = os.path.join(csv_json_data_dir, "clean/match_stats.csv")
    path_to_save_at = os.path.join(csv_json_data_dir, "transformed/match_stats.csv")

    match_stats_csv = pd.read_csv(match_stats_path)

    ############################################################ Create percentage columns ############################################################
    match_stats_csv["home_shots_ont_pct"] = match_stats_csv["home_shots_ont"]/match_stats_csv["home_shots_tot"]
    match_stats_csv["away_shots_ont_pct"] = match_stats_csv["away_shots_ont"]/match_stats_csv["away_shots_tot"]
    if verbose: print("Created shots_ont_pct columns")

    match_stats_csv["home_shots_offt_pct"] = match_stats_csv["home_shots_offt"]/match_stats_csv["home_shots_tot"]
    match_stats_csv["away_shots_offt_pct"] = match_stats_csv["away_shots_offt"]/match_stats_csv["away_shots_tot"]
    if verbose: print("Created shots_offt_pct columns")

    match_stats_csv["home_shots_inb_pct"] = match_stats_csv["home_shots_inb"]/match_stats_csv["home_shots_tot"]
    match_stats_csv["away_shots_inb_pct"] = match_stats_csv["away_shots_inb"]/match_stats_csv["away_shots_tot"]
    if verbose: print("Created shots_inb_pct columns")

    match_stats_csv["home_gksaves_pct"] = match_stats_csv["home_gksaves"]/match_stats_csv["away_shots_ont"]
    match_stats_csv["away_gksaves_pct"] = match_stats_csv["away_gksaves"]/match_stats_csv["home_shots_ont"]
    if verbose: print("Created gksaves_pct columns")

    # Here we are also transforming the stats from offensive stats of team A to defensive stats of team B. Higher stats suggest a more defensive team.
    # So this is the percentage of the oppositions shots that the defending team blocked
    match_stats_csv["home_opp_shots_bl_pct"] = match_stats_csv["away_shots_bl"]/match_stats_csv["away_shots_tot"]
    match_stats_csv["away_opp_shots_bl_pct"] = match_stats_csv["home_shots_bl"]/match_stats_csv["home_shots_tot"]
    if verbose: print("Created opp_shots_bl_pct columns")

    # This is the percentage of the oppositions shots that were taken outside of the box
    match_stats_csv["home_opp_shots_outb_pct"] = match_stats_csv["away_shots_outb"]/match_stats_csv["away_shots_tot"]
    match_stats_csv["away_opp_shots_outb_pct"] = match_stats_csv["home_shots_outb"]/match_stats_csv["home_shots_tot"]
    if verbose: print("Created opp_shots_outb_pct columns")

    # First convert inf values to NaN so they can be converted properly to numeric values.
    match_stats_csv.replace([np.inf, -np.inf], np.nan, inplace=True)
    if verbose: print("Convert any np.inf values to np.nan values.")

    # Fill any NA values with zero in case a denominator was equal to 0
    match_stats_csv.fillna(value={
        "home_shots_ont_pct": 0.5, "away_shots_ont_pct": 0.5, "home_shots_offt_pct": 0.5, "away_shots_offt_pct": 0.5,
        "home_shots_inb_pct": 0, "away_shots_inb_pct": 0, "home_gksaves_pct": 0.5, "away_gksaves_pct": 0.5,
        "home_opp_shots_bl_pct": 1, "away_opp_shots_bl_pct": 1, "home_opp_shots_outb_pct": 1, "away_opp_shots_outb_pct": 1
    }, inplace=True)
    if verbose: print("Filled any NA columns with value of 0, 0.5 or 1 depending on the column and what having no shots may indicate.")

    # Finally round all numeric values to a maximum of 3 decimal places.
    match_stats_csv = match_stats_csv.round(3)
    if verbose: print("Rounded all values to a maximum of 3 decimal places.")
    ############################################################ Create percentage columns ############################################################

    #################################################################### Save data ####################################################################
    match_stats_csv.to_csv(path_to_save_at, index=False)
    if verbose: print(f"Transformed match stats data saved at {path_to_save_at}")
    #################################################################### Save data ####################################################################

    print("COMPLETE - TRANSFORMED match stats data created from clean match stats data and saved.")


def run_etl(
    raw_verbose=False, clean_verbose=False, tranform_verbose=False, feature_verbose=False, events_verbose=False, event_counts_verbose=False, stats_verbose=False, info_verbose=False):
    # Create from JSON data
    create_raw_match_events_csv_from_json(verbose=(raw_verbose & events_verbose))
    create_match_events_count_csv(raw_clean="raw", verbose=(raw_verbose & event_counts_verbose))
    create_raw_match_stats_csv_from_json(verbose=(raw_verbose & stats_verbose))
    create_match_info_data(verbose=info_verbose)

    # Create from RAW data
    create_clean_match_events_csv_from_raw(verbose=(clean_verbose & events_verbose))
    create_match_events_count_csv(raw_clean="clean", verbose=(clean_verbose & event_counts_verbose))
    create_clean_match_stats_csv_from_raw(verbose=(clean_verbose & stats_verbose))

    # Create from CLEAN data
    create_transformed_match_stats_csv_from_clean(verbose=tranform_verbose)

    print(dt.today().strftime("%d-%m-%Y %H:%M:%S"))
