import json
import utils as u

cfg = u.get_config()

def add_league_info_to_key_leagues():
    filepaths = u.get_all_files_with_name("fixtures_all.json", file_dirs=cfg['ALL_LEAGUES_AND_CUPS'])

    for league_file in filepaths:
        with open(league_file, "r") as r:
            data = json.load(r)

        for league in data.keys():
            all_league_data = u.get_league_data(filtered=False, has_match_stats_data=False)

            data[league]['league_id'] = all_league_data[league]['league_id']
            data[league]['country'] = all_league_data[league]['country']
            data[league]['type'] = all_league_data[league]['type']
            data[league]['name'] = all_league_data[league]['name']
            data[league]['season'] = all_league_data[league]['season']
            
        
        with open(league_file, "w") as w:
            json.dump(data, w)
