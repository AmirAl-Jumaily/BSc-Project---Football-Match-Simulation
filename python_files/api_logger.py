import json, os
from datetime import date, datetime

import utils as u

cfg = u.get_config()

####################################################################
###################### Log checking functions ######################
####################################################################

def check_if_call_made_previously(action, request_id):
    """
    This function checks if we have already made a call for this data.
    """
    action = action.lower()
    
    if isinstance(request_id, list):
        request_id.sort()
        request_id = list(map(str, request_id))
        request_id = "_".join(request_id)
    elif not isinstance(request_id, str):
        request_id = str(request_id)

    with open(cfg['FILES']['REQUEST_LOGS'], "r") as r:
        request_logs = json.load(r)
    
    try:
        call_made_previously = request_id in request_logs[action]
    except KeyError:
        request_logs[action] = []
        call_made_previously = request_id in request_logs[action]

    return call_made_previously

def check_if_api_calls_remaining():
    """
    There is a daily limit on the number of API requests we can make. This checks if we have reached our limit.
    """
    todays_date = datetime.strftime(date.today(), "%d/%m/%Y")
    
    with open(cfg['FILES']['COUNT_LOGS'], "r") as r:
        count_logs = json.load(r)
    
    try:
        requests_made = count_logs[todays_date]["total_requests_made"]
    except KeyError:
        try:
            count_logs[todays_date]["total_requests_made"] = 0
        except KeyError:
            count_logs[todays_date] = {}
            count_logs[todays_date]["total_requests_made"] = 0
        
        requests_made = count_logs[todays_date]["total_requests_made"]

        with open(cfg['FILES']['COUNT_LOGS'], "w") as w:
            json.dump(count_logs, w)

    requests_remaining = (requests_made < cfg["DAILY_REQUEST_LIMIT"])

    return requests_remaining

###########################################################################
###################### Log updating helper functions ######################
###########################################################################

def update_call_count(todays_date, url, action):
    """
    There is a daily limit on the number of API requests we can make. This function tracks that.
    Also saves the url with which the request was made
    """
    action = action.lower()
    with open(cfg['FILES']['COUNT_LOGS'], "r") as r:
        count_logs = json.load(r)
    
    # Update the call count
    requests_made = count_logs[todays_date]["total_requests_made"]
    count_logs[todays_date]["total_requests_made"] = requests_made + 1

    try:
        count_logs[todays_date][action]["urls"].append(url)
    except KeyError:
        count_logs[todays_date][action] = {}
        count_logs[todays_date][action]["urls"] = []
        count_logs[todays_date][action]["urls"].append(url)
    
    # Save updated file contents
    with open(cfg['FILES']['COUNT_LOGS'], "w") as w:
        json.dump(count_logs, w)

def update_ids_of_requests_made(action, request_id):
    """
    This is a helper function to log the ids of things we have rquested data for.
    """
    action = action.lower()
    with open(cfg['FILES']['REQUEST_LOGS'], "r") as r:
        request_logs = json.load(r)
    
    # If request_id is already in the logs, then exit the function
    if request_id in request_logs[action]:
        return
    
    # Check and then correct the format of the request_id for the logs
    if isinstance(request_id, list):
        request_id.sort()
        request_id = list(map(str, request_id))
        request_id = "_".join(request_id)
    elif not isinstance(request_id, str):
        request_id = str(request_id)
    
    try:
        request_logs[action].append(request_id)
    except KeyError:
        request_logs[action] = []
        request_logs[action].append(request_id)
    
    with open(cfg['FILES']['REQUEST_LOGS'], "w") as w:
        json.dump(request_logs, w)

#####################################################################
###################### Master logging function ######################
#####################################################################

# TODO: Will need to update it so that the ACTION and REQUEST_IDS are passed into this function, not PARAMS
def update_logs_after_request(response, action, request_id):
    """
    This master function updates our logs for an API request.
    """
    action = action.lower()
    todays_date = datetime.strftime(date.today(), "%d/%m/%Y")

    update_call_count(todays_date, response.url, action)

    update_ids_of_requests_made(action, request_id)
