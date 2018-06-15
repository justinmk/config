from sys import stdin
import json
import requests
import netrc
import urllib

DEBUG=False

def main_():
    netrc_info = netrc.netrc()
    gist_api_cred = netrc_info.authenticators('gitlab.com')
    gist_api_token = gist_api_cred[0]

    gitlab_project_id = urllib.parse.quote_plus('justinmk/gist')

    inputstr = ""
    for line in stdin:
        inputstr = inputstr + line

    gist_api_request_payload = {
      'id': '{}/snippets'.format(gitlab_project_id),
      'title': 'unnamed',
      'file_name': 'unnamed.txt',
      'description': '',
      'visibility': 'private',
      'code': inputstr,
    }

    if DEBUG:
        print(gist_api_request_payload)

    endpoint = 'https://gitlab.com/api/v4/projects/{}/snippets'.format(gitlab_project_id)
    response = requests.post(endpoint,
                        data=gist_api_request_payload,
                        headers={
                            'Private-Token' : '{}'.format(gist_api_token),
                            # GitLab uses form-encoding, not JSON.
                            # 'Content-Type'  : 'application/json',
                        })

    j = response.json()
    try:
        j = response.json()
        print('response: {}'.format(j))
        print('URL: {}'.format(j['web_url']))
    except KeyError:
        print('error: web_url key not in response')

if __name__ == '__main__':
    main_()
