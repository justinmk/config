from sys import stdin
import json
import requests
import netrc

DEBUG=False

def main_():
    netrc_info = netrc.netrc()
    gist_api_cred = netrc_info.authenticators('gist.github.com')
    gist_api_token = gist_api_cred[0]

    inputstr = ""
    for line in stdin:
        inputstr = inputstr + line

    gist_api_request_payload = {
      "description": "",
      "public": False,
      "files": {
        "unnamed.txt": {
          "content": inputstr
        }
      }
    }

    if DEBUG:
        print(gist_api_request_payload)

    response = requests.post("https://api.github.com/gists",
                        data=json.dumps(gist_api_request_payload),
                        headers={
                            'Authorization' : 'token {}'.format(gist_api_token),
                            'Content-Type'  : 'application/json'
                        })

    j = response.json()
    try:
        print('URL: ' + str(response.json()['html_url']))
    except KeyError:
        print('response: ' + str(response.json()))

if __name__ == '__main__':
    main_()
