from sys import stdin
import json
import requests

inputstr = ""
for line in stdin:
    inputstr = inputstr + line

gist_api_request_payload = {
  "description": "",
  "public": True,
  "files": {
    "file.txt": {
      "content": inputstr
    }
  }
}

response = requests.post("https://api.github.com/gists",
                    data=json.dumps(gist_api_request_payload),
                    headers={})

print('response: ' + str(response.json()['html_url']))
