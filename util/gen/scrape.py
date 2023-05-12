import requests, json, os

url = 'https://raw.githubusercontent.com/TomHarte/ProcessorTests/main/nes6502/v1/'
path = '../../data/opcode/'
num = 25

for hx in range(256):
    strhx = hex(hx)[2:].zfill(2)
    full_url = url + strhx + '.json'

    name = '0x' + strhx.upper() + '.json'
    full_path = path + name[:3] + '/' + name

    print(full_url, '->', full_path)
    os.makedirs(os.path.dirname(full_path), exist_ok=True)

    response = requests.get(full_url)
    data = json.loads(response.text)

    with open(full_path, 'w') as f:
        json.dump(data[:num], f, indent=4)
        f.close()
