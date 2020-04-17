import os
from subprocess import Popen, PIPE
import re
import base64
import xml.etree.ElementTree as xml

local_dir = os.path.dirname(os.path.realpath(__file__))

def compress(uncompressed):
    """Compress a string to a list of output symbols."""
 
    # Build the dictionary.
    dict_size = 256
    dictionary = {"{:02x}".format(i): i for i in range(dict_size)}

    w = ""
    result = []
    for i in range(0,len(uncompressed),2):
        c = uncompressed[i:i+2]
        wc = w + c
        if wc in dictionary:
            w = wc
        else:
            result.append(dictionary[w])
            # Add wc to the dictionary.
            dictionary[wc] = dict_size
            dict_size += 1
            w = c
 
    # Output the code for w.
    if w:
        result.append(dictionary[w])
    return result

def requiredBits(value):
    bits = 1
    while(value > 0):
        bits+=1
        value = value >> 1
    return bits

# https://github.com/coells/100days
def lzw_encode(data):
    code, code_bits = {bytes([i]): i for i in range(256)}, 8
    buffer, buffer_bits = 0, 0
    index, aux = 0, []

    while index < len(data):
        # find word
        for j in range(index + 1, len(data) + 1):
            word = data[index:j]

            # store word
            if word not in code:
                code[word] = len(code)
                word = word[:-1]
                break

        # write buffer
        buffer <<= code_bits
        buffer |= code[word]
        buffer_bits += code_bits

        # code length
        if len(code) > 2 ** code_bits:
            code_bits += 1

        # shift
        index += len(word)

        # buffer alignment
        if index >= len(data) and buffer_bits % 8:
            r = 8 - (buffer_bits % 8)
            buffer <<= r
            buffer_bits += r

        # emit output
        if not buffer_bits % 8:
            aux += int.to_bytes(buffer, buffer_bits >> 3, 'big')
            buffer, buffer_bits = 0, 0

    return bytes(aux)

def lzw_decode(data):
    code, code_bits = {i: bytes([i]) for i in range(256)}, 8
    buffer, buffer_bits = 0, 0
    index, aux = 0, []
    prefix = b''

    while index < len(data) or buffer_bits >= code_bits:
        # read buffer
        while index < len(data) and buffer_bits < code_bits:
            buffer <<= 8
            buffer |= data[index]
            buffer_bits += 8
            index += 1
            #print("{:04x}".format(buffer))
        
        # find word
        buffer_bits -= code_bits
        #print(buffer_bits)
        key = buffer >> buffer_bits
        prev_buffer = buffer
        buffer &= (1 << buffer_bits) - 1
        # print("key: {:02x} found: {}".format(key,code.get(key, "no")))
        word = code.get(key, prefix + prefix[:1])
        found = code.get(key, "x")

        # store word
        if prefix:
            code[len(code)] = prefix + word[:1]
            
        print("{} k:0x{:04X} #w:{} ({})".format(index,key,len(word),found))
        prefix = word
        # if(index>10): return

        # code length
        if len(code) >= 2 ** code_bits:
            print("{} bits: {}".format(len(code), code_bits+1))
            code_bits += 1

        # emit output
        aux += word
    x,y=0,0
    for b in aux:
        x+=1
        if x>127:
            x=0
            y+=1
    print(y)
    return bytes(aux)

def get_layer_data(layer, firstgid):
    hexdata = base64.b64decode(layer.find('data').text)
    # convert to bytes (8 bits, not 32)
    data=bytes([])
    for i in range(0,len(hexdata),4):
        j = int.from_bytes(hexdata[i:i+4], byteorder='little', signed=False)
        # shift by first tile ID
        if j!=0: j-=firstgid
        if j<0 or j>255:
            raise Exception("tile index too large: {} at {}/{}".format(j,int(i/4)%128,int(round(i/4/128,0))))
        data += bytes([j])
    return data

def export_layer(layer):
    data = get_layer_data(layer, 1)
    lzw = lzw_encode(data)

    # orig = lzw_decode(lzw)
    print("lzw size:{} bytes -> {} bytes".format(len(lzw),len(data)))

    # compressed length (2 bytes)
    l = "{:04x}".format(len(lzw))
    # swap bytes
    s = l[2:4] + l[0:2]
    # data
    for b in lzw:
        s += "{:02x}".format(b)
    return s

def export_npcs(layer):
    print("exporting level metadata")
    data = get_layer_data(layer, 257)
    x = 0
    y = 0
    s = ""
    n = 0
    for b in data:
        if b!=0:
            # byte layout
            # actor ID
            # x
            # y
            # angle
            # print("found: {} at: {}/{}".format(b,x,y))
            s += "{:02x}{:02x}{:02x}{:02x}".format(b,x,y,0)
            n += 1
        x += 1
        if(x>127):
            x=0
            y+=1
    # length (2 bytes)
    l = "{:04x}".format(n)
    # swap bytes
    return l[2:4] + l[0:2] + s

level_data = xml.parse(os.path.join(local_dir,'level.tmx'))

# shared across all levels 
actors = level_data.getroot().find("./layer[@name='actors']")
if actors is None:
    raise Exception("Unable to find 'actors' layer.")

# export levels
levels = {group.get('name'):group for group in level_data.getroot().findall("./group[@name='levels']/group")}
print(levels)
for i in range(len(levels)):
    name = "level{}".format(i+1)
    print("Exporting level: {} / {}".format(name,len(levels)))
    # pick correct level
    group = levels[name]
    s = export_layer(group.find("layer[@name='level']"))
    s += export_layer(actors)
    s += export_npcs(group.find("layer[@name='npcs']"))

    #if len(map_data)>0:
    print("__map__")
    print(re.sub("(.{256})", "\\1\n", s, 0, re.DOTALL))
