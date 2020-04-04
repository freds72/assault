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

def export_layer(layers, name):
    layer = layers[name]
    if layer is None:
        raise Exception("Layer: {} not found.".format(name))

    print("exporting: {}".format(name))
    hexdata = base64.b64decode(layer.find('data').text)
    # convert to bytes (8 bits, not 32)
    data=bytes([])
    for i in range(0,len(hexdata),4):
        j = int.from_bytes(hexdata[i:i+4], byteorder='little', signed=False)
        # Tiled uses 0 to indicate no tile
        if j!=0: j-=1
        if j>255:
            raise Exception("tile index too large: {}".format(j))
        data += bytes([j])
    
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

level_data = xml.parse(os.path.join(local_dir,'level.tmx'))
layers = {layer.get('name'):layer for layer in level_data.getroot().findall('./layer')}

s = export_layer(layers, 'level')
s += export_layer(layers, 'actors')

#map_data=hexdata
#if len(map_data)>0:
print("__map__")
print(re.sub("(.{256})", "\\1\n", s, 0, re.DOTALL))
