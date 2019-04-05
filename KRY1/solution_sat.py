import argparse
import sys
from os.path import join, abspath, isfile
from z3 import *

SUB = [0, 1, 1, 0, 1, 0, 1, 0]
N_B = 32
N = 8 * N_B


def error(message, exit_code):
    print(message, file=sys.stderr)
    exit(exit_code)


def read_content(path, file):
    abs_path = join(abspath(path), file)

    if not isfile(abs_path):
        error('file ' + path + ' not found', 1)

    return open(abs_path, 'rb').read()


def decode_super_cipher_py(directory, enable_print):
    plaintext = read_content(directory, 'bis.txt')
    cipher = read_content(directory, 'bis.txt.enc')

    keystream = b''
    for i in range(0, len(plaintext)):
        keystream += bytes([plaintext[i] ^ cipher[i]])

    cipher2 = read_content(directory, 'super_cipher.py.enc')
    for i in range(0, len(keystream)):
        cipher2 += bytes([keystream[i] ^ cipher2[i]])

    if enable_print:
        print("".join(map(chr, cipher2)))

    return int.from_bytes(keystream[:32], 'little')


def get_form_for_bit(iBit, var0, var1, var2):
    if iBit == 1:
        res1 = And(Not(var2),Not(var1), var0)       #1 = 001
        res2 = And(Not(var0), Or(var1, var2))       #2,4,6 = xx0, at least one x is has to be 1
    else:
        res1 = And(Not(var2),Not(var1), Not(var0))  #0 = 000
        res2 = And(var0, Or(var1, var2))            #3,5,7 = xx1, at least one x is has to be 1

    res = Or(res1, res2)
    return res


def sat_to_int(satList, varList):
    retInt = 0

    for i in range(N):
        if satList[varList[i]] == True:
            retInt |= (1 << i)

    firstBit = (retInt & 1) << (N-1)
    retInt = firstBit | (retInt >> 1)
    return retInt


def step_rev(y):
    varList = []
    for i in range(N):
        varList.append(Bool('v' + str(i)))

    s = Solver()

    for i in range(N):
        iBit = (y >> i) & 1
        varForm = get_form_for_bit(iBit, varList[i], varList[(i+1)%N], varList[(i+2)%N])
        s.add(varForm)

    s.check()
    satList = s.model()
    x = sat_to_int(satList, varList)
    return x


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument('directory')

    args = argument_parser.parse_args()
    directory_path = args.directory

    keystream = decode_super_cipher_py(directory_path, False)

    for i in range(N//2):
        keystream = step_rev(keystream)

    key = keystream.to_bytes(N_B, 'little').decode('ascii')
    print(key)


if __name__ == '__main__':
    main()
