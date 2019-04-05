#!/usr/bin/python3

import argparse
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


def step_rev(y):
    sat_solver = Solver()

    bool_vars = []
    for i in range(0, N + 3):
        bool_vars.append(Bool('bool_{0}'.format(i % N)))

    for k in range(N):
        if (y >> k) & 1 == 1:
            left = And(Not(bool_vars[k + 2]), Not(bool_vars[k + 1]), bool_vars[k])
            right = And(Not(bool_vars[k]), Or(bool_vars[k + 1], bool_vars[k + 2]))
        else:
            left = And(Not(bool_vars[k + 2]), Not(bool_vars[k + 1]), Not(bool_vars[k]))
            right = And(bool_vars[k], Or(bool_vars[k + 1], bool_vars[k + 2]))

        sat_solver.add(Or(left, right))

    if sat_solver.check() != z3.sat:
        error('sat not working', 2)

    sat_result = sat_solver.model()
    result = 0
    for i in range(0, N):
        if sat_result[bool_vars[i]]:
            result |= 1 << i

    return ((result & 1) << (N - 1)) | (result >> 1)


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument('directory')

    args = argument_parser.parse_args()
    directory_path = args.directory

    keystream = decode_super_cipher_py(directory_path, False)

    for _ in range(N//2):
        keystream = step_rev(keystream)

    key = keystream.to_bytes(N_B, 'little').decode('ascii')[:29]
    print(key, end='')


if __name__ == '__main__':
    main()
