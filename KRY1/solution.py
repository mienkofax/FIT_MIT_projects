#!/usr/bin/python3

import argparse
import sys
from os.path import join, abspath, isfile

SUB = [0, 1, 1, 0, 1, 0, 1, 0]
N_B = 32
N = 8 * N_B

SUB_REV = {
    0: [0, 3, 5, 7],
    1: [1, 2, 4, 6],
}

KEY = 56738875036137836057389961247808140305287205805009902046320671368520784755301


def step(x):
    x = (x & 1) << N+1 | x << 1 | x >> N-1
    y = 0
    for i in range(N):
        y |= SUB[(x >> i) & 7] << i
    return y


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
    results = []

    previous_bits = SUB_REV[y & 1]
    for i in range(1, N):
        results = []
        actual_bits = (y >> i) & 1

        possible = SUB_REV[actual_bits]
        for p in possible:
            for item in previous_bits:
                if p & 0b11 == item >> i:
                    results.append(item | p << i)

        previous_bits = results

    out = []
    for row in results:
        out.append((row >> 1) & ((1 << N) - 1))

    return out


def decode(directory, filename, enable_print):
    cipher2 = read_content(directory, filename)

    keystream_all = b''
    last_key = KEY
    for i in range(0, len(cipher2)//N_B + 1):
        keystream_all += last_key.to_bytes(N_B, byteorder="little")
        last_key = step(last_key)

    cipher2 = read_content(directory, filename)
    out = b''
    for i in range(0, len(cipher2)):
        out += bytes([keystream_all[i] ^ cipher2[i]])

    if enable_print:
        print("".join(map(chr, out)))

    return out


def help_files(directory):
    f = open('hint.gif', 'wb')
    f.write(decode(directory, 'hint.gif.enc', False))
    f.close()

    decode(directory, 'super_cipher.py.enc', True)


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument('directory')
    args = argument_parser.parse_args()

    # help_files(args.directory)

    key = decode_super_cipher_py(args.directory, False)
    for _ in range(N // 2):
        for res in step_rev(key):
            if step(res) == key:
                key = res
                break

    res = key.to_bytes(N_B, 'little')
    print("".join(map(chr, res[:29])), end='')


if __name__ == '__main__':
    main()
