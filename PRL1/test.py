import subprocess
import sys
import copy


largeTests = [
    ("50", "2"),
    ("100", "2"),
    ("100", "3"),
    ("200", "3"),
    ("600", "3"),
    ("800", "4"),
    ("10000", "1"),
    ("10000", "2"),
    ("10000", "3"),
    ("20000", "4"),
    ("30000", "4"),
    ("40000", "4"),
    ("50000", "4"),
    ("100000", "4"),
    ("100000", "8"),
    ("100000", "9"),
    ("100000", "10"),
]

cnt = 0
okcnt = 0


def comp(list1, list2):
    if len(list1) != len(list2):
        return False
    for i in range(len(list1)):
        if list1[i] != list2[i]:
            return False
    return True


for i in range(1, 8):
    for j in range(1, 8):
        out = subprocess.check_output(["./test.sh",
                                       str(i),
                                       str(j)]).decode('utf-8').split("\n")[:-1]
        inp = list(map(int, out[0].split(" ")))
        org_inp = copy.deepcopy(inp)
        inp.sort()
        # print (out)
        out = list(map(int, out[1:]))
        res = comp(inp, out)
        msg = "FAIL"
        cnt += 1
        if res:
            msg = "OK"
            okcnt += 1
        else:
            print("Testing {0} {1}".format(i, j))
            print("-----Original Input-----")
            print(org_inp)
            print("-------Your Output------")
            print(out)
            sys.exit(1)
        print(str(i) + "\t" + str(j) + "\t" + msg)


for (i, j) in largeTests:
    out = subprocess.check_output(["./test.sh", i, j]).decode('utf-8').split("\n")[:-1]
    inp = list(map(int, out[0].split(" ")))
    org_inp = copy.deepcopy(inp)
    inp.sort()
    out = list(map(int, out[1:]))
    res = comp(inp, out)
    msg = "FAIL"
    cnt += 1
    if res:
        msg = "OK"
        okcnt += 1
    else:
        print("Testing {0} {1}".format(i, j))
        print("-----Original Input-----")
        print(org_inp)
        print("-------Your Output------")
        print(out)
        sys.exit(1)
    print(str(i) + "\t" + str(j) + "\t" + msg)

for _ in range(10):
    out = subprocess.check_output(["./test.sh", "7", "6"]).decode('utf-8').split("\n")[:-1]
    inp = list(map(int, out[0].split(" ")))
    org_inp = copy.deepcopy(inp)
    inp.sort()
    out = list(map(int, out[1:]))
    res = comp(inp, out)
    msg = "FAIL"
    cnt += 1
    if res:
        msg = "OK"
        okcnt += 1
    else:
        print("Testing {0} {1}".format(i, j))
        print("-----Original Input-----")
        print(org_inp)
        print("-------Your Output------")
        print(out)
        sys.exit(1)
    print("7" + "\t" + "6" + "\t" + msg)

for _ in range(10):
    out = subprocess.check_output(["./test.sh", "6", "5"]).decode('utf-8').split("\n")[:-1]
    inp = list(map(int, out[0].split(" ")))
    org_inp = copy.deepcopy(inp)
    inp.sort()
    out = list(map(int, out[1:]))
    res = comp(inp, out)
    msg = "FAIL"
    cnt += 1
    if res:
        msg = "OK"
        okcnt += 1
    else:
        print("Testing {0} {1}".format(i, j))
        print("-----Original Input-----")
        print(org_inp)
        print("-------Your Output------")
        print(out)
        sys.exit(1)
    print("6" + "\t" + "5" + "\t" + msg)

for _ in range(10):
    out = subprocess.check_output(["./test.sh", "5", "4"]).decode('utf-8').split("\n")[:-1]
    inp = list(map(int, out[0].split(" ")))
    org_inp = copy.deepcopy(inp)
    inp.sort()
    out = list(map(int, out[1:]))
    res = comp(inp, out)
    msg = "FAIL"
    cnt += 1
    if res:
        msg = "OK"
        okcnt += 1
    else:
        print("Testing {0} {1}".format(i, j))
        print("-----Original Input-----")
        print(org_inp)
        print("-------Your Output------")
        print(out)
        sys.exit(1)
    print("5" + "\t" + "4" + "\t" + msg)

print("Score: " + str(okcnt) + "/" + str(cnt))
if okcnt == cnt:
    print("Well done!")
else:
    print("Some tests are failing!")
