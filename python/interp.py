import sys

ptr = 0
memory = [0]
readcnt = 0
whilemark = []

if len(sys.argv) == 2:
    fname = sys.argv[1]
    with open(fname) as f:
        program = f.read()
    while readcnt < len(program):
        c = program[readcnt]
        if c == ">":
            if ptr < 1000000:
                ptr += 1
                while len(memory) <= ptr:
                    memory.append(0)
            else:
                raise Exception("ptr is too large")
        elif c == "<":
            if ptr > 0:
                ptr -= 1
            else:
                raise Exception("ptr must be positive value")
        elif c == "+":
            memory[ptr] += 1
        elif c == "-":
            memory[ptr] -= 1
        elif c == ".":
            print(chr(memory[ptr]))
        elif c == "[":
            if memory[ptr] == 0:
                wcnt = 1
                readcnt += 1
                while wcnt > 0:
                    if readcnt >= len(program):
                        raise Exception("] isn't found.")
                    if program[readcnt] == "[":
                        wcnt += 1
                    elif program[readcnt] == "]":
                        wcnt -= 1
                    readcnt += 1
            else:
                whilemark.append(readcnt)
        elif c == "]":
            if memory[ptr] != 0:
                readcnt = whilemark[-1]
            else:
                whilemark.pop(len(whilemark)-1)
        readcnt += 1
