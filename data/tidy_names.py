#!/usr/bin/env python3

import random


def cleanfile(filename):
    with open(filename, 'r') as f:
        mnames = f.readlines()
        with open("clean_" + filename, 'w') as g:
            g.writelines("{}\n".format(n.strip()) for n in mnames)

cleanfile("givenname.txt")
cleanfile("surname.txt")

