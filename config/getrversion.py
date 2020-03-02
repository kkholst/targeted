#!/usr/bin/env python3

import argparse
import re

parser = argparse.ArgumentParser()
parser.add_argument('directory',
                    nargs='?',
                    help='Directory containing R-package')
args = parser.parse_args()
if (args.directory is None):
    args.directory = '.'

ver = ''
pkg = ''
file = open(args.directory + '/DESCRIPTION', 'r')
for line in file:
    if re.search("Package:", line):
        pkg = line.split()[1]
    if re.search("Version", line):
        ver = line.split()[1]
        break

print(pkg + '_' + ver + '.tar.gz')
