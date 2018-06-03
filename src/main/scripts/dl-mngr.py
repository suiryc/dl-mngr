#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
import os
import subprocess
import sys

# Note: on Windows if we run inside a Job object (e.g. when Firefox launches a
# native application), spawn children are part of it, and usually when done all
# the attached processes are terminated.
# To break out of the Job, the CREATE_BREAKAWAY_FROM_JOB CreateProcess creation
# flag is needed, but is only defined in python 3.7.
if 'CREATE_BREAKAWAY_FROM_JOB' not in dir( subprocess ):
  subprocess.CREATE_BREAKAWAY_FROM_JOB = 0x01000000

dirScript = os.path.dirname(os.path.realpath(sys.argv[0]))


parser = argparse.ArgumentParser()
parser.add_argument('--background', action='store_true', default=False,
  help='whether to start process in the background')
parser.add_argument('arguments', nargs='*',
  help='dl-mngr arguments')

args = parser.parse_args()


javaExe = 'java'
creationflags = 0
if sys.platform.startswith('win') and args.background:
  javaExe = 'javaw'
  creationflags = 1

classpath = os.pathsep.join([dirScript, os.path.join(dirScript, 'dl-mngr-assembly-0.0.1-SNAPSHOT.jar')])
cmd = [javaExe, '-Xms16M', '-Xmx64M', '-XX:MaxRAM=64M', '-cp', classpath, 'suiryc.dl.mngr.Main'] + args.arguments

if args.background:
  # When passing a non-0 creationflags value under Linux, Popen may not work
  # (without giving error).
  if (creationflags): creationflags=subprocess.CREATE_BREAKAWAY_FROM_JOB
  out = open(os.path.join(dirScript, 'out.log'), 'wb')
  subprocess.Popen(cmd, stdout=out, stderr=subprocess.STDOUT, creationflags=creationflags)
  out.close()
else:
  subprocess.call(cmd)
