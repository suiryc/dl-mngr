#!/usr/bin/python
# -*- coding: utf-8 -*-

import argparse
import asyncio
import os
import subprocess
import sys

# Notes:
# For simplicity, only handle python 3(.7) features.

# Python 2 (at least on Windows):
# Python 2 converts \r to \r\n when writing to stdout/stderr. To workaround
# this, they must be forced to binary mode.
# See: https://stackoverflow.com/a/2374507
#
# We can write bytes to stdout/stderr in Python 2.
# Python 3 expects strings, but we can write to the underlying buffer (which
# does not exist in Python 2).
# See: https://docs.python.org/3/library/sys.html#sys.stdout

# Windows:
# If we run inside a Job object (e.g. when Firefox launches a native
# application), spawn children are part of it, and usually when done all the
# attached processes are terminated.
# To break out of the Job, the CREATE_BREAKAWAY_FROM_JOB CreateProcess creation
# flag is needed, but is only defined in python 3.7.
#
# Use 'javaw' to prevent a (visible) cmd console to be created.

# Linux:
# Use the Java 10 binary explicitly.
# Passing a non-0 creationflags may not work in Popen (without giving error).

if 'CREATE_BREAKAWAY_FROM_JOB' not in dir( subprocess ):
  subprocess.CREATE_BREAKAWAY_FROM_JOB = 0x01000000

dirScript = os.path.dirname(os.path.realpath(sys.argv[0]))
# Use the script folder as working directory
os.chdir(dirScript)


parser = argparse.ArgumentParser()
parser.add_argument('arguments', nargs='*',
  help='dl-mngr arguments')

args = parser.parse_args()


javaExe = '/usr/lib/jvm/java-10-jdk/jre/bin/java'
creationflags = 0
if sys.platform.startswith('win'):
  javaExe = 'javaw'
  creationflags = subprocess.CREATE_BREAKAWAY_FROM_JOB
  # See: https://docs.python.org/3/library/asyncio-platforms.html#asyncio-windows-subprocess
  asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())
  # Apparently equivalent to explicitly setting the loop
  # See: https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.ProactorEventLoop
  #loop = asyncio.ProactorEventLoop()
  #asyncio.set_event_loop(loop)

classpath = os.pathsep.join([dirScript, os.path.join(dirScript, 'dl-mngr-assembly-0.0.1-SNAPSHOT.jar')])
cmd = [javaExe, '-Xms16M', '-Xmx64M', '-XX:MaxRAM=64M', '-cp', classpath, 'suiryc.dl.mngr.Main'] + args.arguments

# In simple cases 'Popen' and 'communicate' is the way to go. But it waits for
# the process to terminate, which is not what we want (especially for the first
# unique instance which needs to remain running).
# An alternative is to 'read'/'readline' instead of 'communicate'. But since
# it is blocking, it may not work smoothly when only one stream is actually
# producing data.
# The best solution is to start an async process and process stdout/stderr
# asynchronously (on lines, for better interleaving).

def sysWrite(stream, b):
  stream.buffer.write(b)
  stream.flush()

async def processStream(src, dst):
  while True:
    line = await src.readline()
    if not line: break
    sysWrite(dst, line)

async def start():
  process = await asyncio.create_subprocess_exec(*cmd, stdin=None, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE, creationflags=creationflags)
  await asyncio.gather(processStream(process.stdout, sys.stdout), processStream(process.stderr, sys.stderr))

# Note:
# Usually we would 'asyncio.run()'.
# Alternatively (more explicitly) 'loop.run_until_complete()' then 'loop.close()'.
# But closing the loop kills the subprocess. So don't do it.
asyncio.get_event_loop().run_until_complete(start())
