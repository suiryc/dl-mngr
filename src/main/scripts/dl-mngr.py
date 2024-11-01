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
# Use the Java 11 binary explicitly.
# Passing a non-0 creationflags may not work in Popen (without giving error).
# 64M/80M of max memory may cause OOMs in JavaFX prism. 96M appears ok.

if 'CREATE_BREAKAWAY_FROM_JOB' not in dir( subprocess ):
  subprocess.CREATE_BREAKAWAY_FROM_JOB = 0x01000000

dirScript = os.path.dirname(os.path.realpath(sys.argv[0]))
# Use the script folder as working directory
os.chdir(dirScript)


parser = argparse.ArgumentParser()
parser.add_argument('arguments', nargs='*',
  help='dl-mngr arguments')

args = parser.parse_args()


javaExe = '/usr/lib/jvm/java-11-openjdk/bin/java'
creationflags = 0
memMax='96M'
if sys.platform.startswith('win'):
  javaExe = 'javaw'
  creationflags = subprocess.CREATE_BREAKAWAY_FROM_JOB
  memMax='64M'
  # See: https://docs.python.org/3/library/asyncio-platforms.html#asyncio-windows-subprocess
  asyncio.set_event_loop_policy(asyncio.WindowsProactorEventLoopPolicy())
  # Apparently equivalent to explicitly setting the loop
  # See: https://docs.python.org/3/library/asyncio-eventloop.html#asyncio.ProactorEventLoop
  #loop = asyncio.ProactorEventLoop()
  #asyncio.set_event_loop(loop)

classpath = os.pathsep.join([dirScript, os.path.join(dirScript, 'dl-mngr.jar'), os.path.join(dirScript, 'lib', '*')])
# 'java.net.preferIPv6Addresses' (and 'java.net.preferIPv4Stack') is read and
# cached at VM startup. In order to prefer IPv6 addresses (when client and
# server handle both IPv4 and IPv6), the property must be set before starting.
# (System.setProperty even as first executed code does not work)
cmd = [javaExe, '-Xms16M', f'-Xmx{memMax}', f'-XX:MaxRAM={memMax}', '-Djava.net.preferIPv6Addresses=true', '-cp', classpath, 'suiryc.dl.mngr.Main'] + args.arguments

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

async def run():
  process = await asyncio.create_subprocess_exec(*cmd, stdin=None, stdout=asyncio.subprocess.PIPE, stderr=asyncio.subprocess.PIPE, creationflags=creationflags)
  await asyncio.gather(processStream(process.stdout, sys.stdout), processStream(process.stderr, sys.stderr))
  # The spawned process output streams are now closed. Either:
  #  1. The process was not meant (CLI parameters) to keep on running in the
  #     background and is now exiting
  #  2. It was the (first) unique instance, and the process is still running
  #  3. The arguments were passed to the unique instance, we got a response and
  #     the instance we spawned is now exiting
  # Due to 2. we must not indefinitely wait for the return code. Otherwise we
  # usually get it in less than 50ms.
  # Upon timeout, assume we were in case 2. and the command was successful.
  try: return await asyncio.wait_for(process.wait(), 0.2)
  except asyncio.TimeoutError: return 0

async def start(remainingAttempts = 2):
  # Execute the command, and re-try (a limited number of times) if we get a
  # communication error code (100).
  while True:
    returncode = await run()
    remainingAttempts -= 1
    if (returncode == 100) and (remainingAttempts > 0):
      # Wait a bit before the next attempt.
      await asyncio.sleep(0.5)
      continue
    return returncode

# Note:
# Usually we would 'asyncio.run()', or alternatively (more explicitly)
# 'loop.run_until_complete()' then 'loop.close()'.
# But this (closing the loop) kills the subprocess. So don't do it.
# Exit with the received return code (or one we decided).
exit(asyncio.get_event_loop().run_until_complete(start()))
