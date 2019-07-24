@echo off
call ..\env.bat

if not exist TestGC.class (
  echo  Compiling TestGC.java
  javac TestGC.java
)

java TestGC 100000

pause
