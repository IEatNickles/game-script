@ECHO OFF

CD ./build
nasm -fwin64 ./out.asm
gcc ./out.obj
CD ..
CALL .\build\a.exe
