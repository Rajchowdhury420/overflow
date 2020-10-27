# Overflow
A command-line tool for exploiting OSCP-like buffer overflows.

## 1. Installation
```sh
$ git clone https://github.com/sradley/overflow.git
$ cd overflow
$ cabal update
$ cabal install
```

## 2. Usage
```sh
$ overflow --help
```
```
A command-line tool for exploiting OSCP-like buffer overflows.

Usage: overflow fuzz | pattern | badchars | exploit

Available options:
  -h,--help                Show this help text

Available commands:
  fuzz                     Finds the approximate length of the buffer.
  pattern                  Sends a cyclic pattern of bytes of specified length
  badchars                 Sends every character from 0x01 to 0xFF
  exploit                  Attempts to execute a specified payload on target
```

### 2.1. Fuzzing 
What if you wanted to find out the length of the target buffer on a server, but
you didn't feel like writing a script for it?

```
$ overflow fuzz 127.0.0.1 4444 -S 100
    ───> Sending 100-byte payload to target...
    ───> Sending 200-byte payload to target...
    ───> Sending 300-byte payload to target...
    ───> Sending 400-byte payload to target...
    ───> Sending 500-byte payload to target...
Done! Length of buffer is in the range (400, 500].
```

### 2.2. Sending a Cyclic Pattern
Say you wanted to find the offset of the EIP register, wouldn't it be great if
you could just send a cyclic pattern of bytes without having to generate it and
send it manually?

```
$ overflow pattern 127.0.0.1 4444 -l 80
    ───> Sending 80-byte cyclic pattern to target.
Success! Finished sending pattern to target
```

```
$ nc -lvp 4444
Connection from 127.0.0.1:48870
Aa0Aa1Aa2Aa3Aa4Aa5Aa6Aa7Aa8Aa9Ab0Ab1Ab2Ab3Ab4Ab5Ab6Ab7Ab8Ab9Ac0Ac1Ac2Ac3Ac4Ac5Ac
```

### 2.3. Finding Bad Characters
```sh
$ overflow badchars --help
```
```
Usage: overflow badchars HOST PORT (-o|--offset OFFSET) [-p|--prefix PREFIX] 
                         [-s|--suffix SUFFIX]
  Sends every character from 0x01 to 0xFF

Available options:
  HOST                     Target machine's IP address
  PORT                     Port the target service is running on
  -o,--offset OFFSET       The offset of the EIP register
  -p,--prefix PREFIX       (optional) Prefix to put before payload
  -s,--suffix SUFFIX       (optional) Suffix to put after payload
  -h,--help                Show this help text
```

### 2.4. Running an Exploit
```sh
$ overflow exploit --help
```
```
Usage: overflow exploit HOST PORT (-o|--offset OFFSET) (-a|--address ADDRESS)
                        (-p|--payload PAYLOAD) [-p|--prefix PREFIX] 
                        [-s|--suffix SUFFIX]
  Attempts to execute a specified payload on target

Available options:
  HOST                     Target machine's IP address
  PORT                     Port the target service is running on
  -o,--offset OFFSET       The offset of the EIP register
  -a,--address ADDRESS     Jump address for executing shellcode
  -p,--payload PAYLOAD     Payload to be executed on target
  -p,--prefix PREFIX       (optional) Prefix to put before payload
  -s,--suffix SUFFIX       (optional) Suffix to put after payload
  -h,--help                Show this help text
```
