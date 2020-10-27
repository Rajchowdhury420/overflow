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
```sh
$ overflow fuzz ...
```
```sh
Usage: overflow fuzz HOST PORT [-p|--prefix PREFIX] [-s|--suffix SUFFIX]
  Finds the approximate length of the buffer.

Available options:
  HOST                     Target machine's IP address
  PORT                     Port the target service is running on
  -p,--prefix PREFIX       (optional) Prefix to put before payload
  -s,--suffix SUFFIX       (optional) Suffix to put after payload
  -h,--help                Show this help text
```

### 2.2. Sending a Cyclic Pattern
```sh
$ overflow pattern ...
```
```sh
Usage: overflow pattern HOST PORT (-l|--length LENGTH) [-p|--prefix PREFIX] 
                        [-s|--suffix SUFFIX]
  Sends a cyclic pattern of bytes of specified length

Available options:
  HOST                     Target machine's IP address
  PORT                     Port the target service is running on
  -l,--length LENGTH       Length of the cyclic pattern to send
  -p,--prefix PREFIX       (optional) Prefix to put before payload
  -s,--suffix SUFFIX       (optional) Suffix to put after payload
  -h,--help                Show this help text
```

### 2.3. Finding Bad Characters
```sh
$ overflow badchars ...
```
```sh
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
$ overflow exploit ...
```
```sh
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
