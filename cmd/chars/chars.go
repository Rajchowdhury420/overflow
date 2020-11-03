package chars

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the chars command
var (
    addr    string
    port    int
    offset  int
    exclude string
    tmpl    string
)

// chars command definition
var Chars = &cobra.Command{
    Use:   "chars",
    Short: "Sends every character from 0x00 to 0xFF, can specify exclusions",
    Run:   chars,
}

// initialisation function for all arguments taken by the chars command
func Init() {
    // host flag (required)
    Chars.Flags().StringVarP(&addr, "addr", "a", "",
        "the target machine's IP address")
    Chars.MarkFlagRequired("addr")

    // port flag (required)
    Chars.Flags().IntVarP(&port, "port", "p", 0,
        "the port the target service is running on")
    Chars.MarkFlagRequired("port")

    // offset flag (required)
    Chars.Flags().IntVarP(&offset, "offset", "o", 0,
        "the offset of the EIP register")
    Chars.MarkFlagRequired("offset")

    // exclude flag (optional)
    Chars.Flags().StringVarP(&exclude, "exclude", "e", "",
        "(optional) characters to exclude from payload")

    // template flag (optional)
    Chars.Flags().StringVarP(&tmpl, "template", "t", "{payload}",
        "(optional) template to format payload with, see docs for more info")
}

// chars command subroutine
func chars(cmd *cobra.Command, args []string) {
    // print the title card
    cli.CharsTitle(addr, port, offset, exclude)

    // run the chars functionality
    c := overflow.NewChars(offset, exclude)
    c.Run(overflow.NewHost(addr, port), tmpl)
}

