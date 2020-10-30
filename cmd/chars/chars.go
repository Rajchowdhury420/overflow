package chars

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the chars command
var (
    host    string
    port    int
    offset  int
    exclude string
    pref    string
    suff    string
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
    Chars.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Chars.MarkFlagRequired("host")

    // port flag (required)
    Chars.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Chars.MarkFlagRequired("port")

    // prefix and suffix flags (optional)
    Chars.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Chars.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // offset flag (required)
    Chars.Flags().IntVarP(&offset, "offset", "o", 0,
        "the offset of the EIP register")
    Chars.MarkFlagRequired("offset")

    // exclude flag (optional)
    Chars.Flags().StringVarP(&exclude, "exclude", "e", "",
        "(optional) characters to exclude from payload")
}

// chars command subroutine
func chars(cmd *cobra.Command, args []string) {
    // print the title card
    cli.CharsTitle(host, port, offset, exclude)

    // run the chars functionality
    overflow.Chars(host, port, offset, exclude, pref, suff)
}

