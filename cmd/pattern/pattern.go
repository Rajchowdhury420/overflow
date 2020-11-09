package pattern

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the pattern command
var (
    addr   string
    port   int
    length int
    tmpl   string
)

// pattern command definition
var Pattern = &cobra.Command{
    Use:   "pattern",
    Short: "Sends a cyclic pattern of bytes of specified length",
    Run:   pattern,
}

// initialisation function for all arguments taken by the pattern command
func Init() {
    // host flag (optional)
    Pattern.Flags().StringVarP(&addr, "addr", "a", "",
        "(optional) the target machine's IP address")

    // port flag (optional)
    Pattern.Flags().IntVarP(&port, "port", "p", 0,
        "(optional) the port the target service is running on")

    // length flag (required)
    Pattern.Flags().IntVarP(&length, "length", "l", 0,
        "the length of the cyclic pattern sent to the target")
    Pattern.MarkFlagRequired("length")

    // template flag (optional)
    Pattern.Flags().StringVarP(&tmpl, "template", "t", "{payload}",
        "(optional) template to format payload with, see docs for more info")
}

// pattern command subroutine
func pattern(cmd *cobra.Command, args []string) {
    // print the title card
    cli.PatternTitle(addr, port, length)

    // run the pattern functionality
    p := overflow.NewPattern(length)
    p.Run(overflow.NewHost(addr, port), tmpl)
}

