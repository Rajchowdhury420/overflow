package pattern

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the pattern command
var (
    host   string
    port   int
    length int
    pref   string
    suff   string
)

// pattern command definition
var Pattern = &cobra.Command{
    Use:   "pattern",
    Short: "Sends a cyclic pattern of bytes of specified length",
    Run:   pattern,
}

// initialisation function for all arguments taken by the pattern command
func Init() {
    // host flag (required)
    Pattern.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Pattern.MarkFlagRequired("host")

    // port flag (required)
    Pattern.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Pattern.MarkFlagRequired("port")

    // prefix and suffix flags (optional)
    Pattern.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Pattern.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // length flag (required)
    Pattern.Flags().IntVarP(&length, "length", "l", 0,
        "the length of the cyclic pattern sent to the target")
    Pattern.MarkFlagRequired("length")
}

// pattern command subroutine
func pattern(cmd *cobra.Command, args []string) {
    // print the title card
    cli.PatternTitle(host, port, length)

    // run the pattern functionality
    overflow.Pattern(host, port, length, pref, suff)
}

