package fuzz

import (
    "github.com/spf13/cobra"
    "overflow/internal/overflow"
    "overflow/internal/cli"
)

// arguments taken by the fuzz command
var (
    host string
    port int
    step int
    pref string
    suff string
)

// fuzz command definition
var Fuzz = &cobra.Command{
    Use:   "fuzz",
    Short: "Finds the approximate length of the target buffer",
    Run:   fuzz,
}

// initialisation function for all arguments taken by the fuzz command
func Init() {
    // host flag (required)
    Fuzz.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Fuzz.MarkFlagRequired("host")

    // port flag (required)
    Fuzz.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Fuzz.MarkFlagRequired("port")

    // prefix and suffix flags (optional)
    Fuzz.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Fuzz.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // step size flag (required)
    Fuzz.Flags().IntVarP(&step, "step", "S", 0,
        "the length by which each subsequent buffer is increased")
    Fuzz.MarkFlagRequired("step")
}

// fuzz command subroutine
func fuzz(cmd *cobra.Command, args []string) {
    // print the title card
    cli.FuzzTitle(host, port, step)

    // run the fuzz functionality
    overflow.Fuzz(host, port, step, pref, suff)
}

