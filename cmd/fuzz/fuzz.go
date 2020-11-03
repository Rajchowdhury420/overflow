package fuzz

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the fuzz command
var (
    addr string
    port int
    step int
    wait int
    tmpl string
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
    Fuzz.Flags().StringVarP(&addr, "addr", "a", "",
        "the target machine's IP address")
    Fuzz.MarkFlagRequired("addr")

    // port flag (required)
    Fuzz.Flags().IntVarP(&port, "port", "p", 0,
        "the port the target service is running on")
    Fuzz.MarkFlagRequired("port")

    // step size flag (required)
    Fuzz.Flags().IntVarP(&step, "step", "S", 0,
        "the length by which each subsequent buffer is increased")
    Fuzz.MarkFlagRequired("step")

    // wait time flag (optional)
    Fuzz.Flags().IntVarP(&wait, "wait", "w", 1000,
        "(optional) the time to wait in between messages in milliseconds")

    // template flag (optional)
    Fuzz.Flags().StringVarP(&tmpl, "template", "t", "{payload}",
        "(optional) template to format payload with, see docs for more info")
}

// fuzz command subroutine
func fuzz(cmd *cobra.Command, args []string) {
    // print the title card
    cli.FuzzTitle(addr, port, step, wait)

    // run the fuzz functionality
    f := overflow.NewFuzz(step, wait)
    f.Run(overflow.NewHost(addr, port), tmpl)
}

