package cmd

import (
    "os"
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/cmd/fuzz"
    "github.com/sradley/overflow/cmd/pattern"
    "github.com/sradley/overflow/cmd/chars"
    "github.com/sradley/overflow/cmd/exploit"
)

// root command definition
var root = &cobra.Command{
    Use:          "overflow",
    SilenceUsage: true,
}

// cobra root execution function
func Execute() {
    if err := root.Execute(); err != nil {
        os.Exit(1)
    }
}

// initialisation function for all subcommands
func init() {
    // initialise fuzz subcommand
    root.AddCommand(fuzz.Fuzz)
    fuzz.Init()

    // initialise pattern subcommand
    root.AddCommand(pattern.Pattern)
    pattern.Init()

    // initialise chars subcommand
    root.AddCommand(chars.Chars)
    chars.Init()

    // initialise exploit subcommand
    root.AddCommand(exploit.Exploit)
    exploit.Init()
}

