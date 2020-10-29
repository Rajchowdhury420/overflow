package cmd

import (
    "os"
    "github.com/spf13/cobra"
    "overflow/cmd/fuzz"
    "overflow/cmd/pattern"
    "overflow/cmd/chars"
    "overflow/cmd/exploit"
)

var root = &cobra.Command{
    Use:          "overflow",
    SilenceUsage: true,
}

func Execute() {
    if err := root.Execute(); err != nil {
        os.Exit(1)
    }
}

func init() {
    // Initialise fuzz subcommand.
    root.AddCommand(fuzz.Fuzz)
    fuzz.Init()

    // Initialise pattern subcommand.
    root.AddCommand(pattern.Pattern)
    pattern.Init()

    // Initialise chars subcommand.
    root.AddCommand(chars.Chars)
    chars.Init()

    // Initialise exploit subcommand.
    root.AddCommand(exploit.Exploit)
    exploit.Init()
}

