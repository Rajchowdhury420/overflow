package fuzz

import (
    "github.com/spf13/cobra"
    "overflow/internal/overflow"
)

var (
    host string
    port int
    step int
    pref string
    suff string
)

var Fuzz = &cobra.Command{
    Use:   "fuzz",
    Short: "Finds the approximate length of the target buffer",
    Run:   fuzz,
}

func Init() {
    // Host flag (required).
    Fuzz.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Fuzz.MarkFlagRequired("host")

    // Port flag (required).
    Fuzz.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Fuzz.MarkFlagRequired("port")

    // Prefix and suffix flags (optional).
    Fuzz.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Fuzz.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // Step size flag (required).
    Fuzz.Flags().IntVarP(&step, "step", "S", 0,
        "the length by which each subsequent buffer is increased")
    Fuzz.MarkFlagRequired("step")
}

func fuzz(cmd *cobra.Command, args []string) {
    overflow.Fuzz(host, port, step, pref, suff)
}
