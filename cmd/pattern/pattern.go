package pattern

import (
    "fmt"
    "github.com/spf13/cobra"
)

var (
    host   string
    port   int
    length int
    pref   string
    suff   string
)

var Pattern = &cobra.Command{
    Use:   "pattern",
    Short: "Sends a cyclic pattern of bytes of specified length",
    Run:   pattern,
}

func Init() {
    // Host flag (required).
    Pattern.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Pattern.MarkFlagRequired("host")

    // Port flag (required).
    Pattern.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Pattern.MarkFlagRequired("port")

    // Prefix and suffix flags (optional).
    Pattern.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Pattern.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // Length flag (required).
    Pattern.Flags().IntVarP(&length, "length", "l", 0,
        "the length of the cyclic pattern sent to the target")
    Pattern.MarkFlagRequired("length")
}

func pattern(cmd *cobra.Command, args []string) {
    fmt.Println("Pattern!")
}
