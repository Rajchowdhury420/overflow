package chars

import (
    "fmt"
    "github.com/spf13/cobra"
)

var (
    host    string
    port    int
    offset  int
    exclude string
    pref    string
    suff    string
)

var Chars = &cobra.Command{
    Use:   "chars",
    Short: "Sends every character from 0x00 to 0xFF, can specify exclusions",
    Run:   chars,
}

func Init() {
    // Host flag (required).
    Chars.Flags().StringVarP(&host, "host", "H", "",
        "the target machine's IP address")
    Chars.MarkFlagRequired("host")

    // Port flag (required).
    Chars.Flags().IntVarP(&port, "port", "P", 0,
        "the port the target service is running on")
    Chars.MarkFlagRequired("port")

    // Prefix and suffix flags (optional).
    Chars.Flags().StringVarP(&pref, "prefix", "p", "",
        "(optional) prefix to put before payload")
    Chars.Flags().StringVarP(&suff, "suffix", "s", "",
        "(optional) suffix to put after payload")

    // Offset flag (required).
    Chars.Flags().IntVarP(&offset, "offset", "o", 0,
        "the offset of the EIP register")
    Chars.MarkFlagRequired("offset")

    // Exclude flag (optional).
    Chars.Flags().StringVarP(&exclude, "exclude", "e", "",
        "(optional) characters to exclude from payload")
}

func chars(cmd *cobra.Command, args []string) {
    fmt.Println("Chars!")
}
