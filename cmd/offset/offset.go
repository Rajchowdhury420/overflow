package offset

import (
    "github.com/spf13/cobra"
    "github.com/sradley/overflow/internal/overflow"
    "github.com/sradley/overflow/internal/cli"
)

// arguments taken by the offset command
var (
    query   string
    reverse bool
    length  int
)

// offset command definition
var Offset = &cobra.Command{
    Use:   "offset",
    Short: "finds the offset of a given byte-value within a cyclic pattern",
    Run:   offset,
}

// initialisation function for all arguments taken by the offset command
func Init() {
    // query flag (required)
    Offset.Flags().StringVarP(&query, "query", "q", "",
        "series of bytes to find the offset of within the pattern")
    Offset.MarkFlagRequired("query")

    // reverse flag (optional)
    Offset.Flags().BoolVarP(&reverse, "reverse", "r", false,
        "(optional) reverse the endianness of the query bytes")

    // length flag (optional)
    Offset.Flags().IntVarP(&length, "length", "l", 0,
        "(optional) the length of the cyclic pattern sent to the target")
}

// offset command subroutine
func offset(cmd *cobra.Command, args []string) {
    // print the title card
    cli.OffsetTitle(query, reverse, length)

    // run the offset functionality 
    o := overflow.NewOffset(query, reverse, length)
    o.Run()
}
