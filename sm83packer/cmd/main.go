package main

import (
	"flag"
	"os"
)

func main() {
	decompress := flag.Bool("d", false, "decompress the input rather than compressing it")
	flag.Parse()
	infile := flag.Arg(0)
	outfile := flag.Arg(1)
	if infile == "" || outfile == "" {
		flag.PrintDefaults()
		os.Exit(1)
	}
}
