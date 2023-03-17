package main

import (
	"log"
	"os"
	"strings"

	"github.com/darkcat013/pw-lab2/cache"
	"github.com/darkcat013/pw-lab2/env"
	httpClient "github.com/darkcat013/pw-lab2/http-client"
)

// json test: https://api.github.com/users/darkcat013/repos
// redirect test: http://tiny.cc/ttc5vz
// cache test: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/User-Agent
// search: lorem ipsum
func main() {

	cache.InitCacheDb("./cache.boltdb")
	argument := os.Args[1]
	switch argument {
	case "-u":
		url := os.Args[2]
		httpClient.PrintBody(httpClient.Request(url))
	case "-s":
		env.LoadEnv()
		query := strings.Join(os.Args[2:], "+")
		httpClient.Search(query)
	default:
		log.Fatal("Unexpected error occured when passing argument " + argument)
	}
}
