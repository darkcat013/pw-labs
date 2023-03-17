package httpclient

import (
	"encoding/json"
	"fmt"
	"strconv"

	"github.com/darkcat013/pw-lab2/env"
	"github.com/darkcat013/pw-lab2/utils"
)

type SearchResult struct {
	Title string `json:"title"`
	Link  string `json:"link"`
}

type SearchResults struct {
	Items []SearchResult `json:"items"`
}

func Search(query string) {
	apiUrl := "https://www.googleapis.com/customsearch/v1?" +
		"key=" + env.GoogleApiKey +
		"&cx=" + env.SearchEngineId +
		"&q=" + query +
		"&num=10" +
		"&gl=md"
	_, body := Request(apiUrl)
	results := SearchResults{}
	err := json.Unmarshal([]byte(body), &results)
	utils.CheckError(err)

	fmt.Println("Top 10 search results")
	fmt.Println()
	for i := 0; i < len(results.Items); i++ {
		fmt.Print(i + 1)
		fmt.Println(". " + results.Items[i].Title)
		fmt.Println("    Link: " + results.Items[i].Link)
		fmt.Println()
	}

	n := 0

	for n <= 0 || n > 10 {
		fmt.Print("Type the number of the link you want to access: ")
		var input string
		_, err := fmt.Scanln(&input)
		if err != nil {
			fmt.Println("Try again.")
			continue
		}
		n, err = strconv.Atoi(input)
		if err != nil {
			fmt.Println("Try again.")
			continue
		}
		if n <= 0 || n > 10 {
			fmt.Println("Try again.")
		}
	}

	PrintBody(Request(results.Items[n-1].Link))
}
