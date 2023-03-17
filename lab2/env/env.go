package env

import (
	"log"
	"os"

	"github.com/darkcat013/pw-lab2/constants"
	_ "github.com/joho/godotenv/autoload"
)

var GoogleApiKey string
var SearchEngineId string

func LoadEnv() {
	GoogleApiKey = os.Getenv(constants.GOOGLE_API_KEY)
	if GoogleApiKey == "" {
		log.Fatal("Could not load Google api key or key is empty in .env")
	}
	SearchEngineId = os.Getenv(constants.SEARCH_ENGINE_ID)
	if SearchEngineId == "" {
		log.Fatal("Could not load Search Engine Id or Id is empty in .env")
	}
}
