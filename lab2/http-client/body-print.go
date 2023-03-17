package httpclient

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"net/textproto"

	"github.com/darkcat013/pw-lab2/constants"
	"github.com/darkcat013/pw-lab2/utils"
	"jaytaylor.com/html2text"
)

func PrintBody(headers textproto.MIMEHeader, body string) {
	contentType := GetContentType(headers)
	switch contentType {
	case constants.CONTENT_TYPE_HTML:
		printHTMLBody(body)
	case constants.CONTENT_TYPE_PLAIN:
		printPlainTextBody(body)
	case constants.CONTENT_TYPE_JSON:
		printJsonBody(body)
	default:
		log.Fatal("Unsupported response content type: " + contentType)
	}
}

func printPlainTextBody(body string) {
	fmt.Println(body)
}

func printJsonBody(body string) {
	var prettifiedJson bytes.Buffer
	err := json.Indent(&prettifiedJson, []byte(body), "", "  ")
	utils.CheckError(err)
	fmt.Println(prettifiedJson.String())
}
func printHTMLBody(body string) {
	parsedHtml, err := html2text.FromString(body, html2text.Options{PrettyTables: true, OmitLinks: true, TextOnly: true})
	utils.CheckError(err)
	fmt.Println(parsedHtml)
}
