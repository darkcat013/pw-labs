package httpclient

import (
	"fmt"
	"net/http"
	"net/textproto"
	"os"
	"strconv"
	"strings"

	"github.com/darkcat013/pw-lab2/cache"
	"github.com/darkcat013/pw-lab2/constants"
	tcpSocket "github.com/darkcat013/pw-lab2/tcp-socket"
)

func Request(urlString string) (headers textproto.MIMEHeader, body string) {

	statusCode, headers, body := tcpSocket.MakeHttpRequest(urlString)
	headers, body, urlString = handleRedirect(statusCode, headers, body, urlString, 0)
	body = handleChunkedEncoding(headers, body)
	cache.UpdateCache(urlString, headers, body)
	return headers, body
}

func handleRedirect(statusCode int, headers textproto.MIMEHeader, body string, urlString string, redirectCount int) (returnHeaders textproto.MIMEHeader, returnBody string, finalUrlString string) {
	if statusCode == http.StatusNotModified {
		cache := cache.Get(urlString)
		headers.Add("Content-Type", cache.ContentType)
		return headers, cache.Content, urlString
	} else if statusCode >= 300 && statusCode <= 308 {

		if redirectCount == constants.MAX_REDIRECTS {
			fmt.Println("Too many redirects, cannot reach desired content.")
			os.Exit(0)
		}
		redirectUrlString := headers.Get("Location")
		fmt.Println("Redirected to: '" + redirectUrlString + "', Status code: " + strconv.Itoa(statusCode))

		nextStatusCode, nextHeaders, nextBody := tcpSocket.MakeHttpRequest(redirectUrlString)

		return handleRedirect(nextStatusCode, nextHeaders, nextBody, redirectUrlString, redirectCount+1)
	}
	return headers, body, urlString
}

func handleChunkedEncoding(headers textproto.MIMEHeader, body string) (returnBody string) {
	if headers.Get("Transfer-Encoding") != "chunked" || GetContentType(headers) != constants.CONTENT_TYPE_JSON {
		return body
	}
	chunkedBody := strings.Split(body, constants.NEW_LINE)
	// skip chunk length and add the content
	for i := 1; i < len(chunkedBody); i += 2 {
		returnBody += chunkedBody[i]
	}
	return returnBody
}

func GetContentType(headers textproto.MIMEHeader) string {
	return strings.FieldsFunc(headers.Get("Content-Type"), func(r rune) bool { return r == ' ' || r == ';' })[0]
}
