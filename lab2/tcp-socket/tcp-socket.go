package tcpsocket

import (
	"bufio"
	"crypto/tls"
	"io"
	"net"
	"net/textproto"
	"net/url"
	"strconv"
	"strings"

	"github.com/darkcat013/pw-lab2/cache"
	"github.com/darkcat013/pw-lab2/constants"
	"github.com/darkcat013/pw-lab2/utils"
)

func MakeHttpRequest(urlString string) (statusCode int, headers textproto.MIMEHeader, body string) {
	requestUrl := parseUrlString(urlString)
	httpRequest := getHttpRequestString(requestUrl)
	httpResponse := makeTcpRequestWithTls(httpRequest, requestUrl)
	return parseHttpResponse(httpResponse)
}

func parseUrlString(urlString string) (parsedUrl *url.URL) {
	if !(strings.HasPrefix(urlString, "http://") || strings.HasPrefix(urlString, "https://")) {
		urlString = "https://" + urlString
	}
	parsedUrl, err := url.Parse(urlString)
	utils.CheckError(err)

	return parsedUrl
}

func getHttpRequestString(requestUrl *url.URL) (request string) {

	request = "GET " + requestUrl.RequestURI() + " HTTP/1.1" + constants.NEW_LINE +
		"Host: " + requestUrl.Host + constants.NEW_LINE +
		"Connection: close" + constants.NEW_LINE +
		"User-Agent: Terminal/1.0" + constants.NEW_LINE +
		"Accept: text/html,text/plain;q=0.9,application/json;q=0.8,*/*;q=0.7" + constants.NEW_LINE

	cache := cache.Get(requestUrl.String())
	if cache.Etag != "" {
		request += "If-None-Match: " + cache.Etag + constants.NEW_LINE
	}

	if cache.LastModified != "" {
		request += "If-Modified-Since: " + cache.LastModified + constants.NEW_LINE
	}
	return request + constants.NEW_LINE
}

func makeTcpRequestWithTls(httpRequest string, requestUrl *url.URL) (httpResponse string) {
	tcpAddr, err := net.ResolveTCPAddr("tcp", requestUrl.Host+":443")
	utils.CheckError(err)
	tcpConn, err := net.DialTCP("tcp", nil, tcpAddr)
	utils.CheckError(err)
	tlsConn := tls.Client(tcpConn, &tls.Config{
		ServerName: requestUrl.Hostname(),
	})

	defer tlsConn.Close()

	err = tlsConn.Handshake()
	utils.CheckError(err)

	_, err = tlsConn.Write([]byte(httpRequest))
	utils.CheckError(err)

	result, err := io.ReadAll(tlsConn)
	utils.CheckError(err)

	return string(result)
}

func parseHttpResponse(httpResponse string) (statusCode int, headers textproto.MIMEHeader, body string) {
	responseHeader, body, _ := strings.Cut(httpResponse, constants.BLANK_LINE)
	statusLine, headersString, _ := strings.Cut(responseHeader, constants.NEW_LINE)

	statusCode, err := strconv.Atoi(strings.Split(statusLine, " ")[1])
	utils.CheckError(err)

	tpReader := textproto.NewReader(bufio.NewReader(strings.NewReader(headersString + constants.BLANK_LINE)))
	headers, err = tpReader.ReadMIMEHeader()
	utils.CheckError(err)

	return statusCode, headers, body
}
