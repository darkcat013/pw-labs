package cache

import (
	"encoding/json"
	"net/textproto"
	"os"
	"strings"
	"time"

	"github.com/boltdb/bolt"
	"github.com/darkcat013/pw-lab2/utils"
)

var fileMode os.FileMode = 0600 // owner can read and write
var db *bolt.DB
var cacheBucket = []byte("cacheBucket")

type Cache struct {
	Etag         string
	LastModified string
	ContentType  string
	Content      string
}

func InitCacheDb(path string) {
	var err error
	db, err = bolt.Open(path, fileMode, &bolt.Options{Timeout: 10 * time.Second})
	utils.CheckError(err)

	err = db.Update(func(tx *bolt.Tx) error {
		_, err := tx.CreateBucketIfNotExists(cacheBucket)
		if err != nil {
			return err
		}
		return nil
	})
	utils.CheckError(err)
}

func Get(url string) Cache {
	var value []byte

	err := db.View(func(tx *bolt.Tx) error {
		b := tx.Bucket(cacheBucket)
		v := b.Get([]byte(url))
		if v != nil {
			value = append(value, v...)
		}
		return nil
	})

	utils.CheckError(err)

	returnValue := Cache{}
	if value == nil {
		return returnValue
	}
	err = json.Unmarshal(value, &returnValue)
	utils.CheckError(err)

	return returnValue
}

func Put(url string, value Cache) {
	byteValue, err := json.Marshal(value)
	utils.CheckError(err)
	err = db.Update(func(tx *bolt.Tx) error {
		b := tx.Bucket(cacheBucket)
		err := b.Put([]byte(url), byteValue)
		return err
	})

	utils.CheckError(err)
}

func UpdateCache(urlString string, headers textproto.MIMEHeader, body string) {
	cacheControlAttributes := headers["Cache-Control"]

	for _, v := range cacheControlAttributes {
		if v == "no-store" {
			return
		}
	}

	currentCache := Get(urlString)

	contentType := strings.Split(headers.Get("Content-Type"), " ")[0]
	etagValue := headers.Get("Etag")
	lastModifiedValue := headers.Get("Last-Modified")
	if lastModifiedValue == "" {
		lastModifiedValue = currentCache.LastModified
	}
	Put(urlString, Cache{Etag: etagValue, ContentType: contentType, Content: body, LastModified: lastModifiedValue})
}
