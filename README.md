# OpenGraph Scraper

This package will scrape a URL for OpenGraph Protocol meta data and will present it as JSON through a web server.

## Endpoints
Present the OpenGraph data at the URL:
```bash
$> curl "localhost:8080?url=https://nos.nl/"
{
	"url": "https://nos.nl/",
	"type": "website",
	"image": "https://nos.nl/img/social/nos.jpg?1910111131",
	"description": "NOS.nl - Nieuws, Sport en Evenementen op Radio, TV en Internet",
	"title": "Nederlandse Omroep Stichting"
}
```
