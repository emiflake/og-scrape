# OpenGraph Scraper

This package will scrape a URL for OpenGraph Protocol meta data and will present it as JSON through a web server.

## Endpoints

Present the OpenGraph data at the URL:

```bash
$> curl "localhost:8080?url=https://nos.nl/" -s | jq .
{
  "image": "https://nos.nl/img/social/nos.jpg?1910141638",
  "url": "https://nos.nl/",
  "image:height": "630",
  "title": "Nederlandse Omroep Stichting",
  "type": "website",
  "image:width": "1200",
  "description": "NOS.nl - Nieuws, Sport en Evenementen op Radio, TV en Internet"
}
```
