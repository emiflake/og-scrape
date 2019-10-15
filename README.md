# OpenGraph Scraper

This package will scrape a URL for OpenGraph Protocol meta data and will present it as JSON through a web server.

# Usage
Running the application:
```bash
$> stack run
```

Generating documentation:
```bash
$> stack haddock
```

Running the tests:
```bash
$> stack test
```

## Endpoints

Present the OpenGraph data at the URL:

```bash
$> curl "localhost:8080?url=https://www.youtube.com/watch?v=7SM816P5G9s" -s | jq .
{
  "image": {
    "height": "720",
    "width": "1280",
    "content": "https://i.ytimg.com/vi/7SM816P5G9s/maxresdefault.jpg"
  },
  "url": "https://www.youtube.com/watch?v=7SM816P5G9s",
  "site_name": "YouTube",
  "video": {
    "height": "720",
    "tag": [
      "computer games",
      "computer graphics",
      "honey simulation",
      "the witness",
      "ai",
      "deep learning",
      "two minute papers"
    ],
    "secure_url": "https://www.youtube.com/embed/7SM816P5G9s",
    "width": "1280",
    "content": "https://www.youtube.com/embed/7SM816P5G9s",
    "type": "text/html"
  },
  "title": "🍯 Realistic Honey Simulations, And Quickly - Possible?",
  "type": "video.other",
  "description": "❤️ Check out Weights & Biases here and sign up for a free demo: https://www.wandb.com/papers 📝 The paper \"A Geometrically Consistent Viscous Fluid Solver wit..."
}
```
